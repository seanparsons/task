{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Core where

import Data.Aeson
import Data.Aeson.Casing
import Control.Lens hiding (argument)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Foldable
import Data.Text hiding (filter, foldl', drop)
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import qualified Data.Vector as V
import GHC.Generics hiding (from)
import Options.Applicative hiding (action)
import Prelude hiding (fail)

-- Core --

data Recurrence
  = Daily
  | Weekly
  deriving (Eq, Show, Generic)

makePrisms ''Recurrence

instance ToJSON Recurrence where
   toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Recurrence where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

data Task
  = Task
      { _taskDescription :: Text,
        _taskRecurrence :: Maybe Recurrence
      }
  deriving (Eq, Show, Generic)

makeLenses ''Task

instance ToJSON Task where
  toJSON = genericToJSON $ aesonDrop 5 camelCase

instance FromJSON Task where
  parseJSON = genericParseJSON $ aesonDrop 5 camelCase

data TaskStatus
  = TaskStatus
      { _taskStatusTask :: Task,
        _taskStatusDeletedAt :: Maybe UTCTime,
        _taskStatusCompletedAt :: Maybe UTCTime
      }
  deriving (Eq, Show, Generic)

makeLenses ''TaskStatus

instance ToJSON TaskStatus where
  toJSON = genericToJSON $ aesonDrop 5 camelCase

instance FromJSON TaskStatus where
  parseJSON = genericParseJSON $ aesonDrop 5 camelCase

type TaskStatuses = V.Vector TaskStatus

data ListOutstandingAction = ListOutstandingAction
  deriving (Eq, Show, Generic)

makeLenses ''ListOutstandingAction

instance ToJSON ListOutstandingAction where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON ListOutstandingAction where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data AddTaskAction
  = AddTaskAction
      { _addTaskActionTask :: Task
      }
  deriving (Eq, Show, Generic)

makeLenses ''AddTaskAction

instance ToJSON AddTaskAction where
  toJSON = genericToJSON $ aesonDrop 14 camelCase

instance FromJSON AddTaskAction where
  parseJSON = genericParseJSON $ aesonDrop 14 camelCase

data CompleteTaskAction
  = CompleteTaskAction
      { _completeTaskActionIndex :: Int,
        _completeTaskActionTimestamp :: UTCTime
      }
  deriving (Eq, Show, Generic)

makeLenses ''CompleteTaskAction

instance ToJSON CompleteTaskAction where
  toJSON = genericToJSON $ aesonDrop 19 camelCase

instance FromJSON CompleteTaskAction where
  parseJSON = genericParseJSON $ aesonDrop 19 camelCase

data DeleteTaskAction
  = DeleteTaskAction
      { _deleteTaskActionIndex :: Int,
        _deleteTaskActionTimestamp :: UTCTime
      }
  deriving (Eq, Show, Generic)

makeLenses ''DeleteTaskAction

instance ToJSON DeleteTaskAction where
  toJSON = genericToJSON $ aesonDrop 17 camelCase

instance FromJSON DeleteTaskAction where
  parseJSON = genericParseJSON $ aesonDrop 17 camelCase

data TaskAction
  = ListOutstanding ListOutstandingAction
  | AddTask AddTaskAction
  | CompleteTask CompleteTaskAction
  | DeleteTask DeleteTaskAction
  deriving (Eq, Show, Generic)

makePrisms ''TaskAction

instance ToJSON TaskAction where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON TaskAction where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

type TaskActions = [TaskAction]

decodeActions :: (Monad m, MonadFail m) => BS.ByteString -> m TaskActions
decodeActions bytesToDecode = either fail pure $ traverse eitherDecode $ BS.lines bytesToDecode

encodeActions :: TaskActions -> BS.ByteString
encodeActions taskActions = BS.unlines $ fmap encode taskActions

processTaskAction :: TaskStatuses -> TaskAction -> TaskStatuses
processTaskAction statuses (ListOutstanding _) =
  statuses
processTaskAction statuses (AddTask (AddTaskAction {..})) =
  V.snoc statuses (TaskStatus _addTaskActionTask Nothing Nothing)
processTaskAction statuses (CompleteTask (CompleteTaskAction {..})) =
  set (ix (_completeTaskActionIndex - 1) . taskStatusCompletedAt) (Just _completeTaskActionTimestamp) statuses
processTaskAction statuses (DeleteTask (DeleteTaskAction {..})) =
  set (ix (_deleteTaskActionIndex - 1) . taskStatusDeletedAt) (Just _deleteTaskActionTimestamp) statuses

processTaskActions :: TaskStatuses -> TaskActions -> TaskStatuses
processTaskActions statuses actions = foldl' processTaskAction statuses actions

shouldRecur :: Maybe Recurrence -> Maybe UTCTime -> UTCTime -> Bool
shouldRecur (Just _) Nothing _ = True
shouldRecur Nothing _ _ = False
shouldRecur (Just Daily) (Just lastCompletedAt) now = utctDay lastCompletedAt < utctDay now
shouldRecur (Just Weekly) (Just lastCompletedAt) now =
  let lastCompletedAtWeek = view _2 (toWeekDate $ utctDay lastCompletedAt)
      nowWeek = view _2 (toWeekDate $ utctDay now)
   in lastCompletedAtWeek < nowWeek

isDeleted :: TaskStatus -> Bool
isDeleted = has (taskStatusDeletedAt . _Just)

isOutstanding :: UTCTime -> TaskStatus -> Bool
isOutstanding now status =
  let lastCompletedAt = firstOf (taskStatusCompletedAt . _Just) status
      notComplete = has (taskStatusCompletedAt . _Nothing) status
      deleted = isDeleted status
      recurringAt = firstOf (taskStatusTask . taskRecurrence . _Just) status
   in not deleted && (notComplete || shouldRecur recurringAt lastCompletedAt now)
