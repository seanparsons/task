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
import Data.Aeson.TH
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

$(deriveJSON defaultOptions ''Recurrence)

data Task
  = Task
      { _taskDescription :: Text,
        _taskRecurrence :: Maybe Recurrence
      }
  deriving (Eq, Show, Generic)

makeLenses ''Task

$(deriveJSON defaultOptions{fieldLabelModifier = camelCase . drop 5} ''Task)

data TaskStatus
  = TaskStatus
      { _taskStatusTask :: Task,
        _taskStatusDeletedAt :: Maybe UTCTime,
        _taskStatusCompletedAt :: Maybe UTCTime
      }
  deriving (Eq, Show, Generic)

makeLenses ''TaskStatus

$(deriveJSON defaultOptions{fieldLabelModifier = camelCase . drop 5} ''TaskStatus)

type TaskStatuses = V.Vector TaskStatus

data ListOutstandingAction = ListOutstandingAction
  deriving (Eq, Show, Generic)

makeLenses ''ListOutstandingAction

$(deriveJSON defaultOptions ''ListOutstandingAction)

data AddTaskAction
  = AddTaskAction
      { _addTaskActionTask :: Task
      }
  deriving (Eq, Show, Generic)

makeLenses ''AddTaskAction

$(deriveJSON defaultOptions{fieldLabelModifier = camelCase . drop 14} ''AddTaskAction)

data CompleteTaskAction
  = CompleteTaskAction
      { _completeTaskActionIndex :: Int,
        _completeTaskActionTimestamp :: UTCTime
      }
  deriving (Eq, Show, Generic)

makeLenses ''CompleteTaskAction

$(deriveJSON defaultOptions{fieldLabelModifier = camelCase . drop 19} ''CompleteTaskAction)

data DeleteTaskAction
  = DeleteTaskAction
      { _deleteTaskActionIndex :: Int,
        _deleteTaskActionTimestamp :: UTCTime
      }
  deriving (Eq, Show, Generic)

makeLenses ''DeleteTaskAction

$(deriveJSON defaultOptions{fieldLabelModifier = camelCase . drop 17} ''DeleteTaskAction)

data TaskAction
  = ListOutstanding ListOutstandingAction
  | AddTask AddTaskAction
  | CompleteTask CompleteTaskAction
  | DeleteTask DeleteTaskAction
  deriving (Eq, Show, Generic)

makePrisms ''TaskAction

$(deriveJSON defaultOptions ''TaskAction)

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
