{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Core where

import Control.Lens hiding (argument)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lens
import Data.Csv hiding (Parser, header)
import Data.Foldable
import Data.Text hiding (filter, foldl')
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Vector as V
import GHC.Generics hiding (from)
import Options.Applicative hiding (action)
import Prelude hiding (fail)

-- Core --

instance FromField UTCTime where
  parseField textToParse = parseTimeM True defaultTimeLocale rfc822DateFormat $ view (from packedChars) textToParse

instance ToField UTCTime where
  toField utctime = view packedChars $ formatTime defaultTimeLocale rfc822DateFormat utctime

data Recurrence
  = Daily
  | Weekly
  deriving (Eq, Show, Generic)

makePrisms ''Recurrence

instance FromField Recurrence where
  parseField "daily" = pure Daily
  parseField "weekly" = pure Weekly
  parseField _ = mzero

instance ToField Recurrence where
  toField Daily = "daily"
  toField Weekly = "weekly"

data Task
  = Task
      { _taskDescription :: Text,
        _taskRecurrence :: Maybe Recurrence
      }
  deriving (Eq, Show, Generic)

makeLenses ''Task

instance FromRecord Task

instance ToRecord Task

data TaskStatus
  = TaskStatus
      { _taskStatusTask :: Task,
        _taskStatusDeletedAt :: Maybe UTCTime,
        _taskStatusCompletedAt :: Maybe UTCTime
      }
  deriving (Eq, Show, Generic)

makeLenses ''TaskStatus

type TaskStatuses = V.Vector TaskStatus

data ListOutstandingAction = ListOutstandingAction
  deriving (Eq, Show, Generic)

makeLenses ''ListOutstandingAction

instance FromRecord ListOutstandingAction

instance ToRecord ListOutstandingAction

data AddTaskAction
  = AddTaskAction
      { _addTaskActionTask :: Task
      }
  deriving (Eq, Show, Generic)

makeLenses ''AddTaskAction

instance FromRecord AddTaskAction where
  parseRecord vec = fmap AddTaskAction $ parseRecord vec

instance ToRecord AddTaskAction where
  toRecord (AddTaskAction task) = toRecord task

data CompleteTaskAction
  = CompleteTaskAction
      { _completeTaskActionIndex :: Int,
        _completeTaskActionTimestamp :: UTCTime
      }
  deriving (Eq, Show, Generic)

makeLenses ''CompleteTaskAction

instance FromRecord CompleteTaskAction

instance ToRecord CompleteTaskAction

data DeleteTaskAction
  = DeleteTaskAction
      { _deleteTaskActionIndex :: Int,
        _deleteTaskActionTimestamp :: UTCTime
      }
  deriving (Eq, Show, Generic)

makeLenses ''DeleteTaskAction

instance FromRecord DeleteTaskAction

instance ToRecord DeleteTaskAction

data TaskAction
  = ListOutstanding ListOutstandingAction
  | AddTask AddTaskAction
  | CompleteTask CompleteTaskAction
  | DeleteTask DeleteTaskAction
  deriving (Eq, Show, Generic)

makePrisms ''TaskAction

type TaskActions = [TaskAction]

instance FromRecord TaskAction where
  parseRecord vec = case (V.toList vec) of
    ("listoutstanding" : rest) -> fmap ListOutstanding $ parseRecord $ V.fromList rest
    ("addtask" : rest) -> fmap AddTask $ parseRecord $ V.fromList rest
    ("completetask" : rest) -> fmap CompleteTask $ parseRecord $ V.fromList rest
    ("deletetask" : rest) -> fmap DeleteTask $ parseRecord $ V.fromList rest
    _ -> mzero

instance ToRecord TaskAction where
  toRecord (ListOutstanding action) = V.cons "listoutstanding" (toRecord action)
  toRecord (AddTask action) = V.cons "addtask" (toRecord action)
  toRecord (CompleteTask action) = V.cons "completetask" (toRecord action)
  toRecord (DeleteTask action) = V.cons "deletetask" (toRecord action)

decodeActions :: (Monad m, MonadFail m) => BS.ByteString -> m TaskActions
decodeActions bytesToDecode = either fail (pure . V.toList) $ decodeWith defaultDecodeOptions NoHeader bytesToDecode

encodeActions :: TaskActions -> BS.ByteString
encodeActions taskActions = encodeWith defaultEncodeOptions taskActions

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
