{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import Data.Generics.Product

-- Core --

data Recurrence
  = Daily
  | Weekly
  deriving (Eq, Show, Generic)

instance ToJSON Recurrence where
   toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Recurrence where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

data Task
  = Task
      { description :: Text,
        recurrence :: Maybe Recurrence
      }
  deriving (Eq, Show, Generic)

instance ToJSON Task where
  toJSON = genericToJSON defaultOptions 

instance FromJSON Task where
  parseJSON = genericParseJSON defaultOptions

data TaskStatus
  = TaskStatus
      { task :: Task,
        deletedAt :: Maybe UTCTime,
        completedAt :: Maybe UTCTime
      }
  deriving (Eq, Show, Generic)

instance ToJSON TaskStatus where
  toJSON = genericToJSON defaultOptions

instance FromJSON TaskStatus where
  parseJSON = genericParseJSON defaultOptions

type TaskStatuses = V.Vector TaskStatus

data ListOutstandingAction = ListOutstandingAction
  deriving (Eq, Show, Generic)

instance ToJSON ListOutstandingAction where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON ListOutstandingAction where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data AddTaskAction
  = AddTaskAction
      { task :: Task
      }
  deriving (Eq, Show, Generic)

instance ToJSON AddTaskAction where
  toJSON = genericToJSON defaultOptions 

instance FromJSON AddTaskAction where
  parseJSON = genericParseJSON defaultOptions 

data CompleteTaskAction
  = CompleteTaskAction
      { index :: Int,
        timestamp :: UTCTime
      }
  deriving (Eq, Show, Generic)

instance ToJSON CompleteTaskAction where
  toJSON = genericToJSON defaultOptions 

instance FromJSON CompleteTaskAction where
  parseJSON = genericParseJSON defaultOptions 

data DeleteTaskAction
  = DeleteTaskAction
      { index :: Int,
        timestamp :: UTCTime
      }
  deriving (Eq, Show, Generic)

instance ToJSON DeleteTaskAction where
  toJSON = genericToJSON defaultOptions 

instance FromJSON DeleteTaskAction where
  parseJSON = genericParseJSON defaultOptions 

data TaskAction
  = ListOutstanding ListOutstandingAction
  | AddTask AddTaskAction
  | CompleteTask CompleteTaskAction
  | DeleteTask DeleteTaskAction
  deriving (Eq, Show, Generic)

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
processTaskAction statuses (AddTask action) =
  V.snoc statuses (TaskStatus (action ^. field @"task") Nothing Nothing)
processTaskAction statuses (CompleteTask action) =
  set (ix ((action ^. field @"index") - 1) . field @"completedAt") (Just (action ^. field @"timestamp")) statuses
processTaskAction statuses (DeleteTask action) =
  set (ix ((action ^. field @"index") - 1) . field @"deletedAt") (Just (action ^. field @"timestamp")) statuses

processTaskActions :: TaskStatuses -> TaskActions -> TaskStatuses
processTaskActions = foldl' processTaskAction

shouldRecur :: Maybe Recurrence -> Maybe UTCTime -> UTCTime -> Bool
shouldRecur (Just _) Nothing _ = True
shouldRecur Nothing _ _ = False
shouldRecur (Just Daily) (Just completedAt) now = utctDay completedAt < utctDay now
shouldRecur (Just Weekly) (Just completedAt) now =
  let lastCompletedAtWeek = view _2 (toWeekDate $ utctDay completedAt)
      nowWeek = view _2 (toWeekDate $ utctDay now)
   in lastCompletedAtWeek < nowWeek

isDeleted :: TaskStatus -> Bool
isDeleted = has (field @"deletedAt" . _Just)

isOutstanding :: UTCTime -> TaskStatus -> Bool
isOutstanding now status =
  let lastCompletedAt = firstOf (field @"completedAt" . _Just) status
      notComplete = has (field @"completedAt" . _Nothing) status
      deleted = isDeleted status
      recurringAt = firstOf (field @"task" . field @"recurrence" . _Just) status
   in not deleted && (notComplete || shouldRecur recurringAt lastCompletedAt now)
