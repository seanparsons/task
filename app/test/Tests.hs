{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests where

import Control.Lens hiding (argument)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lens
import Data.Csv hiding (header, Parser)
import Data.Foldable
import Data.Text hiding (foldl', filter)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Vector as V
import GHC.Generics hiding (from)
import Options.Applicative hiding (action)
import Prelude hiding (fail)
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners
import Text.Read

firstOfJune :: UTCTime
firstOfJune = UTCTime { utctDay = fromGregorian 2020 6 1, utctDayTime = 8 * 60 * 60 }

testProcessActions :: TaskActions -> TaskStatuses -> Assertion
testProcessActions actions expectedResult = do
  let possibleActions = decodeActions $ encodeActions actions
  actualResult <- maybe (assertFailure "Actions did not decode correctly.") (\decoded -> pure $ processTaskActions mempty decoded) possibleActions
  assertEqual "Statuses did not equal what was expected." expectedResult actualResult

processAddTaskTest :: TestTree
processAddTaskTest = testCase "processTaskAction - AddTask" $ do
  let task = Task "Task With Recurring" (Just Daily)
  let addTask = AddTask (AddTaskAction task)
  let expectedResult = V.singleton (TaskStatus task Nothing Nothing)
  testProcessActions [addTask] expectedResult

processCompleteTaskTest :: TestTree
processCompleteTaskTest = testCase "processTaskAction - CompleteTask" $ do
  let task = Task "Task With Recurring" (Just Daily)
  let addTask = AddTask (AddTaskAction task)
  let completeTask = CompleteTask (CompleteTaskAction 1 firstOfJune)
  let expectedResult = V.singleton (TaskStatus task Nothing (Just firstOfJune))
  testProcessActions [addTask, completeTask] expectedResult

processDeleteTaskTest :: TestTree
processDeleteTaskTest = testCase "processTaskAction - CompleteTask" $ do
  let task = Task "Task With Recurring" (Just Daily)
  let addTask = AddTask (AddTaskAction task)
  let deleteTask = DeleteTask (DeleteTaskAction 1 firstOfJune)
  let expectedResult = V.singleton (TaskStatus task (Just firstOfJune) Nothing)
  testProcessActions [addTask, deleteTask] expectedResult

tests :: TestTree
tests = testGroup "Task Tests" [ processAddTaskTest
                               , processCompleteTaskTest
                               , processDeleteTaskTest
                               ]

