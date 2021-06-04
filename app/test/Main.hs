{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Main where

import Core
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Vector as V
import Options.Applicative hiding (action)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (fail)

firstOfJune :: UTCTime
firstOfJune = UTCTime {utctDay = fromGregorian 2020 6 1, utctDayTime = 8 * 60 * 60}

testProcessActions :: TaskActions -> TaskStatuses -> Assertion
testProcessActions actions expectedResult = do
  let possibleActions = decodeActions $ encodeActions actions
  actualResult <- maybe (assertFailure "Actions did not decode correctly.") (pure . processTaskActions mempty) possibleActions
  assertEqual "Statuses did not equal what was expected." expectedResult actualResult

processAddTaskTest :: TestTree
processAddTaskTest = testCase "AddTask" $ do
  let task = Task "Task With Recurring" (Just Daily)
  let addTask = AddTask (AddTaskAction task)
  let expectedResult = V.singleton (TaskStatus task Nothing Nothing)
  testProcessActions [addTask] expectedResult

processCompleteTaskTest :: TestTree
processCompleteTaskTest = testCase "CompleteTask" $ do
  let task = Task "Task With Recurring" (Just Daily)
  let addTask = AddTask (AddTaskAction task)
  let completeTask = CompleteTask (CompleteTaskAction 1 firstOfJune)
  let expectedResult = V.singleton (TaskStatus task Nothing (Just firstOfJune))
  testProcessActions [addTask, completeTask] expectedResult

processDeleteTaskTest :: TestTree
processDeleteTaskTest = testCase "DeleteTask" $ do
  let task = Task "Task With Recurring" (Just Daily)
  let addTask = AddTask (AddTaskAction task)
  let deleteTask = DeleteTask (DeleteTaskAction 1 firstOfJune)
  let expectedResult = V.singleton (TaskStatus task (Just firstOfJune) Nothing)
  testProcessActions [addTask, deleteTask] expectedResult

processActionTests :: TestTree
processActionTests =
  testGroup
    "processAction"
    [ processAddTaskTest,
      processCompleteTaskTest,
      processDeleteTaskTest
    ]

isDeletedMarkedAsDeletedTest :: TestTree
isDeletedMarkedAsDeletedTest = testCase "Marked as deleted" $ do
  let task = Task "Task With Recurring" (Just Daily)
  let status = TaskStatus task (Just firstOfJune) Nothing
  let actualResult = isDeleted status
  assertEqual "Should be marked as deleted." True actualResult

isDeletedNotMarkedAsDeletedTest :: TestTree
isDeletedNotMarkedAsDeletedTest = testCase "Marked as deleted" $ do
  let task = Task "Task With Recurring" (Just Daily)
  let status = TaskStatus task Nothing Nothing
  let actualResult = isDeleted status
  assertEqual "Should not be marked as deleted." False actualResult

isDeletedTests :: TestTree
isDeletedTests =
  testGroup
    "isDeleted"
    [ isDeletedMarkedAsDeletedTest,
      isDeletedNotMarkedAsDeletedTest
    ]

tests :: TestTree
tests =
  testGroup
    "Task Tests"
    [ processActionTests,
      isDeletedTests
    ]

main :: IO ()
main = defaultMain tests
