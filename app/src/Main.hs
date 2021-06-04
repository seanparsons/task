{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Lens hiding (argument)
import Control.Monad hiding (fail)
import Core
import qualified Data.ByteString.Lazy as BS
import Data.Foldable
import Data.Semigroup hiding (option)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import Data.Time.Clock
import qualified Data.Vector as V
import Options.Applicative hiding (action)
import System.Directory
import System.FilePath
import Text.Read
import Prelude hiding (fail)
import Data.Generics.Product

taskActionsConfigDir :: IO FilePath
taskActionsConfigDir = do
  getXdgDirectory XdgData "haskelltasks"

taskActionsFilename :: IO FilePath
taskActionsFilename = do
  configDir <- taskActionsConfigDir
  return (configDir </> "haskelltasks.json")

loadTaskActions :: IO TaskActions
loadTaskActions = do
  actionsFilename <- taskActionsFilename
  exists <- doesFileExist actionsFilename
  let nonExistant = return []
  let doesExist = BS.readFile actionsFilename >>= decodeActions
  if exists then doesExist else nonExistant

saveTaskActions :: TaskActions -> IO ()
saveTaskActions taskActions = do
  actionsFilename <- taskActionsFilename
  createDirectoryIfMissing True $ takeDirectory actionsFilename
  BS.writeFile actionsFilename $ encodeActions taskActions

-- UI --
sepH :: Text
sepH  = "━"

sepV :: Text
sepV  = "┃"

topL :: Text
topL  = "┏"

topC :: Text
topC  = "┳"

topR :: Text
topR  = "┓"

sepLC :: Text
sepLC = "┣"

sepC :: Text
sepC  = "╋"

sepRC :: Text
sepRC = "┫"

bottomL :: Text
bottomL = "┗"

bottomC :: Text
bottomC = "┻"

bottomR :: Text
bottomR = "┛"

outstandingHeaders :: [Text]
outstandingHeaders = ["Task ID", "Description"]

lineWithPadding :: [Int] -> [Text] -> [Text]
lineWithPadding sizes lineParts = fmap (\(s, t) -> t <> T.replicate (s - T.length t) " ") $ zip sizes lineParts

getLineText :: [Int] -> [Text] -> (Text, Text, Text, Text) -> Text
getLineText sizes lineParts (leftBounds, middleSeparator, rightBounds, padWith) =
  let middle = T.intercalate (padWith <> middleSeparator <> padWith) $ lineWithPadding sizes lineParts
   in leftBounds <> padWith <> middle <> padWith <> rightBounds

indexes :: [Int]
indexes = [1..]

printOutstanding :: UTCTime -> TaskStatuses -> IO ()
printOutstanding now statuses = do
  let statusesToPrint = filter (\(s, _) -> isOutstanding now s) $ zip (V.toList statuses) indexes
  let tasksAsText = fmap (\(s, i) -> [T.pack $ show i, s ^. field @"task" . field @"description"]) statusesToPrint
  let everythingAsColumns = transpose ([outstandingHeaders] <> tasksAsText)
  let sizes = fmap (getMax . foldMap (Max . T.length)) everythingAsColumns
  let horizontalLines = fmap (`T.replicate` sepH) sizes
  let topLine = getLineText sizes horizontalLines (topL, topC, topR, sepH)
  let headerLine = getLineText sizes outstandingHeaders (sepV, sepV, sepV, " ")
  let headerSeparatorLine = getLineText sizes horizontalLines (sepLC, sepV, sepRC, sepH)
  let entryLines = fmap (\e -> getLineText sizes e (sepV, sepV, sepV, " ")) tasksAsText
  let bottomLine = getLineText sizes horizontalLines (bottomL, bottomC, bottomR, sepH)
  let linesToPrint = [topLine, headerLine, headerSeparatorLine] <> entryLines <> [bottomLine]
  traverse_ T.putStrLn linesToPrint

handleNewAction :: UTCTime -> TaskAction -> IO ()
handleNewAction now (ListOutstanding _) = do
  actions <- loadTaskActions
  let statuses = processTaskActions mempty actions
  printOutstanding now statuses
handleNewAction _ action@(AddTask addTaskAction) = do
  actions <- loadTaskActions
  let updatedActions = actions <> [action]
  saveTaskActions updatedActions
  let description = view (field @"task" . field @"description") addTaskAction
  putStrLn ("Added task: " <> T.unpack description)
handleNewAction _ action@(CompleteTask _) = do
  actions <- loadTaskActions
  let updatedActions = actions <> [action]
  saveTaskActions updatedActions
handleNewAction _ action@(DeleteTask _) = do
  actions <- loadTaskActions
  let updatedActions = actions <> [action]
  saveTaskActions updatedActions

-- Runner --

recurrenceReader :: ReadM Recurrence
recurrenceReader = maybeReader $ \textToParse -> case textToParse of
  "daily" -> Just Daily
  "weekly" -> Just Weekly
  _ -> Nothing

intReader :: ReadM Int
intReader = maybeReader readMaybe

commandWithHelp :: String -> String -> Parser a -> Mod CommandFields a
commandWithHelp cmd desc parser =
  let parserInfo = info (parser <**> helper) (progDesc desc)
   in command cmd parserInfo

listOutstandingCommand :: Mod CommandFields TaskAction
listOutstandingCommand = commandWithHelp "list" "List outstanding items." (pure (ListOutstanding ListOutstandingAction))

taskParser :: Parser Task
taskParser =
  Task
    <$> strArgument (metavar "DESCRIPTION" <> help "Description of the item to add.")
    <*> option (fmap Just recurrenceReader) (long "recur" <> metavar "RECURRING" <> value Nothing <> help "How the task recurs.")

addTaskParser :: Parser AddTaskAction
addTaskParser = fmap AddTaskAction taskParser

addTaskCommand :: Mod CommandFields TaskAction
addTaskCommand = commandWithHelp "add" "Add a new task." (fmap AddTask addTaskParser)

completeTaskParser :: UTCTime -> Parser CompleteTaskAction
completeTaskParser now =
  CompleteTaskAction
    <$> argument intReader (metavar "TASKINDEX" <> help "Index of the task to complete.")
    <*> pure now

completeTaskCommand :: UTCTime -> Mod CommandFields TaskAction
completeTaskCommand now = commandWithHelp "complete" "Complete a task." (fmap CompleteTask $ completeTaskParser now)

deleteTaskParser :: UTCTime -> Parser DeleteTaskAction
deleteTaskParser now =
  DeleteTaskAction
    <$> argument intReader (metavar "TASKINDEX" <> help "Index of the task to delete.")
    <*> pure now

deleteTaskCommand :: UTCTime -> Mod CommandFields TaskAction
deleteTaskCommand now = commandWithHelp "delete" "Delete a task." (fmap DeleteTask $ deleteTaskParser now)

taskActionParser :: UTCTime -> Parser TaskAction
taskActionParser now = subparser (listOutstandingCommand <> addTaskCommand <> completeTaskCommand now <> deleteTaskCommand now)

optionsParser :: UTCTime -> ParserInfo TaskAction
optionsParser now = info (helper <*> taskActionParser now) (fullDesc <> progDesc "Task tracking tool" <> header "A simpler way to track tasks.")

main :: IO ()
main = do
  now <- getCurrentTime
  newTaskAction <- execParser $ optionsParser now
  handleNewAction now newTaskAction
