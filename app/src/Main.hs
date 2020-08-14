{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (argument)
import Control.Monad hiding (fail)
import Core
import qualified Data.ByteString.Lazy as BS
import Data.Default.Class
import Data.Foldable
import Data.Text hiding (filter, foldl')
import Data.Text.Lens
import Data.Time.Clock
import qualified Data.Vector as V
import Options.Applicative hiding (action)
import System.Directory
import System.FilePath
import Text.Layout.Table
import Text.Read
import Prelude hiding (fail)

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

outstandingColSpec :: [ColSpec]
outstandingColSpec = [numCol, def]

outstandingTableStyle :: TableStyle
outstandingTableStyle = unicodeBoldHeaderS

outstandingHeaderSpec :: HeaderSpec
outstandingHeaderSpec = titlesH ["Task ID", "Description"]

indexAndDescriptionRowGroups :: TaskStatuses -> (TaskStatus -> Bool) -> RowGroup String
indexAndDescriptionRowGroups statuses statusFilter =
  let withIndexes = V.indexed statuses
      filteredStatuses = V.filter (\(_, s) -> statusFilter s) withIndexes
   in rowsG $ fmap (\(i, s) -> [show (i + 1), view (taskStatusTask . taskDescription . unpacked) s]) $ V.toList filteredStatuses

printOutstanding :: UTCTime -> TaskStatuses -> IO ()
printOutstanding now statuses = do
  let linesToPrint = tableLines outstandingColSpec outstandingTableStyle outstandingHeaderSpec $ [indexAndDescriptionRowGroups statuses $ isOutstanding now]
  traverse_ putStrLn linesToPrint

handleNewAction :: UTCTime -> TaskAction -> IO ()
handleNewAction now (ListOutstanding _) = do
  actions <- loadTaskActions
  let statuses = processTaskActions mempty actions
  printOutstanding now statuses
handleNewAction _ action@(AddTask addTaskAction) = do
  actions <- loadTaskActions
  let updatedActions = actions <> [action]
  saveTaskActions updatedActions
  let description = view (addTaskActionTask . taskDescription) addTaskAction
  putStrLn ("Added task: " <> unpack description)
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
