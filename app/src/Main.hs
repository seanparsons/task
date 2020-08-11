{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (argument)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lens
import Data.Csv hiding (header, Parser)
import Data.Default.Class
import Data.Foldable
import Data.Text hiding (foldl', filter)
import Data.Text.Lens
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Vector as V
import GHC.Generics hiding (from)
import Options.Applicative hiding (action)
import Prelude hiding (fail)
import System.Directory
import System.FilePath
import Text.Read
import Text.Layout.Table

-- Core --

instance FromField UTCTime where
  parseField textToParse = parseTimeM True defaultTimeLocale rfc822DateFormat $ view (from packedChars) textToParse

instance ToField UTCTime where
  toField utctime = view packedChars $ formatTime defaultTimeLocale rfc822DateFormat utctime

data Recurrence = Daily
                | Weekly
                deriving (Eq, Show, Generic)

makePrisms ''Recurrence

instance FromField Recurrence where
  parseField "daily"  = pure Daily
  parseField "weekly" = pure Weekly
  parseField _        = mzero

instance ToField Recurrence where
  toField Daily  = "daily"
  toField Weekly = "weekly"

data Task = Task 
          { _taskDescription :: Text
          , _taskRecurrence  :: Maybe Recurrence
          }
          deriving (Eq, Show, Generic)

makeLenses ''Task

instance FromRecord Task
instance ToRecord Task

data TaskStatus = TaskStatus
                { _taskStatusTask         :: Task
                , _taskStatusDeletedAt    :: Maybe UTCTime 
                , _taskStatusCompletedAt  :: Maybe UTCTime
                }
                deriving (Eq, Show, Generic)

makeLenses ''TaskStatus

type TaskStatuses = V.Vector TaskStatus

data ListOutstandingAction = ListOutstandingAction
                             deriving (Eq, Show, Generic)

makeLenses ''ListOutstandingAction

instance FromRecord ListOutstandingAction
instance ToRecord ListOutstandingAction

data AddTaskAction = AddTaskAction
                   { _addTaskActionTask :: Task
                   }
                   deriving (Eq, Show, Generic)

makeLenses ''AddTaskAction

instance FromRecord AddTaskAction where
  parseRecord vec = fmap AddTaskAction $ parseRecord vec

instance ToRecord AddTaskAction where
  toRecord (AddTaskAction task) = toRecord task

data CompleteTaskAction = CompleteTaskAction
                        { _completeTaskActionIndex       :: Int
                        , _completeTaskActionTimestamp   :: UTCTime
                        }
                        deriving (Eq, Show, Generic)

makeLenses ''CompleteTaskAction

instance FromRecord CompleteTaskAction
instance ToRecord CompleteTaskAction

data DeleteTaskAction = DeleteTaskAction
                      { _deleteTaskActionIndex       :: Int
                      , _deleteTaskActionTimestamp   :: UTCTime
                      }
                      deriving (Eq, Show, Generic)

makeLenses ''DeleteTaskAction

instance FromRecord DeleteTaskAction
instance ToRecord DeleteTaskAction

data TaskAction = ListOutstanding ListOutstandingAction
                | AddTask AddTaskAction
                | CompleteTask CompleteTaskAction
                | DeleteTask DeleteTaskAction
                deriving (Eq, Show, Generic)

makePrisms ''TaskAction

type TaskActions = [TaskAction]

instance FromRecord TaskAction where
  parseRecord vec = case (V.toList vec) of
    ("listoutstanding" : rest) -> fmap ListOutstanding $ parseRecord $ V.fromList rest
    ("addtask" : rest)         -> fmap AddTask $ parseRecord $ V.fromList rest
    ("completetask" : rest)    -> fmap CompleteTask $ parseRecord $ V.fromList rest
    ("deletetask" : rest)      -> fmap DeleteTask $ parseRecord $ V.fromList rest
    _                          -> mzero

instance ToRecord TaskAction where
  toRecord (ListOutstanding action) = V.cons "listoutstanding" (toRecord action)
  toRecord (AddTask action)         = V.cons "addtask" (toRecord action)
  toRecord (CompleteTask action)    = V.cons "completetask" (toRecord action)
  toRecord (DeleteTask action)      = V.cons "deletetask" (toRecord action)

taskActionsConfigDir :: IO FilePath
taskActionsConfigDir = do
  getXdgDirectory XdgData "haskelltasks"

taskActionsFilename :: IO FilePath
taskActionsFilename = do
  configDir <- taskActionsConfigDir
  return (configDir </> "haskelltasks.csv")

decodeActions :: (Monad m, MonadFail m) => BS.ByteString -> m TaskActions
decodeActions bytesToDecode = either fail (pure . V.toList) $ decodeWith defaultDecodeOptions NoHeader bytesToDecode

encodeActions :: TaskActions -> BS.ByteString
encodeActions taskActions = encodeWith defaultEncodeOptions taskActions

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

processTaskAction :: TaskStatuses -> TaskAction -> TaskStatuses
processTaskAction statuses (ListOutstanding _) =
  statuses
processTaskAction statuses (AddTask (AddTaskAction{..})) =
  V.snoc statuses (TaskStatus _addTaskActionTask Nothing Nothing)
processTaskAction statuses (CompleteTask (CompleteTaskAction{..})) =
  set (ix (_completeTaskActionIndex - 1) . taskStatusCompletedAt) (Just _completeTaskActionTimestamp) statuses
processTaskAction statuses (DeleteTask (DeleteTaskAction{..})) =
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
  in  lastCompletedAtWeek < nowWeek

isOutstanding :: UTCTime -> TaskStatus -> Bool
isOutstanding now status =
  let lastCompletedAt = firstOf (taskStatusCompletedAt . _Just) status
      notComplete   = has (taskStatusCompletedAt . _Nothing) status
      deleted       = has (taskStatusDeletedAt . _Just) status
      recurringAt   = firstOf (taskStatusTask . taskRecurrence . _Just) status
  in  not deleted && (notComplete || shouldRecur recurringAt lastCompletedAt now)

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
  in  rowsG $ fmap (\(i, s) -> [show i :: String, view (taskStatusTask . taskDescription . unpacked) s :: String]) $ V.toList filteredStatuses

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
                                            "daily"   -> Just Daily
                                            "weekly"  -> Just Weekly
                                            _         -> Nothing

intReader :: ReadM Int
intReader = maybeReader readMaybe

commandWithHelp :: String -> String -> Parser a -> Mod CommandFields a
commandWithHelp cmd desc parser =
  let parserInfo = info (parser <**> helper) (progDesc desc)
  in  command cmd parserInfo

listOutstandingCommand :: Mod CommandFields TaskAction
listOutstandingCommand = commandWithHelp "list" "List outstanding items." (pure (ListOutstanding ListOutstandingAction))

taskParser :: Parser Task
taskParser = Task <$>
  strArgument (metavar "DESCRIPTION" <> help "Description of the item to add.") <*>
  option (fmap Just recurrenceReader) (long "recur" <> metavar "RECURRING" <> value Nothing <> help "How the task recurs.")

addTaskParser :: Parser AddTaskAction
addTaskParser = fmap AddTaskAction taskParser

addTaskCommand :: Mod CommandFields TaskAction
addTaskCommand = commandWithHelp "add" "Add a new task." (fmap AddTask addTaskParser)

completeTaskParser :: UTCTime -> Parser CompleteTaskAction
completeTaskParser now = CompleteTaskAction <$>
  argument intReader (metavar "TASKINDEX" <> help "Index of the task to complete.") <*>
  pure now

completeTaskCommand :: UTCTime -> Mod CommandFields TaskAction
completeTaskCommand now = commandWithHelp "complete" "Complete a task." (fmap CompleteTask $ completeTaskParser now)

deleteTaskParser :: UTCTime -> Parser DeleteTaskAction
deleteTaskParser now = DeleteTaskAction <$>
  argument intReader (metavar "TASKINDEX" <> help "Index of the task to delete.") <*>
  pure now

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
