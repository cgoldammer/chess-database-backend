-- This provides the data types to handle asynchronous tasks.
-- Right now, we only have one type of task, to add evaluations,
-- but in the future, this task will likely cover more use cases.

module Services.Tasks
  ( AllTasks(..)
  , Task(..)
  , Tasks
  , addTask
  , completeActiveTask
  , emptyTasks
  ) where

import Services.Types
import Database.Persist (Entity)

data Task = Task
  { taskName :: String
  , taskGames :: [Entity Game]
  , taskDB :: String
  , userName :: Maybe String
  }

type Tasks = [Task]

data AllTasks = AllTasks
  { tasksScheduled :: Tasks
  , taskActive :: Maybe Task
  , tasksDone :: Tasks
  } deriving (Show)

showTask :: Task -> String
showTask (Task name games db taskUser) =
  "User: " ++
  show taskUser ++ " db: " ++ db ++ " games: " ++ show (length games) ++ " name: " ++ name

instance Show Task where
  show = showTask


moveScheduledToActive :: AllTasks -> AllTasks
moveScheduledToActive tasks@(AllTasks _ (Just _) _) = tasks
moveScheduledToActive tasks@(AllTasks [] _ _) = tasks
moveScheduledToActive (AllTasks (first:rest) Nothing done) =
  AllTasks rest (Just first) done

addTask :: AllTasks -> Task -> AllTasks
addTask (AllTasks sched act done) task =
  moveScheduledToActive $ AllTasks (task : sched) act done

completeActiveTask :: AllTasks -> AllTasks
completeActiveTask tasks@(AllTasks _ Nothing _) = tasks
completeActiveTask (AllTasks sched (Just t) done) =
  moveScheduledToActive $ AllTasks sched Nothing (t : done)

emptyTasks :: AllTasks
emptyTasks = AllTasks [] Nothing []

