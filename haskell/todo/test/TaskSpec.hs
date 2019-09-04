module TaskSpec where

import Task
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  describe "when creating a new task" $ do
    it "should return a named task" $ do
      name (newTask "task name") `shouldBe` "task name"
    it "should have no subtasks" $ do
      subTasks (newTask "task name") `shouldBe` []

  describe "when adding a subtask to a new task" $ do
    let task = addSubTask "subtask 1" $ newTask "test task"
    it "should contain the subtask" $ do
      subTasks task `shouldBe` [Task "subtask 1" []]
  
  describe "when adding a subtask to a task with subtasks" $ do
      let task = addSubTask "subtask 2" $ addSubTask "subtask 1"  $ newTask "test task" 
      it "should contain the subtask" $ do
        head  (subTasks task) `shouldBe` Task "subtask 2" []
      it "should not modify the existing items" $ do
        tail (subTasks task) `shouldBe` [Task "subtask 1" []]