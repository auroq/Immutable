module TaskListSpec where

import Task
import TaskList
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  describe "when adding a new task to the list" $ do
    let taskList = [newTask "task1", newTask "task2"]
    it "should append a new task" $ do
      head (addTask "new task" taskList) `shouldBe` Task "new task" []
    it "should not modify the existing items" $ do
      tail (addTask "new task" taskList) `shouldBe` taskList
      