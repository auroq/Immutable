module TaskListSpec where

import Task
import TaskList
import Test.Hspec
import Test.Validity

testGetAllOfStatus status action expectedTaskList = do
  describe ("getting all " ++ show status ++ " tasks") $ do
    it ("should return all " ++ show status ++ " tasks") $ do
      getCompleted testTaskList `shouldBe` expectedTaskList
    it ("should not return any non-" ++ show status ++ " tasks") $ do
      any (not . isStatus status) (action testTaskList) `shouldBe` False

spec :: Spec
spec = do
  describe "when adding a new task to the list" $ do
    it "should append a new task" $ do
      head (addTask "new task" completeTaskList) `shouldBe` Task Incomplete "new task"
    it "should not modify the existing items" $ do
      tail (addTask "new task" completeTaskList) `shouldBe` completeTaskList
   
  testGetAllOfStatus Complete (getCompleted) completeTaskList
  testGetAllOfStatus Progressing (getProgressing) completeTaskList
  testGetAllOfStatus Incomplete (getIncomplete) completeTaskList

  describe "when removing a new task to the list" $ do
    it "should remove the expected task" $ do
      dropWhile (\x -> x /= (Task Incomplete "incomplete 2")) (removeTask "incomplete 2" testTaskList) `shouldBe` []
    it "should not remove other tasks" $ do
      length (takeWhile (\x -> x /= (Task Incomplete "incomplete 2")) (removeTask "incomplete 2" testTaskList)) `shouldBe` 8


incompleteTaskList =
  [ Task Incomplete "incomplete 1"
  , Task Incomplete "incomplete 2"
  , Task Incomplete "incomplete 3" ]

completeTaskList = 
  [ Task Complete "complete 1"
  , Task Complete "complete 2"
  , Task Complete "complete 3" ]

progressingTaskList =
  [ Task Progressing "progressing 1"
  , Task Progressing "progressing 2"
  , Task Progressing "progressing 3" ]

testTaskList =
  [ Task Progressing "progressing 1"
  , Task Incomplete "incomplete 1"
  , Task Progressing "progressing 2"
  , Task Complete "complete 1"
  , Task Complete "complete 2"
  , Task Incomplete "incomplete 2"
  , Task Progressing "progressing 3"
  , Task Complete "complete 3"
  , Task Incomplete "incomplete 3" ]
