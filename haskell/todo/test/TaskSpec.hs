module TaskSpec where

import Task
import Test.Hspec
import Test.Validity


testGetStatus status = do
  context ("when task is " ++ show status) $ do
    it ("should be " ++ show status) $ do
      getStatus (Task status "testTask") `shouldBe` status

testChangingStatus currentStatus action expectedStatus = do
  context ("when task is " ++ show currentStatus) $ do
    let task = Task currentStatus "task name"
    it ("should be " ++ show expectedStatus) $ do 
      getStatus (action task) `shouldBe` expectedStatus

spec :: Spec
spec = do
  describe"getting status" $ do
    testGetStatus Incomplete
    testGetStatus Progressing
    testGetStatus Complete

  describe "creating a new task" $ do
    it "should return a named task" $ do
      name (newTask "task name") `shouldBe` "task name"
    it "should return an incomplete task" $ do
      status (newTask "task name") `shouldBe` Incomplete
  
  describe "starting a task" $ do
    testChangingStatus Incomplete (start) Progressing
    testChangingStatus Progressing (start) Progressing
    testChangingStatus Complete (start) Complete

  describe "stopping a task" $ do
    testChangingStatus Incomplete (stop) Incomplete
    testChangingStatus Progressing (stop) Incomplete
    testChangingStatus Complete (stop) Complete

  describe "completing a task" $ do
    testChangingStatus Incomplete (complete) Complete
    testChangingStatus Progressing (complete) Complete
    testChangingStatus Complete (complete) Complete
