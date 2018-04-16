context("Input for feature selection is checked")

test_that("target is not missing",{
  train_df <- sim_data()
  expect_error(featureSelection(data = train_df, ) , "I cannot find your target") 
  expect_error(featureSelection(data = train_df, target = "target") , "The data does not contain the target!")        
})

test_that("target is specified correct",{
  train_df <- sim_data()
  expect_error(featureSelection(data = train_df, target = train_df$y ) , "Just give me the name of the target column") 
  expect_error(featureSelection(data = train_df, target = 1 ) , "Just give me the name of the target column") 
  expect_error(featureSelection(data = train_df, target = as.data.frame("y")) , "Just give me the name of the target column")        
})