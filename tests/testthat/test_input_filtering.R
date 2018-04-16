context("Input for feature filtering is checked")

test_that("target is not missing",{
  train_df <- sim_data()
  expect_error(featureFiltering(data = train_df, ) , "I couldn't the target") 
  expect_error(featureFiltering(data = train_df, target = "target") , "The data does not contain the target!")        
})

test_that("target is specified correct",{
  train_df <- sim_data()
  expect_error(featureFiltering(data = train_df, target = train_df$y ) , "You only need to give me the name of the column") 
  expect_error(featureFiltering(data = train_df, target = 1 ) , "You only need to give me the name of the column") 
  expect_error(featureFiltering(data = train_df, target = as.data.frame("y")) , "You only need to give me the name of the column")        
})