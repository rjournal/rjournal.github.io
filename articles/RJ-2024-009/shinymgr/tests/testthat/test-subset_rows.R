library(testthat)
library(shiny)
library(reactable)
source(
  system.file(package = "shinymgr", "shinymgr", "modules", "subset_rows.R")
)

test_that(
  desc = "rows are being randomly subset", 
  code = {testServer(
    app = subset_rows_server,
    args = list(dataset = reactive({iris})),
    expr = {
      
      # collect user inputs and 
      # generate module outputs (subset_data())
      set.seed(1)
      session$setInputs(sample_num = 20)
      
      # create non-reactive test objects
      set.seed(1)
      index_expected <- sample(
        1:nrow(dataset()),
        size = 20,
        replace = FALSE
      )
      
      # test 1 -----
      # expect same random subset of 20 numbers
      expect_equal(
        object = index(),
        expected = index_expected
      )
      
      # test 2 -----
      # expect the same rows to be selected
      expect_equal(
        object = session$returned$subset_data(),
        expected = iris[index_expected,]
      )
      
      # test 3 -----
      # expect a change in sample_num changes length of index 
      session$setInputs(sample_num = 15)
      expect_equal(
        object = nrow(session$returned$subset_data()),
        expected = 15
      )
      
      # test 4 -----
      # Is the re-sample button actually re-sampling the dataset?
      session$setInputs(sample_num = 10)
      set.seed(2)
      index_expected <- sample(
        1:nrow(dataset()),
        size = 10,
        replace = FALSE
      )
      
      set.seed(2)
      session$setInputs(resample = 1)
      
      expect_equal(
        object = index(),
        expected = index_expected
      )
    }
  )}
)
