library(testthat)
library(shiny)
source(
  system.file(package = "shinymgr", "shinymgr", "modules", "iris_cluster.R")
)


test_that("iris data is clustered and returns the appropriate dataframe", {
  
  testServer(iris_cluster_server, {
    
    #test 1 - test that iris is being subset correctly
    data(iris)
    set.seed(1)
    session$setInputs(xcol = "Sepal.Length", ycol = "Sepal.Width")
    expect_equal(selectedData(), iris[, c("Sepal.Length", "Sepal.Width")])
    
    #test 2 - expect the same clusters when kmeans() is run
    set.seed(1)
    session$setInputs(clusters = 3)
    set.seed(1)
    expectedCluster <- kmeans(
      x = iris[, c("Sepal.Length", "Sepal.Width")], 
      centers = 3)
    
    expect_equal(clusters()$cluster, expectedCluster$cluster)
    
    #test 3 - expect the same dataframe being returned
    combineddf <- cbind(iris[, c("Sepal.Length", "Sepal.Width")], cluster = expectedCluster$cluster)
    expect_equal(session$returned$returndf(), combineddf)
    
    #test 4 - expect the selected data to change with different inputs
    session$setInputs(xcol = "Petal.Length", ycol = "Petal.Width")
    expect_equal(selectedData(), iris[, c("Petal.Length", "Petal.Width")])
  })
})

