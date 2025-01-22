app <- ShinyDriver$new("../../")
app$snapshotInit("test-iris_explorer")

app$setInputs(`iris_explorer-iris-ycol` = "Petal.Length")
app$setInputs(`iris_explorer-iris-clusters` = 4)
app$snapshot()
app$setInputs(`iris_explorer-mainTabSet` = "Subset Rows")
app$setInputs(`iris_explorer-subset-sample_num` = 11)
app$snapshot()
