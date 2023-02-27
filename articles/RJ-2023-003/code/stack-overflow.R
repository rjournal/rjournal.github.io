> debug.error(stack.overflow=TRUE)
Your Error: Error in data.frame(x, y, z): arguments imply differing number of rows: 3, 10

Code that led to error message:
  1: 	 w <- 4:6 
2: 	 x <- 1:3 
3: 	 y <- 1:10 
4: 	 z <- w + y 
5: 	 y <- c('a', 'b', 'c') 
6: 	 xyz <- data.frame (x, y, z) 

Results from StackOverflow:
[1] "What does the error \"arguments imply differing number of rows: x, y\" mean?"                                                 
[2] "ggplot gives \"arguments imply differing number of rows\" error in geom_point while it isn't true - how to debug?"            
[3] "Checkpoint function error in R- arguments imply differing number of rows: 1, 38, 37"                                          
[4] "qdap check_spelling Error in checkForRemoteErrors(val) : one node produced an error: arguments imply differing number of rows"
[5] "Creating and appending to data frame in R (Error: arguments imply differing number of rows: 0, 1)"                            
[6] "Caret and GBM: task 1 failed - \"arguments imply differing number of rows\""                                                  

Choose a numeric value that matches your error the best or q to quit: 