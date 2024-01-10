> debug.error()
Your Error: Error in data.frame(x, y, z): arguments imply differing number of rows: 3, 10

  scriptNum startLine                        code
1         1         1                    w <- 4:6
2         1         2                    x <- 1:3
3         1         3                   y <- 1:10
4         1         4                  z <- w + y
5         1         5       y <- c('a', 'b', 'c')
6         1         6 xyz <- data.frame (x, y, z)