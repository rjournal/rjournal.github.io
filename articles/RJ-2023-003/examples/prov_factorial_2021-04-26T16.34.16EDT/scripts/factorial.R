factorial <- function (num) {
  if (num < 0) {
    return (-1)
  }
  
  if (num == 0) {
    return (1)
  }
  
  return (num * factorial (num - 1))
}

cat ("The factorial of", 5, "is", factorial (5), "\n")
cat ("The factorial of", -5, "is", factorial (-5), "\n")
cat ("The factorial of", 0, "is", factorial (0), "\n")
