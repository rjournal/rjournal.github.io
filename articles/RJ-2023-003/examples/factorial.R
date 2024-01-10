factorial <- function (num) {
  if (num < 0) {
    return (-1)
  }
  answer = 1
  
  for (i in 0: num) {
    answer = answer * i
  }
  
  return (answer)
}

cat ("The factorial of", 5, "is", factorial (5), "\n")
cat ("The factorial of", -5, "is", factorial (-5), "\n")
cat ("The factorial of", 0, "is", factorial (0), "\n")
