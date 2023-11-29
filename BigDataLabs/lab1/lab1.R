
#task 2
x <- seq(-10, 5)
y <- seq(-5, 10)

#task 3

z <- rep(NA, length(x) + length(y))
z[seq(1, length(z), by = 2)] <- x
z[seq(2, length(z), by = 2)] <- y

z <- sort(z)

#task 4

my_norm <- function(vect, p = 1){
  if(p == "inf"){
    return(max(abs(vect)))
  }
  else if(p == 1){
    return(sum(abs(vect)))
  }
  else if(p == 2){
    return(sqrt(sum(vect^2)))
  }
}

weighted_norm <- function(vect, weights, p = 1) {
  if(length(weights) != length(vect)){
    stop("lenght of weights differences of length of vector")
  }
  
  if (any(weights < 0)) {
    stop("any weight less than 0")
  }

  if (abs(sum(weights^p) - 1) > 1e-6) {
    stop("sum of weights not equal 1")
  }

  return(sum(weights * abs(vect)^p)^(1/p))
}

scan_weights <- function(p = 1){
  weights <- scan()
  
  if (any(weights < 0)) {
    stop("any weight less than 0")
  }
  
  if (abs(sum(weights^p) - 1) > 1e-6) {
    stop("sum of weights not equal 1")
  }
  
  return(weights)
}

my_norm(x)
my_norm(y)
my_norm(z)

my_norm(x,2)
my_norm(y,2)
my_norm(z,2)

my_norm(x, "inf")
my_norm(y, "inf")
my_norm(z, "inf")

# task 6

my_factorial <- function(n) {
  if (n < 0) {
    stop("n must be positive")
  }
  if (n == 0) {
    return(1)
  } 
  else {
    return(n * factorial(n-1))
  }
}

# task 7
task7 <- function(){
  vect <- scan()
  
  print(min(vect))
  print(max(vect))
  
  print(sum(vect))
  
  #weights_for_task7 <- scan_weights()
  
  my_norm(vect, 2)
  
}

