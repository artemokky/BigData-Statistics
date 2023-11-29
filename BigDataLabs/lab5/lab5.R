gen_sample <- function(){
  x_sample <- rnorm(195, mean = 0, sd = 1)
  added_nums <- c(5, -4, 3.3, 2.99, -3)

  x_sample <- append(x_sample, added_nums)
}

x_sample <- gen_sample()

task1 <- function(){
  x_mean <- mean(x_sample)
  x_var <- var(x_sample)

  outliers <- x_sample[(abs(x_sample - x_mean) > 3*x_var)]
  outliers_inxs <- which((abs(x_sample - x_mean) > 3*x_var))
  print("Выбросы, правило 3х сигм:")
  print(outliers)
  k = seq(1, length(x_sample))
  plot(k, x_sample, pch = 16, col="blue")
  points(outliers_inxs, outliers, pch = 16 , col="red")
  }

task2 <- function(){
  stats = boxplot(x_sample, horizontal = TRUE)
  print(stats$out)
}

task1()
task2()