library(robustbase)

get_distributions <- function(){
  # 1. Стандартное нормальное распределение N(0,1)
  normal <- rnorm(100)

  # 2. Распределение Коши C(0,1)
  cauchy <- rcauchy(100)

  # 3. Смесь 0.9*N(0,1) + 0.1*C(0,1)
  mix_norm_cauch <- (0.9*rnorm(100) + 0.1*rcauchy(100))
  
  return(list(n = normal, c = cauchy, m = mix_norm_cauch))
}

mix_norm_cauchy <- function(size){
  return(0.9*rnorm(size) + 0.1*rcauchy(size))
}

phi <- function(elem, k) {
  if (abs(elem) < k) {
    return(elem)
  } else {
    return(k * sign(elem))
  }
}

huber <- function(x_sample, k = 1.44){
  return(mean(sapply(x_sample, phi, k)))
}

two_stage_assess <- function(x_sample){
  outlies <- boxplot(x_sample, plot = FALSE)$out
  no_outliers <- x_sample[!(x_sample %in% outlies)]
  
  return(mean(no_outliers))
}

monte_carlo <- function(distr, measure){
  measures <- replicate(10000, measure(distr(100)))
  
  print(paste("Среднее: ", mean(measures)))
  print(paste("Дисперсия: ", var(measures)))
}

lab6 <- function() {
  print("Нормальное распределение N(0,1)")
  print("")
  print("Средние")
  print("")
  monte_carlo(rnorm, mean)
  print("")
  print("")
  print("Медиана")
  print("")
  monte_carlo(rnorm, median)
  print("")
  print("")
  print("Оценка Хубера")
  print("")
  monte_carlo(rnorm, huber)
  print("")
  print("")
  print("Двухэтапная оценка")
  print("")
  monte_carlo(rnorm, two_stage_assess)
  print("")
  print("")
  print("")
  
  print("Распределение Коши C(0,1)")
  print("")
  print("Средние")
  print("")
  monte_carlo(rcauchy, mean)
  print("")
  print("")
  print("Медиана")
  print("")
  monte_carlo(rcauchy, median)
  print("")
  print("")
  print("Оценка Хубера")
  print("")
  monte_carlo(rcauchy, huber)
  print("")
  print("")
  print("Двухэтапная оценка")
  print("")
  monte_carlo(rcauchy, two_stage_assess)
  print("")
  print("")
  print("")
  
  print("Смесь 0.9N(0,1) + 0.1C(0,1)")
  print("")
  print("Средние")
  print("")
  monte_carlo(mix_norm_cauchy, mean)
  print("")
  print("")
  print("Медиана")
  print("")
  monte_carlo(mix_norm_cauchy, median)
  print("")
  print("")
  print("Оценка Хубера")
  print("")
  monte_carlo(mix_norm_cauchy, huber)
  print("")
  print("")
  print("Двухэтапная оценка")
  print("")
  monte_carlo(mix_norm_cauchy, two_stage_assess)
}

lab6()
