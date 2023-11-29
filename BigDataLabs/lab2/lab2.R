
gen_models_range <- function(n, h, norm_mean = 0, norm_sd = 1){
  k <- seq(1, n)
  x_sample <- rep(1, n)
  x_model <- rep(1,n)
  
  for (i in k) {
    x_sample[i] <- sqrt(i*h) + rnorm(1, mean = norm_mean, sd = norm_sd)
    x_model[i] <- sqrt(i*h) 
  }
  
  return(list(k = k, x_sample = x_sample, x_model = x_model))
}

simple_roll_mean <- function(x_sample, m_period){
  size = length(x_sample)
  roll_mean <- rep(1,size)
  
  for (i in 1:size){
    if (i <= m_period){
      roll_mean[i] = sum(x_sample[1:(2*i)]) / (2 * i + 1)
    }
    else if (i > size - m_period){
      roll_mean[i] = sum(x_sample[(2*i - size) : size]) / (2*(size - i)+1)
    }
    else {
      roll_mean[i] = sum(x_sample[(i-m_period):(i + m_period)]) / (2 * m_period + 1)
    }
  }
  return(roll_mean)
}

roll_median <- function(x_sample, m_period){
  size = length(x_sample)
  roll_med <- rep(1,size)
  
  for (i in 1:size){
    if (i < m_period && i != 1){
      roll_med[i] = median(x_sample[1:(2*i - 1)])
    }
    else if (i + m_period > size && i != size){
      period = size - i
      roll_med[i] = median(x_sample[(i - period):size])
    }
    else if(i == 1){
      tuyki = c(x_sample[1], x_sample[2], 3*x_sample[2] - 2*x_sample[3])
      roll_med[i] = median(tuyki)
    }
    else if(i == size){
      tuyki = c(x_sample[size], x_sample[size-1], 3*x_sample[size-2] - 2*x_sample[size-1])
      roll_med[i] = median(tuyki)
    }
    else {
      roll_med[i] = median(x_sample[(i-m_period):(i+m_period)])
    }
  }
  return(roll_med)
}

rotation_points_count <- function(x_data){
  count <- 0

  for(i in 1:(length(x_data) - 2)){
    if ((x_data[i+1] < x_data[i]  && x_data[i + 1] < x_data[i+2]) 
        || (x_data[i+1] > x_data[i]  && x_data[i + 1] > x_data[i+2])){
      count = count + 1
    }
  }
  return(count)
}

calc_p_for_kendall <- function(x_data){
  p <- 0
  for (i in (1:length(x_data))){
    for (j in (i:length(x_data))){
      if(x_data[j] > x_data[i]){
        p = p + 1
      }
    } 
  }
  return(p)
}

check_randomness <- function(x_sample, x_trend){
  x_remains = x_sample - x_trend
  
  size = length(x_sample)
  
  p_mean = (2.0/3.0)*(size - 2)
  p_disp = (16 * size - 29) / 90.0
  p_size = rotation_points_count(x_remains)
  
  p = calc_p_for_kendall(x_remains)
  
  kendell = (4*p)/(size * (size - 1)) - 1
  print("Число поворотных точек:")
  print(p_size)
  print("Коэффициент Кенделла: ")
  print(kendell)
  if (p_size < p_mean + p_disp && p_size > p_mean - p_disp){
    print("Ряд случаен")
  }
  else if(p_size < p_mean - p_disp){
    print("Ряд положительно коррелирован")
  }
  else if(p_size > p_mean + p_disp){
    print("Ряд быстро колеблющийся")
  }
}

lab2 <- function(){
h <- 0.05
n <- 500

norm_mean = 0
norm_sd = 1

models = gen_models_range(n, h, norm_mean, norm_sd)
k = models$k
x_sample = models$x_sample
x_model = models$x_model

slide_mean_10 <- simple_roll_mean(x_sample, 10)
slide_mean_25 <- simple_roll_mean(x_sample, 25)
slide_mean_55 <- simple_roll_mean(x_sample, 55)

print("")
print("Метод простейшего скользящего среднего для окна m = 10")
check_randomness(x_sample, slide_mean_10)
print("")
print("Метод простейшего скользящего среднего для окна m = 25")
check_randomness(x_sample, slide_mean_25)
print("")
print("Метод простейшего скользящего среднего для окна m = 55")
check_randomness(x_sample, slide_mean_55)

plot(x = k, y = x_sample, pch = 16, cex = 0.5, col = "blue", 
     main = "Метод простейшего скользящего среднего",
     xlab = "K", ylab = "X")
lines(x = k, y = x_model, col="black", lwd=2)
lines(x = k, y = slide_mean_10, col="red", lwd = 2)
lines(x = k, y = slide_mean_25, col="green", lwd = 2)
lines(x = k, y = slide_mean_55, col="purple", lwd = 2)
legend("topleft", c("model","m = 10", "m = 25", "m = 55"),
       bty = "n",
       lty = 1,
       cex = 0.5,
       lwd = 2,
       col = c("black","red", "green", "purple"))

slide_median_10 <- roll_median(x_sample, 10)
slide_median_25 <- roll_median(x_sample, 25)
slide_median_55 <- roll_median(x_sample, 55)


print("")
print("Метод скользящей медианы для окна m = 10")
check_randomness(x_sample, slide_median_10)
print("")
print("Метод скользящей медианы для окна m = 25")
check_randomness(x_sample, slide_median_25)
print("")
print("Метод скользящей медианы для окна m = 55")
check_randomness(x_sample, slide_median_55)

plot(x = k, y = x_sample, pch = 16, cex = 0.5, col = "blue", 
     main = "Метод скользящей медианы",
     xlab = "K", ylab = "X")
lines(x = k, y = x_model, col="black", lwd=2)
lines(x = k, y = slide_median_10, col="red", lwd = 2)
lines(x = k, y = slide_median_25, col="green", lwd = 2)
lines(x = k, y = slide_median_55, col="purple", lwd = 2)
legend("topleft", c("model","m = 10", "m = 25", "m = 55"),
       bty = "n",
       lty = 1,
       cex = 0.5,
       lwd = 2,
       col = c("black","red", "green", "purple"))
}

lab2()