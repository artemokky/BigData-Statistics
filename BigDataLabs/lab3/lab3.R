library("fBasics")

gen_models_range <- function(n, h, norm_mean = 0, norm_sd = 1){
  k <- seq(1, n)
  x_sample <- rep(1, n)
  x_model <- rep(1,n)
  
  for (i in k) {
    x_sample[i] <- 0.5*sin(i*h) + rnorm(1, mean = norm_mean, sd = norm_sd)
    x_model[i] <- 0.5*sin(i*h)
  }
  
  return(list(k = k, x_sample = x_sample, x_model = x_model))
}

exp_roll_mean <- function(x_sample, alpha){
  exp_mean <- vector("numeric",length=length(x_sample))
  
  exp_mean[1] <- (x_sample[1] + x_sample[2] + x_sample[3]) / 3
  
  for(i in 2:length(x_sample)){
    exp_mean[i]<-alpha*x_sample[i]+(1-alpha)*exp_mean[i-1]
  }
  
  return(exp_mean)
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
  print("")
  print("Среднее:")
  print(mean(x_remains))
  print("Стандартное отклонение:")
  print(sd(x_remains))
  
  print("Проверка на нормальность с помощью критерия Шапиро-Уилка")
  print(normalTest(x_remains, method = "sw"))
  
}

lab3 <- function(){
  h <- 0.1
  n <- 500
  
  norm_mean = 0
  norm_sd = 1
  
  models = gen_models_range(n, h, norm_mean, norm_sd)
  k = models$k
  x_sample = models$x_sample
  x_model = models$x_model
  
  exp_mean_1 <- exp_roll_mean(x_sample, 0.01)
  exp_mean_2 <- exp_roll_mean(x_sample, 0.05)
  exp_mean_3 <- exp_roll_mean(x_sample, 0.1)
  exp_mean_4 <- exp_roll_mean(x_sample, 0.3)
  
  print("")
  print("Метод экспоненциального скользящего среднего для alpha = 0.01")
  check_randomness(x_sample, exp_mean_1)
  print("")
  print("Метод экспоненциального скользящего среднего для alpha = 0.05")
  check_randomness(x_sample, exp_mean_2)
  print("")
  print("Метод экспоненциального скользящего среднего для alpha = 0.1")
  check_randomness(x_sample, exp_mean_3)
  print("")
  print("Метод экспоненциального скользящего среднего для alpha = 0.3")
  check_randomness(x_sample, exp_mean_4)
  
  plot(x = k, y = x_sample, pch = 16, cex = 0.5, col = "blue", 
       main = "Метод экспоненциального скользящего среднего",
       xlab = "K", ylab = "X")
  lines(x = k, y = x_model, col="black", lwd=2)
  lines(x = k, y = exp_mean_1, col="red", lwd = 2)
  lines(x = k, y = exp_mean_2, col="green", lwd = 2)
  lines(x = k, y = exp_mean_3, col="purple", lwd = 2)
  lines(x = k, y = exp_mean_4, col="orange", lwd = 1.5)
  legend("topleft", c("model","alpha = 0.01", "alpha = 0.05", "alpha = 0.1", "alpha = 0.3"),
         bty = "n",
         lty = 1,
         cex = 0.5,
         lwd = 2,
         col = c("black","red", "green", "purple", "orange"))
  
  
  print("task 4")
  f = fft(x_sample)
  amp = 2 / n * abs(f[1:(length(x_sample) %/% 2)])
  freqs <- seq(0, 1/(2.0), length.out = length(x_sample) %/% 2)
  freq <- freqs[which.max(amp)]

  plot(freqs, amp, pch = 16, cex = 0.5, col = "blue",
       main = "Амплитудный спектр остатков",
       xlab = "ω", ylab = "A")
  lines(x = freqs, y = amp, col="blue", lwd=2)
  print(freq)
}

lab3()