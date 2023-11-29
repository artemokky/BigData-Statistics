
parse_data <- function(){
  library(readxl)
  Kostroma_temperature_dataset <- read_excel("Kostroma.xlsx")
  
  return(Kostroma_temperature_dataset)
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

rotation_points_count <- function(x_data){
  count <- 0
  
  for(i in 1:(length(x_data) - 2)){
    if ((x_data[i+1] < x_data[i]  && x_data[i + 1] < x_data[i+2]) 
        || (x_data[i+1] > x_data[i]  && x_data[i + 1] > x_data[i+2])){
      count = count + 1
    }
    else{
      
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

spectre <- function(x_sample){
  f = fft(x_sample)
  n = length(x_sample)
  amp = 2 / n * abs(f[1:(length(x_sample) %/% 2)])
  freqs <- seq(0, 1/(1.0), length.out = length(x_sample) %/% 2)
  freq <- freqs[which.max(amp)]
  
  plot(freqs, amp, pch = 16, cex = 0.5, col = "blue",
       main = "Амплитудный спектр остатков",
       xlab = "ω", ylab = "A")
  lines(x = freqs, y = amp, col="blue", lwd=2)
  print(freq)
}

lab4 <- function(){
  x_data = parse_data()
  x_data = unlist(x_data)
  x_data = rev(x_data)
  slide_mean_55 <- simple_roll_mean(x_data, 55)
  print("Метод простейшего скользящего среднего для окна m = 55")
  check_randomness(x_data, slide_mean_55)
  x = seq(1, length(x_data))
  plot(x= x, y = x_data, pch = 16, cex = 0.5, col = "blue", 
      main = "Средняя температура по дням в Костроме",
      xlab = "День", ylab = "Температура")
  lines(x =x, y = slide_mean_55, col="red", lwd = 2)
  legend("topleft", c("slide mean m = 55"),
        bty = "n",
        lty = 1,
        cex = 0.5,
        lwd = 2,
        col = c("red"))

  spectre(x_data)
}

lab4()