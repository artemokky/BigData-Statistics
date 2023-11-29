generate_x <- function(x_size = 3, count = 20){
  set.seed(7)
  return(matrix(runif(x_size*count), ncol=x_size, byrow=T))
}

func_with_err <- function(x1, x2, x3){
  return(1 + 3*x1 - 2*x2 + x3 + rnorm(1))
}

func <- function(x1, x2, x3){
  return(1 + 3*x1 - 2*x2 + x3)
}

generate_y <- function(x_data, count = 20){
  y_vector <- rep(0.0, count)
  
  for(i in 1:count){
    y_vector[i] <- func_with_err(x_data[i,1], x_data[i,2], x_data[i,3])
  }
  
  return(y_vector)
}

make_dataframe <- function(x_size = 3, count = 20){
  x_data <- generate_x(x_size, count)
  
  x1 <- c(x_data[1:count, 1])
  x2 <- c(x_data[1:count, 2])
  x3 <- c(x_data[1:count, 3])
  
  y <- generate_y(x_data)
  
  dataframe <- data.frame(x1, x2, x3, y)
  
  return(dataframe)
}

evaluation_coeffs <- function(model , dataframe, count = 20){
  y_mean = mean(dataframe$y)
  
  TSS = 0
  for(i in 1:count){
    TSS = TSS + (dataframe$y[i] - y_mean)**2
  }
  RSS = 0
  for(i in 1:count){
    RSS = RSS + (dataframe$y[i] - predict(model, newdata = data.frame(x1 = dataframe$x1[i], x2 = dataframe$x2[i], x3 = dataframe$x3[i])))**2
  }
  
  RSS = RSS[[1]]
  TSS = TSS[[1]]
  RSE = sqrt(1/(count-2)*RSS)
  
  det_coef = (1 - RSS/TSS)
  return(list(RSE = RSE, TSS = TSS, RSS = RSS, det_coef = det_coef))
}

evaluation_coeffs_polynom <- function(model , x, y, count){
  y_mean = mean(y)
  
  TSS = 0
  for(i in 1:count){
    TSS = TSS + (y[i] - y_mean)**2
  }
  
  RSS = 0
  for(i in 1:count){
    RSS = RSS + (y[i] - predict(model, newdata = data.frame(row = x[i]), interval="confidence"))**2
  }
  
  RSS = RSS[[1]]
  TSS = TSS[[1]]
  RSE = sqrt(1/(count-2)*RSS)
  
  det_coef = (1 - RSS/TSS)
  return(list(RSE = RSE, TSS = TSS, RSS = RSS, det_coef = det_coef))
}

read_data <- function(){
  library(readxl)
  daily_website_vists <- read_excel("daily-website-vists.xlsx", 
                                    col_types = c("numeric", "numeric"))
  
  return(list(row = daily_website_vists$Row, loads = daily_website_vists$Page.Loads))
}

polynom_model <- function(x, y, degree, color){
  row <- (poly(x, degree = degree))
  loads <- c(y)
  P <- lm(loads ~ row, data=data.frame(loads, row))
  p <- predict(P, newdata = data.frame(row = row))
  
  points(x, p, pch=16, col=color, cex = 0.5)
  check_randomness(y, p)
  return(P)
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
  print("")
  print("")
  
  #print("Проверка на нормальн/ость с помощью критерия Шапиро-Уилка")
  #print(normalTest(x_remains, method = "sw"))
  
}

task1 <- function(){
  dataframe <- make_dataframe()
  
  A <- lm(y ~ x1 + x2 + x3, data = dataframe)
  
  ev_coefs = evaluation_coeffs(A, dataframe)
  
  print("RSE")
  print(ev_coefs$RSE)
  print("RSS")
  print(ev_coefs$RSS)
  print("Корреляционное отношение")
  print(ev_coefs$det_coef)
}
task2 <- function(){
  dataset = read_data()
  
  row = c(dataset$row)
  loads = c(dataset$loads)
  dataset <- data.frame(row, loads)
  plot(row, loads, pch=16, col="blue", cex = 0.5)
  legend("topleft", 
         legend = c("Данные", "degree = 5 ", "degree = 15 ","degree = 20 ","degree = 25"),
         lwd = 3,
         cex = 0.4,
         col = c("blue", "yellow", "purple", "red", "green"))
  
  print("Полином степени 5")
  P5 <- polynom_model(dataset$row, dataset$loads, 5, "yellow")
  print("Полином степени 15")
  P15 <- polynom_model(dataset$row, dataset$loads, 15, "purple")
  print("Полином степени 20")
  P20 <- polynom_model(dataset$row, dataset$loads, 20, "red")
  print("Полином степени 25")
  P25 <- polynom_model(dataset$row, dataset$loads, 25, "green")
  #points(lowess(dataset$row, dataset$loads, f=0.01))
  
  
}

library('qpcR')
task1()
task2() 



