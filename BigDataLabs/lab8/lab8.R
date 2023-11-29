library(glmnet)

evaluation_coeffs_polynom <- function(model , x, y, count){
  y_mean = mean(y)
  
  TSS = 0
  for(i in 1:count){
    TSS = TSS + (y[i] - y_mean)**2
  }
  
  RSS = 0
  for(i in 1:count){
    RSS = RSS + (y[i] - predict(model, newx = poly(x[i], 11, raw=TRUE), interval="confidence"))**2
  }
  
  RSS = RSS[[1]]
  TSS = TSS[[1]]
  RSE = sqrt(1/(count-2)*RSS)
  
  det_coef = (1 - RSS/TSS)
  return(list(RSE = RSE, TSS = TSS, RSS = RSS, det_coef = det_coef))
}

task1 <- function(){
  x = seq(-2,2,1)
  x_test = seq(-2,2,0.04)
  y <- c(-7, 0, 1, 2, 9)
  x_poly = poly(x, 11, raw=TRUE)[1:5, 1:11]

  a<-c(10,1,0.1,0.01,0.001)
  cols<-c("purple", "blue", "green", "orange", "darkred")

  # гребневая
  print("Гребневая регрессия")
  plot(main="Гребневая регрессия", x,y, col="red", pch = 16, grid = TRUE)
  for (i in 1:length(a)){
    ridge_model <- glmnet(x_poly, y, alpha = 0,lambda=a[i])
    
    y_ridge <- predict(ridge_model, newx = poly(x_test, 11, raw=TRUE))
    lines(x_test, y_ridge, col=cols[i], lwd=1)
    
    print(paste("Лямбда: ", a[i]))
    ev_ridge = evaluation_coeffs_polynom(ridge_model, x, y, length(x))
    
    print(paste("RSE: ", ev_ridge$RSE))
    print(paste("RSS: ", ev_ridge$RSS))
    print(paste("Коэффициент детерминации: ", ev_ridge$det_coef))
    print("Коэффициенты: ")
    print(ridge_model$beta@x)
    print("")
    
  }
  legend("topleft", c("alpha = 10","alpha = 1", "alpha = 0.1", "alpha = 0.01", "alpha = 0.001"),
        bty = "n",
        lty = 1,
        cex = 0.5,
        lwd = 2,
        col = c("purple", "blue", "green", "orange", "darkred"))
  grid(nx = NULL, ny = NULL,
       lty = 2,      # Grid line type
       col = "gray", # Grid line color
       lwd = 2)      # Grid line width

  print("")
  # lasso
  print("Регрессия лассо")
  plot(main="Регрессия лассо", x,y, col="red", pch = 16, grid = TRUE)
  for (i in 1:length(a)){
    lasso_model <- glmnet(x_poly, y, alpha = 1, lambda=a[i])
    
    koeffs = lasso_model$beta@x
    
    
    
    y_lasso <- predict(lasso_model, newx = poly(x_test, 11, raw=TRUE))
    lines(x_test, y_lasso, col=cols[i], lwd=1.6)
    
    print(paste("Лямбда: ", a[i]))
    ev_lasso = evaluation_coeffs_polynom(lasso_model, x, y, length(x))
    print(paste("RSE: ", ev_lasso$RSE))
    print(paste("RSS: ", ev_lasso$RSS))
    print(paste("Коэффициент детерминации: ", ev_lasso$det_coef))
    print("Коэффициенты: ")
    print(koeffs)
    print("")
    
  }  
  legend("topleft", c("alpha = 10","alpha = 1", "alpha = 0.1", "alpha = 0.01", "alpha = 0.001"),
        bty = "n",
        lty = 1,
        cex = 0.5,
        lwd = 2,
        col = c("purple", "blue", "green", "orange", "darkred"))
  grid(nx = NULL, ny = NULL,
       lty = 2,      # Grid line type
       col = "gray", # Grid line color
       lwd = 2)      # Grid line width
}

task2 <- function(){
  x = seq(-2,2,1)
  x_test = seq(-2,2,0.04)
  y <- c(-7, 0, 1, 2, 9)
  y_e = y + rnorm(length(y), 0, )
  x_poly = poly(x, 11, raw=TRUE)[1:5, 1:11]
  
  sigmas <- c(0.3, 0.2, 0.1)
  print("")
  print("Гребневая регрессия")
  par(mfrow = c(1, 3))
  for (i in 1:length(sigmas)){
    y_e = y + rnorm(length(y), 0, sigmas[i])
    
    ridge_model <- glmnet(x_poly, y_e, alpha = 0,lambda=0.1)
    y_ridge <- predict(ridge_model, newx = poly(x_test, 11, raw=TRUE))
    
    plot(x,y_e, col="red", pch = 16, main = paste("ridge", sigmas[i]))
    lines(x_test, y_ridge, col="blue", lwd=1)
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 2)      # Grid line width
    
    print(paste("Сигма: ", sigmas[i]))
    ev_ridge = evaluation_coeffs_polynom(ridge_model, x, y, length(x))
    
    print(paste("RSE: ", ev_ridge$RSE))
    print(paste("RSS: ", ev_ridge$RSS))
    print(paste("Коэффициент детерминации: ", ev_ridge$det_coef))
    print("Коэффициенты: ")
    print(ridge_model$beta@x)
    print("")
  }
  
  print("")
  
  print("Регрессия лассо")
  par(mfrow = c(1, 3))
  for (i in 1:length(sigmas)){
    y_e = y + rnorm(length(y), 0, sigmas[i])
    
    lasso_model <- glmnet(x_poly, y_e, alpha = 0,lambda=0.1)
    y_lasso <- predict(lasso_model, newx = poly(x_test, 11, raw=TRUE))
    
    plot(x,y_e, col="red", pch = 16, main = paste("lasso", sigmas[i]), grid = TRUE)
    lines(x_test, y_lasso, col="blue", lwd=1)
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 2)      # Grid line width
    
    print(paste("Сигма: ", sigmas[i]))
    ev_lasso = evaluation_coeffs_polynom(lasso_model, x, y, length(x))
    print(paste("RSE: ", ev_lasso$RSE))
    print(paste("RSS: ", ev_lasso$RSS))
    print(paste("Коэффициент детерминации: ", ev_lasso$det_coef))
    print("Коэффициенты: ")
    print(lasso_model$beta@x)
    print("")
  }
}

task1()
task2()