library(tidyverse)
library(xts)
library(zoo)
library(svglite)
library(TSstudio)
library(zoo)
library(dlm)


# load data
dt1 <- read.csv("data/cbs_basic_macro_NOT_SEASONCORRECTED_qt.csv", sep=",", stringsAsFactors = FALSE)
dt1$date <- as.Date(dt1$X)
# remove X column, may not be necessary
dt1 <- dt1[,-c(1)]
# reorder
n = ncol(dt1)
new_order = c(n, 1:(n-1))
dt1 = dt1[, new_order]
str(dt1)

# Columns names
colNames <- colnames(dt1)

# Number rows and columns
rows1 <- dim(dt1)[1]
cols1 <- dim(dt1)[2]

#####################################################################
#####################################################################

basicPlots <- function(dt1, colNames, rows1, cols1) {
    print("BasicPlots")

    # for loop to produce basic figures put 4 figures on a page
    # can be modified to account for different sized dataframes, 
    # for now I just calculate by hand how many figures to produce
    for (j in 0:2){
      png(paste("output/GDP/basic/plot_", j, ".png", sep = ""), res = 120, width = 1000, height = 800)
      par(mfrow = c(4, 1), mar = c(4, 1, 1, 4))

      begin <- 1 + (j * 4)
      end   <- 4 + (j * 4)

      if (begin <= 8) {

        for (i in begin:end){
      
          mymain <- colNames[i]

          plot(ts(na.omit(dt1[, i]), frequency = 4, start = c(1995, 1)), main = mymain)
        }

        dev.off()
      } else {

        # this is a repeat of the above code, but for two coluns only
        for (i in 9){
          print(i)

          if (length(na.omit(dt1[, i])) < 20) next
          mymain <- colNames[i]

          plot(ts(na.omit(dt1[, i]), frequency = 4, start = c(1995, 1)), main = mymain)
        }
        dev.off()
      }
    }
}    

basicPlots(dt1, colNames, rows1, cols1)

###########################
# KalmanSmoothing
###########################

kalmanSmoothing <- function(dt1){
  print("kalmanSmoothing")

  emptyDF = data.frame(matrix(NA, nrow = rows1, ncol = cols1))
  colnames(emptyDF) <- colnames(dt1)

  for (i in 1:cols1) {
      print(i)
      y <- dt1[, i]

      # ######################################
      # # Kalman smoothing
      # ######################################

      # Setting of local-level model
      W <- 1
      V <- 2
      m0 <- 100000
      C0 <- 100
      
      mod <- dlmModPoly(order = 1, dW = W, dV = V, m0 = m0, C0 = C0)

      dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

      # Find the mean and standard deviation of the smoothing distribution
      s <- dropFirst(dlmSmoothed_obj$s)

      # which of the original data is missing, for example, sector: 11_Drankenindustrie
      replaceThese <- which(is.na(y))
      y[replaceThese] <- s[replaceThese]

      emptyDF[,i] <- y
  }    
  write.csv(emptyDF, "tmp.csv")
}
kalmanSmoothing(dt1)

###########################
# KalmanSmoothing2 
###########################

kalmanSmoothing2 <- function(dt1){
  print("kalmanSmoothing2")

  for (j in 0:2){
    png(paste("output/GDP/kalman2/plot_", j, ".png", sep = ""), res = 100, width = 1000, height = 800)
    par(mfrow = c(4, 1), mar = c(4, 1, 1, 4))

    begin <- 1 + (j * 4)
    end   <- 4 + (j * 4)

    if (begin <= 8) {
    
      for (i in begin:end){
      
          mymain <- colNames[i]
          print(mymain)
    
          y <- dt1[, i]

          # Model setting: local-trend model + seasonal model (time-domain approach)
          build_dlm_y <- function(par){
          return(
                dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
                dlmModSeas(frequency = 4, dW = c(exp(par[4]), rep(0, times = 2)), dV = 0)
                )
          }

          # Maximum likelihood estimation of parameters and confirmation of the results
          fit_dlm_y <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_y)
          fit_dlm_y

          # Set the maximum likelihood estimates of parameters in the model
          mod <- build_dlm_y(fit_dlm_y$par)

          # Kalman smoothing
          dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

          # Mean of the smoothing distribution
          mu <- dropFirst(dlmSmoothed_obj$s[, 1])
          gamma <- dropFirst(dlmSmoothed_obj$s[, 3])

          # Plot results
          oldpar <- par(no.readonly = TRUE)
          par(mfrow = c(4, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(4, 1, 1, 4))
          ts.plot(    y, ylab = "Observations")
          ts.plot(   mu, ylab = "Level component")
          ts.plot(gamma, ylab = "Seasonal component")
          mtext(text = "Time", side = 1, line = 1, outer = TRUE)
          par(oldpar)

          # Confirm the log-likelihood
          -dlmLL(y = y, mod = mod)
      }
    }  
  }
}

kalmanSmoothing2(dt1)

######################################
## Local-trend model
######################################

localTrend <- emptyDF

#### Local-trend model + seasonal model (time-domain approach)

kalmanPrediction <- function(localTrend){

  print("kalmanFiltering")
  ####
  # Save forecasts
  ####
  forecasts <- c()
  namesCols <- c()

  for (j in 0:2){
    png(paste("output/GDP/kalman/plot_", j, ".png", sep = ""), res = 100, width = 1000, height = 800)
    par(mfrow = c(4, 1), mar = c(4, 1, 1, 4))

    begin <- 1 + (j * 4)
    end   <- 4 + (j * 4)

    if (begin <= 8) {

      for (i in begin:end){
        print(colNames[i])      
        mymain <- colNames[i]

        # Data
        y <- ts(na.omit(localTrend[, i]), frequency = 4, start = c(1995, 1))

        # Model setting: local-trend model + seasonal model (time-domain approach)
        build_dlm_test <- function(par) {
          return(
            dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
            dlmModSeas(frequency = 4, dW = c(exp(par[4]), rep(0, times = 2)), dV = 1)
          )
        }

        # Maximum likelihood estimation of parameters and confirmation of the results
        fit_dlm_test <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_test)
      
        # Set the maximum likelihood estimates of parameters in the model
        mod  <- build_dlm_test(fit_dlm_test$par)

        # # Kalman filtering
        dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
        dlmFiltered_obja <- dlmFiltered_obj  # Save under a different name for later comparison of prediction values

        ######################################
        # Kalman prediction
        ######################################

        dlmForecasted_object <- dlmForecast(mod = dlmFiltered_obj, nAhead = 4)
        forecasts[i] <- as.data.frame(dlmForecasted_object$f)

        # Find the standard deviation and the 2.5% and 97.5% values of the prediction value
        f_sd <- sqrt(as.numeric(dlmForecasted_object$Q))
        f_lower <- dlmForecasted_object$f + qnorm(0.025, sd = f_sd)
        f_upper <- dlmForecasted_object$f + qnorm(0.975, sd = f_sd)

        # Unite the entire observation along with the mean, 2.5%, and 97.5% values of the prediction values into ts class object
        y_union <- ts.union(y, dlmForecasted_object$f, f_lower, f_upper)

        # Plot results
        plot(y_union, plot.type = "single",
            xlim = c(1995, 2026),
            ylim = c(), ylab = "Year-Quarter", 
            lty = c("solid", "solid", "dashed", "dashed"),
            col = c("lightgray", "red", "black", "black"),
            main = mymain)

        # Legend
        legend(legend = c("Observations", "Mean (predictive distribution)", "95% intervals (predictive distribution)"),
              lty = c("solid", "solid", "dashed"),
              col = c("lightgray", "black", "black"),
              x = "topleft", cex = 0.6)

        # Grid
        grid(nx = NULL, ny = NULL,
            lty = 2,      # Grid line type
            col = "gray", # Grid line color
            lwd = 1)      # Grid line width
      }

      dev.off()
    
    } else {

      for (i in 9){

        print(colNames[i])

        mymain <- colNames[i]

        # Data
        y <- ts(na.omit(localTrend[, i]), frequency = 4, start = c(1995, 1))

        # Model setting: local-trend model + seasonal model (time-domain approach)
        build_dlm_test <- function(par) {
          return(
            dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
            dlmModSeas(frequency = 4, dW = c(exp(par[4]), rep(0, times = 2)), dV = 0)
          )
        }

        # Maximum likelihood estimation of parameters and confirmation of the results
        fit_dlm_test <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_test)
        fit_dlm_test

        # Set the maximum likelihood estimates of parameters in the model
        mod  <- build_dlm_test(fit_dlm_test$par)

        # # Kalman filtering
        dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
        dlmFiltered_obja <- dlmFiltered_obj  # Save under a different name for later comparison of prediction values

        ######################################
        # Kalman prediction
        ######################################

        dlmForecasted_object <- dlmForecast(mod = dlmFiltered_obj, nAhead = 4)
        forecasts[i] <-  as.data.frame(dlmForecasted_object$f)

        # Find the standard deviation and the 2.5% and 97.5% values of the prediction value
        f_sd <- sqrt(as.numeric(dlmForecasted_object$Q))
        f_lower <- dlmForecasted_object$f + qnorm(0.025, sd = f_sd)
        f_upper <- dlmForecasted_object$f + qnorm(0.975, sd = f_sd)

        # Unite the entire observation along with the mean, 2.5%, and 97.5% values of the prediction values into ts class object
        y_union <- ts.union(y, dlmForecasted_object$f, f_lower, f_upper)

        # Ignore the display of following codes

        # Plot results
        plot(y_union, plot.type = "single",
            xlim = c(1995, 2026),
            ylim = c(), ylab = "Year-Quarter", 
            lty = c("solid", "solid", "dashed", "dashed"),
            col = c("lightgray", "red", "black", "black"),
            main = mymain)

        # Legend
        legend(legend = c("Observations", "Mean (predictive distribution)", "95% intervals (predictive distribution)"),
              lty = c("solid", "solid", "dashed"),
              col = c("lightgray", "red", "black"),
              x = "topleft", cex = 0.6)

        # Grid
        grid(nx = NULL, ny = NULL,
            lty = 2,      # Grid line type
            col = "gray", # Grid line color
            lwd = 1)      # Grid line width

      }
      dev.off()
    }
  }
}
kalmanPrediction(localTrend)


##########################
# HW #####################
##########################

HW1 <- function(localTrend){

  mygray <- "#80808080"
  dev.off()
  for (j in 0:2){
    png(paste("output/GDP/HW/plot_", j, ".png", sep = ""), res = 100, width = 1000, height = 800)
    par(mfrow = c(4, 1), mar = c(4, 1, 1, 4))

    begin <- 1 + (j * 4)
    end   <- 4 + (j * 4)

    if (begin <= 8) {

      for (i in begin:end){
        print(i)
        if (length(na.omit(localTrend[, i])) < 20) next
        mymain <- colNames[i]

        test_data <- ts(na.omit(localTrend[, i]), frequency = 4, start = c(1995, 1))

        HW_test <- HoltWinters(test_data)
        str(HW_test)

        HW_test_predict <- predict(HW_test, n.ahead = 4)
        str(HW_test_predict)

        # Plot observations along with filtering and prediction values
        plot(HW_test, HW_test_predict, main = mymain,  sub = "HW", col = mygray, 
            col.predicted = "red", lty.predicted = "dashed")
      }
      dev.off()
    } else {

      for (i in 9){

        print(i)
    
        mymain <- colNames[i]

        test_data <- ts(na.omit(localTrend[, i]), frequency = 4, start = c(1995, 1))

        HW_test <- HoltWinters(test_data)
        str(HW_test)

        HW_test_predict <- predict(HW_test, n.ahead = 4)
        str(HW_test_predict)

        # Plot observations along with filtering and prediction values
        plot(HW_test, HW_test_predict, main = mymain,  sub = "HW", col = mygray, 
          col.predicted = "red", lty.predicted = "dashed")      
      }
      dev.off()
    }
  }
}
HW1(localTrend)

######################################
## Local-trend model
######################################

## Seasonal model
### Approach from the time domain
### Approach from the frequency domain
#### Local-trend model + seasonal model (time-domain approach)

library(dlm)

for (j in 0:2){
  png(paste("output/GDP/kalman2/plot_", j, ".png", sep = ""), res = 100, width = 1000, height = 800)
  par(mfrow = c(4, 1), mar = c(4, 1, 1, 4))

  begin <- 1 + (j * 4)
  end   <- 4 + (j * 4)

  if (begin <= 8) {

    for (i in begin:end){
      print(i)
      if (length(na.omit(localTrend[, i])) < 20) next
      mymain <- colNames[i]

      # Data
      y <- ts(na.omit(localTrend[, i]), frequency = 4, start = c(1995, 1))

      # Model setting: local-trend model + seasonal model (time-domain approach)
      build_dlm_test <- function(par) {
        return(
          dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
          dlmModSeas(frequency = 4, dW = c(exp(par[4]), rep(0, times = 2)), dV = 0)
        )
      }

      # Maximum likelihood estimation of parameters and confirmation of the results
      fit_dlm_test <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_test)
      fit_dlm_test

      # Set the maximum likelihood estimates of parameters in the model
      mod  <- build_dlm_test(fit_dlm_test$par)

      # # Kalman filtering
      dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
      dlmFiltered_obja <- dlmFiltered_obj  # Save under a different name for later comparison of prediction values

      ######################################
      # Kalman prediction
      ######################################

      dlmForecasted_object <- dlmForecast(mod = dlmFiltered_obj, nAhead = 4)

      # Find the standard deviation and the 2.5% and 97.5% values of the prediction value
      f_sd <- sqrt(as.numeric(dlmForecasted_object$Q))
      f_lower <- dlmForecasted_object$f + qnorm(0.025, sd = f_sd)
      f_upper <- dlmForecasted_object$f + qnorm(0.975, sd = f_sd)

      # Unite the entire observation along with the mean, 2.5%, and 97.5% values of the prediction values into ts class object
      y_union <- ts.union(y, dlmForecasted_object$f, f_lower, f_upper)

      # Plot results
      plot(y_union, plot.type = "single",
          xlim = c(1995, 2026),
          ylim = c(), ylab = "Year-Quarter", 
          lty = c("solid", "solid", "dashed", "dashed"),
          col = c("lightgray", "red", "black", "black"),
          main = mymain)

      # Legend
      legend(legend = c("Observations", "Mean (predictive distribution)", "95% intervals (predictive distribution)"),
            lty = c("solid", "solid", "dashed"),
            col = c("lightgray", "black", "black"),
            x = "topleft", cex = 0.6)

      # Grid
      grid(nx = NULL, ny = NULL,
          lty = 2,      # Grid line type
          col = "gray", # Grid line color
          lwd = 1)      # Grid line width
    }

    dev.off()
  
  } else {

    for (i in 9){

      print(i)
      if (length(na.omit(localTrend[, i])) < 20) next
      mymain <- colNames[i]

      # Data
      y <- ts(na.omit(localTrend[, i]), frequency = 4, start = c(1995, 1))

      # Model setting: local-trend model + seasonal model (time-domain approach)
      build_dlm_test <- function(par) {
        return(
          dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
          dlmModSeas(frequency = 4, dW = c(exp(par[4]), rep(0, times = 2)), dV = 0)
        )
      }

      # Maximum likelihood estimation of parameters and confirmation of the results
      fit_dlm_test <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_test)
      fit_dlm_test

      # Set the maximum likelihood estimates of parameters in the model
      mod  <- build_dlm_test(fit_dlm_test$par)

      # # Kalman filtering
      dlmFiltered_obj  <- dlmFilter(y = y, mod = mod)
      dlmFiltered_obja <- dlmFiltered_obj  # Save under a different name for later comparison of prediction values

      ######################################
      # Kalman prediction
      ######################################

      dlmForecasted_object <- dlmForecast(mod = dlmFiltered_obj, nAhead = 4)

      # Find the standard deviation and the 2.5% and 97.5% values of the prediction value
      f_sd <- sqrt(as.numeric(dlmForecasted_object$Q))
      f_lower <- dlmForecasted_object$f + qnorm(0.025, sd = f_sd)
      f_upper <- dlmForecasted_object$f + qnorm(0.975, sd = f_sd)

      # Unite the entire observation along with the mean, 2.5%, and 97.5% values of the prediction values into ts class object
      y_union <- ts.union(y, dlmForecasted_object$f, f_lower, f_upper)

      # Ignore the display of following codes

      # Plot results
      plot(y_union, plot.type = "single",
          xlim = c(1995, 2026),
          ylim = c(), ylab = "Year-Quarter", 
          lty = c("solid", "solid", "dashed", "dashed"),
          col = c("lightgray", "red", "black", "black"),
          main = mymain)

      # Legend
      legend(legend = c("Observations", "Mean (predictive distribution)", "95% intervals (predictive distribution)"),
            lty = c("solid", "solid", "dashed"),
            col = c("lightgray", "red", "black"),
            x = "topleft", cex = 0.6)

      # Grid
      grid(nx = NULL, ny = NULL,
          lty = 2,      # Grid line type
          col = "gray", # Grid line color
          lwd = 1)      # Grid line width

    }
    dev.off()
  }
}


##########################
# Kalman_3
##########################

y = dt1[,10]

main = colNames[10]

png(paste("output/GDP/kalman2/plot_", 100, ".png", sep = ""), res = 100, width = 1200, height = 1000)
par(mfrow = c(3, 1), mar = c(3, 1, 1, 3))

# Model setting: local-trend model + seasonal model (time-domain approach)
build_dlm_y <- function(par){
  return(
    dlmModPoly(order = 2, dW = exp(par[1:2]), dV = exp(par[3])) +
    dlmModSeas(frequency = 4, dW = c(exp(par[4]), rep(0, times = 2)), dV = 0)
  )
}

# Maximum likelihood estimation of parameters and confirmation of the results
fit_dlm_y <- dlmMLE(y = y, parm = rep(0, 4), build = build_dlm_y)
fit_dlm_y

# Set the maximum likelihood estimates of parameters in the model
mod <- build_dlm_y(fit_dlm_y$par)

# Kalman smoothing
dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

# Mean of the smoothing distribution
   mu <- dropFirst(dlmSmoothed_obj$s[, 1])
gamma <- dropFirst(dlmSmoothed_obj$s[, 3])

# Plot results
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(3, 1)); par(oma = c(2, 0, 0, 0)); par(mar = c(2, 4, 1, 1))
ts.plot(    y, ylab = "Observations (log-transformed)", main = main)
ts.plot(   mu, ylab = "Level component")
ts.plot(gamma, ylab = "Seasonal component")
mtext(text = "Time", side = 1, line = 1, outer = TRUE)
par(oldpar)
dev.off()