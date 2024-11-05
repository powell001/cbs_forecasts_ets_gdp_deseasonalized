library(tidyverse)
library(xts)
library(zoo)
library(svglite)
library(TSstudio)
library(zoo)
library(dlm)
library(forecast)
library(expsmooth)
library(ggplot2)
#library(ggfortify)
library(changepoint)
library(KFAS)
library(httpgd)
library(funtimes)
library(seastests)
library(car)
library(lmtest)

# Time series plots
# https://cran.rstudio.com/web/packages/ggfortify/vignettes/plot_ts.html


# Model consists of three letters following Hyndman (2008) and here: https://search.r-project.org/CRAN/refmans/forecast/html/ets.html

# First letter is the error type:     A, M or Z
# Second letter is the trend type:    N, A, M, Z
# Third letter is the season type:    N, A, M, Z

# Some of the models have names:
#     ANN is simple exponential smoothing with additive errors.
#     MAM is multiplicative Holt-Winters with multiplicative errors.


##########################

# load data
dt1 <- read.csv("data/cbs_basic_macro_SEASONCORRECTED_qt.csv", sep = ",")
summary(dt1)

# not appropriate for negative numbersw
drops <- c("change_supply_season")
dt1 <- dt1[ , !(names(dt1) %in% drops)]
colnames(dt1)
dim(dt1)

allColumns <- colnames(dt1)

# remove Perioden
allColumns_1 <- allColumns[c(-1)]

##########################
##########################
##########################
for(colName in allColumns_1){ 

#colName <- "gdp_total_season"
print(colName)

# connects all the data
Key1 <- paste(Sys.Date(), "_", colName, sep="")

series1 <- ts(dt1[colName], frequency = 4, start=c(1995,1))
series1 <- na.omit(series1)

plot(series1)

#########################
# Which model to use
#########################

###########################
# Trend or not?
###########################

p_value <- try(notrend_test(series1)$p.value)
print(p_value)

if (is.numeric(p_value) == FALSE) {
  print("assume: Has Trend")
  trendtype <- "A"
} else {
     if (p_value < 0.05) { ############################# CHECK THIS ######################
      print("Has Trend")
      trendtype <- "A"
    } else {
      print("No Trend")
      trendtype <- "N"
      }
}

print(trendtype)

###########################
# Additive or multiplictive?
###########################

decompose_series1 <- decompose(series1, "multiplicative")
decompose_series1_multiplicative <- decompose_series1$random
muladd_mul <- sqrt(mean(abs(decompose_series1_multiplicative)^2, na.rm=TRUE))

decompose_series1 <- decompose(series1, "additive")
decompose_series1_additive <- decompose_series1$random
muladd_add <- sqrt(mean(abs(decompose_series1_additive)^2, na.rm=TRUE))

if (muladd_mul < muladd_add) {
  print("Use Multiplicative")
  errortype <- "M"
  } else {
    print("Use Additive")
    errortype <- "A"
}

print(errortype)

###########################
# Seasonnal or not
###########################

season_Check <- isSeasonal(series1)

if (season_Check == TRUE) {
  print("Use Seasonal")
  seasontype <- "A"
  } else {
    print ("Use Non-Seasonal")
    seasontype <- "N"
  }

print(seasontype)

# error, trend, season

# First letter is the error type:     A, M or Z
# Second letter is the trend type:    N, A, M, Z
# Third letter is the season type:    N, A, M, Z

modelform <- str_c(c(errortype, trendtype, seasontype), collapse = "")

fit <- ets(series1, model=modelform, damped=FALSE)

############
# test period
#############
test_period <- 4

h1 <- test_period
train <- head(series1, round(length(series1) - h1))
test <- tail(series1, h1)

fit <- ets(train, model=modelform, damped=FALSE)
forecasted1 <- forecast(fit, h=h1)

png(filename=paste("output/figures/", Key1, "TrainTestForecast.png", sep = "_"))
print(autoplot(forecasted1, series = "In-sample Forecast", include=h1+16) + autolayer(test, series = "Historical Data") + ggtitle(colName)) + ylab("Historical + Forecast")
dev.off()

####################
# final forecast
####################
fit <- ets(series1, model=modelform, damped=FALSE)
forecast_oneMonth <- forecast(fit, h=1)

png(filename=paste("output/figures/", Key1, "final_forecasts.png", sep = "_"))
print(autoplot(tail(series1, 20)) + autolayer(forecast_oneMonth) + ggtitle(colName))
dev.off()

################################
# Saving
################################

###
# Raw Data
###

data <- data.frame(
  SeriesName   = colName, 
  DateAnalysis = Sys.Date(), 
  ETSmodel = modelform,
  ObservationDate = as.yearqtr(time(series1)),  ##### Careful here, yearmonth is not the same as yearqtr
  RawData = series1
)
data$Key1 <- Key1

colnames(data) <- c("SeriesName", "DateAnalysis", "ETSmodel", "ObservationDate", "RawData", "Key1")

write.table(data, file = paste("output/forecasts/", Key1, "RawData.csv", sep="_"), sep =",",row.names = FALSE)

###
# TrainTestForecast
###
forecast_tibble <- as.data.frame(forecasted1)
forecast_tibble$Key1 <- Key1 

write.table(forecast_tibble, file = paste("output/forecasts/", Key1, "TrainTestForecast.csv", sep="_"), sep =",",row.names = FALSE)

###
# finalForecast
###
forecast_oneMonth <- forecast(fit, h=1)
finalForecast <- as.data.frame(forecast_oneMonth, row.names = NULL)
finalForecast$Key1 <- Key1
finalForecast <- tibble::rownames_to_column(finalForecast, "Forecast_Period")  

write.table(finalForecast, file = paste("output/forecasts/", Key1, "final_forecasts.csv", sep="_"), sep =",",row.names = FALSE)

} 
############# END LOOP ##############
############# END LOOP ##############
############# END LOOP ##############

##############################
# Possible analyses
##############################

###
# Combine all forecasts
###

files <- list.files("output/forecasts" , pattern = "final_forecasts.*\\.csv$", full.names = TRUE)
initial_df <- read.csv(files[1])

# Merge data frames using a loop
for (fl in files[2:length(files)]) {
  file_df <- read.csv(fl)
  initial_df <- rbind(initial_df, file_df)
}
# View the merged data frame
print(initial_df)
write.table(initial_df, file = "output/analyses/combined_final_forecasts.csv", sep =",",row.names = FALSE)

###
# Combine point forecast + entire series 
###

files_Raw <- list.files("output/forecasts" , pattern = "RawData.*\\.csv$", full.names = TRUE)
files_FinalForecasts <- list.files("output/forecasts" , pattern = "final_forecasts.*\\.csv$", full.names = TRUE)

###################################
###################################
###################################
index <- 0
for(fl in files_Raw) {

index <- index + 1

print(index)

# get raw data
firstfile <- files_Raw[index]
initial_df_raw <- read.csv(firstfile)
tmp_raw <- initial_df_raw[,c(4,5)]

series_name <- initial_df_raw[,c(1)][1]

# get final forecasts
firstfile <- files_FinalForecasts[index]
initial_df_finalfore <- read.csv(firstfile)
tmp_forecast <- initial_df_finalfore[,c(1,2)]

# copy column names
colnames(tmp_forecast) <- colnames(tmp_raw)
combined <- rbind(tmp_raw, tmp_forecast)

# change to proper date
#combined$ObservationDate <- as.Date(paste(1, combined$ObservationDate), '%d %B %Y')

# colors for different parts of line
combined$mycolors <- c(rep('hist', length(combined[,1])-2), rep(c('forecast'),2))
combined$mycolors <- as.factor(combined$mycolors)
combined$ObservationDate <- as.Date(as.yearqtr(combined$ObservationDate), frac = 0)

png(filename=paste("output/figures/", series_name, "series_and_forecast.png", sep = "_"))
print(ggplot(combined, aes(x = ObservationDate, y = RawData, col = mycolors, group = 1)) + geom_line())
dev.off()

###
# Combine point forecast + entire series (using data above)
###
combined$seriesDifferenced <- combined['RawData'] - lag(combined['RawData'], 4)
combined$seriesDifferenced <- unlist(combined$seriesDifferenced)


png(filename=paste("output/figures/", series_name, "differenced_forecasts.png", sep = "_"))
plot(ts(combined[,c(2,4)], frequency = 4, start=c(1995,1)), main=series_name)
dev.off()

}
