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
library(data.table)
library(lubridate)
library(stringr)
library(zoo)

##############################
# Possible analyses
##############################

new_data <- "cbs_basic_macro_SEASONCORRECTED_qt_2024_11_18.csv"


# unprocessed data
rawDataFile <- paste0("data/", new_data)

# remove files
do.call(file.remove, list(list.files("output/analyses", full.names = TRUE)))


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

# Remove duplicates
initial_df <- initial_df[!duplicated(initial_df[, c(2,3)]),]

# Remove Totaal
 initial_df <- initial_df[!grepl("Totaal", initial_df[['Key1']]),]

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

######################
# Using biggest changers in Percentage Change, rank list of biggest changes last quarter, remove if missing
######################

fun_remove_Totaals <- function(rawDataFile){

    print("REMOVE TOTAALS")

    df1 <- read.csv(rawDataFile, stringsAsFactors=FALSE)
    # dont understand those Totaal columns yet, remove them
    df1 <- df1 %>% select(-contains('Totaal'))

    write.table(df1, file = "data/cbs_basic_macro_allData_qt.csv", sep =",",row.names = FALSE)
 }    

fun_remove_Totaals(rawDataFile)

fun_bigchanges_absolute_percent <- function(rawDataFile){
    
    bigchangers_df <- read.csv(rawDataFile, stringsAsFactors=FALSE)

    # create row names from list of variables, then drop variables so all columns numeric
    rownames(bigchangers_df) <- bigchangers_df[,1] 
    bigchangers_df <- bigchangers_df[,-1]

    # dont understand those Totaal columns yet, remove them
    bigchangers_df <- bigchangers_df %>% select(-contains('Totaal'))

    # get value four periods before last value
    df_yearBefore <- bigchangers_df[nrow(bigchangers_df)-4,]

    # these are the absolute differences from four years previous
    df1 <- data.frame(diff(as.matrix(bigchangers_df),4))
    absoluteDiff_lastRow <- t(tail(df1,1))

     # divide last column by column 4 periods before and divide by that same column
    percent_YearBefore <- t(100*(absoluteDiff_lastRow/df_yearBefore))

    # merge the two dataframes
    mrg1 <- merge(t(df_yearBefore), absoluteDiff_lastRow, by = 'row.names', all = TRUE)

    # remove duplicates
    lastCol <- ncol(mrg1)
    mrg1 <- mrg1[!duplicated(mrg1[, c(lastCol-1, lastCol)]),]
    
    # order, sort data
    lastCol <- tail(names(mrg1),1)
    o <- order(abs(mrg1[,lastCol]), decreasing = TRUE)
    output1 <- mrg1[o, ]#[c(1,ncol(mrg1))]

    write.table(output1, file = "output/analyses/real_changes_yearBefore.csv", sep =",",row.names = FALSE)
}    

fun_bigchanges_absolute_percent(rawDataFile)


fun_make_figures <- function(raw_data){
    index <- 0
    for(fl in raw_data) {

        index <- index + 1

        print(index)

        # get raw data
        firstfile <- raw_data[index]
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
}

fun_make_figures(files_Raw)

######################
# Type of model for each analysis
######################

fun_ETS_Used <- function(){

    files_Raw  <- list.files("output/forecasts" , pattern = "RawData.*\\.csv$", full.names = TRUE)
    initial_df <- read.csv(files_Raw[1])[1,c(3,1)]

    # Merge data frames using a loop

    for (fl in files_Raw[2:length(files_Raw)]) {
    file_df <- read.csv(fl)
    initial_df <- rbind(initial_df, file_df[1,c(3,1)])
    } 

    initial_df <- initial_df[!grepl("Totaal", initial_df[['SeriesName']]),]


    write.table(initial_df, file = "output/analyses/combined_model_used.csv", sep =",",row.names = FALSE)
}

fun_ETS_Used()

######################
# Biggest change since a year before, based on Forecasts
######################

fun_bigchanges <- function(){
    mylist <- list()
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

        # colors for different parts of line
        combined$mycolors <- c(rep('hist', length(combined[,1])-2), rep(c('forecast'),2))
        combined$mycolors <- as.factor(combined$mycolors)
        combined$ObservationDate <- as.Date(as.yearqtr(combined$ObservationDate), frac = 0)

        ###
        # Combine point forecast + entire series (using data above)
        ###
        combined$seriesDifferenced <- combined['RawData'] - lag(combined['RawData'], 4)
        combined$seriesDifferenced <- unlist(combined$seriesDifferenced)

        df1 <- t(combined[,c(4)])
        colnames(df1) <- t(combined[c(1)])
        df2 <- as.data.frame(df1)
        df3 <- cbind(series_name, df2)

        mylist[[index]] <- as.data.frame(df3)

    }

    series_hist_forecast <- list_rbind(mylist)

    # remove duplicates
    lastCol <- ncol(series_hist_forecast)
    output1 <- series_hist_forecast[!duplicated(series_hist_forecast[, c(lastCol-1, lastCol)]),]

    # remove Totaal
    output1 <- output1[!grepl("Totaal", output1[['series_name']]),]


    write.table(output1, file = "output/analyses/combined_series_hist_forecasts_differences.csv", sep =",",row.names = FALSE)
}

fun_bigchanges()

######################
# Forecasts, using biggest changers in absolute value, rank list of biggest changes last quarter with forecast, remove if missing
######################

fun_bigchanges_absoluteValue_forecastedData <- function(){
    myfile <- "output/analyses/combined_series_hist_forecasts_differences.csv"
    bigchangers_df <- read.csv(myfile, stringsAsFactors=FALSE)

    bigchangers_df <- bigchangers_df[!grepl("Totaal", bigchangers_df$series_name),]

    names(bigchangers_df) <- format(as.Date(paste0(names(bigchangers_df)), 'X%Y.%m.%d'))
    lastCol <- tail(names(bigchangers_df),1)
    o <- order(abs(bigchangers_df[,lastCol]), decreasing = TRUE)
    output1 <- bigchangers_df[o, ][c(1,ncol(bigchangers_df))]

    # omit na
    output1 <- na.omit(output1)

    # remove duplicates
    output2 <- output1[!duplicated(output1[, c(2)]),]
    

    write.table(output2, file = "output/analyses/forecasts_changes_absoluteValue.csv", sep =",",row.names = FALSE)
}

fun_bigchanges_absoluteValue_forecastedData()


######################
# Forecasts, using biggest changers in percentage, rank list of biggest changes last quarter with forecast, remove if missing
######################

fun_big_Percentagechanges_forecastedData <- function(rawDataFile){ 

    # get forecasts from combined_final_forecasts.csv
    myfile <- "output/analyses/combined_series_hist_forecasts_differences.csv"
    df1 <- read.csv(myfile, stringsAsFactors=FALSE)

    # get name of varible and forecast (last column)
    forecast1 <- df1[, c(1,ncol(df1))]

    # move variable name to rowname
    rownames(forecast1) <- forecast1[,1] 

    # change format of date
    names(forecast1) <- format(as.Date(paste0(names(forecast1)), 'X%Y.%m.%d'))

    # get date that is one year before
    dateColumnWeWant <- as.Date(names(forecast1)[2]) - years(1)

    ### Read in raw data
    # absolute value a year before
    df1 <- read.csv(rawDataFile, stringsAsFactors=FALSE)

    # select row equal to the date we want
    df2 <- as.data.frame(t(df1[df1$X %like% dateColumnWeWant,]), stringsAsFactors=FALSE)

    # merge the two dataframes
    df3 <- merge(forecast1, df2, by = 'row.names')

    # remove redundant columns
    df3[,c(2)] <- NULL

    # meaningful column names
    names(df3) <- c("Row.names", "DifferForecast", "YearAgo")

    # percentage change
    df3$PercentChange <- 100 * df3[,'DifferForecast']/as.numeric(df3[,'YearAgo'])

    secondCol <- tail(names(df3))[2]  ################## Careful, this might change
    o <- order(abs(as.numeric(df3[,secondCol])), decreasing = TRUE)
    output1 <- df3[o, ]#[c(1,ncol(df3))]

    # remove duplicates
    lastCol <- ncol(output1)
    output1 <- output1[!duplicated(output1[, c(lastCol-1, lastCol)]),]

    output2 <- output1[!grepl("Totaal", output1[['Row.names']]),]

  

    write.table(output2, file = "output/analyses/forecasts_changes_percentageChange.csv", sep =",",row.names = FALSE)

}

fun_big_Percentagechanges_forecastedData(rawDataFile)


######################
# Historical data plus level forecasts
######################

fun_combine_hist_forecast <- function(){ 

    data <- read.csv(rawDataFile)
    data$X <- as.Date(data$X)
    rownames(data) <- data$X

    finalForecasts <- read.csv("output/analyses/combined_final_forecasts.csv")
    finalForecasts$featureNames <- str_sub(finalForecasts$Key1, 12)
    finalForecasts$X <- as.Date(as.yearqtr(finalForecasts$Forecast_Period, format = "%Y Q%q"))
    f1 <- finalForecasts[c('Point.Forecast','featureNames','X')]

    f2 <- f1 |> 
    pivot_wider(names_from = featureNames, 
                values_from = Point.Forecast)

    f2 <- as.data.frame(f2)
    f2 <- f2[names(data)]
    f3 <- f2[order(f2$X),]
    rownames(f3) <- f3$X

    replaceThese <- which(is.na(data[nrow(data), ]), arr.ind=TRUE)
    lastDate <- data[nrow(data), 'X']

    for (i in as.numeric(replaceThese[, 2])) {
        data[nrow(data), i] <- f3[nrow(f3)-1, i]
    }

    output1 <- rbind(data, tail(f3,1))

    write.table(output1, file = "output/analyses/combined_historical_forecasts_levels.csv", sep =",",row.names = FALSE)

}

fun_combine_hist_forecast()


100 * ((238623 - 237780)/237780)
