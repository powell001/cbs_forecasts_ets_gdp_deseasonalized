install.packages("dplyr")               
library("dplyr")   
library("diffdf")

data1 <- read.csv("data/HandelDiensten_raw.csv", sep = ",")
data2 <- read.csv("data/HandelDiensten_raw1_2024_11_01.csv", sep = ",")

all_equal(data1, data2)  

diffdf(data1, data2)
