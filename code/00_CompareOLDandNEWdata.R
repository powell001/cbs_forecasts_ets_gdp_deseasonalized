library("dplyr")   
library("diffdf")

data1 <- read.csv("data/cbs_basic_macro_SEASONCORRECTED_qt_2024_11_09.csv", sep = ",")
data2 <- read.csv("data/cbs_basic_macro_SEASONCORRECTED_qt_2024_11_13.csv", sep = ",")

all_equal(data1, data2)  

diffdf(data1, data2)
