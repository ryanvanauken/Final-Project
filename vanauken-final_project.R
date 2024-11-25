#set working directory
setwd("H:/sta215")
#load data
raw_data <- read.csv("raw_data.csv")
table(raw_data$gender, raw_data$abortion)
