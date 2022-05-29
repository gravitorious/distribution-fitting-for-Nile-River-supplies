#created by Nikolas Mavrogeneiadis - 161014

# Load libraries ----
library(xts)
library(lmom)
library(lubridate)

# Load functions ----
source("get_month.R")

# Load time series and create the appropriate xts object----
fileName = "NileData_BCM.txt"
filedata = read.csv(file = fileName)
head(rownames(filedata))
alldata = xts(x = filedata$data, order.by = as.Date(filedata$Date))
colnames(alldata) = "data"
#choose start and end day
startday = "1871-01-01"; endDay = "1944-12-01"
maindata = alldata[paste(startday, endDay, sep = "/")]

# Plot the time series ----
#layout(matrix(c(1,2), 1, 2, byrow = TRUE))
#hist(maindata)
#plot(maindata)

