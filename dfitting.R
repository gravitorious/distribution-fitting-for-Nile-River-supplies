#created by Nikolas Mavrogeneiadis - 161014

# Load libraries ----
library(xts)
library(lmom)
library(lubridate)
library(moments)
library(lmom)

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

# Calculate mean, standard deviation and skewness for each month ----
means = c()
stds = c()
skwns = c()

for(i in 1:12){
  sample = get_month(maindata, i, 1)
  means[i] = mean(sample)
  stds[i] = sd(sample)
  skwns[i] = moments::skewness(sample)
}

# Calculate empirical moments ----
lmoments = list()
ratiomom = list()

for(i in 1:12){
  sample = get_month(maindata, i, 1)
  lmoments[[i]] = lmom::samlmu(x = sample, nmom = 3, ratios = F) #calculate L1, L2, L3
  ratiomom[[i]] = c(lmoments[[i]][2]/lmoments[[i]][1], lmoments[[i]][3]/lmoments[[i]][2]) #calculate the L-variation and L-skewness via L2/L2 and L3/L2
}

