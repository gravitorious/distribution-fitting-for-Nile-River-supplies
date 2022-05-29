#created by Nikolas Mavrogeneiadis - 161014

# Load libraries ----
library(xts)
library(lmom)
library(lubridate)
library(moments)
library(lmom)

# Load functions ----
# we assume that functions are in the same folder with the current file
source("get_month.R")
source("EmpCdf.R")

# Load time series and create the appropriate xts object----
fileName = "NileData_BCM.txt"
filedata = read.csv(file = fileName)
head(rownames(filedata))
alldata = xts(x = filedata$data, order.by = as.Date(filedata$Date))
colnames(alldata) = "data"
#choose start and end day
startday = "1871-01-01"; endDay = "1944-12-01"
maindata = alldata[paste(startday, endDay, sep = "/")]

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

# Fit the gamma distribution to the data ----
temp = seq(from = 0, to = 28, by = 0.0001)
gamdis = list()
thgam = list()
for(i in 1:12){
  gamdis[[i]] = lmom::pelgam(lmom = lmoments[[i]]) #L3 = 0
  thgam[[i]] = lmom::cdfgam(x = temp, para = gamdis[[i]])
}

# Plot empirical and theoretical CDF of the time series by each month ----
layout_matrix <- matrix(1:6, ncol = 3) 
layout(layout_matrix)
for(i in 7:12){
  sample = get_month(maindata, i, 1)
  EmpCdf(sample, i)
  lines(temp, thgam[[i]], col='magenta', lwd =3)
}


