#created by Nikolas Mavrogeneiadis - 161014

# Load libraries ----
library(xts)
library(lmom)
library(lubridate)
library(moments)
library(lmom)
library(fitdistrplus)

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
means = rep(NA, 12)
stds = rep(NA, 12)
skwns = rep(NA, 12)

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
  ratiomom[[i]] = c(lmoments[[i]][2]/lmoments[[i]][1], lmoments[[i]][3]/lmoments[[i]][2]) #calculate the L-variation and L-skewness via L2/L1 and L3/L2
}





# Fit the gamma distribution to the data ----
layout_matrix <- matrix(1:6, ncol = 3) 
layout(layout_matrix)



 #Vector of quantiles
gamdis = list()
thgam = list()
for(i in 1:12){
  sample = get_month(maindata, i, 1)
  a = EmpCdf(sample, i, 0)
  temp = seq(from = 0, to = max(a[, 2])+1, by = 0.0001)
  gamdis[[i]] = lmom::pelgam(lmom = lmoments[[i]]) #we pass the first 3 moments, but L3 is not used
  thgam[[i]] = lmom::cdfgam(x=temp,para = gamdis[[i]])
}

# Plot empirical and theoretical CDF of the last 6 time series by each month ----

for(i in 1:6){
  sample = get_month(maindata, i, 1)
  a = EmpCdf(sample, i)
  temp = seq(from = 0, to = max(a[, 2])+1, by = 0.0001)
  lines(temp, thgam[[i]], col='magenta', lwd = 3)
}

#create a time series expressing the mean Nile River supplies for each year ----
ymaindata = xts::apply.yearly(x = maindata, mean)


# Fit the Weibull distribution to the data ----
#temp2 = seq(from = 0, to = 28, by = 0.0001)
#weidis = list()
#thwei = list()
#for(i in 1:12){
#  weidis[[i]] = lmom::pelwei(lmom = lmoments[[i]]) #we pass the first 3 moments, but L3 is not used
#  thwei[[i]] = lmom::cdfwei(x = temp2, para = weidis[[i]])
#}
#layout_matrix2 <- matrix(1:6, ncol = 3) 
#layout(layout_matrix2)
#for(i in 7:12){
#  sample2 = get_month(maindata, i, 1)
#  EmpCdf(sample2, i)
#  lines(temp, thwei[[i]], col='blue', lwd =3)
#}

