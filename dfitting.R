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
  ratiomom[[i]] = c(lmoments[[i]][2]/lmoments[[i]][1], lmoments[[i]][3]/lmoments[[i]][2]) #calculate the L-variation and L-skewness via L2/L1 and L3/L2
}

# Fit the gamma distribution to the data ----
temp = seq(from = 0, to = 28, by = 0.0001)
gamdis = list()
thgam = list()
for(i in 1:12){
  gamdis[[i]] = lmom::pelgam(lmom = lmoments[[i]]) #we pass the first 3 moments, but L3 is not used
  thgam[[i]] = lmom::cdfgam(x = temp, para = gamdis[[i]])
}

# Plot empirical and theoretical CDF of the last 6 time series by each month ----
layout_matrix <- matrix(1:6, ncol = 3) 
layout(layout_matrix)
for(i in 7:12){
  sample = get_month(maindata, i, 1)
  EmpCdf(sample, i)
  lines(temp, thgam[[i]], col='magenta', lwd =3)
}

#create a time series expressing the mean Nile River supplies for each year ----
ymaindata = xts::apply.yearly(x = maindata, mean)


# Fit the Weibull distribution to the data
temp2 = seq(from = 0, to = 28, by = 0.0001)
weidis = list()
thwei = list()
for(i in 1:12){
  weidis[[i]] = lmom::pelwei(lmom = lmoments[[i]]) #we pass the first 3 moments, but L3 is not used
  thwei[[i]] = lmom::cdfwei(x = temp2, para = weidis[[i]])
}
layout_matrix2 <- matrix(1:6, ncol = 3) 
layout(layout_matrix2)
for(i in 7:12){
  sample2 = get_month(maindata, i, 1)
  EmpCdf(sample2, i)
  lines(temp, thwei[[i]], col='blue', lwd =3)
}

# Fit the lognormal, weibull and gamma distributions for month 9 with fitdistrplus library
sample3 = get_month(maindata, 9, 1)
fitGamma= fitdist(data = sample3, distr = 'gamma', method = 'mle') #gamma

fitWei = fitdist(data = sample3, distr = 'weibull', method = 'mle') #weibull

fitLnorm = fitdist(data = sample3, distr = 'lnorm', method = 'mle') #lognormal

Fits=list()
Fits[[1]] = fitGamma
Fits[[2]] = fitWei
Fits[[3]] = fitLnorm
fitdistrplus::cdfcomp(ft = Fits)

# print the Akaike information criterion (AIC) value for each distribution
#AIC = 2k – 2(Log-Likelihood) where k is the number of parameters.
print(paste("The AIC for Gamma distribution is:", fitGamma$aic))
print(paste("The AIC for Weibull distribution is:", fitWei$aic))
print(paste("The AIC for LogNormal distribution is:", fitLnorm$aic))
summary(fitGamma)
summary(fitWei)
summary(fitLnorm)
