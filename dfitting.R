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


# create folder that we will store plots
foldername = "gammadistr"
dir.create(foldername)

wfoldername = "weibulldistr"
dir.create(wfoldername)

errorfolder = 'errors'
dir.create(errorfolder)


# Load time series and create the appropriate xts object----
fileName = "NileData_BCM.txt"
filedata = read.csv(file = fileName)
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



gamdis = list()
thgam = list()
for(i in 1:12){
  sample = get_month(maindata, i, 1)
  a = EmpCdf(sample, i, "Gamma", 0)
  temp = seq(from = 0, to = max(a[, 2])+1, by = 0.0001)
  gamdis[[i]] = lmom::pelgam(lmom = lmoments[[i]]) #we pass the first 3 moments, but L3 is not used
  thgam[[i]] = lmom::cdfgam(x=temp,para = gamdis[[i]])
}

dtfm = list() #list with dataframes for each month for theoritical distr of gamma function

# Plot empirical and theoretical CDF by each month ----
for(i in 1:6){
  sample = get_month(maindata, i, 1)
  #plot to file
  plotfile = gsub(" ", "", paste(foldername, "/Month", i, ".png", sep = ""))
  png(file=plotfile, width=600, height=350)
  a = EmpCdf(sample, i, "Gamma")
  temp = seq(from = 0, to = max(a[, 2])+1, by = 0.0001)
  lines(temp, thgam[[i]], col='magenta', lwd = 3)
  dev.off()
  #plot to screen
  a = EmpCdf(sample, i, "Gamma")
  lines(temp, thgam[[i]], col='magenta', lwd = 3)
  
  #save to dataframe
  dtfm[[i]] = data.frame(temp, thgam[[i]])
  colnames(dtfm[[i]]) = c('x', 'prob')
}


layout_matrix2 <- matrix(1:6, ncol = 3) 
layout(layout_matrix2)
for(i in 7:12){
  sample = get_month(maindata, i, 1)
  #plot to file
  plotfile = gsub(" ", "", paste(foldername, "/Month", i, ".png", sep = ""))
  png(file=plotfile, width=600, height=350)
  a = EmpCdf(sample, i, "Gamma")
  temp = seq(from = 0, to = max(a[, 2])+1, by = 0.0001)
  lines(temp, thgam[[i]], col='magenta', lwd = 3)
  dev.off()
  #plot to screen
  a = EmpCdf(sample, i, "Gamma")
  lines(temp, thgam[[i]], col='magenta', lwd = 3)
  
  
  #save to dataframe
  dtfm[[i]] = data.frame(temp, thgam[[i]])
  colnames(dtfm[[i]]) = c('x', 'prob')
}


#create a time series expressing the mean Nile River supplies for each year ----
ymaindata = xts::apply.yearly(x = maindata, mean)


# Fit the Weibull distribution to the data ----
gdtfm = list() #list with dataframes for each month for theoritical distr of gamma function
weidis = list()
thwei = list()
temp2 = NA
for(i in 1:12){
  sample = get_month(maindata, i, 1)
  a = EmpCdf(sample, i, "Weibull", 0)
  temp2 = seq(from = 0, to = max(a[, 2])+1, by = 0.0001)
  weidis[[i]] = lmom::pelwei(lmom = lmoments[[i]]) #we pass the first 3 moments, but L3 is not used
  thwei[[i]] = lmom::cdfwei(x = temp2, para = weidis[[i]])
}

#save plots
# Plot empirical and theoretical CDF by each month ----
for(i in 1:12){
  sample = get_month(maindata, i, 1)
  #plot to file
  plotfile = gsub(" ", "", paste(wfoldername, "/Month", i, ".png", sep = ""))
  png(file=plotfile, width=600, height=350)
  a = EmpCdf(sample, i, "Weibull")
  temp = seq(from = 0, to = max(a[, 2])+1, by = 0.0001)
  lines(temp, thwei[[i]], col='blue', lwd = 3)
  dev.off()
  #save to dataframe
  gdtfm[[i]] = data.frame(temp, thwei[[i]])
  colnames(gdtfm[[i]]) = c('x', 'prob')
}

#the following code would be better to be a function...
#counting errors for gamma distribution
gammaerrors = rep(NA,12)
for(j in 1:12){
  sample = get_month(maindata, j, 1)
  empcdf = EmpCdf(sample, j, "Gamma", 0)
  sum = 0
  for(i in 1:length(sample)){ #74
    empvalue = empcdf$u[empcdf$x == sample[i]]
    if(length(empvalue) > 1){
      empvalue = empvalue[length(empvalue)]
    }
    thvalue = dtfm[[j]]$prob[dtfm[[j]]$x == signif(sample[i], digits = 3)]
    #check if thvalue is empty. Problem with compare floating point numbers. 
    #How can we solve that?
    sample[i] = signif(sample[i], digits = 5)
    try = 0
    while(length(thvalue) == 0){
      thvalue = dtfm[[j]]$prob[dtfm[[j]]$x == sample[i]+try] #try the next one
      try = try + 0.001
    }
    thvalue = signif(thvalue, digits = 4)
    empvalue = signif(empvalue, digits = 4)
    sum = sum + signif((thvalue - empvalue)^2, digits = 6)
  }
  
  gammaerror = sqrt(sum)
  gammaerror = signif(gammaerror, digits = 6)
  gammaerrors[j] = gammaerror
  #message('Error for month: ', j, 'is: ', gammaerror)
}


#counting errors for weibull distribution
weibullerrors = rep(NA,12)
for(j in 1:12){
  sample = get_month(maindata, j, 1)
  empcdf = EmpCdf(sample, j, "Weibull", 0)
  sum = 0
  for(i in 1:length(sample)){ #74
    empvalue = empcdf$u[empcdf$x == sample[i]]
    if(length(empvalue) > 1){
      empvalue = empvalue[length(empvalue)]
    }
    thvalue = gdtfm[[j]]$prob[gdtfm[[j]]$x == signif(sample[i], digits = 3)]
    #check if thvalue is empty. Problem with compare floating point numbers. 
    #How can we solve that?
    sample[i] = signif(sample[i], digits = 5)
    try = 0
    while(length(thvalue) == 0){
      thvalue = gdtfm[[j]]$prob[gdtfm[[j]]$x == sample[i]+try] #try the next one
      try = try + 0.001
    }
    thvalue = signif(thvalue, digits = 4)
    empvalue = signif(empvalue, digits = 4)
    sum = sum + signif((thvalue - empvalue)^2, digits = 6)
  }
  
  weibullerror = sqrt(sum)
  weibullerror = signif(weibullerror, digits = 6)
  weibullerrors[j] = weibullerror
  message('Error for month: ', j, 'is: ', weibullerror)
}

layout_matrix3 <- matrix(1:1) 
layout(layout_matrix3)
plotfile = gsub(" ", "", paste(errorfolder, "/errors.png"))
png(file=plotfile, width=600, height=350)

months = 1:12
plot(months,gammaerrors, type = 'l' ,col="red", xlab = 'months', ylab = 'errors')
lines(months,weibullerrors,col="green")

dev.off()

