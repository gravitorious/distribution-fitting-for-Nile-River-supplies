EmpCdf = function(data, i, plot=1) {
  
  cdf = ppoints(n = data, a = 0)
  xx = sort(data)
  
  dfcdf=data.frame(u = cdf, x = xx)
  
  if (plot==1) {
    plot(xx, cdf, type='p', main=paste('Empirical CDF for month:', i), 
         xlab='m^3 10^9', ylab='Prob.', pch = 19, xlim = c(0, 28) )
  }
  
  return(dfcdf)
} 



