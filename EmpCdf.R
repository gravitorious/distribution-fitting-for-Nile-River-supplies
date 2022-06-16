EmpCdf = function(data, i, name, plot=1) {
  
  cdf = ppoints(n = data, a = 0)
  xx = sort(data)
  
  dfcdf=data.frame(u = cdf, x = xx)
  
  if (plot==1) {
    plot(xx, cdf, type='p', main=paste(name, 'distribution fit for month:', i), 
         xlab=bquote(m^3 ~ 10^9), ylab='Prob.', pch = 19, xlim = c(0, max( dfcdf[, 2])+1) )
  }
  
  return(dfcdf)
} 










