get_month=function(ts, mon, asvec=0){
  # ts: an xts object
  # mon: a scalar between 1 and 12 indicating which month we wish to select. e.g., if mon = 3, it will return only the March data.
  # asvec: Boolean, indicating if the function will return a a vector (asvec=1) or an xts object (asvec=0)
  D = ts[lubridate::month(ts) == mon]
  
  if (asvec == 1){ D = as.vector(D) }
  return(D)
}