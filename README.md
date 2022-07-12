Given the monthly time series of supplies in billions of cubic meters of the Nile River for the period 1/1/1870 to 1/12/1945 we fit two different distributions, the gamma, and Weibull, for each month. For fitting and estimating the parameters, we use the λ-moments method (you can read more about this method at https://www.jstor.org/stable/2345653). Finally, we compare the two distributions by measuring the root mean square error and plotting the errors. 
We represent time series using xts packet.


```
├── dfitting.R 					#main program
├── EmpCdf.R					#custom function to calculate the empirical CDF
├── errors						#final plot of the errors
├── gammadistr					#plots fitting Gamma distribution
├── get_month.R					#custom function to get the data for each month
├── NileData_BCM.txt			#main data for Nile River supplies
├── weibulldistr				#plots fitting Weibull distribution
└── yearlyfolder				#Convert the monthly time series to a time series expressing the average annual supply
```
