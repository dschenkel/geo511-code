##
## general_functions: several functions used for statistics and trend analysis for all three research questions
## dschenkel, mar 2015
##

library(caTools)
library(raster)
library(stats)
library(zoo)


################################################

#####		Statistics part                #####

################################################


##
## correlate_2d
##
## correlate 2 matrices, output linear regression statistics
## 
## input: 
## map1, map2: 2 matrices with the same extents to be compared
## namelist: list with names for:
##	- two maps (map1, map2), 
##  - statistics filename to use for output (statfile),
##  - title of the plot (title),
##  - rootdir for output (outdir)
## output: csv with linear regression stats and jpg of scatterplot, linfit and R^2
correlate_2d <- function(map1, map2, namelist, mask ){
	
   # stopifnot(checkDim3(A,B))
    A = map1*mask
    B = map2*mask
	
	datafr = data.frame(as.vector(A), as.vector(B))
	colnames(datafr) <- c(namelist$map1,namelist$map2)
	
	lm.out = lm(datafr, na.action=na.exclude)
	
	# print scatterplot with R^2 and linear fit
	jpeg(paste(namelist$outdir,namelist$statfile,"_corr.jpg",sep=""))
	plot(datafr, main=namelist$title)
	abline(lm.out, col="red")
	legend("bottomright", bty="n", legend=paste("R2 = ", format(summary(lm.out)$adj.r.squared, digits=4)))
	dev.off()
	
	print(nobs(lm.out))
	
	#par(mfrow=c(2,2))
	#jpeg(paste(outdir,"stats.jpg",sep=""))
	#plot(lm.out)
	#dev.off()
	lmOut(lm.out, paste(namelist$outdir,namelist$statfile,".csv",sep=""))

	#return(object)	
}




## 
## lmOut:
##
## writer for linear regression results
##
## input:
##  - res: output from linear regression (lm(...))
##  - file: full path to filename
##  - ndigit: number of digits to be written (standard is 3)
##  - writecsv: if TRUE, csv will be written to HD, if false, output will only be sent to R bash
## output: csv with linear regression results
 
lmOut <- function(res, file="test.csv", ndigit=3, writecsv=TRUE) {
  # If summary has not been run on the model then run summary
  n_ob = nobs(res)
  if (length(grep("summary", class(res)))==0) res <- summary(res)
  
  co <- res$coefficients
  nvar <- nrow(co)
  ncol <- ncol(co)
  f <- res$fstatistic
  formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)
  
  # This sets the number of rows before we start recording the coefficients
  nstats <- 4
  
  # G matrix stores data for output
  G <- matrix("", nrow=nvar+nstats, ncol=ncol+2)
    
  G[1,1] <- toString(res$call)
  
  # Save rownames and colnames
  G[(nstats+1):(nvar+nstats),1] <- rownames(co)
  G[nstats, 2:(ncol+1)] <- colnames(co)
  
  # Save Coefficients
  G[(nstats+1):(nvar+nstats), 2:(ncol+1)] <- formatter(co)
  
  # Save F-stat
  G[1,2] <- paste0("F(",f[2],",",f[3],")")
  G[2,2] <- formatter(f[1])
  
  # Save F-p value
  G[1,3] <- "Prob > P"
  G[2,3] <- formatter(1-pf(f[1],f[2],f[3]))
  
  # Save R2
  G[1,4] <- "R-Squared"
  G[2,4] <- formatter(res$r.squared)
  
  # Save Adj-R2
  G[1,5] <- "Adj-R2"
  G[2,5] <- formatter(res$adj.r.squared)
  
  G[1,6] <- "Number of observation"
  G[2,6] <- n_ob
  
  print(G)
  if (writecsv) write.csv(G, file=file, row.names=FALSE)
}







################################################

#####		Trend analysis                 #####

################################################


##
## days_per_decade.proc
##
## procedural function for days_per_decade function. Does linear regression,
## checks for significance (0.05 is standard)
## inputs: 
## - vector: time-series to be analyzed
## - annual: boolean to decide if annual change rate (TRUE) or decadal change rate (FALSE) should be given
## - years: time-frame for the time-series (amount of steps), standard is 1982:2011
## - alpha: significance level, standard is 0.05
## output: trend in $unit/decade or $unit/year (i.e. days/decade or %/decade, depending on input units) for a single pixel
days_per_decade.proc <- function(vector, annual, years = 1982:2011, alpha=0.05) {
	#years = 1982:2011

	if(annual == TRUE) {
		fac = 1.0
	} else {
		fac = 10.0
	}
	
	# linear regression
	out = lm(vector ~ years)
	sout = summary(out)
	# if slope is NA, return NA
	if(is.na(sout$coefficients["years",4])) {
		return(NA)
	}
	
	if(sout$coefficients["years",4]<=alpha) { # check on significance level
		ndays = out$coefficients["years"]*fac #return trend, *10 for decadal change
		return(ndays)
	}
	else {
		return(NA)
	}
}

## days_per_decade
##
## main function to get change rates by cycling through each pixel, 
## interpolating values if needed (if less than 10 are missing and first and last value are not NA),
## inputs:
## - map: array of yearly maps which need change-rate extraction
## - annual: TRUE: give change rate in $unit/year, if FALSE, give change rate in $unit/decade
## output: matrix with trend/decade or trend/year for every valid pixel
days_per_decade <- function(map,annual=FALSE) {
	map.dim = dim(map)
	output = array(dim=c(map.dim[1],map.dim[2]))
	for(i in 1:map.dim[1]) { #cycle through rows
		for(j in 1:map.dim[2]) { # cycle through columns
			vec = as.vector(map[i,j,]) # set vector of 30-years of data (either climatic controls or LSP parameters)
			if(sum(is.na(vec)) > 10 || is.na(vec[1]) || is.na(vec[length(vec)])) { # if more than 10 values are NA, or if the last or first value is NA
				output[i,j] = NA
			} 
			else {
				if(sum(is.na(vec)) != 0) { # if there still are NA's, interpolate
					vec.zoo <- zoo(vec) # use zoo for na.approximate
					vec.approx <- na.approx(vec.zoo)
					# once done send interpolated data to days_per_decade.proc
					output[i,j] = days_per_decade.proc(vec.approx,annual=annual)
				} else {
					output[i,j] = days_per_decade.proc(vec,annual=annual)
				}
			}
		}
	}
	return(output)
}
