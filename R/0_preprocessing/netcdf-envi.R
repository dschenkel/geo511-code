##
## set LAI3g water values (250) to 0 or NA
## dschenkel, dec 2014
##

library(ncdf4)
library(raster)
library(fields)
library(caTools)

# extract climatic controls in 24 yearly scenes (15 days each)

for(prod in c("MOIST_FAC", "TEMP_FAC")) {
	for(year in 1982:2011){
		rootdir = paste("~/Documents/Uni/Masterarbeit/",sep="")
		# ??open.ncdf
		# open ncdf
		filename <- paste(rootdir,"LAIre/raw_data/Global-0.5x0.5.analysis.",year,".nc",sep="")
		nc <- nc_open(filename)
		
		# get climatic control 
		mtrx <- ncvar_get(nc, prod)
		
		# build assign rasters from day 3 to 362 in order to be divisible by 15 (day 1, 2, and last days of year are ignored)
		for( i in 3:362 ){ 
			rstr.temp <- raster(mtrx[,,i])
			rstr.temp <-  flip(t(d), direction = "y") # flip and transform matrix because ncdf reads it in in strange ways
		    assign(paste0("r", i), rstr.temp) 
		}

		## create rasterstack of all days
		rstr.stack <- stack(mget(paste0("r",3:362) , envir = .GlobalEnv))	
		
		# get means for each 15 day period and create 24 scenes
		out.stack <- calc(rstr.stack , fun = function(x){ by(x , c(rep(1:24 , each=15)), mean) } )
	
	
		out.stack = as.array(out.stack)
	
		write.ENVI(out.stack, paste(rootdir,"climatic_controls/",prod,"/",year,"_",prod, sep=""), interleave = "bsq" ) 

		rm(mtrx, out.stack, rstr.stack, nc)

	}
}