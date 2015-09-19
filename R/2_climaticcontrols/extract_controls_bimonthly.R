##
## extract bimonthly controls from the LAIre dataset (daily values)
## dschenkel, apr 2015
##

library(ncdf4)
library(raster)
library(fields)
library(caTools)


for(prod in c("LIGHT_FAC")) {
	for(year in 1982:2011){
		rootdir = paste("~/Documents/Uni/Masterarbeit/",sep="")
		# ??open.ncdf
		filename <- paste(rootdir,"LAIre/raw_data/Global-0.5x0.5.analysis.",year,".nc",sep="")
		nc <- nc_open(filename)

		mtrx <- ncvar_get(nc, prod)
		
		
		for( i in 3:362 ){
			rstr.temp <- raster(mtrx[,,i])
			rstr.temp <-  flip(t(rstr.temp), direction = "y") #flip and mirror image to get proper projection
		    assign(paste0( "r" , i) , rstr.temp) 
		}

		## stack them (360 of them, anyway, to get 15 day means)
		rstr.stack <- stack( mget( paste0("r",3:362) , envir = .GlobalEnv ) )	

		rstr.bimonthly <- calc(rstr.stack , fun = function(x){ by(x , c( rep( 1:24 , each=15)) , mean ) } )
	
	
		rastr.out = as.array(rstr.bimonthly)
	
		write.ENVI(rstr.out, paste(rootdir,"climatic_controls/",prod,"/",year,"_",prod, sep=""), interleave = "bsq" ) 

		rm(mtrx, rastr.out, rstr.bimonthly, rstr.stack, nc)
		print(year)
	}
}