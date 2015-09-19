##
## Resize LAI_re from daily to 24 bimonthly scenes
## dschenkel, march 2015
##

library(caTools)
library(raster)
for (year in 1982:2011) {

	filename = paste("~/Documents/Uni/Masterarbeit/LAIre/yearly_envi/Global-0.5x0.5.analysis.",year,"_rot.envi", sep="") 
	
	mtrx = read.ENVI(filename, headerfile=paste(filename, ".hdr", sep="")) 
	
	
	
	#get 360 (dividable by 15) days
	for( i in 3:362 ){
	    assign( paste0( "r" , i ) , raster( mtrx[,,i] ) ) 
	}

	## stack rasters
	rstr.stack <- stack( mget( paste0("r",3:362) , envir = .GlobalEnv ) )	
	## get 15-day means in rasterstack
	rstr.bimonthly <- calc(rstr.stack, fun = function(x){ by(x , c( rep( 1:24 , each=15)), mean ) } )
	
	#convert to matrix for write.ENVI
	rstr.out = as.array(rstr.bimonthly)
	
	outname = paste("~/Documents/Uni/Masterarbeit/LAIre/bimonthly_means/LAIre_bimonthly_",year, sep="") 
	write.ENVI(rstr.out, outname, interleave = "bsq" ) 
	rm(mtrx, rstr.bimonthly, rstr.stack, rstr.out)
}
