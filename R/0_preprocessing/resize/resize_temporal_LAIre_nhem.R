##
## Resize LAI_re from daily to 24 bimonthly scenes, only for northern hemisphere
## dschenkel, february 2015
##


library(caTools)
library(raster)
for (year in 1982:2011) {

	# get original file
	filename = paste("~/Documents/Uni/Masterarbeit/LAIre/yearly_envi/Global-0.5x0.5.analysis.",year,"_rot.envi", sep="") 
	mtrx = read.ENVI(filename, headerfile=paste(filename, ".hdr", sep="")) 
	
	
	#get 360 (dividable by 15) days
	for( i in 3:362 ){
	    assign( paste0( "r" , i ) , raster( mtrx[1:180,,i] ) ) 
	}

	## stack rasters
	rstr.stack <- stack( mget( paste0("r",3:362) , envir = .GlobalEnv ) )	
	## get 15-day means in rasterstack
	rstr.bimonthly <- calc(rstr.stack, fun = function(x){ by(x , c( rep( 1:24 , each=15)), mean ) } )
	
	#convert to matrix for write.ENVI
	mtrx.bimonthly = as.array(rstr.bimonthly)
	
	outlayer <- array(data=NA, dim=c(360,720,24)) # set processed nhem back to into raster with extent of whole globe (empty southern hemisphere)
	outlayer[1:180:360,,] <- mtrx.bimonthly
	
	outname = paste("~/Documents/Uni/Masterarbeit/LAIre/bimonthly_means/NHEM/LAIre_nhem_bimonthly_",year, sep="") 
	write.ENVI(rstr.out, outname, interleave = "bsq" ) 
	rm(mtrx, rstr.out, rstr.bimonthly,rstr.stack)
}
