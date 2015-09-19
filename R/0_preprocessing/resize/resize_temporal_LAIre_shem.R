##
## Resize LAI_re from daily to 24 bimonthly scenes, for southern hemisphere: $YEAR is actually: 2nd half of $YEAR + 1st half of $NEXT_YEAR
## dschenkel, february 2015
##


library(caTools)
library(raster)
library(abind)
#set year boundaries to know when "last" or "next" year data is NOT available
minyear = 1982
maxyear = 2011

for (year in 1982:2011) {

	#load first half of the year
	filename.first = paste("~/Documents/Uni/Masterarbeit/LAIre/yearly_envi/Global-0.5x0.5.analysis.",year,"_rot.envi", sep="") 
	mtrx.first = read.ENVI(filename.first, headerfile=paste(filename.first, ".hdr", sep="")) 
	# load second half of the year
	filename.second = paste("~/Documents/Uni/Masterarbeit/LAIre/yearly_envi/Global-0.5x0.5.analysis.",year+1,"_rot.envi", sep="") 	
	if(year == maxyear) { #special case for next year: repeat current year if it's 2011 (no data in 2012)
		filename.second = paste("~/Documents/Uni/Masterarbeit/LAIre/yearly_envi/Global-0.5x0.5.analysis.",year,"_rot.envi", sep="") 
	}
	
	mtrx.second = read.ENVI(filename.second, headerfile=paste(filename.second, ".hdr", sep="")) 
	
	mtrx = array(data=0, dim=c(180,720,365)) 

	# connect last 183 days of $year and first 182 days of $next_year (==365 days) 
	mtrx = abind(mtrx.first[181:360,,183:365],mtrx.second[181:360,,1:182])
	#shem_mask.name = paste("~/Documents/Uni/Masterarbeit/watermask/watermask_shem.envi", sep="") 
	#nhem_mask = read.ENVI(nhem_mask.name, headerfile=paste(nhem_mask.name, ".hdr", sep="")) 
	
	#nhem_mask[nhem_mask == 0] <-- NA
	
	#get 360 (dividable by 15) days
	for(i in 3:362 ){
	    assign( paste0( "r" , i ) , raster( mtrx[1:180,,i] ) ) 
	}

	## stack rasters
	rstr.stack <- stack( mget( paste0("r",3:362) , envir = .GlobalEnv ) )	
	## get 15-day means in rasterstack
	rstr.bimonthly <- calc(rstr.stack, fun = function(x){ by(x , c( rep( 1:24 , each=15)), mean ) } )
	
	mtrx.bimonthly = as.array(rstr.bimonthly)

	outlayer <- array(data=NA, dim=c(360,720,24)) # set processed shem back to into raster with extent of whole globe (empty northern hemisphere)
	outlayer[181:360,,] <- mtrx.bimonthly
	
	outname = paste("~/Documents/Uni/Masterarbeit/LAIre/bimonthly_means/SHEM/LAIre_shem_bimonthly_",year, sep="") 
	write.ENVI(outlayer, outname, interleave = "bsq" ) 
	rm(mtrx, rstr.out, rstr.bimonthly,rstr.stack)
}
