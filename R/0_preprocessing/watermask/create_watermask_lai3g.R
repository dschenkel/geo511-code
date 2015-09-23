##
## create yearly watermasks from LAI3g: 
## - strict: every 0.5 degree pixel made up of at least one no-data pixel (value = 250) in original LAI3g is masked out
## - semi-strict: if half of the original LAI3g pixels that make up new 0.5 deg pixel (18 out of 36 pixels) are waterpixels, mask them out
##
## dschenkel, apr 2015
##

rm(list = ls())

library(ncdf)
library(raster)
library(fields)
library(caTools)

# set month lists to deal with LAI3g naming
month = list("01","02","03","04","05","06","07","08","09","10","11","12")
month.name = list("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")


for (year in 2010:2011) {
	mtrx.mask.year = matrix(1,nrow=360,ncol=720) # creat empty strict mask mtrx with default pixel = 1
	mtrx.mask.nsyear = matrix(1,nrow=360,ncol=720) # creat empty semi-strict mask mtrx with default pixel = 1

	for (mon in 1:12) {
		
		# double processing for files month_a and month_b files
		
		# read LAI3g binary
  	  file.name = paste("~/Documents/Uni/Masterarbeit/LAIv3g/raw_data/avhrrbulai_v01/AVHRRBUVI01.", year, month.name[mon], "a.abl", sep="") 
	  binread = readBin(con=file.name, what="integer", n=9331200, size=1, signed=F, endian="little")
	  
	  # convert into matrix
	  mtrx = matrix(binread, nrow=2160, ncol=4320, byrow=F)
	  mtrx.mask.month = matrix(1,nrow=360,ncol=720) 
	  for(i in seq(from=1, to=2160, by=6)) { # cycle through by 6*6 (1/12 degree resolution to 1/2 degree resolution) 
		  for(j in seq(from=1, to=4320, by=6)) { 
			  imax = i+5
			  jmax = j+5
			  	if(max(mtrx[i:imax,j:jmax])==250) { # if there is a no-value pixel in the 6*6 window...
					inew = imax/6
					jnew = jmax/6
					#mtrx.mask.month[inew,jnew] = NA
					mtrx.mask.year[inew,jnew] = NA # set pixel = NA for strict mask
					if(sum(mtrx[i:imax,j:jmax]==250)>18) { # if more than half are no-value:
						mtrx.mask.nsyear[inew,jnew] = NA # set pixel = NA for semi-strict mask
					}
				}
			}
		}
  	#outname.month = paste("~/Documents/Uni/Masterarbeit/watermask/lai3g/monthly/watermask.", year, month.name[mon], "a", sep="")
  	#write.ENVI(mtrx.mask.month, outname.month, interleave = "bsq" ) 
	
		# rinse and repeat for second half of month
  	  file.name = paste("~/Documents/Uni/Masterarbeit/LAIv3g/raw_data/avhrrbulai_v01/AVHRRBUVI01.", year, month.name[mon], "b.abl", sep="") 
  	  binread = readBin(con=file.name, what="integer", n=9331200, size=1, signed=F, endian="little")
  	  # covert binary object into a matrix using specifications from the metadata
  	  mtrx = matrix(binread, nrow=2160, ncol=4320, byrow=F)
  	  mtrx.mask.month = matrix(1,nrow=360,ncol=720)
  	  for(i in seq(from=1, to=2160, by=6)) {
  		  for(j in seq(from=1, to=4320, by=6)) {
  			  imax = i+5
  			  jmax = j+5
  			  	if(max(mtrx[i:imax,j:jmax])==250) {
  					inew = imax/6
  					jnew = jmax/6
  					#mtrx.mask.month[inew,jnew] = NA
  					mtrx.mask.year[inew,jnew] = NA
					if(sum(mtrx[i:imax,j:jmax]==250)>18) {
						mtrx.mask.nsyear[inew,jnew] = NA
					}
  				}
  			}
  		}
  		#outname.month = paste("~/Documents/Uni/Masterarbeit/watermask/lai3g/monthly/watermask.", year, month.name[mon], "b", sep="")
  		#write.ENVI(mtrx.mask.month, outname.month, interleave = "bsq" ) 
	}
	#save yearky masks
	outname = paste("~/Documents/Uni/Masterarbeit/watermask/lai3g/yearly_strict/watermask_max_strict.", year, sep="")
	write.ENVI(mtrx.mask.year, outname, interleave = "bsq" ) 
	
	outname = paste("~/Documents/Uni/Masterarbeit/watermask/lai3g/yearly_semistrict/watermask_max_semistrict.", year, sep="")
	write.ENVI(mtrx.mask.nsyear, outname, interleave = "bsq" ) 
	
}
