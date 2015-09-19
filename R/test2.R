##
## Testfile2, random R stuff for dataprocessing/plotting tests 
## dschenkel, oct 2014
##

library(sp)
library(raster)
library(ncdf)
library(fields)
library(rgdal)
#library(rgdal)
#file.name = paste("J:/GIMMS/avhrrbulai_v01/AVHRRBUVI01.",year[i],
#                  month.name[j], letter, '.abl', sep="")






file.name = paste("~/Documents/Uni/Masterarbeit/LAIv3g/raw_data/avhrrbulai_v01/AVHRRBUVI01.1993auga.abl", sep="") 
binread = readBin(con=file.name, what="integer", n=9331200, size=1, signed=F, endian="little")
# covert binary object into a matrix using specifications from the metadata
mtrx = matrix(binread, nrow=2160, ncol=4320, byrow=F)
# set NA values (according to the metadata n=250 is a NULL value)
mtrx[mtrx == 250] <-- 0

# multiplication by 0.1 for LAI and 0.01 for FAPAR sets the actual value
#mtrx = mtrx*0.1
#dimnames(plt) <- list(-180:180,d.lat,d.time)
#plt[,,2*i-1] = mtrx
#abind(frm,mtrx,rev.along=0)  
# convert to matrix to a raster object
rstr = raster(mtrx)
extent(rstr) <- extent(c(-180, 180, -90, 90))
projection(rstr) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#rstr.new = resample(rstr, raster(nrow=720, ncol=360), method='bilinear')

writeRaster(rstr, filename="~/Documents/Uni/Masterarbeit/LAIv3g/raw_data/avhrrbulai_v01/AVHRRBUVI01.1993auga.envi", format="ENVI", overwrite=TRUE)

month = list("01","02","03","04","05","06","07","08","09","10","11","12")
month.name = list("jan", "feb","mar","apr","may","jun","jul","aug",
              "sep","oct","nov","dec")
#plt = array(0,dim=c(2160,4320,24))
#length(seq(-90,89.92,by=1/12))
#plt.lon <- seq(-90,89.92,by=1/12)
#plt.lat <- seq(-180,179.92,by=1/12)
#dimnames(plt) <- list(plt.lon,plt.lat,1:12)
for (year in 1984:2011) {
	for (i in 1:12) {
	  #month
  
	  file.name = paste("~/Documents/Uni/Masterarbeit/LAIv3g/raw_data/avhrrbulai_v01/AVHRRBUVI01.", year, month.name[i], "a.abl", sep="") 
	  binread = readBin(con=file.name, what="integer", n=9331200, size=1, signed=F, endian="little")
	  # covert binary object into a matrix using specifications from the metadata
	  mtrx = matrix(binread, nrow=2160, ncol=4320, byrow=F)
	  # set NA values (according to the metadata n=250 is a NULL value)
	  mtrx[mtrx == 250] <-- NA
  
	  # multiplication by 0.1 for LAI and 0.01 for FAPAR sets the actual value
	  mtrx = mtrx*0.1
	  #dimnames(plt) <- list(-180:180,d.lat,d.time)
	  #plt[,,2*i-1] = mtrx
	  #abind(frm,mtrx,rev.along=0)  
	  # convert to matrix to a raster object
	  rstr = raster(mtrx)
  
	  # specify extent and projection
	  extent(rstr) <- extent(c(-180, 180, -90, 90))
	  #projection(rstr) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
	  rstr.new = resample(rstr, raster(nrow=720, ncol=360), method='bilinear')
	  #plot(rstr)
	  writeRaster(rstr.new, filename=file.name, format="ENVI", overwrite=TRUE)

	  #plot(rstr)
	  rm(file.name,mtrx, rstr.new, rstr)
  
	  file.name = paste("~/Documents/Uni/Masterarbeit/LAIv3g/raw_data/avhrrbulai_v01/AVHRRBUVI01.", year, month.name[i], "b.abl", sep="") 
	  binread = readBin(con=file.name, what="integer", n=9331200, size=1, signed=F, endian="little")
	  # covert binary object into a matrix using specifications from the metadata
	  mtrx = matrix(binread, nrow=2160, ncol=4320, byrow=F)
	  # set NA values (according to the metadata n=250 is a NULL value)
	  mtrx[mtrx == 250] <-- NA
  
	  # multiplication by 0.1 for LAI and 0.01 for FAPAR sets the actual value
	  mtrx = mtrx*0.1
	  #dimnames(plt) <- list(-180:180,d.lat,d.time)
	  #plt[,,2*i-1] = mtrx
	  #abind(frm,mtrx,rev.along=0)  
	  # convert to matrix to a raster object
	  rstr = raster(mtrx)
  
	  # specify extent and projection
	  extent(rstr) <- extent(c(-180, 180, -90, 90))
	  #projection(rstr) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
	  rstr.new = resample(rstr, raster(nrow=720, ncol=360), method='bilinear')
	  #plot(rstr)
	  writeRaster(rstr.new, filename=file.name, format="ENVI", overwrite=TRUE)

	  #plot(rstr)
	  rm(file.name,mtrx, rstr.new, rstr)
	  gc()
	}
}
#plt.max <- apply(plt,c(1,2),max)
#plt.max <- t(plt.max)
#image.plot(1:2160, 1:4320, plt.max)
#which.max( apply( plt , 3 , max ) )

#plt[450,2500,]
#plot(1:24, plt[450,2500,])
