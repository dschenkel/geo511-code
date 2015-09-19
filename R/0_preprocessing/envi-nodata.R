##
## set LAI3g water values (250) to 0 or NA
## dschenkel, dec 2014
##

library(caTools)

for (year in 1982:2011) {
	#filename = paste("~/Documents/Uni/Masterarbeit/LAIv3g/LAIv3g_8211_INT_BSQ", sep="") 

	filename = paste("~/Documents/Uni/Masterarbeit/LAIv3g/yearly/LAIv3g_",year, sep="") 

	mtrx = read.ENVI(filename, headerfile=paste(filename, ".hdr", sep="")) 
	mtrx[mtrx == 250] <-- 0

	write.ENVI(mtrx, paste(filename,"_nodata", sep=""), interleave = "bsq" ) 
	rm(mtrx)
}