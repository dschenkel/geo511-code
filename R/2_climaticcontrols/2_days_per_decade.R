##
## trends in climatic controls by 15-day period using general_functions.R (days_per_decade)
## dschenkel, jun 2015
##

#
# bimonthly maps of changes (if any?) of climatic control strength

library(caTools)
library(raster)
source("general_functions.R")

# annual or decadal
annual = FALSE

# tfac = temperature factor,
# mfac = moisture factor,
# lfac = light factor
mtrx.tfac <- array(dim=c(360,720,30,24))
mtrx.mfac <- array(dim=c(360,720,30,24))
#mtrx.lfac <- array(dim=c(360,720,30,24))

cat("go")
for (year in 1982:2011) {
	filename.mfac = paste("~/Documents/Uni/Masterarbeit/climatic_controls/MOIST_FAC/",year,"_MOIST_FAC", sep="") 
	filename.tfac = paste("~/Documents/Uni/Masterarbeit/climatic_controls/TEMP_FAC/",year,"_TEMP_FAC", sep="")
	#filename.lfac = paste("~/Documents/Uni/Masterarbeit/climatic_controls/LIGHT_FAC/",year,"_LIGHT_FAC", sep="")

	mtrx.tfac.temp = read.ENVI(filename.tfac) 
	mtrx.mfac.temp = read.ENVI(filename.mfac) 
	#mtrx.lfac.temp = read.ENVI(filename.lfac) 
	
	mtrx.tfac[,,year-1981,] = mtrx.tfac.temp
	mtrx.mfac[,,year-1981,] = mtrx.mfac.temp
	#mtrx.lfac[,,year-1981,] = mtrx.lfac.temp
}
print("finished_years")

#plot(1982:2011,mtrx.tfac[152,439,,4])

for(i in 1:24) {
	if(annual==TRUE) {
		rootdir = "~/Documents/Uni/Masterarbeit/2_controls/bimonthly_changes_annual/"
	}
	else {
		rootdir = "~/Documents/Uni/Masterarbeit/2_controls/bimonthly_changes/"
	}

	# calculate temperature trends
	out.tfac = days_per_decade(mtrx.tfac[,,,i],annual=annual)
	outname.tfac = paste(rootdir,"TEMP_FAC_",i,sep="")
	write.ENVI(out.tfac, outname.tfac, interleave = "bsq" ) 

	# calculate moisture trends
	out.mfac = days_per_decade(mtrx.mfac[,,,i],annual=annual)
	outname.mfac = paste(rootdir,"MOIST_FAC_",i,sep="")
	write.ENVI(out.mfac, outname.mfac, interleave = "bsq" ) 
	
	# calcluate light trends
	#out.lfac = days_per_decade(mtrx.lfac[,,,i],annual=annual)
	#outname.lfac = paste(rootdir,"LIGHT_FAC_",i,sep="")
	#write.ENVI(out.lfac, outname.lfac, interleave = "bsq" ) 
	
	print(i)
}


