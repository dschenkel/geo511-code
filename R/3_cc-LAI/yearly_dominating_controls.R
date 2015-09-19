##
## yearly dominating controls at SOS/EOS, copied and adapted from research question 2
## dschenkel, jun 2015
##
# 1 = temp
# 2 = moisture
# 3 = light



library(caTools)
library(raster)
source("../general_functions.R")


#mtrx.dominating <- array(dim=c(360,720,30))
#mtrx.changeflag <- array(dim=c(360,720))
print("go")
for(phenop in c("SOS","EOS")) {
	for(prod in c("LAI3g","LAIre")) {
		for (year in 1982:2011) {
			
			#load all climatic factors
			filename.mfac = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/",prod,"/monthly_MOIST_FAC_at_",phenop,"_",year, sep="") 
			filename.tfac = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/",prod,"/monthly_TEMP_FAC_at_",phenop,"_",year, sep="") 
			filename.lfac = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/",prod,"/monthly_LIGHT_FAC_at_",phenop,"_",year, sep="") 

			mtrx.tfac.sum = read.ENVI(filename.tfac)
			mtrx.mfac.sum = read.ENVI(filename.mfac)
			mtrx.lfac.sum = read.ENVI(filename.lfac)
	
			mtrx.dominating.tmp = matrix(data=NA,nrow=360,ncol=720)
			
			#if temp_fac has lowest sum, it's dominating
			mtrx.dominating.tmp[mtrx.lfac.sum>=mtrx.tfac.sum & mtrx.mfac.sum>=mtrx.tfac.sum] <- 1
			
			#if light_fac has lowest sum, it's dominating
			mtrx.dominating.tmp[mtrx.tfac.sum>=mtrx.mfac.sum & mtrx.lfac.sum>=mtrx.mfac.sum] <- 2
			
			#if light_fac has lowest sum, it's dominating
			mtrx.dominating.tmp[mtrx.mfac.sum>=mtrx.lfac.sum & mtrx.tfac.sum>=mtrx.lfac.sum] <- 3
	
			outname = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/yearly_dominating/",prod,"/",phenop,"_dominating_control_",year,sep="")
			write.ENVI(mtrx.dominating.tmp, outname, interleave = "bsq" ) 

			#mtrx.dominating[,,year-1981] = mtrx.dominating.tmp
			print(year)
		}
	}
}

print("finished_years")



