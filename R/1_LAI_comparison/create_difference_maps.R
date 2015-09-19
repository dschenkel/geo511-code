##
## Create maps showing differences in SOS, EOS, GSL for each year
## -> LAIre - LAI3g
## dschenkel, apr 2015
##

library(caTools)
library(raster)

#
# 1 SOS_mean
# 2 SOS_absChange
# 3 SOS_pvalue
# 4 EOS_mean
# 5 EOS_absChange
# 6 EOS_pvalue
# 7 LGS_mean
# ...
#

filtered = FALSE

for (meth in c("MI","MP")) {
	for (year in 1982:2011) {

		filename.mask = paste("~/Documents/Uni/Masterarbeit/watermask/lai3g/yearly_strict/watermask_max_strict.",year,sep="")
		mtrx.mask = read.ENVI(filename.mask)
	

		filename.shem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/SHEM/PHENO/",year,"_SHEM_",meth,"__PHENO", sep="") 
		filename.nhem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/NHEM/PHENO/",year,"_NHEM_",meth,"__PHENO", sep="") 
		
		filename.shem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/SHEM/PHENO/LAI3g_",year,"_SHEM_",meth,"__PHENO", sep="") 
		filename.nhem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/NHEM/PHENO/LAI3g_",year,"_NHEM_",meth,"__PHENO", sep="") 

		mtrx.shem.laire = read.ENVI(paste(filename.shem.laire,".bsq",sep=""), headerfile=paste(filename.shem.laire, ".hdr", sep="")) 
		mtrx.nhem.laire = read.ENVI(paste(filename.nhem.laire,".bsq",sep=""), headerfile=paste(filename.nhem.laire, ".hdr", sep="")) 
		mtrx.shem.lai3g = read.ENVI(paste(filename.shem.lai3g,".bsq",sep=""), headerfile=paste(filename.shem.lai3g, ".hdr", sep="")) 
		mtrx.nhem.lai3g = read.ENVI(paste(filename.nhem.lai3g,".bsq",sep=""), headerfile=paste(filename.nhem.lai3g, ".hdr", sep="")) 
	
	

		mtrx.laire <- array(data=NA, dim=c(360,720,11))
		mtrx.lai3g <- array(data=NA, dim=c(360,720,11))
		
		#combine hemispheres
		mtrx.laire[181:360,,] <- mtrx.shem.laire[181:360,,] 
		mtrx.laire[1:180,,] <- mtrx.nhem.laire[1:180,,]

		mtrx.lai3g[181:360,,] <- mtrx.shem.lai3g[181:360,,] 
		mtrx.lai3g[1:180,,] <- mtrx.nhem.lai3g[1:180,,]
		
		# calculate differences
		mtrx.diff.sos = mtrx.laire[,,1]-mtrx.lai3g[,,1]
		mtrx.diff.gsl = mtrx.laire[,,7]-mtrx.lai3g[,,7]
		mtrx.diff.eos = mtrx.laire[,,4]-mtrx.lai3g[,,4]
		
		# mask out waterpixels
		mtrx.diff.sos[mtrx.mask == NA] <- NA
		mtrx.diff.gsl[mtrx.mask == NA] <- NA
		mtrx.diff.eos[mtrx.mask == NA] <- NA
		
		#write files
		
		outname.sosdiff = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/maps/SOS_diff/",meth,"/",year,"_LAIre-LAI3g_SOS", sep="") 
		outname.gsldiff = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/maps/GSL_diff/",meth,"/",year,"_LAIre-LAI3g_GSL", sep="") 
		outname.eosdiff = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/maps/EOS_diff/",meth,"/",year,"_LAIre-LAI3g_EOS", sep="") 

		
		write.ENVI(mtrx.diff.sos, outname.sosdiff, interleave = "bsq" ) 
		write.ENVI(mtrx.diff.gsl, outname.gsldiff, interleave = "bsq" ) 
		write.ENVI(mtrx.diff.eos, outname.eosdiff, interleave = "bsq" ) 
		rm(mtrx.shem.laire, mtrx.nhem.laire, mtrx.shem.lai3g, mtrx.nhem.lai3g, mtrx.laire, mtrx.lai3g, mtrx.diff.sos, mtrx.diff.gsl)
	}
}
