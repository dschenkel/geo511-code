##
## quick extraction statistics
## dschenkel, jul 2015
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
	table.3g = matrix(data = NA, nrow = 30, ncol = 3)
	table.re = matrix(data = NA, nrow = 30, ncol = 3) #year, not NA pixels, NA pixels

	for (year in 1982:2011) {

		# load watermask and datasets
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
	
	

		mtrx.laire <- array(data=NA, dim=c(360,720))
		mtrx.lai3g <- array(data=NA, dim=c(360,720))
		
		# combine hemispheres
		mtrx.laire[181:360,] <- mtrx.shem.laire[181:360,,1] 
		mtrx.laire[1:180,] <- mtrx.nhem.laire[1:180,,1]

		mtrx.lai3g[181:360,] <- mtrx.shem.lai3g[181:360,,1] 
		mtrx.lai3g[1:180,] <- mtrx.nhem.lai3g[1:180,,1]
		
		#mtrx.laire = mtrx.laire*mtrx.mask
		#mtrx.lai3g = mtrx.lai3g*mtrx.mask
		
		#LAI3g
		table.3g[year-1981,1] = year
		table.3g[year-1981,2] = sum(!is.na(as.vector(mtrx.lai3g))) # sum of non-NA pixels
		table.3g[year-1981,3] = sum(is.na(as.vector(mtrx.lai3g)))-sum(is.na(mtrx.mask)) #number of NA pixels which are NOT in the watermask (i.e. NA for some reason other than being masked out anyway)
		
		# same for LAIre
		table.re[year-1981,1] = year
		table.re[year-1981,2] = sum(!is.na(as.vector(mtrx.laire)))
		table.re[year-1981,3] = sum(is.na(as.vector(mtrx.laire)))-sum(is.na(mtrx.mask))
	}
	# write csv's
	write.csv(table.3g, file = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/meta_stats/pixelcount_",meth,"_lai3g.csv",sep=""))
	write.csv(table.re, file = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/meta_stats/pixelcount_",meth,"_laire.csv",sep=""))
	
}
