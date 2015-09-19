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

library(caTools)
library(raster)
library(stats)
source("../general_functions.R")

meth="MP"

out = matrix(nrow=30,ncol=3,dimnames=list(1982:2011,c("SOS","EOS","GSL")))

for (year in 1982:2011) {
	
#	year=1982
	filename.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/bimonthly_means/LAIre_bimonthly_",year, sep="") 
	filename.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/yearly/LAIv3g_",year,"_0.5", sep="") 

	mtrx.laire = read.ENVI(filename.laire, headerfile=paste(filename.laire, ".hdr", sep=""))
	mtrx.lai3g = read.ENVI(filename.lai3g, headerfile=paste(filename.lai3g, ".hdr", sep=""))

	filename.mask = paste("~/Documents/Uni/Masterarbeit/watermask/lai3g/yearly_strict/watermask_max_strict.",year,sep="")
	mtrx.mask = read.ENVI(filename.mask)
	
	#SOS_corr
	
	filename.shem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/SHEM/PHENO/",year,"_SHEM_",meth,"__PHENO", sep="") 
	filename.nhem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/NHEM/PHENO/",year,"_NHEM_",meth,"__PHENO", sep="") 
	
	filename.shem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/SHEM/PHENO/LAI3g_",year,"_SHEM_",meth,"__PHENO", sep="") 
	filename.nhem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/NHEM/PHENO/LAI3g_",year,"_NHEM_",meth,"__PHENO", sep="") 

	mtrx.shem.laire = read.ENVI(paste(filename.shem.laire,".bsq",sep=""), headerfile=paste(filename.shem.laire, ".hdr", sep="")) 
	mtrx.nhem.laire = read.ENVI(paste(filename.nhem.laire,".bsq",sep=""), headerfile=paste(filename.nhem.laire, ".hdr", sep="")) 
	mtrx.shem.lai3g = read.ENVI(paste(filename.shem.lai3g,".bsq",sep=""), headerfile=paste(filename.shem.lai3g, ".hdr", sep="")) 
	mtrx.nhem.lai3g = read.ENVI(paste(filename.nhem.lai3g,".bsq",sep=""), headerfile=paste(filename.nhem.lai3g, ".hdr", sep="")) 

	mtrx.laire.pheno <- array(data=NA, dim=c(360,720,11))
	mtrx.lai3g.pheno <- array(data=NA, dim=c(360,720,11))

	mtrx.laire.pheno[181:360,,] <- mtrx.shem.laire[181:360,,] 
	mtrx.laire.pheno[1:180,,] <- mtrx.nhem.laire[1:180,,]

	mtrx.lai3g.pheno[181:360,,] <- mtrx.shem.lai3g[181:360,,] 
	mtrx.lai3g.pheno[1:180,,] <- mtrx.nhem.lai3g[1:180,,]
	
	namelist = list("map1" = "LAIre", "map2" = "LAI3g", 
		"outdir" = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/statistics/phenology/",sep=""))
		
	namelist["statfile"] = paste("statistics_",year,"_SOS_",meth,sep="")
	namelist["title"] = "Linear Regression SOS: LAIre - LAI3g"
	correlate_2d(mtrx.laire.pheno[,,1],mtrx.lai3g.pheno[,,1],namelist,mtrx.mask)
	namelist["statfile"] = paste("statistics_",year,"_EOS_",meth,sep="")
	namelist["title"] = "Linear Regression EOS: LAIre - LAI3g"
	correlate_2d(mtrx.laire.pheno[,,4],mtrx.lai3g.pheno[,,4],namelist,mtrx.mask)
	namelist["statfile"] = paste("statistics_",year,"_GSL_",meth,sep="")
	namelist["title"] = "Linear Regression GSL: LAIre - LAI3g"
	correlate_2d(mtrx.laire.pheno[,,7],mtrx.lai3g.pheno[,,7],namelist,mtrx.mask)
#	out
	
	#write.ENVI(mtrx.diff.sos, outname.sosdiff, interleave = "bsq" ) 
	#write.ENVI(mtrx.diff.gsl, outname.gsldiff, interleave = "bsq" ) 
	#rm(mtrx.laire, mtrx.lai3g)
	print(year)
}

	