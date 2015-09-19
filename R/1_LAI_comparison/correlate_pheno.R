##
## Correlate phenological metrics; uses correlate_2d and prints average correlation coefficients, too.
## has to be edited further down to correlate high norhtern latitudes (45-90 degrees north)
## dschenkel, apr 2015
##



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



for(meth in c("MP","MI")){
	#initiate averages
	avg.sos = 0
	avg.eos = 0
	avg.gsl = 0
	#out = matrix(nrow=30,ncol=3,dimnames=list(1982:2011,c("SOS","EOS","GSL")))
	for (year in 1982:2011) {
		
		# load both datasets
		#filename.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/bimonthly_means/LAIre_bimonthly_",year, sep="") 
		#filename.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/yearly/LAIv3g_",year,"_0.5", sep="") 

		#mtrx.laire = read.ENVI(filename.laire, headerfile=paste(filename.laire, ".hdr", sep=""))
		#mtrx.lai3g = read.ENVI(filename.lai3g, headerfile=paste(filename.lai3g, ".hdr", sep=""))
		
		# use strict watermask
		filename.mask = paste("~/Documents/Uni/Masterarbeit/watermask/lai3g/yearly_strict/watermask_max_strict.",year,sep="")
		mtrx.mask = read.ENVI(filename.mask)


		# load VIProcessor results for both hemispheres and both datasets
		filename.shem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/SHEM/PHENO/",year,"_SHEM_",meth,"__PHENO", sep="") 
		filename.nhem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/NHEM/PHENO/",year,"_NHEM_",meth,"__PHENO", sep="") 

		filename.shem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/SHEM/PHENO/LAI3g_",year,"_SHEM_",meth,"__PHENO", sep="") 
		filename.nhem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/NHEM/PHENO/LAI3g_",year,"_NHEM_",meth,"__PHENO", sep="") 

		mtrx.shem.laire = read.ENVI(paste(filename.shem.laire,".bsq",sep=""), headerfile=paste(filename.shem.laire, ".hdr", sep=""))
		mtrx.nhem.laire = read.ENVI(paste(filename.nhem.laire,".bsq",sep=""), headerfile=paste(filename.nhem.laire, ".hdr", sep=""))
		
		mtrx.shem.lai3g = read.ENVI(paste(filename.shem.lai3g,".bsq",sep=""), headerfile=paste(filename.shem.lai3g, ".hdr", sep=""))
		mtrx.nhem.lai3g = read.ENVI(paste(filename.nhem.lai3g,".bsq",sep=""), headerfile=paste(filename.nhem.lai3g, ".hdr", sep=""))
		
		# combine into one hemisphere for global assessment
		mtrx.laire.pheno <- array(data=NA, dim=c(360,720,11))
		mtrx.lai3g.pheno <- array(data=NA, dim=c(360,720,11))

		mtrx.laire.pheno[181:360,,] <- mtrx.shem.laire[181:360,,]
		mtrx.laire.pheno[1:180,,] <- mtrx.nhem.laire[1:180,,] 

		mtrx.lai3g.pheno[181:360,,] <- mtrx.shem.lai3g[181:360,,]
		mtrx.lai3g.pheno[1:180,,] <- mtrx.nhem.lai3g[1:180,,] 

		#NHEM correlations
		namelist = list("map1" = "LAIre", "map2" = "LAI3g", 
			"outdir" = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/statistics/phenology/NHEM_notropics/",meth,"/",sep=""))

			# block out either:
			# 90:360 to compare only 45-90 degrees northern latitude (nrow=271)
			# 180:360 to compare whole northern hemisphere (nrow=181)
		mtrx.nhem.laire[90:360,,1] <- matrix(NaN,nrow=271,ncol=720)
		mtrx.nhem.lai3g[90:360,,1] <- matrix(NaN,nrow=271,ncol=720)
		
		mtrx.nhem.laire[90:360,,4] <- matrix(NaN,nrow=271,ncol=720)
		mtrx.nhem.lai3g[90:360,,4] <- matrix(NaN,nrow=271,ncol=720)
	
		mtrx.nhem.laire[90:360,,7] <- matrix(NaN,nrow=271,ncol=720)
		mtrx.nhem.lai3g[90:360,,7] <- matrix(NaN,nrow=271,ncol=720)
		
		namelist["statfile"] = paste("statistics_NHEM_",year,"_SOS_",meth,sep="")
		namelist["title"] = "Linear Regression SOS: LAIre - LAI3g (NHEM, no tropics)"
		#correlate_2d(mtrx.nhem.laire[,,1],mtrx.nhem.lai3g[,,1],namelist,mtrx.mask)
		avg.sos = avg.sos + cor(as.vector(mtrx.nhem.lai3g[,,1] * mtrx.mask),as.vector(mtrx.nhem.laire[,,1] * mtrx.mask),method="pearson",use="pairwise.complete.obs")
		
		namelist["statfile"] = paste("statistics_NHEM_",year,"_EOS_",meth,sep="")
		namelist["title"] = "Linear Regression EOS: LAIre - LAI3g (NHEM, no tropics)"
		#correlate_2d(mtrx.nhem.laire[,,4],mtrx.nhem.lai3g[,,4],namelist,mtrx.mask)
		#print(mtrx.nhem.lai3g[,,4])
		avg.eos = avg.eos + cor(as.vector(mtrx.nhem.lai3g[,,4] * mtrx.mask),as.vector(mtrx.nhem.laire[,,4] * mtrx.mask),method="pearson",use="pairwise.complete.obs")
		
		namelist["statfile"] = paste("statistics_NHEM_",year,"_GSL_",meth,sep="")
		namelist["title"] = "Linear Regression GSL: LAIre - LAI3g (NHEM, no tropics)"
		#correlate_2d(mtrx.nhem.laire[,,7],mtrx.nhem.lai3g[,,7],namelist,mtrx.mask)
		avg.gsl = avg.gsl + cor(as.vector(mtrx.nhem.lai3g[,,7] * mtrx.mask),as.vector(mtrx.nhem.laire[,,7] * mtrx.mask),method="pearson",use="pairwise.complete.obs")
		
		# southern hemisphere
		if(FALSE) {
		namelist = list("map1" = "LAIre", "map2" = "LAI3g", 
			"outdir" = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/statistics/phenology/SHEM/",meth,"/",sep=""))
		
		namelist["statfile"] = paste("statistics_SHEM_",year,"_SOS_",meth,sep="")
		namelist["title"] = "Linear Regression SOS: LAIre - LAI3g (SHEM)"
		correlate_2d(mtrx.shem.laire[,,1],mtrx.shem.lai3g[,,1],namelist,mtrx.mask)
		namelist["statfile"] = paste("statistics_SHEM_",year,"_EOS_",meth,sep="")
		namelist["title"] = "Linear Regression EOS: LAIre - LAI3g (SHEM)"
		correlate_2d(mtrx.shem.laire[,,4],mtrx.shem.lai3g[,,4],namelist,mtrx.mask)
		namelist["statfile"] = paste("statistics_SHEM_",year,"_GSL_",meth,sep="")
		namelist["title"] = "Linear Regression GSL: LAIre - LAI3g (SHEM)"
		correlate_2d(mtrx.shem.laire[,,7],mtrx.shem.lai3g[,,7],namelist,mtrx.mask)
	#	out

		#write.ENVI(mtrx.diff.sos, outname.sosdiff, interleave = "bsq" ) 
		#write.ENVI(mtrx.diff.gsl, outname.gsldiff, interleave = "bsq" ) 
		}
		rm(mtrx.laire, mtrx.lai3g)
		print(year)
	}
	# print average correlation coefficients
	print(avg.sos/30)
	print(avg.eos/30)
	print(avg.gsl/30)	
}
