##
## extract 30-year trends from LSP parameters SOS, EOS and GSL 
## dschenkel, jun 2015
##


# set parameter number and name from PHENO files output from VIprocessor:
# ("means" for multi-year extraction, but since VIprocessor processed individual years, means = absolute LSP parameter for each yer)
# 1 SOS_mean    <---- SOS
# 2 SOS_absChange
# 3 SOS_pvalue
# 4 EOS_mean    <---- EOS 
# 5 EOS_absChange
# 6 EOS_pvalue
# 7 LGS_mean    <---- GSL
# ...
# -> 

param = 1
param.name = "SOS"

library(caTools)
library(raster)
source("general_functions.R")


for (meth in c("MP", "MI")) {
	mtrx.laire <- array(dim=c(360,720,30))
	mtrx.lai3g <- array(dim=c(360,720,30))
	cat("go")
	
	# set yearly nhem and shem datasets back together into one big global array spanning all years
	for (year in 1982:2011) {
		filename.shem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/SHEM/PHENO/",year,"_SHEM_",meth,"__PHENO", sep="") 
		filename.nhem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/NHEM/PHENO/",year,"_NHEM_",meth,"__PHENO", sep="") 
	
		filename.shem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/SHEM/PHENO/LAI3g_",year,"_SHEM_",meth,"__PHENO", sep="") 
		filename.nhem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/NHEM/PHENO/LAI3g_",year,"_NHEM_",meth,"__PHENO", sep="") 

		mtrx.shem.laire = read.ENVI(paste(filename.shem.laire,".bsq",sep=""), headerfile=paste(filename.shem.laire, ".hdr", sep="")) 
		mtrx.nhem.laire = read.ENVI(paste(filename.nhem.laire,".bsq",sep=""), headerfile=paste(filename.nhem.laire, ".hdr", sep="")) 
		mtrx.shem.lai3g = read.ENVI(paste(filename.shem.lai3g,".bsq",sep=""), headerfile=paste(filename.shem.lai3g, ".hdr", sep="")) 
		mtrx.nhem.lai3g = read.ENVI(paste(filename.nhem.lai3g,".bsq",sep=""), headerfile=paste(filename.nhem.lai3g, ".hdr", sep="")) 


		mtrx.laire[181:360,,year-1981] <- mtrx.shem.laire[181:360,,param] 
		mtrx.laire[1:180,,year-1981] <- mtrx.nhem.laire[1:180,,param]

		mtrx.lai3g[181:360,,year-1981] <- mtrx.shem.lai3g[181:360,,param] 
		mtrx.lai3g[1:180,,year-1981] <- mtrx.nhem.lai3g[1:180,,param]
	
	}
	print("finished_years")
	
	# hand over 30-year global array to days_per_decade function from general_functions.R
	
	out.3g = days_per_decade(mtrx.lai3g)
	outname.3g = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/decade_change/LAI3g/LAI3g_changepdec_",param.name,"_",meth,sep="")
	write.ENVI(out.3g, outname.3g, interleave = "bsq" ) 
	print("lai3g done")
	out.re = days_per_decade(mtrx.laire)
	outname.re = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/decade_change/LAIre/LAIre_changepdec_",param.name,"_",meth,sep="")
	write.ENVI(out.re, outname.re, interleave = "bsq" ) 
	
	print("laire done")
	
}
