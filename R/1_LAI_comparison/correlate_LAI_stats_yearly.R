##
## Correlate yearly mean, min, max values of LAIre and LAI3g
## dschenkel, apr 2015
##

library(caTools)
library(raster)
library(stats)
source("general_functions.R")

strict = "strict"


out = matrix(nrow=30,ncol=3,dimnames=list(1982:2011,c("max","mean","min")))

# prepare tables to be used as output 
table.max = matrix(data = NA, nrow = 30, ncol = 3)
table.min = matrix(data = NA, nrow = 30, ncol = 3)
table.mean = matrix(data = NA, nrow = 30, ncol = 3)
for (year in 1982:2011) {


	# use strict watermask
	filename.mask = paste("~/Documents/Uni/Masterarbeit/watermask/lai3g/yearly_",strict,"/watermask_max_",strict,".",year,sep="")
	mtrx.mask = read.ENVI(filename.mask)
	
	#get both datasets for the current year
	filename.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/bimonthly_means/LAIre_bimonthly_",year, sep="") 
	filename.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/yearly/LAIv3g_",year,"_0.5", sep="") 

	mtrx.laire = read.ENVI(filename.laire, headerfile=paste(filename.laire, ".hdr", sep=""))
	mtrx.lai3g = read.ENVI(filename.lai3g, headerfile=paste(filename.lai3g, ".hdr", sep=""))
	
	# divide lai3g by 10 to get same units as LAIre
	mtrx.lai3g = mtrx.lai3g/10
	
	# get mean, max and min for both datasets
	data.mean.laire <- apply(mtrx.laire,c(1,2),mean)
	data.mean.lai3g <- apply(mtrx.lai3g,c(1,2),mean)
	data.max.laire <- apply(mtrx.laire,c(1,2),max)
	data.max.lai3g <- apply(mtrx.lai3g,c(1,2),max)
	data.min.laire <- apply(mtrx.laire,c(1,2),min)
	data.min.lai3g <- apply(mtrx.lai3g,c(1,2),min)


	#use either correlate_2d to create individual statistics for each year and variable,
	# or calcluate cor and cov and output to CSV
	
	# for max
	namelist = list("map1" = "LAI3g", "map2" = "LAIre", 
		"outdir" = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/statistics/raw_lai/",sep=""))
	namelist["statfile"] = paste("statistics_",year,"_max",sep="")
	namelist["title"] = "Linear Regression: LAI3g_max - LAIre_max"
	#correlate_2d(data.max.lai3g,data.max.laire,namelist,mtrx.mask)
	vec.max.3g = as.vector(data.max.lai3g*mtrx.mask)
	vec.max.re = as.vector(data.max.laire*mtrx.mask)
	cor = cor(vec.max.3g,vec.max.re,method="pearson",use="pairwise.complete.obs")
	cov = cov(vec.max.3g,vec.max.re,use="pairwise.complete.obs")
	table.max[year-1981,1] = year
	table.max[year-1981,2] = cor
	table.max[year-1981,3] = cov
	
	# for min
	namelist["statfile"] = paste("statistics_",year,"_min",sep="")
	namelist["title"] = "Linear Regression: LAI3g_min - LAIre_min"
	#correlate_2d(data.min.lai3g,data.min.laire,namelist,mtrx.mask)
	vec.min.3g = as.vector(data.min.lai3g*mtrx.mask)
	vec.min.re = as.vector(data.min.laire*mtrx.mask)
	cor = cor(vec.min.3g,vec.min.re,method="pearson",use="pairwise.complete.obs")
	cov = cov(vec.min.3g,vec.min.re,use="pairwise.complete.obs")
	table.min[year-1981,1] = year
	table.min[year-1981,2] = cor
	table.min[year-1981,3] = cov
	

	# for mean
	namelist["statfile"] = paste("statistics_",year,"mean",sep="")
	namelist["title"] = "Linear Regression: LAI3g_mean - LAIre_mean"
	#correlate_2d(data.mean.lai3g,data.mean.laire,namelist,mtrx.mask)
	#write.ENVI(mtrx.diff.sos, outname.sosdiff, interleave = "bsq" ) 
	#write.ENVI(mtrx.diff.gsl, outname.gsldiff, interleave = "bsq" ) 
	#rm(mtrx.laire, mtrx.lai3g)
	vec.mean.3g = as.vector(data.mean.lai3g*mtrx.mask)
	vec.mean.re = as.vector(data.mean.laire*mtrx.mask)
	cor = cor(vec.mean.3g,vec.mean.re,method="pearson",use="pairwise.complete.obs")
	cov = cov(vec.mean.3g,vec.mean.re,use="pairwise.complete.obs")
	table.mean[year-1981,1] = year
	table.mean[year-1981,2] = cor
	table.mean[year-1981,3] = cov
	
	
	print(year)
}
#write.csv(out, file = "~/Documents/Uni/Masterarbeit/1_LAI_comparison/max_mean_min_corr.csv")
# write min, mean, max stats
write.csv(table.max, file = "~/Documents/Uni/Masterarbeit/1_LAI_comparison/yearly_max.csv")
write.csv(table.min, file = "~/Documents/Uni/Masterarbeit/1_LAI_comparison/yearly_min.csv")
write.csv(table.mean, file = "~/Documents/Uni/Masterarbeit/1_LAI_comparison/yearly_mean.csv")

