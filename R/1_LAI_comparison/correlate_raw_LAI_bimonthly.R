##
## Correlate bimonthly raw values of LAIre vs LAI3g
## dschenkel, may 2015
##

library(caTools)
library(raster)
library(stats)
source("../general_functions.R")

# use strict masking
strict = "strict"


#month.name = list("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
month.name = list("jana", "janb", "feba","febb","mara","marb","apra","aprb", "maya","mayb","juna","junb","jula","julb","auga","augb","sepa","sepb","octa","octb","nova","novb","deca","decb")


table = matrix(data = NA, nrow = 720, ncol = 4)
full.laire = numeric(0)
full.lai3g = numeric(0)
counter = 1
for (year in 1982:2011) {
	# load datasets
	filename.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/bimonthly_means/LAIre_bimonthly_",year, sep="") 
	filename.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/yearly/LAIv3g_",year,"_0.5", sep="") 	
	mtrx.laire = read.ENVI(filename.laire, headerfile=paste(filename.laire, ".hdr", sep=""))
	mtrx.lai3g = read.ENVI(filename.lai3g, headerfile=paste(filename.lai3g, ".hdr", sep=""))

	for (i in 1:24) { # cycle through 15-day periods
		# open monthly mask (lai3g based, strict)
	 	filename.mask = paste("~/Documents/Uni/Masterarbeit/watermask/lai3g/monthly/watermask.",year,month.name[i],sep="")
		mtrx.mask = read.ENVI(filename.mask)
		# load current month
		mtrx.laire.cur = mtrx.laire[,,i]
		mtrx.lai3g.cur = mtrx.lai3g[,,i]
		mtrx.lai3g.cur = mtrx.lai3g.cur/10
		
		mtrx.lai3g.cur[mtrx.mask==NA] <-- NA
		mtrx.laire.cur[mtrx.mask==NA] <-- NA
		
		laire = as.vector(mtrx.laire.cur)
		lai3g = as.vector(mtrx.lai3g.cur)
	#	full.laire = append(full.laire,laire) # add up for 
	#	full.lai3g = append(full.lai3g,lai3g)
		
		# get correlation and stats for each 15-day period
		# set year and period (i.e. 1995_jana)
		entryname = paste(year,"_",month.name[i],sep="")
		stat = cor.test(laire, lai3g,method="pearson",use="pairwise.complete.obs")
		#rbind(table,c(entryname,stat$estimate,stat$p.value))
		
		# fill table with
		# month
		table[counter,1] = entryname
		# correlation coefficient (person's r)
		table[counter,2] = stat$estimate #cor(laire, lai3g,method="spearman",use="pairwise.complete.obs")
		# significance
		table[counter,3] = stat$p.value
		# covariance
		table[counter,4] = cov(laire,lai3g, use="pairwise.complete.obs")
		counter = counter + 1


	}
		print(year)
}

#print(table)
# write CSV
write.csv(table, file = "~/Documents/Uni/Masterarbeit/1_LAI_comparison/bimonthly_raw_corr_cov.csv")
