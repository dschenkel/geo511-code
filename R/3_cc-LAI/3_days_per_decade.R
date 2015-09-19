##
## trends in climatic controls AT SOS and EOS by 15-day period using general_functions.R (days_per_decade)
## dschenkel, jun 2015
##

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
source("general_functions.R")

# set if annual or decadal should be given as output
annual = FALSE

for(prod in c("TEMP_FAC","MOIST_FAC","LIGHT_FAC")) {
	mtrx.sos.laire <- array(dim=c(360,720,30))
	mtrx.sos.lai3g <- array(dim=c(360,720,30))
	mtrx.eos.laire <- array(dim=c(360,720,30))
	mtrx.eos.lai3g <- array(dim=c(360,720,30))
	cat("go")
	for (year in 1982:2011) {
		# load climatic controls at SOS, EOS (for both datasets) and build stack of 30 years
		filename.sos.lai3g = paste("/Users/davidschenkel/Documents/Uni/Masterarbeit/3_cc-LAI/LAI3g/monthly_",prod,"_at_SOS_",year, sep="") 
		filename.eos.lai3g = paste("/Users/davidschenkel/Documents/Uni/Masterarbeit/3_cc-LAI/LAI3g/monthly_",prod,"_at_EOS_",year, sep="") 
	
		filename.sos.laire = paste("/Users/davidschenkel/Documents/Uni/Masterarbeit/3_cc-LAI/LAIre/monthly_",prod,"_at_SOS_",year, sep="") 
		filename.eos.laire = paste("/Users/davidschenkel/Documents/Uni/Masterarbeit/3_cc-LAI/LAIre/monthly_",prod,"_at_EOS_",year, sep="")  

		mtrx.sos.laire[,,year-1981] = read.ENVI(filename.sos.laire, headerfile=paste(filename.sos.laire, ".hdr", sep="")) 
		mtrx.eos.laire[,,year-1981] = read.ENVI(filename.eos.laire, headerfile=paste(filename.eos.laire, ".hdr", sep="")) 
		mtrx.sos.lai3g[,,year-1981] = read.ENVI(filename.sos.lai3g, headerfile=paste(filename.sos.lai3g, ".hdr", sep="")) 
		mtrx.eos.lai3g[,,year-1981] = read.ENVI(filename.eos.lai3g, headerfile=paste(filename.eos.lai3g, ".hdr", sep="")) 
	}
	print(paste("finished_years",prod))
	
	if(annual == TRUE) {
		rootdir = "~/Documents/Uni/Masterarbeit/3_cc-LAI/monthly_annual_change_signf/"
	}
	else {
		rootdir = "~/Documents/Uni/Masterarbeit/3_cc-LAI/monthly_decadal_change/"
	}
	# extract trends from stack using days_per_decade (general_functions.R)
	
	# LAI3g
	#SOS
	out.sos.3g = days_per_decade(mtrx.sos.lai3g)
	outname.sos.3g = paste(rootdir,"LAI3g_",ifelse(annual==TRUE,"annual","decadal"),"_change_SOS_",prod,sep="")
	write.ENVI(out.sos.3g, outname.sos.3g, interleave = "bsq" ) 
	print(paste("lai3g SOS done",prod))
	#EOS
	out.eos.3g = days_per_decade(mtrx.eos.lai3g)
	outname.eos.3g = paste(rootdir,"LAI3g_",ifelse(annual==TRUE,"annual","decadal"),"_change_EOS_",prod,sep="")
	write.ENVI(out.eos.3g, outname.eos.3g, interleave = "bsq" ) 
	print(paste("lai3g EOS done",prod))
	
	#LAIre
	#SOS
	out.sos.re = days_per_decade(mtrx.sos.laire)
	outname.sos.re = paste(rootdir,"LAIre_",ifelse(annual==TRUE,"annual","decadal"),"_change_SOS_",prod,sep="")
	write.ENVI(out.sos.re, outname.sos.re, interleave = "bsq" ) 
	print(paste("laire SOS done",prod))	
	#EOS
	out.eos.re = days_per_decade(mtrx.eos.laire)
	outname.eos.re = paste(rootdir,"LAIre_",ifelse(annual==TRUE,"annual","decadal"),"_change_EOS_",prod,sep="")
	write.ENVI(out.eos.re, outname.eos.re, interleave = "bsq" ) 
	print(paste("laire EOS done",prod))
}
