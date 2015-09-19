#
# yearly dominating facto

# 1 = temp = 1
# 2 = moisture = 10
# 3 = light = 100



library(caTools)
library(raster)
source("../general_functions.R")


mtrx.stack <- array(dim=c(360,720,30))
mtrx.changeflag <- array(dim=c(360,720))
print("go")
for(phenop in c("SOS","EOS")) {
	for(prod in c("LAIre","LAI3g")) {
		for (year in 1982:2011) {
			filename = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/yearly_dominating/",prod,"/",phenop,"_dominating_control_",year,sep="")
			mtrx.temp = read.ENVI(filename)
			mtrx.temp[mtrx.temp==2] <- 10
			mtrx.temp[mtrx.temp==3] <- 100
	
			mtrx.stack[,,year-1981] = mtrx.temp 
		}
		print("finished_years")

		#mtrx.stack[155,360,]
		mtrx.out.uchanges = apply(mtrx.stack,c(1,2),function(x) length(unique(x)))
		#days_per_decade.proc(mtrx.stack[90,470,])
		#diff(mtrx.stack[mtrx.out.uchanges==2])


		#mtrx.out.diff_chng = apply(mtrx.stack,c(1,2),)
		#mtrx.out.nchanges = apply(mtrx.out.diff_chng,c(1,2),function(x) length(x[x>0]))
		#mtrx.out.nchanges
		#mtrx.out.nchanges = days_per_decade(mtrx.stack)
		filename.out.uchanges = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/yearly_dominating/",phenop,"_",prod,"_unique_dominating_factors",sep="")
		write.ENVI(mtrx.out.uchanges,filename.out.uchanges,interleave = "bsq")
	}
}


