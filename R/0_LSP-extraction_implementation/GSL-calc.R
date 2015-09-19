library(caTools)
hemispheres = c('NHEM', 'SHEM')
products = c("LAIv3g")
methods = c("MI","MP")

for (year in 1982:1982) {
	for(hem in hemispheres) {
		for(prod in products) {
			for(meth in methods) {

				filename.sos <- paste("~/Documents/Uni/Masterarbeit/",prod,"/LSPout/",hem,"/",meth,"/",year,"_sos_",meth,sep="") 
				filename.eos <- paste("~/Documents/Uni/Masterarbeit/",prod,"/LSPout/",hem,"/",meth,"/",year,"_eos_",meth,sep="") 

				mtrx.sos <- read.ENVI(filename.sos, headerfile=paste(filename.sos, ".hdr", sep="")) 
				mtrx.eos <- read.ENVI(filename.eos, headerfile=paste(filename.eos, ".hdr", sep=""))
				
				mtrx.gsl <- mtrx.eos - mtrx.sos

				######
				### Build in checks for length here (negative values, ...)
				######
				filename.gsl <- paste("~/Documents/Uni/Masterarbeit/",prod,"/LSPout/",hem,"/",meth,"/",year,"_gsl_",meth,sep="") 
		
				write.ENVI(mtrx.gsl, filename.gsl, interleave = "bsq") 
				rm(mtrx.gsl, mtrx.sos, mtrx.eos)
			}
		}
	}
}