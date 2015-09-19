##
## correlate rates of change for SOS and EOS and for climatic controls at SOS, EOS (was discareded due to lack of explanatory power)
## dschenkel, jun 2015
##


# Correlate SOS/EOS Changerate to CC changerate at that date
library(caTools)
meth = "MP"

table = matrix(data = NA, nrow = 5, ncol = 5)
table[1,3] = "MOISTURE"
table[1,4] = "TEMPERATURE"
table[1,5] = "LIGHT"
counter = 2

	for(pheno in c("SOS","EOS")) {
		table[counter,1] = pheno
		table[counter+1,1] = pheno
		for(platf in c("LAIre","LAI3g")) {
			contr = 3
			for(prod in c("MOIST_FAC","TEMP_FAC", "LIGHT_FAC")) {
				
			table[counter,2] = platf
			
			filename.cc = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/monthly_decadal_change/",platf,"_decadal_change_",pheno,"_",prod,sep="")
			filename.pheno = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/decade_change/",platf,"/",platf,"_changepdec_",pheno,"_",meth,sep="")
			mtrx.cc = read.ENVI(filename.cc)
			mtrx.pheno = read.ENVI(filename.pheno)
			table[counter,contr]
			#print(prod)
			#print(pheno)
			#print(platf)
			cc = as.vector(mtrx.cc)
			phenol = as.vector(mtrx.pheno)
			out = lm(cc ~ phenol)
			corrr = cor(as.vector(mtrx.cc),as.vector(mtrx.pheno),method="pearson",use="pairwise.complete.obs")
			
			if(summary(out)$coefficients['phenol',4] <= 0.05) {
				table[counter,contr] = paste(corrr,"*") 	
			}
			else {
				table[counter,contr] = corrr 
			}
			contr = contr + 1
			
		
		}
		counter = counter + 1
	
	}
}



write.csv(table, file = "~/Documents/Uni/Masterarbeit/3_cc-LAI/correlate_changerates.csv")
