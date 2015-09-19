##
## plot results of research question 3 (climatic controls at SOS, EOS)
## dschenkel, jul 2015
##

source("quickplot.R")


# dominating controls at SOS, EOS
for(phenop in c("SOS","EOS")) {
	for(prod in c("LAIre","LAI3g")) {
		for (year in 1982:2011) {
			#plot yearly dominating controls for both SOS and EOS
			filename = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/yearly_dominating/",prod,"/",phenop,"_dominating_control_",year,sep="")
			mtrx = read.ENVI(filename)
			classes=c(0.5,1.5,2.5,3.5)
			classes.names = c("Temperature","Moisture","Radiation")
			colorPal=brewer.pal(3,"Pastel1")
			filename.out = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/yearly_dominating/plots/",prod,"/",phenop,"_dominating_control_",year,".png",sep="")
			quickplot(mtrx,classes=classes,color=colorPal,classes.names=classes.names, smoothing=FALSE, outname=filename.out)
			
		}
		#maskname = "~/Documents/Uni/Masterarbeit/watermask/watermask.envi"
		#mtrx.mask = read.ENVI(maskname)
		
		#plot number of unique dominating factors
		filename.uchanges = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/yearly_dominating/",phenop,"_",prod,"_unique_dominating_factors",sep="")
		mtrx = read.ENVI(filename.uchanges) 
		classes = c(0.5,1.5,2.5,3.5)
		classes.names = c("no change in domniating control", "change between 2 controls", "change between 3 controls")
		colorPal = brewer.pal(3,"Pastel1")
		colorPal[1] = "#FFFFFF"
		quickplot(mtrx,classes=classes,classes.names=classes.names,colors=colorPal,outname=paste(filename.uchanges,".png",sep=""))
	}
}

source("quickplot.R")

#plot bimonthly decadal changes
for(prod in c("MOIST_FAC","TEMP_FAC","LIGHT_FAC")) {
	for(pheno in c("EOS","SOS")) {
		for(platf in c("LAIre","LAI3g")) {
			filename = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/monthly_decadal_change_signf/",platf,"_annual_change_",pheno,"_",prod,sep="")
			filename.out = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/monthly_decadal_change_signf/plots/",platf,"_decadal_change_",pheno,"_",prod,".png",sep="")
			mtrx = read.ENVI(filename)
		
			#automatic classificaiton (overridden later on for final plots)
			classes = classify_image.div(mtrx,9,0.01)
			colorPal=brewer.pal(9,"BrBG")

			#individual thesis plots based on actual values, to get same classes for SOS and EOS
			# classification for moisture (for final print)
			if(prod=="MOIST_FAC" && platf=="LAIre" && pheno=="SOS") {
				classes = c(min(mtrx,na.rm=TRUE),-0.13,-0.09,-0.05,-0.01,0.01,0.03,0.06,max(mtrx,na.rm=TRUE))
				colorPale=brewer.pal(9,"BrBG")
				colorPal = colorPale[1:8]
			}
			if(prod=="MOIST_FAC" && platf=="LAIre" && pheno=="EOS") {
				classes = c(min(mtrx,na.rm=TRUE),-0.13,-0.09,-0.05,-0.01,0.01,0.03,0.06,0.09,max(mtrx,na.rm=TRUE))
			}

			# classification for temperature (for final print)
			if(prod=="TEMP_FAC" && platf=="LAIre" && pheno=="SOS") {
				classes = c(min(mtrx,na.rm=TRUE),-0.09,-0.06,-0.03,-0.01,0.01,0.07,0.14,0.2,max(mtrx,na.rm=TRUE))
			}
			if(prod=="TEMP_FAC" && platf=="LAIre" && pheno=="EOS") {
				classes = c(min(mtrx,na.rm=TRUE),-0.03,-0.01,0.01,0.07,0.14,0.2,max(mtrx,na.rm=TRUE))
				colorPale=brewer.pal(9,"BrBG")
				colorPal = colorPale[3:9]

			}

			# classification for light  (for final print)
			if(prod=="LIGHT_FAC" && platf=="LAIre" && pheno=="SOS") {
				classes = c(min(mtrx,na.rm=TRUE),-0.1,-0.07,-0.04,-0.01,0.01,0.04,0.07,0.1,max(mtrx,na.rm=TRUE))
			}
			if(prod=="LIGHT_FAC" && platf=="LAIre" && pheno=="EOS") {
				classes = c(min(mtrx,na.rm=TRUE),-0.07,-0.04,-0.01,0.01,0.04,0.07,0.1,max(mtrx,na.rm=TRUE))
				colorPale=brewer.pal(9,"BrBG")
				colorPal = colorPale[2:9]

			}

			classes.names = name_classes(classes)

			quickplot(mtrx,classes=classes,color=colorPal,classes.names=classes.names, smoothing=FALSE, outname=filename.out)
		
		}
	}
}




#source("../quickplot.R")


# yearly dominating controls
for(year in 1982:2011) {
	for(plat in c("LAIre","LAI3g")) {
		for(pheno in c("SOS","EOS")) {
			
			filename = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/yearly_dominating/",plat,"/",pheno,"_dominating_control_",year,sep="")
			filename.out = paste("~/Documents/Uni/Masterarbeit/3_cc-LAI/yearly_dominating/plots/",plat,"/",pheno,"_dominating_control_",year,".png",sep="")
			mtrx = read.ENVI(filename)
	
			classes=c(0.5,1.5,2.5,3.5)
			classes.names = c("Temperature","Moisture","Radiation")
			colorPal=brewer.pal(3,"Pastel1")
			quickplot(mtrx,classes=classes,color=colorPal,classes.names=classes.names, smoothing=FALSE, outname=filename.out)
	
			
			
		}
	}

}
#filename = "~/Documents/Uni/Masterarbeit/2_controls/yearly_dominating/1982_dominating_control"
#mtrx = read.ENVI(filename)
#quickplot(mtrx,classes=FALSE, smoothing=TRUE,outname="/Users/davidschenkel/Documents/Uni/Masterarbeit/test2.png")
