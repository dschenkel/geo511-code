##
## Plot results with quickplot.R
## set classes, class names, colors etc. 
## dschenkel, jun 2015
##


source("quickplot.R")


for(meth in c("MP", "MI")) {
	for(prod in c("GSL", "EOS", "SOS")) {
		for(platf in c("LAI3g","LAIre")) {
if(TRUE) {			
			# plot trends in SOS, EOS, GSL
			filename = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/decade_change/",platf,"/",platf,"_changepdec_",prod,"_",meth,"",sep="")
			filename.out = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/decade_change/plots/",platf,"_changeperdec_",prod,"_",meth,".png",sep="")
			mtrx = read.ENVI(filename)
		
			classes.min = min(mtrx,na.rm=TRUE)
			classes.max = max(mtrx,na.rm=TRUE)
			classes.mid = 5.0
			#classes.bmid = -classes.mid+(classes.min+classes.mid)/2
			#classes.umid = classes.mid+(classes.max-classes.mid)/2
		
			#classes = c(classes.min,classes.bmid,-classes.mid,classes.mid,classes.umid,classes.max)
		
			#classes.names = c(paste(signif(classes.min,digits=3),"-",signif(classes.bmid,digits=3)),
			#paste(signif(classes.bmid,digits=3),"-",signif(-classes.mid,digits=3)),
			#paste(signif(-classes.mid,digits=3),"-",signif(classes.mid,digits=3)),
			#paste(signif(classes.mid,digits=3),"-",signif(classes.umid,digits=3)),
			#paste(signif(classes.umid,digits=3),"-",signif(classes.max,digits=3))
			#)
		
			classes = c(classes.min,-8,-6,-4,-2,2,4,6,8,classes.max)
			classes.names = c("<= -8 days", "-8 - -6 days", "-6 - -4 days", "-4 - -2 days", "-2 - 2 days", "2 - 4 days", "4 - 6 days", "6 - 8 days", ">= 8 days")
			
		
			# custom color palette green to brown (greening to browning)
			colorPal = c("#1a9850", "#66bd63", "#a6d96a", "#d9ef8b", "#F7F7F7", "#F6E8C3", "#DFC27D", "#BF812D", "#8C510A")
			if(prod=="GSL" || prod == "EOS") { #turn it around for GSL and EOS (less days = browner, more days = greener)
				colorPal = rev(colorPal)
			}
			

			quickplot(mtrx,classes=classes,color=colorPal,classes.names=classes.names, smoothing=FALSE, outname=filename.out)
			rm(classes, classes.names, mtrx, colorPal)
			
		

		}
	}

		#plot difference maps
	
if(TRUE) {
	for(year in c(1982:2011)) {
			
			filename = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/maps/",prod,"_diff/",meth,"/",year,"_LAIre-LAI3g_",prod,sep="")
			filename.out = paste("~/Documents/Uni/Masterarbeit/1_LAI_comparison/maps/plots/",prod,"_diff/",meth,"/",prod,"_diff_LAIre-LAI3g_",meth,"_",year,"_",meth,".png",sep="")
			mtrx = read.ENVI(filename)
	
			classes.min = min(mtrx,na.rm=TRUE)
			classes.max = max(mtrx,na.rm=TRUE)


		
			classes = c(classes.min,-120,-60,-30,-15,15,30,60,120,classes.max)
	
			classes.names = c("<= -120 days", 
								"-120 - -60 days", 
								"-60 - -30 days", 
								"-30 - -15 days", 
								"-15 - 15 days", 
								"15 - 30 days", 
								"30 - 60 days", 
								"60 - 120 days", 
								">= 120 days")
		
	
			colorPal=brewer.pal(9,"BrBG")
			quickplot(mtrx,classes=classes,color=colorPal,classes.names=classes.names, smoothing=FALSE, outname=filename.out)
			rm(classes, classes.names, mtrx, colorPal)
	
			}
		}
	}
}



