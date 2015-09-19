##
## plot results for second research questions (trends and dominating climatic controls)
## most plots are automatically classified by quickplot.R functions
## dschenkel, jul 2015
##

source("../quickplot.R")
month <- c("jana","janb","feba","febc","mara","marb","apra","aprb","maya","mayb","juna","junb","jula","julb","auga","augb","sepa","sepb","octa","octb","nova","novb","deca","decb")

# plot QUARTERLY trends in climatic controls
for(prod in c("TEMP_FAC","MOIST_FAC")) {
	mtrx.seasonal = matrix(0,nrow=360,ncol=720)
	
	for(i in 1:24) {
		

		filename.wtr = "~/Documents/Uni/Masterarbeit/watermask/watermask.envi"
		mask = read.ENVI(filename.wtr)
		mask[mask==0] <- NA
		filename = paste("~/Documents/Uni/Masterarbeit/2_controls/bimonthly_changes/",prod,"_",i,sep="")
		mtrx = read.ENVI(filename) #in annual percent
		
		# add up for quarterly-maps
		mtrx.n = mtrx
		mtrx.n[is.na(mtrx.n)] <- 0
		mtrx.seasonal = mtrx.seasonal + mtrx.n
				
		if(i %% 6 == 0) { #if a quarter is reached
			mtrx.seasonal[mtrx.seasonal == 0] <-- NA
			mtrx.seasonal = (mtrx.seasonal*mask)/6
			if(prod=="TEMP_FAC" && i==12) { # set classes for second quarter of temperature
				classes = classify_image.div(mtrx.seasonal,7,0.01)
				colorPale = brewer.pal(9,"BrBG")
				colorPal = colorPale[2:8]
				
			}
			else if (prod=="TEMP_FAC" && i==6) { # set classes for first quarter of temperature
				classes = c(min(mtrx.seasonal, na.rm=TRUE),-0.03,-0.023,-0.016,-0.01,0.01,0.05,0.1,0.16,max(mtrx.seasonal, na.rm=TRUE))
				colorPal = brewer.pal(9,"BrBG")
				
			}
			else { #all other classifications
				classes = classify_image.div(mtrx.seasonal,9,0.01)
				colorPal = brewer.pal(9,"BrBG")
				
			}
			#classes = classify_image.div(mtrx.seasonal,9,0.01)
			#classes = c(min(mtrx.seasonal, na.rm=TRUE),-0.06,-0.05,-0.03,-0.01,0.01,0.03,0.05,0.06,max(mtrx.seasonal, na.rm=TRUE))
			classes.names = name_classes(classes)
			filename.out = paste("~/Documents/Uni/Masterarbeit/2_controls/bimonthly_changes/plots/quarter_",prod,"_",i,".png",sep="")
			quickplot(mtrx.seasonal, classes=classes, classes.names = classes.names, color=colorPal, smoothing=FALSE, outname=filename.out)
			mtrx.seasonal = matrix(0,nrow=360,ncol=720)
		}

	}
}

# plot number of unique dominating factors per pixel (1, 2 or 3)
filename.uchanges = "~/Documents/Uni/Masterarbeit/2_controls/unique_dominating_factors"
mtrx = read.ENVI(filename.uchanges) 
classes = c(0.5,1.5,2.5,3.5)
classes.names = c("no change in domniating control", "change between 2 controls", "change between 3 controls")
colorPal = brewer.pal(3,"Pastel1")
colorPal[1] = "#FFFFFF" # set 0 changes per pixel to white, not interested in those
quickplot(mtrx,classes=classes,classes.names=classes.names,colors=colorPal,outname=paste(filename.uchanges,".png",sep=""))

#plot bimonthly changes
for(prod in c("TEMP_FAC","MOIST_FAC")) {
	mtrx.seasonal = matrix(0,nrow=360,ncol=720)
	
	for(i in 1:24) {
		
		
		filename = paste("~/Documents/Uni/Masterarbeit/2_controls/bimonthly_changes/",prod,"_",i,sep="")
		mtrx = read.ENVI(filename) #in annual percent
		
	
		filename.out = paste("~/Documents/Uni/Masterarbeit/2_controls/bimonthly_changes/plots/",prod,"_",i,"_",month[i],".png",sep="")
		
		classes = classify_image.div(mtrx,5,0.05)
		classes.names = name_classes(classes)
		colorPal=brewer.pal(5,"BrBG")
		quickplot(mtrx,classes=classes,classes.names=classes.names,color=colorPal, smoothing=FALSE, outname=filename.out)
		
	}
}





source("../quickplot.R")


# yearly dominating controls
for(year in 1982:2011) {
	filename = paste("~/Documents/Uni/Masterarbeit/2_controls/yearly_dominating/",year,"_dominating_control",sep="")
	filename.out = paste("~/Documents/Uni/Masterarbeit/2_controls/yearly_dominating/plots/",year,"_dominating_control.png",sep="")
	mtrx = read.ENVI(filename)
	
	classes=c(0.5,1.5,2.5,3.5) # 3 classes to catch values of 1, 2 and 3
	classes.names = c("Temperature","Moisture","Radiation")
	colorPal=brewer.pal(3,"Pastel1") #pastel colors fit well
	quickplot(mtrx,classes=classes,color=colorPal,classes.names=classes.names, smoothing=FALSE, outname=filename.out)
	
}
#filename = "~/Documents/Uni/Masterarbeit/2_controls/yearly_dominating/1982_dominating_control"
#mtrx = read.ENVI(filename)
#quickplot(mtrx,classes=FALSE, smoothing=TRUE,outname="/Users/davidschenkel/Documents/Uni/Masterarbeit/test2.png")
