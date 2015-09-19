##
## quickplot: functions used for classifiying and plotting maps
## dschenkel, jun 2015
##

rm(list = ls())

# probably not all of those are necessary anymore.
#library(ncdf)
library(raster)
library(fields)
library(ggplot2)
library(caTools)
library(reshape2)
library(rasterVis)
library(colorspace)
library(maps)
library(mapdata)
library(maptools)
library(RColorBrewer)


# smoothing function (unused, but nice to have, right?)
# inputs: raster to be smoothed (rstr), window size to smooth over (x*y)
# output: rasterfile with smoothed image
smooth_image <- function(rstr,x_win,y_win) {
	rstr.out <- focal(rstr, fun=median,w=matrix(1,nrow=x_win,ncol=y_win), na.rm=FALSE)
	return(rstr.out)
}

# automatically name classes according class borders (A – B, with <= and >= for end pieces)
# inputs: a vector with the classes, number of digits to be used in names (standard is 3)
# output: vector with class names
name_classes <- function(classes,ndig=3) {
	classes.names = character((length(classes)-1))
	classes.names[1] = paste("<=",signif(classes[2],digits=ndig))
	for(i in 2:(length(classes)-2)) {
		classes.names[i] = paste(signif(classes[i],digits=ndig),"-",signif(classes[i+1],digits=ndig))
	}
	classes.names[(length(classes)-1)] = paste(">=",signif(classes[(length(classes)-1)],digits=ndig))

	return(classes.names)
}

# automatically classify image in diverging classes with 0 being assumed the dividing line
# same amount of classes around 0 are assumed.
# 
# great for getting a quick overview of data with no regard to compatability to other plots
# inputs: 
#  - map to classify (matrix, might also work with raster actually)
#  - classes: number of classes wanted (should be an _odd_ number)
#  - divider: set buffer around 0 (i.e. 0.1 if all values between -0.1 and 0.1 are probably "ininteresting")
#			The rest of the classes are automatically set automatically with equal size between 0 and min, bzw. 0 and max 
# output: vector with classes to form a diverging classification around 0
classify_image.div <- function(map,classes,divider) {
	map.min = min(map, na.rm=TRUE)
	map.max = max(map, na.rm=TRUE)
	
	if(map.min>0 | map.max<0) { # if values are all under or above 0, this won't work, next
		return(next)
	}
	
	#sub-zero classes
	classes_subz = seq(map.min,ifelse(map.min>= -divider,map.min/2,-divider),length.out=((classes+1)/2))
	
	#above zero classes
	classes_abz = seq(ifelse(map.max<=divider,map.max/2,divider),map.max,length.out=((classes+1)/2))

	outclasses = c(classes_subz,classes_abz)
	
	return(outclasses)
	
}

# classify image, classifies a matrix (map) based on a class-vector (classes, e.g. c(-1,-0.5,0.5,1))
# alternatively just number of classes for equally spaced classes from min to max data (for very quick plotting which might go utterly wrong)
# output: a new raster with integers for classes (1...nClasses)

classify_image <- function(map,classes) { #classes = class borders or number of classes
	if(length(classes) == 1) { #assume equal spacing if class is integer (number of classes
		map.min = min(map, na.rm=TRUE)
		map.max = max(map, na.rm=TRUE)
		group_size = (map.max-map.min)/classes
		classes = seq(map.min,map.max,by=group_size)
	}


	map.out = array(data=NA,dim=dim(map)) 
	for(i in 1:(length(classes)-1)) { # class length is length(classes) - 1 because both lowest and highest class border are part of the vector
		if(i==(length(classes)-1)) { # if last class is reached
			#print(classes[i])
			#print(classes[i+1]) 
			# include lowest class value as well as highest class value
			map.out[map>=classes[i] & map<=classes[i+1]] <- i
		}
		else {
			
			#if highest class is not reached yet, include those values equal to lower class border, but not higher (values exactly equal higher class border included in next class)
			map.out[map>=classes[i] & map<classes[i+1]] <- i			
		}
	}
	return(map.out)
}

# UNUSED:  create boundary layer.. direct copy from stackoverflow and not used...
boundary_layer <- function(rstr) {
	ext = extent(rstr)
	boundaries <- map('world', fill=TRUE,
	    xlim=ext[1:2], ylim=ext[3:4],
	    plot=FALSE)

	## read the map2SpatialPolygons help page for details
	IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
	bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
	                              proj4string=CRS(projection(rstr)))
	return(bPols)
}

# main function to plot
# inputs:
#  - map: matrix with the map to be plotted
#  - classes: amount of classes OR vector with class borders (including lowest and highest)
#  - classes.names: names of classes (otherwise created with name_classes())
#  - colors: color-vector including as many colors as classes (recommended to be produced by colorbrewer)
#  - outname: full path and name of image output (or false if no output is wanted)
#  - smoothing: smoothing of image if needed. not used in this case
# output: PNG with legend

quickplot <- function(map, classes, classes.names, colors, outname=FALSE,smoothing=FALSE) {
	if(is.vector(classes)) { #if classes is not a vector, produce classes
		map <- classify_image(map,classes)
	}
	map[map==NA] <- 0 #remove NA's from dataset and set to 0
	
	rstr = raster(map) # rasterize
	
	if(smoothing != FALSE) { # smooth if true
		rstr <- smooth_image(rstr,3,3)
	}
	
	extent(rstr) <- extent(-180, 180, -90, 90) #set extent
	
	rstr <- ratify(rstr) # ratify raster

	rat <- levels(rstr)[[1]] # set levels (for levelplot later on)
	
	
	rat$classes <- classes.names # assign class names (can lead to problems if not all classes have values; should be fixed at some point)
	levels(rstr) <- rat # assign levels incl. names to the raster
	
	# set PNG output options
	png(filename = outname,
	    width = 850, height = 480, units = "px",
	     bg = "white")
		 
		 # load world boundaries (continents)
		 wld <- map('world', interior=F,boundary=T, xlim=c(-180,180), 
		 ylim=c(-79,90),plot=FALSE) # limit to -79 southern latitude to get around ugly border effects

		 wld <- data.frame(lon=wld$x, lat=wld$y) 


		 #?maps::map
		 # levleplot with colors
		 p0 <- levelplot(rstr, col.regions=colors) +	
   		 xyplot(lat ~ lon, wld, type='l', lty=1, lwd=1, col='black') # plot world map on top of levelplot

	
	print(p0)
	
	dev.off() #push it all out
 

}

