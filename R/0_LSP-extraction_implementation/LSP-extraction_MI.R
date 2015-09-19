##
## LSP parameter extraction: Maxincrease method
## dschenkel, dec 2014
##

library(abind)
library(caTools)
library(ncdf4)


# simple function to plot LAI profile of a pixel and SOS/EOS
plotpixel <- function(ts, scene.sos, value.sos, scene.eos, value.eos) {

	# Graph cars using a y axis that ranges from 0 to 12
	plot(scene.sos, value.sos, col="blue", ylim=c(0,max(ts)), xlim=c(0,length(ts)))
	text(scene.sos+1, value.sos,"SOS")
	points(scene.eos,value.eos, col="green")
	text(scene.eos+1, value.eos,"EOS")
	# Graph trucks with red dashed line and square points
	lines(ts, type="o", col="red")
	
}

# set variables (hemisphere, rootdir, maskdir, LAI dataset (v3g or re)
# minimum value and minimum variability in 1 year (attention: LAI3g is *10 (~0-80) from LAIre(~0-8))
hem = "NHEM"
data.rootdir = "~/Documents/Uni/Masterarbeit/"
data.mask <- read.ENVI(paste(data.rootdir,"watermask/watermask_",tolower(hem),".envi",sep=""))
lainame= "LAIv3g"
mindelta = 15

year.min = 1982
year.max = 1982

for(year in year.min:year.max) {
	#get LAIre for timing (length of year)
	data.meta <- nc_open(paste(data.rootdir, "LAIre/raw_data/Global-0.5x0.5.analysis.",year,".nc",sep=""))
	data.meta.time <- length(ncvar_get(data.meta, "time"))
	
	#get LAI dataset to be processed
	data.name <- paste(data.rootdir,lainame,"/hantsout/",hem,"/smoothed/smoothed",year, sep="") 
	data = read.ENVI(data.name)
	
	#set year before and year after. if the year is the first/last year, use the current year as the year before/after
	if(year==year.min) {
		data.ly.name <- paste(data.rootdir,lainame,"/hantsout/",hem,"/smoothed/smoothed",year.min, sep="") 	
	}
	else {
		data.ly.name <- paste(data.rootdir,lainame,"/hantsout/",hem,"/smoothed/smoothed",year-1, sep="") 
	}
	data.ly = read.ENVI(data.ly.name)
	
	if(year==year.max) {
		data.ny.name <- paste(data.rootdir,lainame,"/hantsout/",hem,"/smoothed/smoothed",year.max, sep="") 	
	}
	else {
		data.ny.name <- paste(data.rootdir,lainame,"/hantsout/",hem,"/smoothed/smoothed",year+1, sep="") 
	}
	data.ny = read.ENVI(data.ny.name)
	
	#get minimum, maximum per year, calculate difference
	data.max <- apply(data,c(1,2),max)
	data.min <- apply(data,c(1,2),min)
	data.delta <- data.max-data.min
	#data.midpoint <- data.min + data.delta/2
	
	
	# get second time series, shifted by 1 timestep
	data.dims = dim(data)
	data.shifted = abind(data[,,2:data.dims[3]],data.ny[,,1]) 

	# calculate difference between data and shifted data -> get differences for each time step
	data.shifted = data.shifted-data
	
	#  get index of max inflection (maximum value of shifted - not_shifted)
	data.shifted.max = apply(data.shifted,c(1,2),which.max)


	
	data.out.sos <- array(data=NaN, dim=c(data.dims[1],data.dims[2]))
	data.out.eos <- array(data=NaN, dim=c(data.dims[1],data.dims[2]))
		
	#go through latitudes
	for(i in 1:data.dims[1]) {
		
		#if whole line (latitude) is empty, go to next
		if(sum(data.mask[i,]) == 0){
			next()
		}
		
		# go through longitudes 
		for(j in 1:data.dims[2]) {
			# if pixel is not waterbody
			if(data.mask[i,j] == 1) {
				
				# intra-annual variation too small
				if(data.delta[i,j]<mindelta) {
					next()
				}
				
				
				# set number of days for each scene
				scenelength = data.meta.time/data.dims[3];
				
				# set date-step below max-inflection point
				index.min.sos = data.shifted.max[i,j]
				
				#set actual time of SOS exactly between the two indices marking the max-inflection
				index.act.sos = index.min.sos + 0.5
				
				# value of timestep below max-inflection
				val.min.sos = data[i,j,index.min.sos]
				
				# set value of timestep above max-inflection
				
				# check for date at end of year (use first scene of next year)
				if(data.shifted.max[i,j] == data.dims[3]) {
					val.max.sos = data.ny[i,j,1] 
				}
				else {
					val.max.sos = data[i,j,data.shifted.max[i,j]+1]					
				}
				# set actual LAI value at SOS_MI
				val.act.sos = (val.min.sos+val.max.sos)/2
				
				# calculatae actual day-of year by calcualting time-step index * scenelength
				data.out.sos[i,j] = scenelength*(index.min.sos-0.5)
				
				
				if(data[i,j,1]>val.act.sos) {  #southern hem case: if actual SOS value is BEFORE start of the year, EOS has to be determined by last year's data
					# set data for year before (last year)
					data.ly.dims = dim(data.ly)
					data.ly.shifted = abind(data[i,j,2:data.ly.dims[3]],data[i,j,1]) 
					#print(data.shifted)
					data.ly.shifted = data.ly.shifted-data.ly[i,j,]
	
					#  get index of max inflection
					data.ly.shifted.max = which.max(data.ly.shifted)

					if(data.ly.shifted.max==data.ly.dims[3]){
						val.act.eos <- (data.ly[i,j,data.ly.shifted.max]+data[i,j,1])/2
					}
					else {
						val.act.eos <- (data.ly[i,j,data.ly.shifted.max]+data.ly[i,j,data.ly.shifted.max+1])/2
					}
					index.max.eos <- min(which(data[i,j,] <= val.act.eos))
					index.min.eos <- index.max.eos-1					
					
				}
				else { #~northern hem, set EOS
					val.act.eos <- val.act.sos
					index.min.eos <- max(which(data[i,j,] >= val.act.sos))
					index.max.eos <- index.min.eos+1
				}
				
				if(index.min.eos==0) { #border case if EOS at beginning of year (get last year's data)
					val.min.eos = data.ly[i,j,data.dims[3]]	
				}
				else {
					val.min.eos = data[i,j,index.min.eos]					
				}
				if(index.max.eos>data.dims[3]) { #border case if EOS at end of year
					val.max.eos = data.ny[i,j,1] 
				}
				else {
					val.max.eos = data[i,j,index.max.eos]					
				}
				
				
				#get actual EOS timestep
				index.act.eos = index.min.eos + (val.act.eos-val.min.eos)/(val.max.eos-val.min.eos) 
				#calculate date of year
				data.out.eos[i,j] =  scenelength*(index.act.eos-1)
				
				#plot select pixels for testing
					if(i==99 && j == 201 && year == 1989) {
						print(val.act.sos)
						print(val.act.eos)
					
						#print(data.out.sos[i,j])
						plotpixel(data[i,j,],index.act.sos,val.act.sos,index.act.eos,val.act.eos)
					}
					if(i == 231 && j == 420 && year == 1989) {
						print(val.act.sos)
						print(val.act.eos)
					
						#print(data.out.sos[i,j])
						plotpixel(data[i,j,],index.act.sos,val.act.sos,index.act.eos,val.act.eos)
					}
			}
		}
	}
	#write data
	write.ENVI(data.out.sos,paste(data.rootdir,lainame,"/LSPout/",hem,"/MI/",lainame,"_",year,"_sos_MI",sep=""))
	write.ENVI(data.out.eos,paste(data.rootdir,lainame,"/LSPout/",hem,"/MI/",lainame,"_",year,"_eos_MI",sep=""))
	rm(data.out.eos, data.out.sos, data.ny, data.ly, data.meta)
}	