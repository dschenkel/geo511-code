##
## extract climatic controls prior to SOS and EOS
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

library(ncdf4)
library(raster)
library(fields)
library(caTools)

# function to get ncdf to array
ncdf_to_mtrx <- function(filename, prod) {
	nc <- nc_open(filename)

	mtrx <- ncvar_get(nc, prod)
	dims <- dim(mtrx)
	#print(dims[3])
	rS = stack()
	for( i in 1:dims[3]) {
		rstr <- raster(mtrx[,,i]) #convert to raster so we can t and flip it easily to get it in right projection
		rstr <-  flip(t(rstr), direction = "y") 
		rstr.stack <- stack(rstr.stack, rstr)
	}
	out <- as.array(rstr.stack)
	#print(dim(out))
	return(out)
}

# function to exctract means of climatic controls before SOS, EOS; 
# doy = SOS, EOS date
# cur_cc = climatic control (either temp, light or moisture) of current year (vector)
# prev_cc = climatic control of previous year (vector)
# next_cc = climatic control of next year (vector)
# dt = timeframe in days, default is 30 days
extract_mean_cc <- function(doy, cur_cc, prev_cc, next_cc, dt=30) {
	if(doy>dt) { #if day of year bigger than timeframe ...
		if(doy>length(cur_cc)) { #... if day of year bigger than number of days in this year (length of cur_cc should be amount of days in current year)
			end_date.next = doy-length(cur_cc) # end of our timeframe must be in next year then (next_cc)
			if((doy-dt)>length(cur_cc)) { # both start and end_date are in next year
				end_date = doy - length(cur_cc)
				start_date = end_date - dt
				return(mean(next_cc[start_date:end_date])) #return mean of 30 days from next year's climatic controls
			}
			else { # year-end problem if 30 days are overlapping end of year
				end_date.next = doy - length(cur_cc)
				start_date.next = 1
				end_date.cur = length(cur_cc)
				start_date.cur = length(cur_cc) - (dt - end_date.next)
				#extract controls from both years
				tempvec.cur = cur_cc[start_date.cur:end_date.cur]
				tempvec.next = next_cc[start_date.next:end_date.next]
				#combine vectors anre take mean
				return(mean(c(tempvec.cur,tempvec.next)))
			}
		}
		else { # date completely in current year, get timeframe and take mean
			start_date = doy - dt
			end_date = doy
			return(mean(cur_cc[start_date:end_date]))
		}
	}
	else if(doy<=dt && doy>0) { #if dt laps over into last year
		start_date.cur = 1
		end_date.cur = doy
		# get climatic controls from last year ..
		end_date.prev = length(prev_cc)
		start_date.prev = end_date.prev - (dt-end_date.cur)
	
		tempvec.prev = prev_cc[start_date.prev:end_date.prev]
		tempvec.cur = cur_cc[start_date.cur:end_date.cur]
		# .. combine with current year's cc's and return
		return(mean(c(tempvec.prev, tempvec.cur)))
	}
} 


# set method
meth <- "MP"
#rootdir
rootdir = paste("~/Documents/Uni/Masterarbeit/",sep="")

# amount of days before SOS/EOS date to include
#dt = 30 #30 days is standard

table.laire = matrix(data = NA, nrow = 60, ncol = 4)
table.lai3g = matrix(data = NA, nrow = 60, ncol = 4)

colnames(table.laire) <- c("year","TEMP_FAC","MOIST_FAC","LIGHT_FAC")
colnames(table.lai3g) <- c("year","TEMP_FAC","MOIST_FAC","LIGHT_FAC")

counter = 1
param.name = list("SOS","bla","blub", "EOS") #lazyness fix since 1 = SOS, 4 = EOS in PHENO dataset (VIprocessor output); bla and blub can be ignored
for(param in c(1,4)) {
	for(year in 1982:2011) {
		filename.shem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/SHEM/PHENO/",year,"_SHEM_",meth,"__PHENO", sep="") 
		filename.nhem.laire = paste("~/Documents/Uni/Masterarbeit/LAIre/VIprocessor_output/NHEM/PHENO/",year,"_NHEM_",meth,"__PHENO", sep="") 
	
		filename.shem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/SHEM/PHENO/LAI3g_",year,"_SHEM_",meth,"__PHENO", sep="") 
		filename.nhem.lai3g = paste("~/Documents/Uni/Masterarbeit/LAIv3g/VIprocessor_output/NHEM/PHENO/LAI3g_",year,"_NHEM_",meth,"__PHENO", sep="") 

		mtrx.shem.laire = read.ENVI(paste(filename.shem.laire,".bsq",sep=""), headerfile=paste(filename.shem.laire, ".hdr", sep="")) 
		mtrx.nhem.laire = read.ENVI(paste(filename.nhem.laire,".bsq",sep=""), headerfile=paste(filename.nhem.laire, ".hdr", sep="")) 
		mtrx.shem.lai3g = read.ENVI(paste(filename.shem.lai3g,".bsq",sep=""), headerfile=paste(filename.shem.lai3g, ".hdr", sep="")) 
		mtrx.nhem.lai3g = read.ENVI(paste(filename.nhem.lai3g,".bsq",sep=""), headerfile=paste(filename.nhem.lai3g, ".hdr", sep="")) 



		mtrx.laire <- array(data=NA, dim=c(360,720))
		mtrx.lai3g <- array(data=NA, dim=c(360,720))
		mtrx.out.laire <- array(data=NA, dim=c(360,720))
		mtrx.out.lai3g <- array(data=NA, dim=c(360,720))


		mtrx.laire[181:360,] <- mtrx.shem.laire[181:360,,param]+182
		mtrx.laire[1:180,] <- mtrx.nhem.laire[1:180,,param]

		mtrx.lai3g[181:360,] <- mtrx.shem.lai3g[181:360,,param]+182
		mtrx.lai3g[1:180,] <- mtrx.nhem.lai3g[1:180,,param]
		rm(mtrx.nhem.laire,mtrx.shem.laire,mtrx.nhem.lai3g,mtrx.shem.lai3g)
		
		table.laire[counter,"year"] <- year
		table.lai3g[counter,"year"] <- year
		 
		for(prod in c("TEMP_FAC","MOIST_FAC","LIGHT_FAC")) { #go through all controls
			# ??open.ncdf
			# load climatic controls for extraction
			filename.cc.cur <- paste(rootdir,"LAIre/raw_data/Global-0.5x0.5.analysis.",year,".nc",sep="")
			filename.cc.prev <- paste(rootdir,"LAIre/raw_data/Global-0.5x0.5.analysis.",year-1,".nc",sep="") #1981 climatic controls are available in dataset
			filename.cc.next <- paste(rootdir,"LAIre/raw_data/Global-0.5x0.5.analysis.",year+1,".nc",sep="") #2012 climatic controls are available in dataset

			mtrx.cc.cur <- ncdf_to_mtrx(filename.cc.cur, prod)
			mtrx.cc.prev <- ncdf_to_mtrx(filename.cc.prev, prod)
			mtrx.cc.next <- ncdf_to_mtrx(filename.cc.next, prod)
			
			#lines
			for(i in 1:360) {
				#if((sum(mtrx.laire[i,]) == 0) && (sum(mtrx.lai3g[i,]) == 0)) { 
				#	next()
				#}
				for(j in 1:720) { 	#go through each pixel,
					if(!is.na(mtrx.laire[i,j])) { # extract based on LAIre LSP dta
						mtrx.out.laire[i,j] = extract_mean_cc(mtrx.laire[i,j],mtrx.cc.cur[i,j,],mtrx.cc.prev[i,j,],mtrx.cc.next[i,j,]) 
					}
					if(!is.na(mtrx.lai3g[i,j])) { # extract based on LAI3g LSP data
						mtrx.out.lai3g[i,j] = extract_mean_cc(mtrx.lai3g[i,j],mtrx.cc.cur[i,j,],mtrx.cc.prev[i,j,],mtrx.cc.next[i,j,]) 
					}
				}	
			}
			
			# statistics for extracted CCs
			#table.laire[counter,prod] <- cor(as.vector(mtrx.out.laire),as.vector(mtrx.laire),method="spearman",use="pairwise.complete.obs")
			#table.lai3g[counter,prod] <- cor(as.vector(mtrx.out.lai3g),as.vector(mtrx.lai3g),method="spearman",use="pairwise.complete.obs")
			
			#write maps
			write.ENVI(mtrx.out.lai3g, paste(rootdir,"3_cc-LAI/LAI3g/monthly_",prod,"_at_",param.name[param],"_",year, sep=""), interleave = "bsq" ) 
			write.ENVI(mtrx.out.laire, paste(rootdir,"3_cc-LAI/LAIre/monthly_",prod,"_at_",param.name[param],"_",year, sep=""), interleave = "bsq" ) 

			rm(mtrx.cc.cur, mtrx.cc.prev, mtrx.cc.next)
		}
		rm(mtrx.laire,mtrx.lai3g)
		print(year)
		counter = counter + 1
	}
}

# write statistics
#write.csv(table.laire, file = "~/Documents/Uni/Masterarbeit/3_cc-LAI/LAIre_corr_yearly.csv")
#write.csv(table.lai3g, file = "~/Documents/Uni/Masterarbeit/3_cc-LAI/LAI3g_corr_yearly.csv")
