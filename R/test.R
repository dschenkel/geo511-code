##
## Testfile1, random R stuff for dataprocessing testing 
## dschenkel, oct 2014
##
rm(list = ls())

library(ncdf)
library(raster)
library(fields)
LAIre = "~/Documents/Uni/Masterarbeit/LAIre/Global-0.5x0.5.analysis.1995.nc"
# ??open.ncdf
nc <- open.ncdf(LAIre)
nc
c <- get.var.ncdf(nc, "ens")

d.lat <- get.var.ncdf(nc, "lat")
d.lon <- get.var.ncdf(nc, "lon")
d.time <- get.var.ncdf(nc, "time")
d <- get.var.ncdf(nc, "PFT_PCT")
d.water <- d[,,35]
#d.water
max(d.water)
d.water[d.water <= 99] <- 1
d.water[d.water > 99] <- 0

d.waterrst = raster(t(d.water))

# specify extent and projection
extent(d.waterrst) <- extent(c(-180, 180, -90, 90))
projection(d.waterrst) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

filename <- "~/Documents/Uni/Masterarbeit/watermask/watermask"
writeRaster(d.waterrst, filename=filename, format="ENVI", overwrite=TRUE)

image.plot(d.lon, d.lat, d.water)

str(d[,,35])
dimnames(d) <- list(d.lon,d.lat,d.time)
str(d)
mean(d)
#max(d[,,"4"])

d[290,41,]
d.max <- apply(d,c(1,2),max)

#library(ggplot2)
#qplot(LAIreDat.lon, LAIreDat.lat, data=d, fill=d, geom= "raster")
#fields::image.plot 
#?image
#library(hyperSpec)
#LAI3g = "../LAIv3g/LAIv3g_8211_INT_BSQ"
