library(readxl)
library(momentuHMM)

rawData <- read_xlsx("C:\\Users\\beani\\Documents\\Honours\\Research Task\\Cheetah Data\\Cheetah Data.xlsx")

# Select and rename relevant columns
rawData <- rawData[c(4:1182), c(12, 3, 4, 5, 7)]
colnames(rawData) <- c("ID", "time", "lon", "lat", "temp")

# convert times from factors to POSIX
rawData$time <- as.POSIXct(rawData$time, tz = "GMT")

# project to UTM coordinates using package rgdal
library(rgdal)

llcoord <- SpatialPoints(rawData[, 3:4], 
                         proj4string = CRS("+proj=longlat +datum=WGS84"))
utmcoord <- spTransform(llcoord, CRS("+proj=utm +zone=30 ellps=WGS84"))

# add UTM locations to dataframe
rawData$x <- attr(utmcoord,"coords")[,1]
rawData$y <- attr(utmcoord,"coords")[,2]

# fit crawl model
crwOut <- crawlWrap(obsData = rawData, timeStep = "6 hours",
                    theta = c(6.096, -0.502), fixPar = c(NA,NA))  # theta found using crwMLE

library(conicfit)
plot(crwOut,ask=FALSE)

cheetahData <- prepData(data = crwOut, covNames = "temp")

# Fitting regression

library(circular)
reg1 <- lm.circular(type = "c-l", y = cheetahData$angle, x = cheetahData$step, init = 0.0)

mult.x <- matrix(data = c(cheetahData$step, cheetahData$temp), ncol = 2, nrow = nrow(cheetahData), byrow = FALSE)

reg2 <- lm.circular(type = "c-l", y = cheetahData$angle, x = mult.x, init = c(0.0, 0.0))

# GPS coords on map

library(mapview)
library(sf)

cheetah_no_na <- na.omit(cheetahData)

GPS_coords <- st_as_sf(cheetah_no_na, coords = c("lon", "lat"), crs = 4326)

mapview(GPS_coords, map.types = "Esri.WorldImagery")

mapview(GPS_coords, map.types = "OpenTopoMap")

mapview(GPS_coords, map.types = "Esri.NatGeoWorldMap")

mapview(cheetahData, xcol = "lon", ycol = "lat", crs = 4326, grid = FALSE)

# Plots of data


sl_avg <- mean(cheetah_no_na$step)

hist(cheetahData$step, xlab = "Step length", main = "")

library(CircStats)

angle_avg <- circ.mean(cheetah_no_na$angle)

circular::rose.diag(cheetah_no_na$angle, axes = TRUE, bins = 12, radii.scale = "sqrt", prop = 2, shrink = 0.9, tol = 0.17)
                    #ticks = TRUE, prop = 2, tol = 0.1)


