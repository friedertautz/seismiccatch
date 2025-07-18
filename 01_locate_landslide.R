#!/usr/bin/env Rscript
## location of seismic sources
## prepare script
library("eseis")
library("terra")
Sys.setenv(TZ = "UTC")
# load directories to data, output...etc.
load("/mnt/projects/retrogress/programmes/r/seismic/tautz/thesis/directories.rda")

## read station info table, distance data, coupling factors, DEM and hillshade
st <- read.table(file = paste0(dirs$stations,"stationinfo_analysis.txt"),
                sep = ",",
                header = TRUE,
                stringsAsFactors = FALSE)
# just stations: 3,6,7
# 8 too erroneous, needs to be added manually
st_i <- st[c(3,6,12,13),]

## load DEM
dem <- rast(x = paste0(dirs$geo, "dem.tif"))
# hillshade
hs <- rast(x = paste0(dirs$geo, "hs_larg.tif"))
# outline watershed
otln <- vect(paste0(dirs$geo, "outline_CG_watershed.shp"))

## DAS NUR EINMALIG UND EXTRA -------------------------------------------------
## calculate and save distance data sets
# distance matrix has travel time for each pixel to each station
dist_name <- "D_3678_max.rda" # just CGC03,06,08
if ( file.exists(paste0(dirs$dist, dist_name))) {
    load(paste0(dirs$dist, dist_name))
} else {
    D <- spatial_distance(stations = st_i[,3:4], dem = dem, verbose = TRUE,
                        aoi = ext(hs) - 0.1) 
    save(D, file = paste0(dirs$dist, dist_name)) # takes some 10 min
}
## END EXTRA -----------------------------------------------------------------

# landslide date
load(paste0(dirs$data,"picks_ls1.rda"))
ls_date = as.POSIXct(picks$start[6], tz = "UTC")
# same length as CGC08
# is corrected manually
start = ls_date - 10
duration = picks$duration[6] + 30

## frequency range for filtering
f_filter <- c(5, 15)

## read test event
# just for station 3 and 6, 7; 8 added manually
st_i <- st[c(3, 6, 12),]
s <- read_data(start = start,
               duration = duration,
               station = st_i$ID,
               dir = dirs$seismic)
st_i <- st[c(3,6,12,13),]
# aggregate data to same sampling rate
# 08: 0.01
# 3,6,7: 0.005, agg 2 values to one: n = 2
s$CGC03 <- signal_aggregate(data = s$CGC03, n = 2)
s$CGC06 <- signal_aggregate(data = s$CGC06, n = 2)
s$CGC07 <- signal_aggregate(data = s$CGC07, n = 2)
load(paste0(dirs$data, "CGC08_ls1.rda"))
s$CGC08 <- s8_clip


## filter to target frequency range and calculate envelope
s_f <- signal_filter(data = s, f = f_filter)
e <- signal_envelope(data = s_f) # Hilbert envelope, makes everything pos*coefficient

# still not working properly
# I run spatial_migrate manually
l_mig <- spatial_migrate(data = e, # looks for the pixel where the time matches best
                         d_stations = D$matrix, # dist to travel time, compare time difference (from crosscorr.),
                                                # when match, most probable point of source
                         d_map = D$maps,
                         v = 800,
                         verbose = TRUE) # try and error

save(s,picks,st,D,file = paste0(dirs$data, "locating.rda"))

l_mig_cut <- spatial_clip(data = l_mig, quantile = 0.9) # clip r² plot to values of quantile
l_mig_xy <- spatial_pmax(data = l_mig_cut) # most probable point of source
l_mig_xy <- xyFromCell(l_mig_cut,where.max(l_mig_cut)[2]) # most probable point

dev.off()
jpeg(paste0(dirs$figures, "l_mig.jpg"))
# cut dem to extrent of hs
dem_croped <- crop(dem,ext(hs))
otln <- project(otln, crs(dem_croped)) # make sure otln is in same CRS as dem
p_ls <- matrix(data = c(694466.82,5284210.48),ncol=2)
plot(dem, col = grey.colors(200), legend = FALSE,mar = c(2,2,2,4))
plot(otln, add = TRUE, col = "black", lwd = 2)
plot(l_mig_cut, add = TRUE)
points(p_ls[1], p_ls[2], col = "green", pch = 19, cex = 1.5)
points(l_mig_xy[1], l_mig_xy[2], col = "red", pch = 19, cex = 1.5)
points(st_i$X, st_i$Y, col = "blue", pch = 19, cex = 1.5)
text(st_i$x,                                # Add labels
     st_i$y,
     # just ID numbers
     labels = gsub("CGC", "", st_i$ID),
     pos = 2,
     col = "blue",
     cex = 0.8)
dev.off()
