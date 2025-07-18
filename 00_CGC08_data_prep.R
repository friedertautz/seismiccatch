#!/usr/bin/env Rscript
library(eseis)
Sys.setenv(TZ="UTC")

# load directories to data, output...etc.
load("/mnt/projects/retrogress/programmes/r/seismic/directories.rda")

# read station info table, distance data, coupling factors, DEM and hillshade
st <- read.table(file = paste0(dirs$stations,"stationinfo_analysis.txt"),
                 sep = ",",
                 header = TRUE,
                 stringsAsFactors = FALSE)
st_i <- st[13,]

# landslide date
load(paste0(dirs$picks,"picks_ls1.rda"))
start = as.POSIXct(picks$start[6], tz = "UTC") - 10
duration = picks$duration[6] + 30

# read in the CGC08 data
s8 <- read_mseed(
    file = paste0(dirs$seismic, "2023/236/CGC08.23.236.03.03.36.BHZ")
)
s8$meta$starttime <- as.POSIXct(s8$meta$starttime, tz = "UTC")

# clip to landslide occurrence
s8_clip <- signal_clip(
    data = s8,
    limits = c(start, start + duration)
)

# deconvolve instrument response
s8_clip <- signal_deconvolve(
    data = s8_clip
)

# save data as .rda
save(s8_clip, file = paste0(dirs$data, "/CGC08_ls1.rda"))
