## load necessary libraries
library(eseis)

## set working directory
setwd("/mnt/hydro_cifs/retrogress/seismic/")

# time zone UTZ
Sys.setenv(TZ = "UTC")

## read station info file for services
st <- read.table(file = "/mnt/projects/retrogress/programmes/r/seismic/stations/stationinfo_analysis.txt",
                 sep = ",",
                 header = TRUE,
                 stringsAsFactors = FALSE)

# get Julian Day, just as an example
format(x = as.POSIXct("2023-08-24"), "%j")

## FULL PERIOD PSD ------------------------------------------------------------
# check data availability: aux_checkfiles()
# landslide between 7.8 and 4.9.2023
# tageweise PSD von 7.8.2023 bis 4.9.2023
# change jpeg name with time stamp for day

# time window
start <- as.POSIXct("2023-08-04 00:00:00")
stop <- as.POSIXct("2023-09-07 00:00:00")

# window length
w <- 360

# directory for total PSD plots
dir_total_psd <- "/mnt/projects/retrogress/figures/seismic/psd/total_4.8.-7.9.23/"

for(i in 1:nrow(st)){
     # if(file.exists(paste0(dir_total_psd, st$ID[i], "_window", w, ".jpg"))){
     #      next
     # }else {
          try(
               p <- aux_psdsummary(start = start,
                                   stop = stop,
                                   ID =  st$ID[i],
                                   dir = "seismic/mseed",
                                   window = 360,
                                   n = 100,
                                   sensor = st$sensor_type[i],
                                   logger = st$logger_type[i],
                                   gain = st$gain[i],
                                   verbose = TRUE,
                                   cpu = 0.8)
          )
          jpeg(filename = paste0(dir_total_psd, st$ID[i], "_window",w, ".jpg"),
               width = 2000,
               height = 1000,
               res = 150)
          try(
               plot(p)
          )
          dev.off()
     #}
}

## DAILY PSD ------------------------------------------------------------------
t <- seq(from = as.POSIXct("2023-08-04", tz = "UTC"),
         to = as.POSIXct("2023-09-07", tz = "UTC"),
         by = 24 * 3600)
# directory for daily PSD plots
dir_daily_psd <- "/mnt/projects/retrogress/figures/seismic/plots/psd/daily/"

for(i in 1:nrow(st)){
     for(j in 1:(length(t) - 1)){
          # if(file.exists(paste0(dir_daily_psd, st$ID[i], "_window", w, "_", format(x = t[j], "%Y-%m-%d"), ".jpg"))){
          #      next
          # }else {
               try(
                    p <- aux_psdsummary(start = t[j],
                                        stop = t[j + 1],
                                        ID =  st$ID[i],
                                        dir = "seismic/mseed",
                                        window = w,
                                        n = 100,
                                        sensor = st$sensor_type[i],
                                        logger = st$logger_type[i],
                                        gain = st$gain[i],
                                        verbose = TRUE,
                                        cpu = 0.8)
               )
               jpeg(filename = paste0(dir_daily_psd, st$ID[i], "_window", w, "_", format(x = t[j], "%Y-%m-%d"), ".jpg"),
                    width = 2000,
                    height = 1000,
                    res = 150)
               try(
                    plot(p)
               )
               dev.off()
          #}
     }
}