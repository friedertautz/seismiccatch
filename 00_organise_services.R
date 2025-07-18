## load necessary libraries
library(eseis)

## set working directory
setwd("/mnt/hydro_cifs/retrogress/seismic/")

## read station info file for services
st <- read.table(file = "/mnt/projects/retrogress/programmes/r/seismic/stations/stationinfo_service_server.txt",
                 sep = ",",
                 header = TRUE,
                 stringsAsFactors = FALSE)

# loop through services
for (i in 1:4) {
    print(paste0("Service: ", i))
    ## isolate service of interest
    st_i <- st[st$service == i, ]

    ## convert all cube files to mseed files
    aux_organisecubefiles(station = st_i,
                        input = paste0("seismic/cube/service_", i, "/"),
                        output = "seismic/mseed",
                        gipptools = "/mnt/projects/retrogress/programmes/r/seismic/gipptools-2024.170/",
                        format = "mseed",
                        cpu = 0.8,
                        verbose = TRUE)
}
