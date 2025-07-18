#!/usr/bin/env Rscript

## packages
library(eseis)
library(ggplot2)
library(gridExtra)
library(caTools)
Sys.setenv(TZ = "UTC")

# directories
load("/mnt/projects/retrogress/programmes/r/seismic/directories.rda")

# read station info table
st <- read.table(file = paste0(dirs$stations,"stationinfo_analysis.txt"),
                sep = ",",
                header = TRUE)

# just of one station
# CGC03
st_i <- st[3,]

# date of landslide
ls_date <- as.POSIXct("2023-08-24 3:15:54 UTC")

# time window before landslide
# precipitation:one week before, 1 day after
w_before = 3600 *24 * 7
w_after = 3600 * 24
p <- c(50,90)
# discharge
q <-c(5,25)

# landslide: 1 day before, 1 day after
l <- c(5,15) # from PSD for station CGC03, gap in discharge

ls = list(p,q,l)
ls_str <- c("precipitation","discharge","landslide")

# read seismic data
s <- read_data(start = ls_date - w_before,
                duration = w_before + w_after,
                station = st_i$ID,
                format = "mseed",
                dir = dirs$seismic)

# deconvolution
s <- signal_deconvolve(
    data = s,
    sensor = st_i$sensor_type,
    logger = st_i$logger_type,
    gain = st_i$gain,
    verbose = TRUE
)
# save plots
p <- vector("list", length(ls))
# loop over each frequency band
for (i in 1:length(ls)) {
    # filter by frequency
    s_f <- signal_filter(data = s, f = ls[[i]], p = 0.01)

    # envelope
    s_f_env <- signal_envelope(data = s_f)

    # rolling window
    k <- caTools::runquantile(s_f_env$signal, k = 200*60, 0.5)
    # time vector
    time <- seq(from = ls_date - w_before,
                to = ls_date + w_after,
                length.out = nrow(k))
    df <- data.frame(time = time, k = k)
    # plot
    p[[i]] <- ggplot(df) +
        geom_line(aes(x = time, y = k), color = "grey") +
        geom_vline(xintercept = ls_date, linetype = "dashed", color = "red") +
        labs(title = paste0(ls_str[i], " frequency range: ", ls[i])) +
        theme_minimal() +
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.background = element_blank()
        )
}
jpeg(paste0(dirs$figures, "quick_CGC03", ".jpeg"))
grid.arrange(grobs = p, ncol = 1,
            bottom = "Time [UTC]",
            left = "Amplitude [m/s]")
dev.off()