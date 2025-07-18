#!/usr/bin/Rscript
# script to plot seismic data around landslide occurrence
library(eseis)
library(ggplot2)
library(gridExtra)
Sys.setenv(TZ="UTC")

# directories
load("/mnt/projects/retrogress/programmes/r/seismic/tautz/thesis/directories.rda")

# landslide date
load(paste0(dirs$data,"picks_ls1.rda"))
start = as.POSIXct(picks$start[6]) - 10
duration = picks$duration[6] + 30

# read station info
st <- read.table(file = paste0(dirs$stations,"stationinfo_analysis.txt"),
                 sep = ",",
                 header = TRUE,
                 stringsAsFactors = FALSE)
# stations 3,6,7,8
st_i <- st[c(3,6,12,13),]

p <- list()

# loop through stations
# plot in 1 times 4 grid
for (i in 1:(nrow(st_i) - 1)){
    # read data
    s <- read_data(
        start = start,
        duration = duration,
        station = st_i$ID[i],
        format = "mseed",
        dir = dirs$seismic
    )
    # deconvolve instrument response
    s <- signal_deconvolve(
        data = s,
        sensor = st_i$sensor_type[i],
        logger = st_i$logger_type[i],
        gain = st_i$gain[i]
    )

    # save to list
    p[[i]] <- s
}
# manual loading for station 08
# wrong transfer from cube to mseed
load(paste0(dirs$data,"/CGC08_ls1.rda"))

p[[4]] <- s8_clip

# plot
# create time vector
time367 <- seq(from = start, to = start + duration, by = 0.005)
time367 <- time367[1:(length(time367) - 1)]
time8 <- seq(from = start, to = start + duration, by = 0.01)
time8 <- time8[1:(length(time8) - 1)]
# as dataframe
df <- data.frame(time = time367)
# add data
for (i in 1:3){
    s_f <- signal_filter(data = p[[i]], f = c(5,15))
    df <- cbind(df, s_f$signal)
}
s8_f <- signal_filter(p[[4]], f = c(5,15))
df8 <- data.frame(time = time8, CGC08 = s8_f$signal)
# rename columns
colnames(df) <- c("time", "CGC03", "CGC06", "CGC07")
plots <- list()
# prepare plots
plots[[1]] <- ggplot(data = df, aes(x = time)) +
        geom_line(aes(y = CGC03)) +
        geom_vline(xintercept = picks$start[6]) +
        scale_x_datetime(date_labels = "%H:%M:%S") +
        labs(title = st_i$ID[1]) +
        theme_minimal()+
        theme(
            axis.title = element_blank(),
            plot.background = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())

plots[[2]] <- ggplot(data = df, aes(x = time)) +
    geom_line(aes(y = CGC06)) +
    geom_vline(xintercept = picks$start[6]) +
    scale_x_datetime(date_labels = "%H:%M:%S") +
    labs(title = st_i$ID[2]) +
    theme_minimal()+
    theme(
            axis.title = element_blank(),
            plot.background = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
plots[[3]] <- ggplot(data = df, aes(x = time)) +
    geom_line(aes(y = CGC07)) +
    geom_vline(xintercept = picks$start[6]) +
    scale_x_datetime(date_labels = "%H:%M:%S") +
    labs(title = st_i$ID[3]) +
    theme_minimal()+
    theme(
            axis.title = element_blank(),
            plot.background = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())

plots[[4]] <- ggplot(data = df8, aes(x = time)) +
    geom_line(aes(y = CGC08)) +
    geom_vline(xintercept = picks$start[6]) +
    scale_x_datetime(date_labels = "%H:%M:%S") +
    labs(title = st_i$ID[4]) +
    theme_minimal()+
    theme(axis.title = element_blank(),
          plot.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank())
# plot
# prepare grid for plotting
jpeg(filename = paste0(dirs$figures,"signal_ls/landslide_5-15_",
                        format(start,"%m_%d"),"_",".jpg"),
                        width = 800, height = 400, quality = 100)
# plot underneath each other
grid.arrange(grobs = plots, ncol = 2,
            top = paste0("Seismic data around landslide occurrence on ",
                          format(start, "%Y-%m-%d %H:%M:%S")),
            bottom = "Time (UTC)",
            left = "Amplitude (m/s²)"
)
dev.off()

# spectogram of station 3
i = 1
# start and duration a bit earlier/longer
# better visibility of the signal
start = as.POSIXct(picks$start[6]) - 30
duration = picks$duration[6] + 60
s <- read_data(
    start = start,
    duration = duration,
    station = st_i$ID[i],
    format = "mseed",
    dir = dirs$seismic
)
s <- signal_deconvolve(
    data = s,
    sensor = st_i$sensor_type[i],
    logger = st_i$logger_type[i],
    gain = st_i$gain[i]
)

psd <- signal_spectrogram(
    data = s,
    dt = 0.05,
    window = 3
)

jpeg(filename = paste0(dirs$figures,"signal_ls/spectrogram_",
                        format(start,"%m_%d"),"_",st_i$ID[i],".jpg"),
                        width = 800, height = 400, quality = 100)
plot(psd, 
     main = paste0("Spectrogram of ", st_i$ID[i], " around landslide occurrence on ",
                   format(start + 30, "%Y-%m-%d %H:%M:%S")),
     xlab = "Time (UTC)")
dev.off()

# save as .rda
save(p, file = paste0(dirs$data, "/all_stations_ls1.rda"))
