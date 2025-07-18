#!/usr/bin/env Rscript
## set up script --------------------------------------------------------------
library("eseis")
Sys.setenv(TZ = "UTC")

# load directories
load("/mnt/projects/retrogress/programmes/r/seismic/tautz/thesis/directories.rda")

# read station info
st <- read.table(
  file = paste0(dirs$stations,"stationinfo_analysis.txt"),
  sep = ",",
  header = TRUE,
  stringsAsFactors = FALSE
  )
st_i <- st[c(3, 6),]

## PICK COMMONLY DETECTED EVENTS ----------------------------------------------
name_picks <- "/picks_ls1.rda"
if (!file.exists(paste0(dirs$data, name_picks))) {
  picks <- aux_picknetwork(
    start = "2023-08-04",
    stop = "2023-09-07",
    res = 3600, # time window to look at, always windows of size 3600
    buffer = c(300, 300), # windows need to overlap, not missing an event
    station = st_i$ID,
    dir = dirs$seismic,
    f = c(5, 15), # frequency spectrum, landslide 5-15 Hz
    sta = 0.5, # short term amplitude in s
    lta = 180, # long-term amplitude in s
    on = 4, # threshold when event is found, ration sta/lta
    off = 1, # threshold when even is over
    freeze = TRUE, # lta stops being calculated, when event found, so that it does not shoot up as well.
    dur_min = 10, # duration in s, min
    dur_max = 120, # max
    n_common = 2, # number of stations with signal
    t_common = 1, # time difference between events, distance between stations
    t_pause = 5, # event need to rest for at least
    verbose = TRUE
  )

  save(picks, file = paste0(dirs$data,"/picks_ls1.rda"))
}else{
  load(paste0(dirs$data,"/picks_ls1.rda"))
}
## OVERVIEW OF RAW EVENT PROPERTIES -------------------------------------------

picks_24h <- as.numeric(format(x = picks$start, "%H")) +
  as.numeric(format(x = picks$start, "%M")) / 60 +
  as.numeric(format(x = picks$start, "%S")) / 3600
picks_24h_wrap <- c(picks_24h - 24 * 3600, picks_24h, picks_24h + 24 * 3600)

# save overview of picks

jpeg(filename = paste0(dirs$figures, "picks/picks_cum.jpg"),
     width = 800, height = 800, quality = 100)

plot(x = picks$start,
     y = 1:nrow(picks),
     type = "b",
     main = "Events cumulative by time")

dev.off()

jpeg(filename = paste0(dirs$figures, "/picks/picks_duration.jpg"),
     width = 800, height = 800, quality = 100)

plot(density(x = picks$duration,
             bw = 0.5,
             from = 5,
             to = 180),
     log = "x",
     main = "Event duration")

dev.off()

jpeg(filename = paste0(dirs$figures, "/picks/picks_24h.jpg"),
     width = 800, height = 800, quality = 100)

plot(density(x = picks_24h_wrap,
             bw = 0.1,
             from = 0,
             to = 24),
     main = "Events by day time")

dev.off()

## PSD AND SEISMOGRAM ---------------------------------------------------------

# creating seismogramm for CGC03, 06, 07, 08
st_i <- st[c(3, 6, 12, 13), ]

# reduce picks by duration, longer than 50 s
thrshld = 50
picks_s <- picks[picks$duration > thrshld,]

# load all stations for pick 6
load(paste0(dirs$data, "/all_stations_ls1.rda"))

# loop through single picks
# create seismograms and save
signal_j <- vector(mode = "list", length = nrow(st_i))
psd_j <- vector(mode = "list", length = nrow(st_i))
pck <- picks_s[3,]

# start time 3 s before event
# end time 3 s after event
start <- pck$start - 3
end <- pck$start + pck$duration + 3

# if data saved as .rda file, load data
if (file.exists(paste0(dirs$data, "/ls1_seismo_psd.rda"))) {
  print("Loading existing seismograms and PSDs")
  load(paste0(dirs$data, "/ls1_seismo_psd.rda"))
} else {
  print("Processing event:")
  print(paste0("    Start: ", pck$start))
  print(paste0("    Duration: ", pck$duration, " s"))
  # create seismograms for each station
  for(j in 1:nrow(st_i)){
    print(paste0("    Station: ", st_i$ID[j]))
    # load data
    s <- signal_detrend(data = p[[j]])
    # calculate PSD
    # time taken from loaded .rda file, clipping for seismogram
    psd <- signal_spectrogram(
      data = s,
      Welch = TRUE, window = 5, overlap = 0.9, window_sub = 3, overlap_sub = 0.9,
      cpu = 0.8
    )
    # read data
    s <- signal_clip(
      data = s,
      limit = c(start, end)
    )
    # filter data
    # psd should be calculated before filtering
    s <- signal_filter(data = s, f = c(5, 15), p = 0.01)

    # save seismogram
    signal_j[[j]] <- s

    # save PSD
    psd_j[[j]] <- psd
  }
  # save all stations
  save(signal_j, psd_j, file = paste0(dirs$data, "/ls1_seismo_psd.rda"))
  print("    Saved seismograms and PSDs")
}
# create plot
jpeg(filename = paste0(dirs$figures, "/picks/5-15Hz_",thrshld,"s/",
    format(pck$start,"%m_%d_%H:%M"), ".jpg"),
    width = 20, height = 10, units = "cm", res = 300)

# create plot output
# prepare multi-panel plot
par(mfrow = c(4,2))
# loop through seismograms and PSDs
for(i in 1:(nrow(st_i) - 1)){
  t <- seq(from = signal_j[[i]]$meta$starttime,
           length.out = signal_j[[i]]$meta$n,
           by = signal_j[[i]]$meta$dt)
  s <- signal_j[[i]]$signal
  par(mar = c(0.5, 0, 1, 0), xaxt = "n", yaxt = "n") # margins for plot
  plot(t,s, axes = FALSE,
    main = NA,
    type = "l",
    col = "black"
  )
  # add title
  title(main = paste0("Seismogram ",st_i$ID[i]), cex.main = 0.8)
  # add vertical line for event start
  abline(v = pck$start, col = "red", lty = "dashed", lwd = 1)
  
  # plot PSDs
  par(mar = c(0.5, 3, 1, 0.5), xaxt = "n", yaxt = "s") # margins for plot
  plot_spectrogram(psd_j[[i]],
                  xlab = NA,
                  ylab = NA,
                  main = NA,
                  legend = FALSE)
  title(main = paste0("PSD ", st_i$ID[i]), cex.main = 0.8)
  title(ylab = "Frequency [Hz]", cex.lab = 0.8, line = 2)
}

# plot CGC08
par(mar = c(2, 0, 1, 0), xaxt = "s") # margins for plot
t <- seq(from = signal_j[[4]]$meta$starttime,
           length.out = signal_j[[4]]$meta$n,
           by = signal_j[[4]]$meta$dt)
s <- signal_j[[4]]$signal
plot(t,s, axes = FALSE,
     main = NA,
     xlab = NA,
     ylab = NA,
     type = "l",
     col = "black")
ticks_pos <- seq(as.POSIXct("2023-08-24 03:16:00"), by = "30 s", length.out = 3)
ticks_label <- format(ticks_pos, "%H:%M:%S")
axis(1, at = ticks_pos, labels = ticks_label, tcl = -0.3)
# add vertical line for event start
abline(v = pck$start, col = "red", lty = "dashed", lwd = 1)
title(main = paste0("Seismogram ", st_i$ID[4]), cex.main = 0.8)
# plot PSDs
par(mar = c(2, 3, 1, 0.5), xaxt = "s") # margins for plot
# plot spectrogram
plot_spectrogram(psd_j[[4]],
              format = "%H:%M:%S",
              main = NA,
              xlab = NA,
              ylab = NA,
              ylab = "Frequency [Hz]",
              legend = FALSE)
title(main = paste0("PSD ", st_i$ID[4]), cex.main = 0.8)
title(ylab = "Frequency [Hz]", cex.lab = 0.8, line = 2)
dev.off()
