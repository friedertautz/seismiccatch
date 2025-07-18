#!/usr/bin/env Rscript
#' @title Precipitation and El Volcan
#' @description This script extracts precipitation data form the
#' Vaisala station at El Volcan and plots it.
#' @author Tautz, Frieder
#' @date 2025-05-26
#' 

# Load necessary libraries
library(ggplot2)
library(lubridate)
library(dplyr)

# Set timezone to UTC
Sys.setenv(TZ = "UTC")

# define directories
load("/mnt/projects/retrogress/programmes/r/seismic/tautz/thesis/directories.rda")

# define start, end time
ls_date <- as.POSIXct("2023-08-24 03:15:54", tz = "UTC")
start <- round_date(ls_date - 60 * 60 * 24 * 7, unit = "hour") # 7 days before
end <- round_date(ls_date + 60 * 60 * 24, unit ="hour") # 1 days after

# get files in directory
files <- list.files("/mnt/projects/retrogress/data/1-raw-NO_CHANGE/weather_station/elvolcan/23Jan-24Feb/vaisala/UPLD/",
                     pattern = "log$", full.names = TRUE)

# loop through files and extract precipitation data
precip_data <- data.frame(time = as.POSIXct(character()),
                            accum_mm = numeric(),
                            duration_s = numeric(),
                            intensity_mmh = numeric())

for (file in files) {
    # if file date is not in range, skip it
    # file date format at beginning: "2023-01-01_00-00-00"
    file_date_str <- sub(".*UPLD//(\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}).*", "\\1", file)
    file_date <- as.POSIXct(file_date_str, format = "%Y-%m-%d_%H-%M-%S", tz = "UTC")
    # check if file date is within the start and end range, end date longer to include full day
    if (file_date < start || file_date > end + 3600 * 24) {
        next  # skip this file if it is not in the date range
    }
    for (line in readLines(file)) {
        # check if line starts with time stamp of format: YY-MM-DD_HH-MM-SS followed by a whitespace
        if (grepl("^23-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2} ", line)) {
            # split line by whitespace
            parts <- strsplit(line, "\\s+")[[1]]
            # extract time string and precipitation value
            time_str <- parts[1]
            # convert time string to POSIXct
            time <- as.POSIXct(time_str, format = "%y-%m-%d_%H-%M-%S", tz = "UTC")
            # split string by comma
            val <- strsplit(parts[2], ",")[[1]]

            # check 2. part of line starts with 0R3
            if (!grepl("^0R3", val[1])) {
                next  # skip this line if it does not start with 0R3
            }
            # precip values after string Rc= for rain_accumulation
            # Rd= for rain_duration
            # Ri= for rain_intensity
            
            precip_accumulation <- as.numeric(sub(".*=([0-9.]+).*", "\\1", val[grep("^Rc=", val)]))
            precip_duration <- as.numeric(sub(".*=([0-9.]+).*", "\\1", val[grep("^Rd=", val)]))
            precip_intensity <- as.numeric(sub(".*=([0-9.]+).*", "\\1", val[grep("^Ri=", val)]))
            # append to data frame
            precip_data <- rbind(precip_data, data.frame(time = time,
                                                        accum_mm = precip_accumulation,
                                                        duration_s = precip_duration,
                                                        intensity_mmh = precip_intensity))

            print(paste(
                "time:", time,
                "precip_accumulation:", precip_accumulation,
                "precip_duration:", precip_duration,
                "precip_intensity:", precip_intensity,
                sep = ", "
            ))
        }
    }
}

# sum precipitation for every hour
hourly_summary <- precip_data %>%
  mutate(hour = floor_date(time, "hour")) %>%
  group_by(hour) %>%
  summarise(hourly_precip = sum(accum_mm, na.rm = TRUE),
            duration = sum(duration_s, na.rm = TRUE))

# remove rows where time is not within the start and end range
hourly_summary <- hourly_summary %>%
  filter(hour >= start & hour <= end)
# create a new data frame with time and accumulated precipitation
precip_data_hour <- data.frame(time = hourly_summary$hour,
                          accum_mm = hourly_summary$hourly_precip,
                          duration_s = hourly_summary$duration,
                          intensity_mmh = hourly_summary$hourly_precip / (hourly_summary$duration / 3600)) # convert duration to hours
# convert time to POSIXct
precip_data_hour$time <- as.POSIXct(precip_data_hour$time, tz = "UTC")
# remove rows before start and after end
precip_data_hour <- precip_data_hour %>%
  filter(time >= start & time <= end)

# plot intensity
jpeg(paste0(dirs$figures, "elvolcan_precip_intensity.jpg"), width = 800, height = 600)
precip_data_hour %>%
  filter(time >= "2023-08-23 00:00:00" & time <= "2023-08-24 06:00:00") %>%
  ggplot() + aes(x = time, y = intensity_mmh) +
  geom_line() + 
  labs(x = "Time", y = "Intensity (mm/h)",
       title = "Precipitation Intensityat El Volcan") +
  theme_minimal()
dev.off()

# plots duration
jpeg(paste0(dirs$figures, "elvolcan_precip_duration.jpg"), width = 800, height = 600)
precip_data_hour %>%
  filter(time >= "2023-08-23 12:00:00" & time <= "2023-08-24 06:00:00") %>%
  ggplot() + aes(x = time, y = duration_s / 3600) + # convert duration to hours
  geom_line() + 
  labs(x = "Time", y = "Duration (hours)",
       title = "Precipitation Duration at El Volcan") +
  theme_minimal()
dev.off()

# plots accumulated precipitation
jpeg(paste0(dirs$figures, "elvolcan_precip_accum.jpg"), width = 800, height = 600)
precip_data_hour %>%
  filter(time >= "2023-08-23 12:00:00" & time <= "2023-08-24 06:00:00") %>%
  ggplot() + aes(x = time, y = accum_mm) +
  geom_line() + 
  labs(x = "Time", y = "Accumulated Precipitation (mm)",
       title = "Accumulated Precipitation at El Volcan") +
  theme_minimal()
dev.off()

# last precip event before landslide
# starting at 2023-08-24 00:00:00
# get duration and accumulation
last_precip_event <- precip_data %>%
  filter(time >= "2023-08-23 12:00:00" & time <= "2023-08-23 15:00:00") %>%
  summarise(
    duration_s = sum(duration_s, na.rm = TRUE),
    accum_mm = sum(accum_mm, na.rm = TRUE)
  )

# save precip data to .rda file
save(precip_data, precip_data_hour, file = paste0(dirs$data, "/precip_elvolcan.rda"))
#load(paste0(dirs$data, "/precip_elvolcan.rda"))
