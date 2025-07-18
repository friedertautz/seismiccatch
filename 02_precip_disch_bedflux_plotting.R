# script to plot results from landslide pick,
# precipitation and discharge

# load libraries
library(eseis)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
Sys.setenv(TZ="UTC")

# general parameter setting
par_model <- list(
  d_s = 0.1,     # bedload grain diameter D_50 [m]
  s_s = 0.5,     # grain diameter standard deviation [log m]
  r_s = 2650,     # sediment density [kg/m³]
  w_w = 20,        # average channel width [m]
  a_w = 0.34,   # channel slope [rad]
  f = c(5,25),   # frequency range [Hz]
  r_0 = 90,      # distance river center to station [m]
  f_0 = 1,        # reference frequency 
  q_0 = 23,    # ground quality factor
  v_0 = 500,      # rayleigh wave velocity
  p_0 = 0.62,     # variation coefficient v_0
  e_0 = 0.07,     # Q-factor increase with frequency
  n_0 = c(0.6, 0.8), # greens fct. amplitude coeff.
  res = 100       # output resolution
)

# landslide date
ls_date <- as.POSIXct("2023-08-24 03:15:54", tz = "UTC")
# directories
load("/mnt/projects/retrogress/programmes/r/seismic/tautz/thesis/directories.rda")

# precipitation pick + discharge + bedload
# load data
load("/mnt/projects/retrogress/data/2-processed/seismic/rda-data/fmi_inv.rda")
load("/mnt/projects/retrogress/data/2-processed/seismic/rda-data/rain_50-90Hz_CGC03.rda")
# plot precipitation of el volcan for comparison
load("/mnt/projects/retrogress/data/2-processed/seismic/rda-data/precip_elvolcan.rda")

jpeg(filename = "/mnt/projects/retrogress/data/3-figures/seismic/disch_bedflux_precip.jpg",
     width = 20, height = 10, units = "cm", res = 300)
p_list <- list()

# create data frame for plotting
df_fmi <- data.frame(
     time = t_agg,
     h_w = round(fmi_inv$parameters$h_w * 100, 1),
     q_s = fmi_inv$parameters$q_s * par_model$r_s,
     h_w_flat = caTools::runquantile(fmi_inv$parameters$h_w, k = 5, 0.5),
     q_s_flat = caTools::runquantile(fmi_inv$parameters$q_s * par_model$r_s, k = 5, 0.5)
)

df_rain <- data.frame(
     time = rain_picks_hourly$start,
     count = rain_picks_hourly$count,
     elvolcan = precip_data_hour$accum
)

## plot water level
p_list[[1]] <- ggplot(df_fmi, aes(x = time)) +
     geom_line(aes(y = h_w), color = "blue") +
     geom_vline(xintercept = ls_date, linetype = "dashed", color = "red") +
     labs(y = "Water level [cm]",
          title = "FMI f(5,25)") +
     scale_y_continuous(breaks = seq(20,30,5)) +
     theme_minimal()+
     theme(
          axis.title.x = element_blank(),
          plot.background = element_blank(),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          title = element_text(size = 6)
     )

# plot precipitation
p_list[[2]] <- ggplot(df_rain, aes(x = time)) +
  geom_line(aes( y = count), color = "grey") +
  geom_vline(xintercept = ls_date, linetype = "dashed", color = "red") +
  labs(y = "Rain picks [count]", title = "STA/LTA f(50,90)") +
  theme_minimal()+
  theme(
     axis.title.x = element_blank(),
     plot.background = element_blank(),
     axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          axis.text.y = element_text(margin = margin(r = 0, l = 10)),
          title = element_text(size = 6)
  )

## plot bedload flux
p_list[[3]] <- ggplot(data = df_fmi, aes(x = time)) +
     geom_line(aes(y = q_s), color = "brown") +
     geom_vline(xintercept = ls_date, linetype = "dashed", color = "red") +
     labs(y = "Bedload flux [kg/sm]",
          x = "Time [UTC]") +
     theme_minimal()+
     theme(
          plot.background = element_blank(),
          axis.text = element_text(size = 6),
          title = element_text(size = 6)
)


# plot double mass curve
# cumulative sum of precipitation
df_rain$cum_elvolcan <- rescale(cumsum(precip_data_hour$accum), to = c(0, 1))
# cumulative sum of rain drop impacts
df_rain$cum_count <- rescale(cumsum(rain_picks_hourly$count), to = c(0, 1))
coeff <- 6

p_list[[4]] <- ggplot(df_rain, aes(x = time)) +
     geom_line(aes(y = elvolcan), color = "royalblue1") +
     geom_vline(xintercept = ls_date, linetype = "dashed", color = "red") +
     geom_line(aes(y = cum_elvolcan*coeff, color = "El Volcan"), color = "royalblue1") +
     geom_line(aes(y = cum_count*coeff, color = "Rain pick CGC03"), color = "grey") + labs(x = "Time [UTC]", y = "Precipitation [mm]", title = "El Volcan",
          color = "Normalised cumulative sum") +
     annotate("text", x = as.POSIXct("2023-08-19 16:00:00"), y = 2.7,
               label = "Normalised cumulative sum", angle = 20, size = 2) +
     theme_minimal() +
     theme(
          plot.background = element_blank(),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          title = element_text(size = 6)
     )

# prepare plots
# get max width of all plots
for (i in 1:length(p_list)) {
  p_list[[i]] <- ggplot_gtable(ggplot_build(p_list[[i]]))
}
# get max width
for (i in 1:(length(p_list)-1)) {
  max_width <- unit.pmax(p_list[[i]]$widths, p_list[[i+1]]$widths)
}
# assign max width to all plots
for (i in 1:length(p_list)) {
  p_list[[i]]$widths <- max_width
}
grid.arrange(grobs = p_list, top = "CGC03", ncol = 2)
dev.off()
