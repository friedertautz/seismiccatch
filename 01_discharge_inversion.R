## load packages
library(eseis)
library(caTools)
library(fields)
Sys.setenv(TZ="UTC")

# load station info
st <- read.table(
  file = "/mnt/projects/retrogress/programmes/r/seismic/stations/stationinfo_analysis.txt",
  sep = ",",
  header = TRUE,
  stringsAsFactors = FALSE
)
# general parameter setting
par_model <- list(
  d_s = 0.1,     # bedload grain diameter D_50 [m]
  s_s = 0.5,     # grain diameter standard deviation [log m]
  r_s = 2650,     # sediment density [kg/m³]
  w_w = 20,        # average channel width [m]
  a_w = 0.34,   # channel slope [rad], high variation in river
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

# station of interest
st_i <- st[3,]

## load PSD of interest
dir_seismic <- "/mnt/hydro_cifs/retrogress/seismic/seismic/mseed/"
ls_date = as.POSIXct("2023-08-24 03:15:54 UTC")
t_before = 3600 * 24 * 7
t_after = 3600 * 24 * 1

## load PSD from .rda
## file if it exists, otherwise calculate PSD
## from raw data
## and save it to .rda
psd_fmi_inv = "/mnt/projects/retrogress/data/2-processed/seismic/rda-data/psd_fmi_inv.rda"

if( !file.exists(psd_fmi_inv) ){
     psd <- aux_psdsummary(start = ls_date - t_before,
                         stop = ls_date + t_after,
                         ID =  st_i$ID,
                         dir = dir_seismic,
                         window = 360,
                         n = 100,
                         sensor = st_i$sensor_type,
                         logger = st_i$logger_type,
                         gain = st_i$gain,
                         verbose = TRUE,
                         cpu = 0.8)
     # save PSD to .rda
     save(psd, file = psd_fmi_inv)
} else {
     load(psd_fmi_inv)
}

# interpolate PSD to frequency range of interest
p_emp <- psd$PSD$S
t_emp <- psd$PSD$t
f_emp <- psd$PSD$f

# truncate spectrogram by frequency
i_cut <- f_emp >= par_model$f[1] & f_emp <= par_model$f[2]
p_emp <- t(p_emp[i_cut,])
f_emp <- f_emp[i_cut]

# aggregate spectrogram by frequency
f_agg <- seq(from = par_model$f[1], to = par_model$f[2], length.out = par_model$res)
t_agg <- t_emp
p_agg <- apply(X = p_emp, MARGIN = 1,
                 FUN = function(x) {
                   approx(x = f_emp,
                          y = x,
                          rule = 2,
                          xout = f_agg)$y}
)

## define reference parameters with bedlaod 
fmi_pars_1 <- fmi_parameters(
     n = 5000, 
     d_s = par_model$d_s,
     s_s = par_model$s_s,
     r_s = par_model$r_s,
     q_s = c(0, 15) / par_model$r_s, # unit sediment flux [m²/s]
     h_w = c(0.2, 1), # min, max water depth [m]
     w_w = par_model$w_w,
     a_w = par_model$a_w,
     f_min = par_model$f[1],
     f_max = par_model$f[2],
     r_0 = par_model$r_0,
     f_0 = par_model$f_0,
     q_0 = par_model$q_0,
     v_0 = par_model$v_0,
     p_0 = par_model$p_0,
     e_0 = par_model$e_0,
     n_0_a = par_model$n_0[1],
     n_0_b = par_model$n_0[2],
     res = par_model$res
)

## define reference parameters without bedlaod
fmi_pars_2 <- fmi_parameters(
     n = 5000,
     d_s = par_model$d_s,
    s_s = par_model$s_s,
    r_s = par_model$r_s,
    q_s = 0,
    h_w = c(0.2, 1),
    w_w = par_model$w_w,
    a_w = par_model$a_w,
    f_min = par_model$f[1],
    f_max = par_model$f[2],
    r_0 = par_model$r_0,
    f_0 = par_model$f_0,
    q_0 = par_model$q_0,
    v_0 = par_model$v_0,
    p_0 = par_model$p_0,
    e_0 = par_model$e_0,
    n_0_a = par_model$n_0[1],
    n_0_b = par_model$n_0[2],
    res = par_model$res
)

## combine parameters
fmi_pars <- c(fmi_pars_1, fmi_pars_2)

## calculate reference spectra
fmi_spec <- fmi_spectra(parameters = fmi_pars, n_cores = 40)

## invert data set
fmi_inv <- eseis::fmi_inversion(reference = fmi_spec,
                          data = p_agg,
                          n_cores = 40)

# save inversion results as .rda
save(fmi_inv, t_agg, file = "/mnt/projects/retrogress/data/2-processed/seismic/rda-data/fmi_inv.rda")

jpeg(filename = "/mnt/projects/retrogress/data/3-figures/seismic/inversion.jpg",
     width = 800, height = 800, res = 150)

##prepare plot area
par(mfcol = c(2, 2))

## plot prepared spectrogram
fields::image.plot(x = t_agg, y = f_agg, z = t(p_agg), main = " PSD")

## plot error matrix

fields::image.plot(x = t_agg, y = f_agg, z = fmi_inv$rmse,
                   main = "RMSE error matrix", xlab = "Time (UTC)",
                   ylab = "Frequency (Hz)")

## plot water level
plot(x = t_agg, y = fmi_inv$parameters$h_w, type = "l",
     main = "Water level inversion", xlab = "Time (UTC)", 
     ylab = "Water level (m)")
lines(x = t_agg, y = caTools::runquantile(fmi_inv$parameters$h_w, 
                                          k = 5, 0.5), lwd = 2)

## plot bedload flux
plot(x = t_agg, y = fmi_inv$parameters$q_s * par_model$r_s, type = "l",
     main = "Bedload flux inversion", xlab = "Time (UTC)", 
     ylab = "Bedload flux (kg/sm)")
lines(x = t_agg, y = caTools::runquantile(fmi_inv$parameters$q_s * par_model$r_s, 
                                          k = 5, 0.5), lwd = 2)
dev.off()
