dirs <- list(
    seismic = "/mnt/hydro_cifs/retrogress/seismic/seismic/mseed/",
    figures = "/mnt/projects/retrogress/data/3-figures/seismic/",
    data = "/mnt/projects/retrogress/data/2-processed/seismic/rda-data/",
    geo = "/mnt/projects/retrogress/data/2-processed/seismic/geo/",
    dist = "/mnt/projects/retrogress/data/2-processed/seismic/dist/",
    stations = "/mnt/projects/retrogress/programmes/r/seismic/stations/",
    picks = "/mnt/projects/retrogress/data/2-processed/seismic/picks/"
)
save(dirs, file = "/mnt/projects/retrogress/programmes/r/seismic/tautz/thesis/directories.rda")
