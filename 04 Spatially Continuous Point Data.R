### Interpolation in R

# This is cleaner than using library, require and install.packages
if (!require("pacman")) 
  install.packages("pacman")

p_load(rgdal, tmap)

# Load precipitation data
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds"))
P <- readRDS(z)

# Load Texas boundary map
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/texas.rds"))
W <- readRDS(z)

# Replace point boundary extent with that of Texas
P@bbox <- W@bbox

tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="Precip_in", palette = "RdBu", 
          title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

