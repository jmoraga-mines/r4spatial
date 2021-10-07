### Point pattern analysis in R
# Data used in the following exercises can be loaded into your current R session
# by running the following chunk of code.
load(url("https://github.com/mgimond/Spatial/raw/main/Data/ppa.RData"))

# This is cleaner than using library, require and install.packages
if (!require("pacman")) 
  install.packages("pacman")

# Load all libraries
p_load(sf, maptools, raster)

##### Don't run this code:
##### # Load an MA.shp polygon shapefile 
##### s  <- 
##### w  <- as.owin(s)
##### w.km <- rescale(w, 1000)
##### # Load a starbucks.shp point feature shapefile
##### s  <- st_read("starbucks.shp")  
##### starbucks  <- as.ppp(s)
##### marks(starbucks) <- NULL
##### starbucks <- rescale(starbucks, 1000)
##### Window(starbucks) <- starbucks 
##### # Load a pop_sqmile.img population density raster layer
##### img  <- raster("pop_sqmile.img")
##### pop  <- as.im(img)

p_load(spatstat)
marks(starbucks)  <- NULL
Window(starbucks) <- ma
plot(starbucks, main=NULL, cols=rgb(0,0,0,.2), pch=20)

hist(pop, main=NULL, las=1)

pop.lg <- log(pop)
hist(pop.lg, main=NULL, las=1)

Q <- quadratcount(starbucks, nx= 6, ny=3)

plot(starbucks, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)  # Add quadrat grid

# Compute the density for each quadrat
Q.d <- intensity(Q)
# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(starbucks, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

starbucks.km <- rescale(starbucks, 1000, "km")
ma.km <- rescale(ma, 1000, "km")
pop.km    <- rescale(pop, 1000, "km")
pop.lg.km <- rescale(pop.lg, 1000, "km")

# Compute the density for each quadrat (in counts per km2)
Q   <- quadratcount(starbucks.km, nx= 6, ny=3)
Q.d <- intensity(Q)

# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(starbucks.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

brk  <- c( -Inf, 4, 6, 8 , Inf)  # Define the breaks
Zcut <- cut(pop.lg.km, breaks=brk, labels=1:4)  # Classify the raster
E    <- tess(image=Zcut)  # Create a tesselated surface

plot(E, main="", las=1)
