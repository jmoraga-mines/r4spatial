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

# Next, we’ll tally the quadrat counts within each tessellated area then compute
# their density values (number of points per quadrat area).
Q   <- quadratcount(starbucks.km, tess = E)  # Tally counts
Q.d <- intensity(Q)  # Compute density
Q.d

# Plot the density values across each tessellated region.
plot(intensity(Q, image=TRUE), las=1, main=NULL)
plot(starbucks.km, pch=20, cex=0.6, col=rgb(1,1,1,.5), add=TRUE)

# Let’s modify the color scheme.
cl <-  interp.colours(c("lightyellow", "orange" ,"red"), E$n)
plot( intensity(Q, image=TRUE), las=1, col=cl, main=NULL)
plot(starbucks.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)

### Kernel density raster
# The spatstat package has a function called density which computes an isotropic
# kernel intensity estimate of the point pattern. Its bandwidth defines the 
# kernel’s window extent.

# This next code chunk uses the default bandwidth.
K1 <- density(starbucks.km) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)

# In this next chunk, a 50 km bandwidth (sigma = 50) is used. Note that the 
# length unit is extracted from the point layer’s mapping units (which was 
# rescaled to kilometers earlier in this exercise).
K2 <- density(starbucks.km, sigma=50) # Using a 50km bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)

# The kernel defaults to a gaussian smoothing function. The smoothing function 
# can be changed to a quartic, disc or epanechnikov function. For example, to 
# change the kernel to a disc function type:
K3 <- density(starbucks.km, kernel = "disc", sigma=50) # Using a 50km bandwidth
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)

