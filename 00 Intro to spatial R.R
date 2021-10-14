####### R for spatial statistics, Introduction
###### 01. Mapping data in R 
### from: https://mgimond.github.io/Spatial/mapping-data-in-r.html

# Loads the "pacman" library
library("pacman")
# In most cases, this will just give you an error because the library is not
# installed by default in your system

# This is cleaner than using library, require and install.packages for every
# new package, just do it once for pacman (Package manager)
if (!require("pacman")) 
  install.packages("pacman")

# use p_load instead: pacman::p_load(package1, package2, package_n)
pacman::p_load(sf)

# loads variables from this hosted R data file
load(url("https://github.com/mgimond/Spatial/raw/main/Data/Sample1.RData"))

### Basic R
# Functions

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
add5 <- function(x) {
  return(x+5) # Returns the value plus 5
}

### Using standard comments with roxygen format for automatic documentation
#' Implements Rectified Linear Unit function
#'
#' @param x Input value
#'
#' @return return value is 0 if x <=0; x otherwise
#'
#' @examples
#' ReLu(10)
#' [1] 10
#' ReLu(-10)
#' [1] 0
ReLu <- function(x){
  if(x<0)
    return(0)
  else
    return(x)
}

print(add5(10))
print(ReLu(-10))

# A loop, using a sequence of numbers from -2 to 2
for(i in seq(-2,2)){
  print(paste("ReLu of (", sprintf("%2d", i), "): ", ReLu(i), sep=""))
}

for(i in seq(-2,2)){
  cat(paste("ReLu of (", sprintf("%2d", i), "): ", ReLu(i), sep=""), fill=TRUE)
}
### End of Basic R

### Start ### Mapping data in R
# The data objects consist of five layers: 
# - an elevation raster (elev.r), 
# - an interstate polyline layer (inter.sf), 
# - a point cities layer (p.sf), 
# - a railroad polyline layer (rail.sf) and 
# - a Maine counties polygon layer (s.sf). 
# All vector layers are sf objects. 
# All layers are in a UTM/NAD83 projection (Zone 19N) except p.sf 
# which is in a WGS 1984 geographic coordinate system.

?print
print(elev.r)
print(p.sf)

# tmap
# The tmap package is specifically developed for mapping spatial data. 
# As such, it offers the greatest mapping options. The package recognizes sf, 
# raster and Spatial* objects.

# Loads the tmap library
p_load(tmap)

# To map the counties polygon layer using a grey color scheme
tm_shape(s.sf) + tm_polygons(col="grey", border.col="white")

# The col parameter defines either the polygon fill color or the spatial 
# object’s attribute column to be used to define the polygons’ color scheme. 
# For example, to use the Income attribute value to define the color scheme
tm_shape(s.sf) + tm_polygons(col="Income", border.col = "white")

# Note the + symbol used to piece together the functions (this is similar to 
# the ggplot2 syntax).
# You can customize the map by piecing together various map element functions. 
# For example, to move the legend box outside of the main map body add the 
# tm_legend(outside = TRUE) function to the mapping operation.
tm_shape(s.sf) + tm_polygons("Income",  border.col = "white") + 
  tm_legend(outside = TRUE)

# You can also choose to omit the legend box (using the legend.show = FALSE 
# parameter) and data frame border (use the tm_layout(frame = FALSE) function)
tm_shape(s.sf) + 
  tm_polygons("Income",  border.col = "white", legend.show=FALSE) +
  tm_layout(frame = FALSE)

# If you want to omit the polygon border lines from the plot, simply add the 
# border.col = NULL parameter to the tm_polygons function.
tm_shape(s.sf) + 
  tm_polygons("Income", border.col = NULL) + 
  tm_legend(outside = TRUE)

### Combining layers
# You can easily stack layers by piecing together additional tm_shapefunctions. 
# In the following example, the railroad layer and the point layer are added to 
# the income map. The railroad layer is mapped using the tm_lines function and 
# the cities point layer is mapped using the tm_dots function. Note that layers 
# are pieced together using the + symbol.
tm_shape(s.sf) + 
  tm_polygons("Income", border.col = NULL) + 
  tm_legend(outside = TRUE) +
  tm_shape(rail.sf) + tm_lines(col="grey70") +
  tm_shape(p.sf) + tm_dots(size=0.3, col="black") 
tm_shape(s.sf) + 
  tm_polygons("Income", border.col = NULL) +
  tm_shape(rail.sf) + tm_lines(col="grey70") +
  tm_shape(p.sf) + tm_dots(size=0.3, col="black")  + 
  tm_legend(outside = TRUE) 

# Adding items to the legend
tm_shape(s.sf) + 
  tm_polygons("Income", border.col = NULL) +
  tm_shape(rail.sf) + tm_lines(col="grey70") +
  tm_shape(p.sf) + tm_dots(size=0.3, col="black")  + 
  tm_legend(outside = TRUE) + 
  tm_add_legend("line", col="grey70", lwd=1, lty="solid", title="Rail")+ 
  tm_legend(outside = TRUE) + 
  tm_add_legend("symbol", size=0.3, col="black", title="Cities" )

### Tweaking classification schemes
# You can control the classification type, color scheme, and bin numbers via 
# the tm_polygons function. For example, to apply a quantile scheme with 6 bins 
# and varying shades of green
tm_shape(s.sf) + 
  tm_polygons("Income", style = "quantile", n = 6, palette = "Greens") + 
  tm_legend(outside = TRUE)

tm_shape(s.sf) + 
  tm_polygons("Income", style = "fixed", palette = "Greens",
              breaks = c(0, 23000, 27000, 100000 )) + 
  tm_legend(outside = TRUE)

tm_shape(s.sf) + 
  tm_polygons("Income", style = "kmeans", palette = "Greens", n=6) + 
  tm_legend(outside = TRUE, title="KMeans (6 clusters)")

tm_shape(s.sf) + 
  tm_polygons("Income", style = "kmeans", palette = "Greens", n=3) + 
  tm_legend(outside = TRUE, main.title="KMeans (3 clusters)")

# If you want a bit more control over the legend elements, you can tweak the 
# labels parameter
tm_shape(s.sf) + 
  tm_polygons("Income", style = "fixed",palette = "Greens",
              breaks = c(0, 23000, 27000, 100000 ),
              labels = c("under $23,000", "$23,000 to $27,000", "above $27,000"),
              text.size = 1) + 
  tm_legend(outside = TRUE)

# Controlling colors
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE)

# Colors and heading name (note: "\n" is interpreted as the start of a new line)
tm_shape(s.sf) + 
  tm_polygons("NoSchool", style="quantile", palette = "YlOrBr", n=8, 
              title="Fraction without \na HS degree") + 
  tm_legend(outside = TRUE)

# Inverting the colors
tm_shape(s.sf) + 
  tm_polygons("NoSchool", style="quantile", palette = "-YlOrBr", n=8, 
              title="Fraction without \na HS degree") + 
  tm_legend(outside = TRUE)

### Adding labels
# You can add text and labels using the tm_text function. In the following 
# example, point labels are added to the right of the points with the text left 
# justified (just = "left") and with an x offset of 0.5 units for added buffer 
# between the point and the text.
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1", border.col = "white") + 
  tm_legend(outside = TRUE) +
  tm_shape(p.sf) +   
  tm_dots(size=  .3, col = "red") +
  tm_text("Name", just = "left", xmod = 0.5, size = 0.8)

### Adding a grid or graticule
# You can add a grid or graticule to the map using the tm_grid function. 
# You will need to modify the map’s default viewport setting via the tm_layout 
# function to provide space for the grid labels. In the following example, the 
# grid is generated using the layer’s UTM coordinate system and is divided into 
# roughly four segments along the x-axis and five segments along the y-axis. 
# The function will adjust the grid placement so as to generate “pretty” label 
# values.
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE,
          n.x = 4, n.y = 5)

# We will also modify the grid placement by explicitly specifying the lat/long 
# grid values, and the projection as an EPSG (or OGC) code. Here, we’ll use 
# EPSG:4326 which defines the WGS 1984 geographic coordinate system.
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE, 
          x = c(-70.5, -69, -67.5),
          y = c(44, 45, 46, 47),
          projection = "EPSG:4326")

# Adding the ° symbol to the lat/long values requires a bit more code:
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE, 
          x =  c(-70.5, -69, -67.5) ,
          y = c(44, 45, 46, 47),
          projection = "+proj=longlat",
          labels.format = list(fun=function(x) {paste0(x,intToUtf8(176))} ) )


###### 04. Interpolation in R
# From: https://mgimond.github.io/Spatial/interpolation-in-r.html

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

# Create proximity interpolation (Thiessen polygons), using spatstat::dirichlet
p_load(spatstat)  # Used for the dirichlet tessellation function
p_load(maptools)  # Used for conversion from SPDF to ppp
p_load(raster)    # Used to clip out thiessen polygons

# Create a tessellated surface
th  <-  as(dirichlet(as.ppp(P)), "SpatialPolygons")

# The dirichlet function does not carry over projection information
# requiring that this information be added manually
proj4string(th) <- proj4string(P)

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z     <- over(th, P, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)

# Finally, we'll clip the tessellated  surface to the Texas boundaries
th.clp   <- raster::intersect(W,th.spdf)

# Map the data
tm_shape(th.clp) + 
  tm_polygons(col="Precip_in", palette="RdBu", 
              title="Predicted precipitation \n(in inches)") +
  tm_legend(legend.outside=TRUE)

## Inverse distance weighted (IDW) interpolation
#' The IDW output is a raster. This requires that we first create an empty 
#' raster grid, then interpolate the precipitation values to each unsampled grid
#' cell. An IDW power value of 2 (idp=2.0) will be used.

p_load(gstat) # Use gstat's idw routine
p_load(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Precip_in ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu",
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#### Fine-tuning the interpolation
#' The choice of power function can be subjective. To fine-tune the choice of 
#' the power parameter, you can perform a leave-one-out validation routine to 
#' measure the error in the interpolated values.
# Leave-one-out validation routine
IDW.out <- vector(length = length(P))
for (i in 1:length(P)) {
  IDW.out[i] <- idw(Precip_in ~ 1, P[-i,], P[i,], idp=2.0)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ P$Precip_in, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ P$Precip_in), col="red", lw=2,lty=2)
abline(0,1)
par(OP)  # Reset chart to original parameters

# Compute root-mean-square error (RMSE) [or root-mean-square deviation (RMSD)]
sqrt( sum((IDW.out - P$Precip_in)^2) / length(P))
length(IDW.out)
length(P$Precip_in)
length(P)

#### 1st order polynomial fit
#' To fit a first order polynomial model of the form:
#' precip =  intercept+aX+bY to the data
# Define the 1st order polynomial equation
f.1 <- as.formula(Precip_in ~ X + Y) 

# Add X and Y to P
P$X <- coordinates(P)[,1]
P$Y <- coordinates(P)[,2]

# Run the regression model
lm.1 <- lm( f.1, data=P)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", 
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#### Second order polynomial
# Define the 2nd order polynomial equation
f.2 <- as.formula(Precip_in ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Add X and Y to P
P$X <- coordinates(P)[,1]
P$Y <- coordinates(P)[,2]

# Run the regression model
lm.2 <- lm( f.2, data=P)

# Use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.2nd)
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", 
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


#### Kriging
#' Fit the variogram model
#' First, we need to create a variogram model. Note that the variogram model is 
#' computed on the de-trended data. This is implemented in the following chunk 
#' of code by passing the 1st order trend model (defined in an earlier code 
#' chunk as formula object f.1) to the variogram function.
# Define the 1st order polynomial equation
f.1 <- as.formula(Precip_in ~ X + Y) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, P, cloud = FALSE, cutoff=1000000, width=89900)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=590000, nugget=0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,1000000))


#### Generate Kriged surface
#' Next, use the variogram model dat.fit to generate a kriged interpolated 
#' surface. The krige function allows us to include the trend model thus saving 
#' us from having to de-trend the data, krige the residuals, then combine the 
#' two rasters. Instead, all we need to do is pass krige the trend formula f.1.
#' 

# Define the trend model
f.1 <- as.formula(Precip_in ~ X + Y) 

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, P, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", 
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

###### 03. Point pattern analysis in R
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

## Rescale to kilometers
starbucks.km <- rescale(starbucks, 1000, "km")
ma.km <- rescale(ma, 1000, "km")
pop.km    <- rescale(pop, 1000, "km")
pop.lg.km <- rescale(pop.lg, 1000, "km")

# Compute the density for each quadrant (in counts per km2)
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

####### 05. ###  Spatial autocorrelation in R
# From: https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html


# This is cleaner than using library, require and install.packages
if (!require("pacman")) 
  install.packages("pacman")

# Load data
load(url("https://github.com/mgimond/Spatial/raw/main/Data/moransI.RData"))
print(s1)
print(as.data.frame(s1))
print(typeof(s1))
save(s1, file="data/s1.RData")
rgdal::writeOGR(s1, "data/s1_Income.shp", driver = "ESRI Shapefile", 
                layer = c("Income"), overwrite_layer = TRUE)
rgdal::writeOGR(s1, "data/s1_All.shp", driver = "ESRI Shapefile", 
                layer = names(s1), overwrite_layer = T)
# sf::st_write(s1, "s1_layers.shp", "NAME")
s2 <- sf::st_read("data/s1_All.shp")
print(names(s2))
s3 <- sf::st_read("data/s1_Income.shp")
print(names(s3))

# Plot map
p_load(tmap)
tm_shape(s1) + tm_polygons(style="quantile", col = "Income") +
  tm_legend(outside = TRUE, text.size = .8) 

tm_shape(s2) + tm_polygons(col = "NAME", palette = "Set1") +
  tm_legend(outside = TRUE, text.size = .8, main.title="s2 file") +
  tm_text("NAME", just = "center", size = 0.5)


### Define neighboring polygons
p_load(spdep)
nb <- poly2nb(s1, queen=TRUE)
print(nb)

cat(paste("\n\nNeighbors of:", s1$NAME[1]), fill=TRUE); 
nb_names <- lapply(c(nb[1]), function(x) {(paste0(sprintf("%02d", x), ". ", 
                                                  s1$NAME[x]))})
cat(unlist(nb_names), sep="\n")

lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]
Inc.lag <- lag.listw(lw, s1$Income)
d <- data.frame(s1$NAME, s1$Income, Inc.lag)
names(d) <- c("County", "Income", "Neighbor Avg Inc.")
print(d)

### Computing the Moran’s I statistic: the hard way
# We can plot lagged income vs. income and fit a linear regression model to the 
# data.
# Create a regression model
M <- lm(Inc.lag ~ s1$Income)

# Plot the data
op <- par(mar = c(5,7,4,2) + 0.1) # Increase left margin from 5 to 7 lines
plot( Inc.lag ~ s1$Income, pch=20, asp=1, las=1, ann = FALSE)
mtext(side = 1, text = "Income", line = 3)
mtext(side = 2, text = "Neighbor Avg. Income", line = 5)

# Add the model's curve
abline(M, col="red")
par(op) # Resets margins to original parameters

# The slope of the regression line is the Moran’s I coefficient.
coef(M)[2]

# We then compare the observed Moran’s I value to this distribution.
n <- 599L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector

for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(s1$Income, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(lw, x)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

# Plot the histogram of simulated Moran's I values
# then add our observed Moran's I value to the plot
hist(I.r, main=NULL, xlab="Moran's I", las=1)
abline(v=coef(M)[2], col="red")
# The simulation suggests that our observed Moran’s I value is not consistent 
# with a Moran’s I value one would expect to get if the income values were not 
# spatially autocorrelated. In the next step, we’ll compute a pseudo p-value 
# from this simulation.

?sf::st_write

### Montecarlo (MC) simulation
#' First, we need to find the number of simulated Moran’s I values values 
#' greater than our observed Moran’s I value.
N.greater <- sum(coef(M)[2] > I.r)

#' To compute the p-value, find the end of the distribution closest to the 
#' observed Moran’s I value, then divide that count by the total count. Note 
#' that this is a so-called one-sided P-value. 
p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p
print(paste0("Chance of being wrong: ", sprintf("%0.4f",p), "%"))

#' In our working example, the p-value suggests that there is a small chance 
#' of being wrong in stating that the income values are not clustered 
#' at the county level

### Computing the Moran’s I statistic: the easy way
#' To get the Moran’s I value, simply use the moran.test function.

moran.test(s1$Income,lw)

#' Note that the p-value computed from the moran.test function is not computed 
#' from an MC simulation but analytically instead. This may not always prove to
#' be the most accurate measure of significance. To test for significance using 
#' the MC simulation method instead, use the moran.mc function.

MC<- moran.mc(s1$Income, lw, nsim=n)

# View results (including p-value)
MC

print(paste0("Chance of being wrong: ", sprintf("%0.4f",MC$p.value), "%"))

# Plot the distribution (note that this is a density plot instead of a histogram)
plot(MC, main="Distribution", las=1)


file.remove("data/s1.RData")
files2delete <- list.files(path = "data", pattern = "^s1_", full.names = T)
file.remove(files2delete)

# files2delete <- list.files(pattern = "valid_lst", full.names = T)
# print(files2delete)
# delete_results <- lapply(files2delete, file.remove)
