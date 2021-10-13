### Interpolation in R
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

###### 
