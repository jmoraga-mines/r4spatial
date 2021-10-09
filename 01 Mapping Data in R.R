### Mapping data in R 
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
p_load(sf)

# loads variables from this hosted R data file
load(url("https://github.com/mgimond/Spatial/raw/main/Data/Sample1.RData"))

### Basic R
# Functions

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

