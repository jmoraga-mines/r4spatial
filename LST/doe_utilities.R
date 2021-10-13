#### Utility functions, used for DOE project ####


# Check Package manager is required
if (!require("pacman")){
  install.packages("pacman")
  require("pacman")  # if the package isn't available locally it will install it for you
} 

# It checks to see if a package is installed, if not it attempts to install the package
# from CRAN and/or any other repository in the pacman repository list.
pacman::p_load(raster, rgdal)
# Package ‘raster’ is used for the;
# Reading, writing, manipulating, analyzing and modeling of spatial data


# Landsat-8 LST files use Albers projection in meters
# Resolution is 30 by 30 mts
extent_hymap_lst <- raster(xmn=325209, xmx=336531, ymn = 4395438, ymx=4412103, 
                       res=c(30,30), crs=crs('+init=epsg:32611'))
# Assigning the values of the extent_hymap_lst

extent_hymap <- raster(xmn=325209, xmx=336531, ymn = 4395438, ymx=4412103, 
                           res=c(3,3), crs=crs('+init=epsg:32611'))
# # Assigning the values of the extent_hymap


# Defining the function that helps to write Raster format
doe_writeRaster <- function(x, filename, format="raster", overwrite=TRUE, bandorder="BSQ"){
  if(tools::file_ext("filename.grd") != "grd") {
    filename <- tools::file_path_sans_ext(filename)
    filename <- paste(filename, ".grd", sep="")
  }
  f1<-writeRaster(x=x, filename=filename, bandorder=bandorder, 
                  format=format, overwrite=overwrite)
  hdr(f1, "ENVI")
  return(f1)
}

# Defining the function that helps to convert coordinate points to the Spatial data
coords2spatial <- function(coord, sp_data, projection = crs("+init=epsg:32611")){
  print("Creating Points")
  d0 <- SpatialPoints(coord, proj4string = projection)
  print("converting")
  sp_data <- as.data.frame(sp_data)
  print("Assigning values")
  d0$sp_data <- sp_data
  return (d0)
}

# Defining the function that helps to convert coordinates to the Spatial dataframe points
coords2spatialdf <- function(coord, sp_data, projection = crs("+init=epsg:32611")){
  d0 <- SpatialPointsDataFrame(coord, sp_data, proj4string = projection)
  return (d0)
}

# Definig the function that use df_data as an Input and calculates the mean, min, max, and Standart deviation(sd)
cluster_fun <- function(df_data) {
  clusters <- data.frame(matrix(ncol=6, nrow=0))
  colnames(clusters) <- c("image", "avg", "min", "max", "stddev", "range")
  
  for (i in names(df_data)) {
    sub_cluster <- df_data[i]
    s_mean <- sapply(sub_cluster, mean, na.rm=TRUE)
    s_min <- sapply(sub_cluster, min, na.rm=TRUE)
    s_max <- sapply(sub_cluster, max, na.rm=TRUE)
    s_sd  <- sapply(sub_cluster, sd, na.rm=TRUE)
    clusters[i,1:6] <- list(i,s_mean, s_min, s_max, s_sd, s_max-s_min)
  }
  return(clusters)
}

# Defining the function that takes df_img as an Input and create another plt_img format of .GTiff
df_plot <- function( df_img, save_file = FALSE ) {
  plt_img <- df_img
  coordinates(plt_img) = ~x+y
  proj4string(plt_img)  <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  gridded(plt_img) <- TRUE
  plot(plt_img)
  if (save_file != FALSE) {
    writeRaster(raster(plt_img), filename = save_file, format="GTiff", overwrite = TRUE)
  }
  return(plt_img)
}

# Defining the function that organize the data of the a_df_lst
get_subset <- function( a_df_lst, a_df_mask, img_name, a_class){
  return (a_df_lst[(a_df_mask[img_name]==a_class),c("x","y",img_name)])
}

# Defining the save plot
save_plot <- function(a_raster, a_name) {
  png(paste(a_name,".png", sep=""), width = 600, height = 800)
  print(
    spplot(a_raster[[a_name]], 
           col.regions=c( "Yellow", "Cyan", "Magenta", "Green", "Red","Black"), 
           main=paste("K-means clustering (", a_name,")", sep=""))
  )
  # plot(a_raster[[a_name]], col=c("Black", "Yellow", "Cyan", "Magenta", "Green", "Red"))
  
  dev.off()
}


