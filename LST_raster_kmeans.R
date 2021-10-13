# Enter the source of the doe_utilities directories
source('LST/doe_Utilities.R')

#### temperature
kmeans_tmp <- function(pt_lst_16, img_name, n_clusters=5, plot_image = FALSE) {
  pt_tmp_dst <- pt_lst_16[,c(img_name)]
  k_tmp_dst_pt<-pt_lst_16[,c("x","y")]
  df1 <- na.omit(df[,c("x", "y", img_name)])
  pt_tmp_dst <- df1[,3]
  k_tmp_dst_pt <- df1[,c("x","y")]

  pt_tmp_dst<-(pt_tmp_dst-mean(pt_tmp_dst))/(sd(pt_tmp_dst))
  k_tmp_dst <- kmeans(pt_tmp_dst,n_clusters, iter.max = 2000)
  cluster_lst <- as.data.frame(k_tmp_dst$centers)
  cluster_lst$cluster <- seq(1,length(k_tmp_dst$centers))
  colnames(cluster_lst)<-c("temp", "cluster")
  sorted_clusters <- cluster_lst[order(cluster_lst[,1]),]
  k_tmp_dst$cluster <- match(unlist(k_tmp_dst$cluster), sorted_clusters[,2])
  k_tmp_dst$centers <- unlist(sorted_clusters$temp)
  k_tmp_dst$coord <- k_tmp_dst_pt
  if (plot_image){
    k_tmp_dst_pt[,3] <- k_tmp_dst$cluster
    coordinates(k_tmp_dst_pt) <- ~ x+ y
    proj4string(k_tmp_dst_pt) <- crs(" +proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    gridded(k_tmp_dst_pt) <- TRUE
    plot(k_tmp_dst_pt, col=rev(rainbow(n_clusters)), main=img_name)
  }
  #summary(pt_tmp_dst)
  return (k_tmp_dst)
}


#### temperature + distance
kmeans_dst <- function(pt_lst_16, img_name, n_clusters=5, plot_image = FALSE) {
  pt_tmp_dst <- pt_lst_16[,c("x","y",img_name)]
  k_tmp_dst_pt<-pt_tmp_dst
  pt_tmp_dst["x"]<-(pt_tmp_dst["x"]-mean(pt_tmp_dst[,"x"]))/(sd(pt_tmp_dst[,"x"]))
  pt_tmp_dst["y"]<-(pt_tmp_dst["y"]-mean(pt_tmp_dst[,"y"]))/(sd(pt_tmp_dst[,"y"]))
  pt_tmp_dst[3]<-(pt_tmp_dst[3]-mean(pt_tmp_dst[,3]))/(sd(pt_tmp_dst[,3]))
  #summary(pt_tmp_dst)
  k_tmp_dst <- kmeans(pt_tmp_dst,n_clusters, iter.max = 2000)
  #cluster_lst <- as.data.frame(k_tmp_dst$centers)
  #cluster_lst$cluser <- seq(1,length(k_tmp_dst$centers))
  #colnames(cluster_lst)<-c("temp", "cluster")
  #sorted_clusters <- cluster_lst[order(cluster_lst[,1]),]
  #k_tmp_dst$cluster <- match(unlist(k_tmp_dst$cluster), sorted_clusters[,2])
  #k_tmp_dst$centers <- unlist(sorted_clusters$temp)
  if (plot_image){
    k_tmp_dst_pt[,3] <- k_tmp_dst$cluster
    coordinates(k_tmp_dst_pt) <- ~ x+ y
    proj4string(k_tmp_dst_pt) <- crs(" +proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    gridded(k_tmp_dst_pt) <- TRUE
    plot(k_tmp_dst_pt, col=rev(rainbow(n_clusters)))
  }
  # View(pt_tmp_dst)
  # summary(pt_tmp_dst)
  return (k_tmp_dst)
}

# Read all Landsat-8 LST files from data directory
# data_directory = "F:/Geothermal Energy Project/Data/LST/BradyandDesert/Landsat" 
# data_directory = "F:/Geothermal Energy Project/Data/LST/BradyandDesert/Landsat2/ST" 
data_directory = "data/." 
lst_file_names = list.files(data_directory, pattern="([:alnum:]|_)*_ST[.]tif", recursive=TRUE, full.names = TRUE)
lst_file_names = lst_file_names[grep("tif$", lst_file_names)]
lst_list <- vector(mode = "list", length = 0)

# Landsat-8 LST files use Albers projection in meters
# Resolution is 30 by 30 mts
extent_hymap_lst <- raster(xmn=325209, xmx=336531, ymn = 4395438, ymx=4412103, 
                       res=c(30,30), crs=crs('+init=epsg:32611'))
for (a_file in lst_file_names){
  n = paste("x", substr(basename(a_file), 16, 23), sep = "")
  r <- raster(a_file)
  r <- projectRaster(r,extent_hymap_lst)
  n <- substr(basename(a_file), 16, 23)
  n <- as.Date(n, format = "%Y%m%d")
  raster_name <- paste("LST", n, sep="_") 
  names(r) <- raster_name 
  lst_list[raster_name] <- r
}

all_lst <- stack(unlist(lst_list))
names(all_lst)
# raster_names = paste("LST", as.Date(substr(basename(lst_file_names), 16, 23), "%Y%m%d"), sep="_")
# names(all_lst) <- raster_names
rm(lst_list)

all_lst <- projectRaster(all_lst, extent_hymap_lst, res = res(all_lst))
# extent(all_lst)

# new_filename = paste(data_directory,"all_lst.tif", sep="/")
# f1 <- writeRaster(all_lst, new_filename, format = "GTiff", overwrite=TRUE)


########## START # Doing the same but for QA files ###
# data_directory = "F:/Geothermal Energy Project/Data/LST/BradyandDesert/Landsat2/STQA" 
lst_file_names = list.files(data_directory, pattern="([:alnum:]|_)*_PIXELQA[.]tif", recursive=TRUE, full.names = TRUE)
lst_file_names = lst_file_names[grep("tif$", lst_file_names)]
lst_list <- vector(mode = "list", length = 0)

# Landsat-8 LST files use Albers projection in meters
# Resolution is 30 by 30 mts
extent_hymap_lst <- raster(xmn=325209, xmx=336531, ymn = 4395438, ymx=4412103, 
                       res=c(30,30), crs=crs('+init=epsg:32611'))
for (a_file in lst_file_names){
  n = paste("x", substr(basename(a_file), 16, 23), sep = "")
  r <- raster(a_file)
  r <- projectRaster(r,extent_hymap_lst)
  n <- substr(basename(a_file), 16, 23)
  n <- as.Date(n, format = "%Y%m%d")
  raster_name <- paste("LST", n, sep="_") 
  names(r) <- raster_name 
  lst_list[raster_name] <- r
}

qa_stack <- stack(unlist(lst_list))
# doe_writeRaster(qa_stack, 'final_results/all_tifs')
# all_lst <- 'final_results/all_tifs.gri'
########## END # Doing the same but for QA files ###

clean_stack <- stack(all_lst)
clean_stack[qa_stack > 325] <- NA
n_cells <- clean_stack@nrows * clean_stack@ncols

# plot(clean_stack[[1:12]])
# plot(clean_stack[[13:24]])
# plot(clean_stack[[25:36]])
# plot(clean_stack[[37:48]])
# plot(clean_stack[[49:50]])
s <- summary(clean_stack)
na_cells <- (s["NA's",])/n_cells
valid_cells <- names(clean_stack)[(na_cells<0.5)]
clean_stack <- clean_stack[[valid_cells]]
# plot(clean_stack[[1:6]])
plot(clean_stack[[valid_cells]])
print("Delete from stack the anomalous remaining layers")
anomalous <- c("LST_2018.09.07")
clean_stack <- dropLayer(clean_stack, anomalous)
plot(clean_stack)

doe_writeRaster(clean_stack, "valid_lst")

# new_filename = paste(data_directory,"valid_lst.tif", sep="/")
new_filename <- "valid_lst_tif.tif"
f1 <- writeRaster(clean_stack, new_filename, format = "GTiff", overwrite=TRUE)
hdr(f1, format="ENVI")
rm(qa_stack, all_lst, lst_list)

###### Conversions from LST format (Kelvin*10) to Celsius
### K = LST/10
### C = K - 273.15
valid_lst <- stack("valid_lst.gri")


valid_lst <- valid_lst/10.0-273.15
###
spplot(valid_lst[[12:15]])
df_coord <- coordinates(valid_lst)
df <- as.data.frame(valid_lst)
df <- cbind(df_coord, df)
img_names <- names(valid_lst)

k_means_list <- vector(mode = "list", length = 0)
for (img in img_names) {
  k <- kmeans_tmp(df, img, plot_image = FALSE)
  k_df <- as.data.frame(cbind(k$coord, k$cluster))
  colnames(k_df) <- c("x", "y", img)
  d1 <- rasterFromXYZ(k_df, res=c(30,30), crs=crs('+init=epsg:32611'))
  d1 <- projectRaster(d1, extent_hymap_lst)
  k_means_list[img] <- d1
}
k_means_stack <- stack(unlist(k_means_list))

f1 <- doe_writeRaster(k_means_stack, "LST_kmeans_stack")
rm(df, k, k_df, d1)

kmeans_stack <- brick("LST_kmeans_stack.gri")

k_df <- rasterToPoints(kmeans_stack)

rm(k_means_list, kmeans_stack)
k_df[is.na(k_df)] <- 0
summary_df <- as.data.frame(k_df[,1:2])
k_df <- as.data.frame(k_df[,3:(ncol(k_df))])
# n <- colnames(k_df)
# i_df <- matrix(as.integer( unlist(k_df) ), ncol=ncol(k_df), byrow=FALSE)
# k_df <- as.data.frame(i_df)
# colnames(k_df) <- n
summary_df$na_count <- rowSums((k_df==0))
summary_df$hottest <- rowSums(k_df == 5, na.rm = TRUE)
summary_df$hot <- rowSums(k_df == 4, na.rm = TRUE)
summary_df$normal <- rowSums(k_df == 3, na.rm = TRUE)
summary_df$cold <- rowSums(k_df == 2, na.rm = TRUE)
summary_df$coldest <- rowSums(k_df == 1, na.rm = TRUE)
summary_df$valid <- rowSums(k_df >= 1, na.rm = TRUE)
rm(k_df)
summary_df$probability <- (summary_df$hottest+summary_df$hot*.5-summary_df$coldest)/summary_df$valid
summary_df$probability <- (summary_df$hottest)/summary_df$valid
sp_df <- coords2spatialdf(summary_df[c("x", "y")], as.data.frame(summary_df$probability))
r <- rasterize(sp_df, extent_hymap_lst)
# r <- dropLayer(r, 1)
r <- r[[2]]
names(r)<-"LST_prob"
doe_writeRaster(r, "kmeans_weighted")
spplot(r, main="SP plot cluster 5 prob")
