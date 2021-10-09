###
# Book:
# https://spatialanalysisonline.com/An%20Introduction%20to%20Spatial%20Data%20Analysis%20in%20R.pdf

# Data: (requires registration)
# https://data.cdrc.ac.uk/dataset/introduction-spatial-data-analysis-and-visualisation-r


tables_dir <- "data/Camden/tables"
Ethnicity <- read.csv(file.path(tables_dir, "KS201EW_oa11.csv"))
Rooms <- read.csv(file.path(tables_dir, "KS403EW_oa11.csv"))
Qualifications <-read.csv(file.path(tables_dir, "KS501EW_oa11.csv"))
Employment <-read.csv(file.path(tables_dir, "KS601EW_oa11.csv"))

# to view the top 1000 cases of a data frame
View(Employment)

# view column names of a dataframe
names(Employment)

# selecting specific columns only
# note this action overwrites the labels you made for the original data,
# so if you make a mistake you will need to reload the data into R
Ethnicity <- Ethnicity[, c(1, 21)]
Rooms <- Rooms[, c(1, 13)]
Employment <- Employment[, c(1, 20)]
Qualifications <- Qualifications[, c(1, 20)]

# to change an individual column name
names(Employment)[2] <- "Unemployed"
View(Employment)

# to change both column names
names(Ethnicity)<- c("OA", "White_British")
names(Rooms)<- c("OA", "Low_Occupancy")
names(Employment)<- c("OA", "Unemployed")
names(Qualifications)<- c("OA", "Qualification")


