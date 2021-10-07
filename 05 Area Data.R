###  Spatial autocorrelation in R
# This is cleaner than using library, require and install.packages
if (!require("pacman")) 
  install.packages("pacman")

# Load data
load(url("https://github.com/mgimond/Spatial/raw/main/Data/moransI.RData"))
print(s1)
print(as.data.frame(s1))

# Plot map
p_load(tmap)
tm_shape(s1) + tm_polygons(style="quantile", col = "Income") +
  tm_legend(outside = TRUE, text.size = .8) 

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


