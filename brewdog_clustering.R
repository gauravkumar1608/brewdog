## PROJECT: Product segmentation using hierarchical clustering  ##

# Clearing the environment
rm(list = ls())

# Importing data
brewdog <- read.csv("Brewdog.csv", header = TRUE)

# Data exploration
summary(brewdog)

# Understanding missing values
# md.pattern will generate plot showing how many values 
# are missing in which feature
library("mice")  # md.pattern function is available in mice package
md.pattern(brewdog)
md.pattern(brewdog, rotate.names = TRUE) # Rotating the names on plot 

# aggr will also generate plot showing missing values in features
library("VIM") # aggr function is available in VIM package
aggr(brewdog)
# Converting proportions displayed in plot to numbers
aggr(brewdog, numbers = TRUE, prop = FALSE)

# matrixplot will plot all data on screen
matrixplot(brewdog) # missing values will be highlighted in red colour

# Using is.na to determine number of values missing in feature
# For feature "ABV"
isnabrewdog1 <- sum(is.na(brewdog$ABV))
isnabrewdog1
# For feature "EBC"
isnabrewdog2 <- sum(is.na(brewdog$EBC))
isnabrewdog2

# Optional: To display all rows were data is missing
brewdog[!complete.cases(brewdog),]

# Creating correlation plot using corrgram
library("corrgram")

# Shaded corrgram plot
corrgram(brewdog)

# Pie chart based coorelation gram
corrgram(brewdog, lower.panel=panel.pie, upper.panel=panel.pie)

# Optional: Displaying correlations derived from corrgram on a scatter plot
plot(brewdog$ABV,brewdog$AttenuationLevel)
plot(brewdog$ABV,brewdog$IBU)

# Performing Imputation of missing data

# Performing Simple Imputation
brewdogsi <- brewdog #Creating a copy of brewdog
# Replacing missing values of ABV with its mean
brewdogsi$ABV[!complete.cases(brewdogsi$ABV)] <- mean(brewdogsi$ABV
                                                      , na.rm =TRUE)
# Replacing missing values of EBC with its mean
brewdogsi$EBC[!complete.cases(brewdogsi$EBC)] <- mean(brewdogsi$EBC
                                                      , na.rm =TRUE)
summary(brewdogsi)

# Visual examination using histogram after performing simple imputation
hist(brewdogsi$ABV)
hist(brewdogsi$EBC)

# Performing Multiple Imputation 
impute <- mice(brewdog, m = 10, seed = 1234)
brewdogimp <- complete(impute)

summary(brewdogimp)

# Visual examination using histogram after performing multiple imputation
hist(brewdogimp$ABV)
hist(brewdogimp$EBC)

## Performing hierarchical clustering
library("cluster") # To use daisy function
library("NbClust") # To use
brewdogmi <- brewdogimp # Using the dataframe produced out of MI

# Scaling the data
# scale function works with numerical data only
brewdogmi[,2:8] <- scale (brewdogmi[,2:8], center = TRUE, scale = TRUE) 

# Converting Yeast feature into factors
brewdogmi$Yeast <- as.factor(brewdogmi$Yeast) 

# Creating a dissimilarity matrix
# Please refer the report to understand why dissimilarity matrix is created 
# using daisy function
bd <- daisy(brewdogmi[2:9])

# Performing clustering
# Please refer report why "weighted" method is chosen
clust <- agnes(bd, diss = TRUE, method = "weighted")

# Plotting the cluster
plot(clust, labels = brewdogmi$Name, which.plots = 2, cex = 0.2, hang = -1)

# Determining the number of clusters
# Determining the number of clusters
# Using silhouette index
res <- NbClust(diss = bd, distance = NULL, min.nc=2, max.nc=15
               , method = "mcquitty"
               , index = "silhouette")
res$Best.nc
# Using frey index
res1 <- NbClust(diss = bd, distance = NULL, min.nc=2, max.nc=15
                , method = "mcquitty"
                , index = "frey")
res1$Best.nc
# Using mcclain index
res2 <- NbClust(diss = bd, distance = NULL, min.nc=2, max.nc=15
                , method = "mcquitty"
                , index = "mcclain")
res2$Best.nc
# Using cindex
res3 <- NbClust(diss = bd, distance = NULL, min.nc=2, max.nc=15
                , method = "mcquitty"
                , index = "cindex")
res3$Best.nc
# Using dunn
res4 <- NbClust(diss = bd, distance = NULL, min.nc=2, max.nc=15
                , method = "mcquitty"
                , index = "dunn")
res4$Best.nc

# Choosing number of clusters as 4
# Please refer report why 4 is chosen
# Drawing rectangle around 4 clusters
rect.hclust(clust,4)

# Constructing a table showing how many beers fall in each cluster
cluster <- data.frame(brewdogimp, clusterNo = cutree(clust, k = 4))
library("dplyr")
cluster %>% group_by(clusterNo) %>% count(clusterNo)

# Displaying Beer Name, Yeast Type and Cluster No in which beer falls
cluster[c(1,9,10)][order(cluster[10]),]
