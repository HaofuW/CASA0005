### SET THE DATA
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)

## read the data
# get the london borough boundaries
LondonBoroughs <- st_read(here::here("statistical-gis-boundaries-london", "ESRI", "London_Borough_Excluding_MHW.shp"))

# pull out london using str_detect() function and E09 is district code of london
library(stringr)

BoroughMap <- LondonBoroughs %>% 
  dplyr::filter(str_detect(GSS_CODE, "^E09")) %>% 
  st_transform(., 27700)

qtm(BoroughMap)

summary(BoroughMap)

# get the location of all Blue Plaques in london
BluePlaques <- st_read(here::here("open-plaques-london-2018-04-08.geojson")) %>% 
  st_transform(., 27700)

summary(BluePlaques)

# plot the blue plaques in the city
tmap_mode("plot")

tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(BluePlaques) +
  tm_dots(col = "blue")

################################################################################

### DATA CLEANING
# some blue points are at the outside of the london boundary so we need to erase them
# remove duplicates
library(tidyverse)
library(sf)
BluePlaques <- distinct(BluePlaques)

### SPATIAL SUBSETTING
# select the points inside london
BluePlaquesSub <- BluePlaques[BoroughMap,]

# check whether they have been removed
tmap_mode("plot")

tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

### STUDY THE DATA
# extract one borough
Harrow <- BoroughMap %>% 
  filter(., NAME == "Harrow")

# check whether its the correct borough that has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)

# clip the Blue Plaques to have a subset of those fall within the borough
BluePlaquesSub <- BluePlaques[Harrow,]

# check whether it work
tmap_mode("plot")

tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

## we set a window to observe for sparstat to carry out the analysis within
window <- as.owin(Harrow)
plot(window)

## spatstat has its own set of spatial objects.
## As a result, for point pattern analysis, we need to create a point pattern (ppp) object.
# create a sp object
BluePlaquesSub <- BluePlaquesSub %>% 
  as(., 'Spatial')

# create a ppp object
BluePlaquesSub.ppp <- ppp(x = BluePlaquesSub@coords[,1],
                          y = BluePlaquesSub@coords[,2],
                          window = window)

# the meaning of [,1] is as follows
BluePlaquesSub@coords[,1]

# check the new ppp object
BluePlaquesSub.ppp %>% 
  plot(., pch = 16, cex = 0.5,
       main = "Blue Plaques Brent")

###############################################################################
###############################################################################

#### IMPORTANT
### 6.6.1 POINT PATTERN ANALYSIS
## Kernel Density Estimation

# The size and shape of the Kernel affects the density pattern produced.
BluePlaquesSub.ppp %>% 
  density(., sigma = 500) %>% 
  plot()

# The sigma value is set for the diameter of the Kernel (by meters here)
BluePlaquesSub.ppp %>% 
  density(., sigma = 200) %>% 
  plot()


### 6.6.2 Quadrat Analysis
## REMEMBER its not recommended to use in reality
# to understand whether the distribution of points in our study area differs from 'complete spatial randomness' --CSR
# the basic test of CSR is a quadrat analysis

# first plot the points
plot(BluePlaquesSub.ppp,
     pch = 16,
     cex = 0.5,
     main = "Blue Plaques in Brent")

# now count the points in that fall in 6x6
BluePlaquesSub.ppp %>% 
  quadratcount(., nx = 6, ny = 6) %>% 
  plot(., add = T, col = "red")

# compare our observed distribution of the points with the Poisson distribution
# run the quadrat count to calculate the frequency of blue plaques.
Qcount <- BluePlaquesSub.ppp %>% 
  quadratcount(., nx = 6, ny = 6) %>% 
  as.data.frame() %>% 
  dplyr::count(Var1 = Freq) %>% 
  dplyr::rename(Freqquadratcount = n)

# check the data type in the first column.
# if its factor, we will need to convert it to numeric
Qcount %>% 
  summarise_all(class)

Qcount

# calculate the expected values with the frequency table based on the Poisson distribution
# the equation is in the practical book
sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 

lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)

# calculate the expected value "Pr". 
# k is the number of blue plaques counted in a square and is found in the first column of the table.
QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  # now calculate the expected counts based on our total number of plaques
  # and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

# Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")

points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)

points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)

## Next part is to check whether our pattern fit the Complete Spatial Randomness
## We use quadrat.test() to do a Chi Squared Test to compare the observed and Expected frequencies
## Chi Squared test is to check the association between the variables.
## The higher the Chi-squared value, the greater the difference.
## If the p-value of the Chi-squared test is < 0.05, we can reject the null hypothesis test that "There is no pattern (CSR)."
## If p-value is > 0.05, it indicates that we have CSR and there is no pattern in our points.
## If p-value is < 0.05, it indicates that we do have clustering in our points.

teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)

plot(BluePlaquesSub.ppp, pch = 16, cex = 0.5, main = "Blue Plaques in Harrow")
plot(teststats, add = T, col = "red")
# The results here can be further explained in the practical book 6.6.2



### 6.6.3 Further test for the Quadrant analysis
#First plot the points
plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 10 x 10
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 10, ny = 10)%>%
  plot(., add=T, col="red")

#run the quadrat count
Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)

Qcount %>% 
  summarise_all(class)

sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 

lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)

QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)

teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 10, ny = 10)

plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")


### 6.6.4 Ripley's K
## Ripley's K is to aviod the limitation of Quadrat analysis
## It is to compare the observed distribution of points with the poisson random model for a whole range of differnet distance radii.
## we use kest() function

K <- BluePlaquesSub.ppp %>% 
  Kest(., correction = "border") %>% 
  plot()

Kval <- as.data.frame(Kest(BluePlaquesSub.ppp, correction = "border"))
# The results here can be further explained in the practical book 6.6.4


#### 6.7 DBSCAN
## The limitation of quadrat and Ripley's K is that it can tell us whether we have clusters in our point data but they cannot tell where they are.
## DBSCAN can help is discover the clusters in space. (Density-based spatial clustering of applications with noise)

library(raster)
library(fpc)

# we now analysis the blue plaques with DBSCAN to see if there are any clusters
#first check the coordinate reference system of the Harrow spatial polygon:
st_geometry(BoroughMap)

# there are two parameters that DBSCAN requires
# Epsilon: the radius within which the algorithm with search for clusters
# MinPts: the minimum number of points that should be considered a cluster
# According to the Ripley's K, the radius of getting clustering up is around 1200m, with the largest bulge around 700m.
# we start with 700m and search for clusters of at least 4 points.

# first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>% 
  coordinates(.) %>% 
  as.data.frame()

# run the DBSCAN analysis
db <- BluePlaquesSubPoints %>% 
  fpc::dbscan(., eps = 700, MinPts = 4)

# plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add = T)

# use the kNNdistplot() to find suitable eps
library(dbscan)

BluePlaquesSubPoints %>% 
  dbscan::kNNdistplot(., k = 4)
# it shows for each point the average distance to the k neighbours. 

# make it prettier
library(ggplot2)

db

db$cluster

BluePlaquesSubPoints <- BluePlaquesSubPoints %>% 
  mutate(dbcluster = db$cluster)

chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

#chulls2 <- ddply(BluePlaquesSubPoints, .(dbcluster), 
#  function(df) df[chull(df$coords.x1, df$coords.x2), ])

chulls <- chulls %>%
  filter(dbcluster >=1)

dbplot <- ggplot(data=BluePlaquesSubPoints, 
                   aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()

## add a basemap
## First get the bbox in lat long for Harrow
HarrowWGSbb <- Harrow %>%
  st_transform(., 4326)%>%
  st_bbox()

# change the basemap to national grip map
library(OpenStreetMap)

basemap <- OpenStreetMap::openmap(c(51.5549876,-0.4040502),c(51.6405356,-0.2671315),
                                  zoom=NULL,
                                  "stamen-toner")

# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")

#autoplot(basemap_bng) sometimes works
autoplot.OpenStreetMap(basemap_bng)+ 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5)  



###############################################################################
###############################################################################


#### 6.9 Analysing Spatial Autocorrelation with Moran's I and LISA
## data download
# download the ward boundaries data

library(here)
library(janitor)
library(dplyr)

LondonWards <- st_read(here::here("statistical-gis-boundaries-london","ESRI","London_Ward.shp"))

LondonWardsMerged <- st_read(here::here("statistical-gis-boundaries-london","ESRI",
                             "London_Ward_CityMerged.shp")) %>% 
  st_transform(., 27700)

LondonWardsMerged

WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",locale = locale(encoding = "latin1"),na = c("NA", "n/a")) %>%
  clean_names()

LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)

WardData

#have a look to check that it's 
#in the right projection
st_crs(LondonWardsMerged)

tmap_mode("view")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

### Data cleaning
summary(BluePlaques)

BluePlaquesSub <- BluePlaques[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

### Data manipulation
## we need continuous observations (counts of blue plaques, average GCSE scores, average incomes etc.) for the measures of spatial autocorrelation
## we now use st_join() to count all of the blue plaques that fall within each Ward in the city to create a continuous observation.

library(sf)

points_sf_joined <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%
  add_count(ward_name)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)

# draw a quick choropleth map to see how we are getting on
points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

# we use Moran's I to check some clustering of blue plaques from the map above in the center London
# to calculate Moran's I, we need to first define a W_ij spatial weights matrix.

library(spdep)

# first calculate the centroids of all wards in London

coordsW <- points_sf_joined %>% 
  st_centroid() %>% 
  st_geometry()

plot(coordsW, axes = TRUE)

## now we need a spatial weight matrices
## we start with a simple binary matrix of queen's case neighbours (a.k.a. Coutiguity edges corners)
## the method means that polygons with a shared edge or a corner will be included in computations for the target polygons
## we create a neighbours list with poly2nb() with the argument queen = T saying we want to use Queens case.

# create a neighbors list
LWard_nb <- points_sf_joined %>% 
  poly2nb(., queen = T)

# we get some summary info and plot the matrix
summary(LWard_nb)

# as we can see from the summary, the average number of neighbours is 5.88.
# now lets plot the neighbours
plot(LWard_nb, st_geometry(coordsW), col = "red")

# add a map underneath
plot(points_sf_joined$geometry, add = T)

## now we need to make a spatial weight matrix
## the matrix has the size of the number of neighbourhoods with values indicating if the elements in the rows are a neighbour or not.
## The style of weight is important B, W, C, U, S.

# lets start with binary
# create a spatial weights matrix from these weights
Lward.lw <- LWard_nb %>% 
  nb2mat(., style = "B")

sum(Lward.lw)
# summing the binary (1/0) shows that we have 3680 neighbours.
# if we use global standardisation, we use 3680 to divide our 625 wards meaning each spatial weight has a value of 0.169.
# or we can do row standardisation where 1 is divided by the sum of the number of neighbors in each row.
# e.g. row 1 here sums to 6, meaning each weight would be 0.166 in row 1 only.

sum(Lward.lw[,1])


#### 6.9.4 Spatial autocorrelation
## now we have W_ij matrix, we can calculate the Moran's I and other statistics.
## however, Moran's I requires a spatial weight list type object instead of matrix

Lward.lw <- LWard_nb %>% 
  nb2listw(., style = "C")

## Moran's I tells us whether we have clustered values (close to 1) or dispersed values (close to -1)
## we will calculate for the densities rather than raw values

I_LWard_Global_Density <- points_sf_joined %>% 
  pull(density) %>% 
  as.vector() %>% 
  moran.test(., Lward.lw)

I_LWard_Global_Density

## calculate Geary's C
## This tells us whether similar values or dissimilar values are clustering

C_LWard_Global_Density <- 
  points_sf_joined %>% 
  pull(density) %>% 
  as.vector() %>% 
  geary.test(., Lward.lw)

C_LWard_Global_Density

## calculate Getis Ord General G.
## this tells whether high or low values are clustering.
## If G > Expected = High values. If G < expected = low values clustering

G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

# The result analysis can be found in the end of 6.9.4

## we can now calculate local versions of the Moran's I statistic (for each Ward) 
## and a Getis Ord Gi statistic to see where we have hot-spots.

#use the localmoran function to generate I for each ward in the city

I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))


#### Mapping Outputs
## Now we can plot a map of local Moran's I outputs

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

MoranColours<- rev(brewer.pal(8, "RdGy"))

tm_shape(points_sf_joined) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")

### You can find the map of Getis Ord G and other variables in 6.9.5 and 6.9.6