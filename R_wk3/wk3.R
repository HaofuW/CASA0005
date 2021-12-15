#3.5.1
#load the .gpkg file
library(sf)
library(sp)
library(here)
st_layers(here("gadm36_AUS.gpkg"))

#read in the Geopackage layer of the whole Australia
Ausoutline <- st_read(here("gadm36_Aus.gpkg"),layer = "gadm36_AUS_0")

#check
print(Ausoutline)

#use Proj4 to identify a coordinate reference system
st_crs(Ausoutline)$proj4string 

#EPSG, to set a spatial reference system
Ausoutline <- Ausoutline %>% 
  st_set_crs(., 4326)

#reprojecting the spatial data
#here we change from WGS84 to GDA94, EPSG code 4326 to 3112
AusoutlinePROJECTED <- Ausoutline %>% 
  st_transform(.,3112)

print(AusoutlinePROJECTED)

#if encounter with SP object from sp pachage, we need to transform the sp object to sf and changing the projection
#from sf to sp
AusoutlineSP <- Ausoutline %>% 
  as(., "Spatial")

#from sp to sf
AusoutlineSF <- Ausoutline %>% 
  st_as_sf()

#############################################
#3.5.2
#Raster DATA from WorldClim
library(raster)
jan <- raster(here("wc2.1_5m_tmax","wc2.1_5m_tmax_01.tif"))
jan

#plot it in the geographic projection of WGS84
plot(jan)

#to reproject a raster, the whole grid must be recomputed
#to use projectRaster() we need to convert it to PROJ4 strings
#use Mollweide projection
#set the proj 4 to a new boject
newproj<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# get the jan raster and give it the new proj4
pr1 <- jan %>%
  projectRaster(., crs=newproj)
plot(pr1) 

##if we want go back from Mollweide to WGS84
##pr1 <- pr1 %>% 
##  projectRaster(., crs = "+init=epsg:4326")
##plot(pr1)

#3.5.3
#look into the folder and find the files end with .tif
library(fs)
dir_info("wc2.1_5m_tmax/")

library(tidyverse)
listfiles <- dir_info("wc2.1_5m_tmax") %>% 
  filter(str_detect(path, ".tif")) %>% 
  dplyr::select(path) %>% 
  pull()

listfiles

#now load all of the data onto a raster stack which is a collection of raster layers
worldclimtemp <- listfiles %>% 
  stack()

worldclimtemp

#there are 12 layers standing for 12 months
#to access single layers within the stack
#access the january layer
worldclimtemp[[1]]

#raname the layers in the stack. since it's raster data, we cannot use rename() from dplyr as last week
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month

worldclimtemp$Jan

#3.5.4
#extract data from the raster stack and make a dataframe
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
              "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
              "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- raster::extract(worldclimtemp, samples)

#Add the city names to the rows of AUcitytemp
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")

###############################
#3.6.1
#take Perth as an example and subset the data using the row name
Perthtemp <- Aucitytemp2 %>% 
  filter(site=="Perth")

#make a histogram of Perth's temperature
hist(as.numeric(Perthtemp))

#define the breaks in the histogram
userbreak <- c(8,10,12,14,16,18,20,22,24,26)
hist(as.numeric(Perthtemp),
     breaks=userbreak,
     col='red',
     main='Histogram of Perth Temperature',
     xlab='Temperature',
     ylab='Frequency')

#check the histogram information
histinfo <- Perthtemp %>% 
  as.numeric() %>% 
  hist(.)

histinfo

#next part we will try to check the entire Australia
#check the layer by plotting the geometry
#plot(Ausoutline$geom) #### Too big to run
 
#simplify the .shp plot 
#load the rmapshaper package
install.packages('rmapshaper')
library(rmapshaper)

#simplify the shapefile
#keep specifies the % of points
#to keep
AusoutSIMPLE <- Ausoutline %>% 
  ms_simplify(., keep=0.05)

plot(AusoutSIMPLE$geom)

#set the map extent to the outline of Australia then crop the WorldClim dataset in it
###we need to make sure both of the layers are in the same coordinate reference system
print(Ausoutline)
crs(worldclimtemp)

#combine
Austemp <- Ausoutline %>% 
  crop(worldclimtemp,.)

plot(Austemp)

#to save time, the outline needs to be cropped to be exact
exactAus <- Austemp %>% 
  mask(.,Ausoutline,na.rm=TRUE)

#re-compute the histogram in March
#subset using the known location of the raster
hist(exactAus[[3]], col="red", main = "March Temperature")

#3.6.4
#use a dataframe or tibble to make our raster data be compatible with ggplot2
exactAusdf <- exactAus %>% 
  as.data.frame()

library(ggplot2)

#set up the basic histogram
gghist <- ggplot(exactAusdf,
                 aes(x = Mar)) + 
  geom_histogram(color = "black",
                 fill = "white") + 
  labs(title = "Ggplot2 histogram of Australian March temperatures",
       x = "Temperature",
       y = "Frequency")

#add a vertical line to the histogram showing mean temperature
gghist + geom_vline(aes(xintercept = mean(Mar,
                                          na.rm = TRUE)),
                    color = "blue",
                    linetype = "dashed",
                    size = 1) + 
  theme(plot.title = element_text(hjust = 0.5))

#use pivot_longer to put the variable(Month) in one column. We put the month in a new column and the values in another
squishdata <- exactAusdf %>% 
  pivot_longer(
    col = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )

#select two months using filter()
twomonths <- squishdata %>% 
  # | = or
  filter(., Month == "Jan" | Month == "Jun")

#get the mean for each month we selected
meantwomonths <- twomonths %>% 
  group_by(Month) %>% 
  summarise(mean = mean(Temp, na.rm = TRUE))

meantwomonths
#fill the color based on the month. The intercept is the mean we just calculated.
ggplot(twomonths, aes(x = Temp, color = Month, fill = Month)) + 
  geom_histogram(position = "identity", alpha = 0.5) + 
  geom_vline(data = meantwomonths,
             aes(xintercept = mean,
                 color = Month),
             linetype = "dashed") + 
  labs(title = "Ggplot2 histogram of Australian Jan and Jun Temperature",
       x = "Temperature",
       y = "Frequency") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))

# further deal with the data with NAs and the bin size(default 30)
data_complete_cases <- squishdata %>% 
  drop_na() %>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                          "Aug","Sep","Oct","Nov","Dec")))

#plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))

########################################
#extra
install.packages("plotly")
library(plotly)

jan <- squishdata %>% 
  drop_na() %>% 
  filter(., Month == "Jan")

jun <- squishdata %>% 
  drop_na() %>% 
  filter(., Month == "Jun")

#give axis titles
x <- list(title = "Temperature")
y <- list(title = "Frequency")

#set the bin width
xbinsno <- list(start = 0, end = 40, size = 2.5)

#plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist

#################################
#more tips for plotting

#standard deviation
#maximum per month
#minimun per month
#interquartlie range per month