# read csv data
mycsv <- read.csv("fly-tipping-borough.csv")

# basic plot
Data1 <- c(1:100)
Data2 <- c(101:200)
plot(Data1, Data2, col="red")

# create some data normally distributed
Data3 <- rnorm(100, mean = 53, sd = 34)
Data4 <- rnorm(100, mean = 64, sd = 14)
plot(Data3, Data4, col="blue")

# dataframe
df <- data.frame(Data1, Data2)
plot(df, col="green")

# for large dataframe
library(tidyverse)
# show the first 10 and then last 10 rows of data in df, %>% is called pipe and the meaning is "then"
df %>%
  head()
df %>%
  tail()

# elements of data frame: data.frame[row,column]
df[1:10, 1]
df[5:15, ]
df[c(2,3,6), 2]
df[c(2,3,6), 1]
df[,1]

# rename the column headings using rename() from dplyr
library(dplyr)
df <- df %>% 
  dplyr::rename(column1 = Data1, column = Data2)

# to select the column by name
df %>% 
  dplyr::select(column1)
# it's the same with $ operator
df$column1
# we can also use double square branket operator [[]]
df[["column1"]]

# Reading Data-2.5
LondonDataOSK <- read.csv("LondonData.csv",
                          header = TRUE,
                          sep = ",",
                          encoding = "latin1") #latin1 and UTF-8 are both encoding format
library(here)
# to find out where is the file path
here::here()

library(readr)
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")

# to check what data type the data set
class(LondonData)
class(LondonDataOSK)

LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"))

# DO NOT USE LondonData <- edit(LondonData) UNTIL YOU WANT TO EDIT THE DATA
LondonData %>% 
  colnames() %>% 
  head()

# Selecting rows
LondonBoroughs <- LondonData[626:658,]
# Another method
LondonBoroughs <- LondonData %>% 
  slice(626:658)

# Do some filters based on the data
# extracting all the wards where female life expextancy is greather than 90
Femalelifeexp <- LondonData %>% 
  filter("Female life expectancy -2009-13">90)

# To filter the string
LondonBoroughs <- LondonData %>% 
  filter(str_detect('New code',"^E09"))
LondonBoroughs$`Ward name`

# Same
LondonBoroughs %>% 
  dplyr::select('Ward name') %>% 
  print()

LondonBoroughs <- LondonBoroughs %>% 
  distinct()

LondonBoroughs_manualcols <- LondonBoroughs[,c(1,19,29,21)]
LondonBoroughs_contains <- LondonBoroughs %>% 
  dplyr::select(contains("expectancy"),
                contains("obese -2011/12 to 2013/14"),
                contains("Ward name"))

library(janitor)

LondonBoroughs <- LondonBoroughs %>% 
  dplyr::rename(Borough = "Ward name") %>% 
  clean_names()

LondonBoroughs <- LondonBoroughs %>% 
  clean_names(., case = "big_camel")

# We skip 2.5.4 - 2.5.7 here.
#life expectancy
Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))

# 2.5.8 Spatial data in R
library(maptools)
library(classInt)
library(RColorBrewer)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)

# Making the choropleth maps
EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")

EW <- st_read(here::here("Local_Authority_Districts_(December_2015)_Boundaries",
                         "Local_Authority_Districts_(December_2015)_Boundaries.shp"))

# Pull out London using str_detect()
LondonMap <- EW %>% 
  filter(str_detect(lad15cd,"^E09")) #London is E09 here
# plot using qtm function
qtm(LondonMap)

library(janitor)
LondonData <- clean_names(LondonData)
BoroughDataMap <- EW %>% 
  clean_names() %>% 
  # the . here means the data already loaded
  filter(str_detect(lad15cd,"^E09")) %>% 
  # left join can also be used here
  merge(.,
        LondonData,
        by.x = "lad15cd",
        by.y = "new_code",
        no.dups = TRUE) %>% 
  distinct(., lad15cd,
           .keep_all = TRUE)

tmap_mode("plot")
qtm(BoroughDataMap,
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

# Use OpenStreetMap(OSM) to add a base map
tmaplondon <- BoroughDataMap %>% 
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

# style shows how to divide the data into out color breaks
# palette shows the color scheme to use
tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Life_expectancy4map) + 
  tm_polygons("UKdiff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))

### Having trouble at 2.5.5.4