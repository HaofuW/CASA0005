# load the package
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)

# read the data
Londonwards <- st_read(here::here("statistical-gis-boundaries-london", "ESRI", "London_Ward_CityMerged.shp"))

# check the data
qtm(Londonwards)

LondonWardProfiles <- read_csv(here::here("ward-profiles-excel-version.csv"),
                               na = c("", "NA", "n/a"), 
                               locale = locale(encoding = 'Latin1'), 
                               col_names = TRUE)


#check all of the columns have been read in correctly
Datatypelist <- LondonWardProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

# use commin ID to combine the shp and csv file
# merge boundaries and data
LondonWardProfiles <- Londonwards%>%
  left_join(.,
            LondonWardProfiles, 
            by = c("GSS_CODE" = "New code"))

#let's map our dependent variable to see if the join has worked:
tmap_mode("plot")
qtm(LondonWardProfiles, 
    fill = "Average GCSE capped point scores - 2014", 
    borders = NULL,  
    fill.palette = "Blues")

# let's add some school data
#might be a good idea to see where the secondary schools are in London too
london_schools <- read_csv("https://data.london.gov.uk/download/london-schools-atlas/57046151-39a0-45d9-8dc0-27ea7fd02de8/all_schools_xy_2016.csv")

#from the coordinate values stored in the x and y columns, which look like they are latitude and longitude values, create a new points dataset
lon_schools_sf <- st_as_sf(london_schools, 
                           coords = c("x","y"), 
                           crs = 4326)

lond_sec_schools_sf <- lon_schools_sf %>%
  filter(PHASE=="Secondary")

tmap_mode("plot")
qtm(lond_sec_schools_sf)

### Analysing GCSE exam performance - testing a research hypothesis
## Hypothesis: here are other observable factors occurring in Wards in London that might affect the average GCSE scores of students living in those areas.

## Regression Basics
# scatter plot
q <- qplot(x = `Unauthorised Absence in All Schools (%) - 2013`, 
           y = `Average GCSE capped point scores - 2014`, 
           data=LondonWardProfiles)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()

## run a regression model
#run the linear regression model and store its outputs in an object called model1
Regressiondata<- LondonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014, 
                unauthorised_absence_in_all_schools_percent_2013)

#now model
model1 <- Regressiondata %>%
  lm(average_gcse_capped_point_scores_2014 ~
       unauthorised_absence_in_all_schools_percent_2013,
     data=.)

#show the summary of those outputs
summary(model1)

# we use broom to clean our results
library(broom)
tidy(model1)

glance(model1)


library(tidypredict)
Regressiondata %>%
  tidypredict_to_column(model1)

#########################################

## Assumptions Underpinning Linear Regression
## Assumption 1

# IF the scatter plot is not gonna work, we can check the frequency distribution of the two variables
# If they are normally distributed, then there is a good chance that if the two variables are in some way correlated, this will be a linear relationship.
