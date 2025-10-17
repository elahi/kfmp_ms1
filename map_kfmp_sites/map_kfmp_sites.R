################################################################################
##' @title Plot kfmp video survey sites from kfmp_transect_video_metadata
##' @author Robin Elahi
##' @date 2025-10-16
##' @log 
################################################################################

#### File paths ####
here::i_am("map_kfmp_sites/map_kfmp_sites.R")
library(here)
folder <- "map_kfmp_sites"
file_name <- "map_kfmp_sites"

##### PACKAGES, DATA #####
library(sf)
library(tidyverse)
library(readxl)
library(ggmap)
source(here("R", "ggplot_settings.R"))

# Load kfmp transect data
kfmp_transect_video_metadata_250305

dat <- read_excel("data/kfmp_transect_video_metadata_251016.xlsx") |> 
  select(id:depth_m)

glimpse(dat)

##### WRANGLE KFMP TRANSECT DATA #####
dat <- dat |>
  mutate(year = year(date), 
         lat = as.numeric(latitude), 
         lon = as.numeric(longitude))

# Spatial and temporal variability
dat |> count(stratum)
dat |> count(year)
dat |> count(distance_m)

# Subset
dat <- dat |> 
  filter(#distance_m > 15, 
         #year != 2021 & year != 2024, 
         stratum != "shal") 

# Get lat lon bounds
lat_max <- max(dat$lat)
lat_min <- min(dat$lat)
lon_max <- max(dat$lon)
lon_min <- min(dat$lon)
lat_mean <- mean(lat_max, lat_min)
lon_mean <- mean(lon_max, lon_min)
bbox <- c(
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max)

# Convert to spatial dataframe
dat_sp <- dat %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)
dat_sp

##### WRANGLE STRATA #####
# Load strata (points)
strata_df <- read_excel("data/kfmp_strata_250727.xlsx")
strata_df

strata_df$stratum <- fct_reorder(strata_df$stratum, strata_df$lat, 
                                 .desc = TRUE)

strata_sp <- strata_df %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)
strata_sp

# Load Pt Pinos hi-res coastline
coast_all <- st_read("shapefiles/CA36A06/historicl1.shp") %>% 
  filter(ATTRIBUTE != "User Added Line")

# Crop to study bounding box
study_bbox <- st_bbox(bbox, crs = st_crs(4326)) %>% 
  st_as_sfc() %>% 
  st_transform(st_crs(coast_all)) %>% 
  st_bbox()

coast <- st_crop(coast_all, study_bbox) %>% 
  st_transform(st_crs(32610)) %>% # UTM zone 10N
  st_simplify(dTolerance = 50) 

# NOTE: coast simplification necessary to avoid this bug:
# https://trac.osgeo.org/geos/ticket/810

##### ASSIGN KFMP SITES TO STRATA #####
# Sanity check
ggplot() + geom_sf(data = coast)

# Plot with sites
ggplot() + 
  geom_sf(data = coast) +
  geom_sf(data = dat_sp) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Assign kfmp sites to strata (points)
dat_sp <- dat_sp %>% 
  mutate(nearest_strata = strata_sp$stratum[st_nearest_feature(., strata_sp)])
dat_sp
levels(dat_sp$nearest_strata)

#### MAPS ####

## Assigned strata
ggplot() + 
  geom_sf(data = coast, linetype = "dashed") +
  geom_sf(aes(fill = stratum), 
          strata_sp, size = 1.5, shape = 21) +
  geom_sf(aes(color = nearest_strata), dat_sp, 
          size = 0.8, alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_blank(), 
        legend.title = element_blank()) + 
  guides(color = "none")

#ggsave(paste(folder, "/figs/", file_name, "_a.pdf", sep = ""), height = 4, width = 7)

## Manual strata
ggplot() + 
  geom_sf(data = coast, linetype = "solid") +
  geom_sf(aes(color = stratum), dat_sp, alpha = 0.7) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_blank(), 
        legend.title = element_blank()) + 
  guides(color = "none")

#ggsave(paste(folder, "/figs/", file_name, "_b.pdf", sep = ""), height = 7, width = 11)

##### GGMAP #####
devtools::install_github("dkahle/ggmap")
m <- get_googlemap(center = c(lon_mean, lat_mean))

#### OSM ####
install.packages("osmdata")
library(osmdata)

q <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary", "residential")) %>%
  add_osm_feature(key = "water", value = "river")

osm_data <- osmdata_sf(q)

#### OPENSTREETMAP ####
install.packages("OpenStreetMap")
library(OpenStreetMap)

