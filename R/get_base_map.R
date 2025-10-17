################################################################################
##' @title Get a simple base map
##' @author Robin Elahi
##' @date 2021-10-04
##' @log 
################################################################################



get_basemap <- function(x, latitude = "lat", longitude = "long", high_res = FALSE, 
                        range_extension = 0.1){
  
  library(mapdata)
  library(tidyverse)
  
  SI_LONG = x[, longitude]
  SI_LATI = x[, latitude]
  
  # Get latitude and longitude range limits
  rangeLat <- range(SI_LATI) + c(-range_extension, range_extension)
  rangeLong <- range(SI_LONG) + c(-range_extension, range_extension)
  
  ## Use fortify
  if(high_res == TRUE){
    coast_map <- map_data("worldHires", xlim = rangeLong, ylim = rangeLat)
  }
  
  if(high_res == FALSE){
    coast_map <- map_data("world", xlim = rangeLong, ylim = rangeLat)
  }
  
  return(coast_map)
  
}

## 
get_basemap_general <- function(x, latitude = "lat", longitude = "long", 
                                 range_extension = 0.1, my_color = "black", 
                                 my_fill = "gray", high_res = TRUE){
  
  library(mapdata)
  library(tidyverse)
  
  SI_LONG = x[, longitude]
  SI_LATI = x[, latitude]
  
  # Get latitude and longitude range limits
  rangeLat <- range(SI_LATI) + c(-range_extension, range_extension)
  rangeLong <- range(SI_LONG) + c(-range_extension, range_extension)
  
  ## Use fortify
  if(high_res == TRUE){
    coast_map <- fortify(map("worldHires", fill = TRUE, 
                             xlim = rangeLong, ylim = rangeLat, 
                             plot = FALSE)) 
  }

  if(high_res == FALSE){
    coast_map <- fortify(map("world", fill = TRUE, 
                             xlim = rangeLong, ylim = rangeLat, 
                             plot = FALSE)) 
  }
  
  map1 <- ggplot(coast_map, aes(long, lat)) + 
    geom_map(map = coast_map, aes(map_id = region), 
             color = my_color, fill = my_fill, size = 0.25) + 
    coord_fixed() + 
    labs(x = "Longitude", y = "Latitude") + 
    scale_x_continuous(limits = rangeLong) + 
    scale_y_continuous(limits = rangeLat) + 
    theme(panel.grid = element_blank())
  
  # Return base map
  map1 
}


