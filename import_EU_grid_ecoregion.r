library(sf)
library(raster)
library(exactextractr)
library(magrittr)
library(dplyr)

# Import EU grid and Biographical regions shapefiles

eu_grid <- st_read("data/europe_10km.shp") %>% st_set_crs(3035)
bio_reg <- st_read("data/BiogeoRegions2016.shp") %>% st_set_crs(3035)

# First feature of EBRs is alpine, as coverage_fraction() assigns empty cells to the first feature rasterized
# we need to process the Alpine BR separately

alp_reg <- bio_reg[bio_reg$short_name == "alpine", ]

# Empty raster necessary for coverage_fraction()

r <- raster(ext = extent(eu_grid), crs = crs(eu_grid), res = 10000)

# Rasterize the EBRs by weighting the identity of a cell on the area of the features therein

bio_reg_lst <- coverage_fraction(r, bio_reg)
bio_reg_st <- raster::stack(bio_reg_lst)
bio_reg_eu <- bio_reg_st[[c(1:9, 11:12)]] # by rasterizing "outside" separately we can assign every cells on the outer boundary to the EBR with largest extent therein
bio_reg_rast <- calc(bio_reg_eu, which.max)
values(bio_reg_rast)[values(bio_reg_rast) == 11] <- 12
values(bio_reg_rast)[values(bio_reg_rast) == 10] <- 11
sea_mask <- calc(bio_reg_st, function(x, ...) {all(x == 0)}) # correcting the sea values by using NAs values instead of first feature (i.e. Alpine BR)
values(bio_reg_rast)[values(sea_mask) == 1] <- NA
outside_mask <- calc(bio_reg_st[[10]], function(x, ...) x > 0)
alp_reg_lst <- coverage_fraction(r, alp_reg)
values(outside_mask)[values(alp_reg_lst[[1]]) > 0] <- FALSE
values(bio_reg_rast)[values(bio_reg_rast) == 1 & values(outside_mask) == TRUE] <- 10

# Convert the raster file to a table format where each row corresponds to the centroid of a cell
# In columns cells ID (according to European grid), centroids of the cell (EPSG: 3035) and 
# biogeographical region

bio_reg_pnts <- rasterToPoints(bio_reg_rast) %>% 
  as.data.frame() %>% 
  st_as_sf(., coords = c("x", "y")) %>% 
  st_set_crs(3035)
bio_reg_pnts <- st_join(bio_reg_pnts, eu_grid)
lkp_tab <- data.frame(layer = 1:12, biogeographical_region = c("Alpine", "Anatolian", "Arctic", "Atlantic",
                                                               "Black Sea", "Boreal", "Continental", "Macaronesian",
                                                               "Mediterranean", "Outside", "Pannonian", "Steppic"))
bio_reg_pnts <- left_join(bio_reg_pnts, lkp_tab)
bio_reg_tab <- cbind.data.frame(st_drop_geometry(bio_reg_pnts[, c(2, 5)]), 
                 st_coordinates(bio_reg_pnts))
