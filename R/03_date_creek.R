
library(sf)
library(dplyr)

in_dir <- "D:/Sync/BVRC/SIP program/SIP02/04_mapping/date_creek"

dc_cams <- read.csv(file.path(in_dir, "Date_creek_camera_locs.csv"))

dc_cams_sf <- st_as_sf(dc_cams, coords = c("Longitude", "Latitude"), crs = 4269)

plot(dc_cams_sf$geometry)

write_sf(dc_cams_sf, file.path(in_dir,"Date_creek_cam_locs.gpkg"))
