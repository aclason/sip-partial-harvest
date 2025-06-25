library(bcdata)
library(sf)
library(dplyr)
library(data.table)
library(lubridate)
library(terra)

in_dir <- "D:/Spatial Data/RESULTS/RESULTS_openings_swv_Jan12024/RSLT_OPENING_SVW"
out_dir <- "03_sites"

res_bc <- read_sf(file.path(in_dir,"RSLT_OPNGS_polygon.shp"))
res_bc_dt <- data.table(res_bc)

mv_sites <- fread(file.path("data","Site Tracking 2024 - Site histories.csv"), 
                  na.strings = "n/a")

mv_sites <- merge(mv_sites, res_bc_dt, by.x = "Opening ID", by.y = "OPENING_ID")
mv_sites <- mv_sites[, geometry := NULL]

fwrite(mv_sites, file = file.path(out_dir,"mv_site_results.csv"))
