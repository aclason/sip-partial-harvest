
library(bcdata)
library(sf)
library(dplyr)
library(data.table)
library(lubridate)
library(terra)

in_dir <- "D:/Spatial Data/RESULTS/RESULTS_openings_swv_Jan12024/RSLT_OPENING_SVW"
out_dir <- "01_cuts"

res_bc <- read_sf(file.path(in_dir,"RSLT_OPNGS_polygon.shp"))
res_bc_dt <- data.table(res_bc)


#get the year of cuts
res_bc_dt[, DN1_year := year(ymd(DN1_CMP_DT))]
res_bc_dt[, DN2_year := year(ymd(DN2_CMP_DT))]
res_bc_dt[,.(DN1_CMP_DT, DN1_year)]


unique(res_bc_dt$DN1_SILSYS)
unique(res_bc_dt$DN2_SILSYS)

#any opening that has never been clear cut/clear cut with reserve - 
#not as straightforward as I hoped - there could be a clearcut, but without areas and more details, 
#it seems that some of these combinations represent gap cuts

cut_types <- c("SELEC", "PATCT", "SHELT", "SEEDT", "RETEN")
#res_bc_dt_part <- res_bc_dt[DN1_SILSYS %in% cut_types | DN2_SILSYS %in% cut_types]
#res_bc_dt_part[is.na(DN1_SILSYS)]
#out_dir <- "D:/Sync/BVRC/SIP program/SIP02/02_partial_harvest/data"
#write_sf(st_as_sf(res_bc_dt_part), file.path(out_dir,"part_cut_bc.gpkg"))


#make the partcut/commercial thin/clearcut label ---------------------------------------------
res_bc_dt[DN1_SILSYS %in% cut_types | DN2_SILSYS %in% cut_types, partcut := "P"]
# grab commercial thins
res_bc_dt[DN1_SILSYS == "IMCUT" | DN2_SILSYS == "IMCUT", partcut := "CT"]
res_bc_dt[is.na(partcut), partcut :="N"]

tsa <- c("Lakes TSA","Bulkley TSA", "Morice TSA")
tsa_bounds <- list()
for(i in 1:length(tsa)){
  tsa_bound <- bcdata::bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980") %>%
    filter(TSA_NUMBER_DESCRIPTION == tsa[i]) %>%
    collect()
  
  #I think this will give the right polygon - but could be glitchy
  #to do test other tsas
  tsa_bounds[[i]] <- tsa_bound %>%
    dplyr::filter(is.na(TSB_NUMBER))
  #st_write(tsa_bound,"./Inputs/ltsa.gpkg")
}
tsa_bounds <- do.call(rbind,tsa_bounds)

res_bc_dt_sf <- st_as_sf(res_bc_dt[!is.na(DN1_SILSYS) & !is.na(DN1_SILSYS),
                                   .(OPENING_ID, G_BGC_ZONE,PREV_SI,
                                     DN1_SILSYS,DN1_year, DN1_CUTPHZ,DN2_SILSYS, DN2_year, 
                                     DN2_CUTPHZ, PREP1_TECH, PREP2_TECH,PREP_COUNT,
                                     PLNT_COUNT,PLNT1_TECH, PLNT2_TECH,BRSH_COUNT,
                                     SPAC_COUNT, FERT_COUNT, PRUN_COUNT, partcut, geometry)])

#intersect with TSA boundary:
cuts_int <- res_bc_dt_sf %>%
  st_intersects(.,tsa_bounds, sparse = TRUE)

cuts_int_rem <- filter(res_bc_dt_sf, lengths(cuts_int) > 0)
#write_sf(cuts_int_rem, "cuts_int_rem.gpkg")

#intersect with recent historic fire - burned since the youngest block (doesn't matter if happened before:
recent_fire <- bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702") %>%
  filter(FIRE_YEAR > 2013) %>%
  collect()

cuts_burned <- cuts_int_rem %>%
  st_intersects(.,recent_fire, sparse = TRUE)

cuts_not_old_burned <- filter(cuts_int_rem, lengths(cuts_burned) == 0)

#most recent fire (not in historic layer yet)
recent_fire <- bcdc_query_geodata("cdfc2d7b-c046-4bf0-90ac-4897232619e1") %>%
  collect()

cuts_cur_burned <- cuts_not_old_burned %>%
  st_intersects(.,recent_fire, sparse = TRUE)

cuts_not_burned <- filter(cuts_not_old_burned, lengths(cuts_cur_burned) == 0)

write_sf(cuts_not_burned, file.path(out_dir, "cuts_not_burned.gpkg"))

#read it in ---------------------------------------------------------------------------------------
cuts_not_burned <- read_sf(file.path(out_dir,"cuts_not_burned.gpkg"))

cuts_int_rem_dt <- data.table(cuts_not_burned)

cuts_int_rem_dt[, oldest_cut := min(DN1_year, DN2_year, na.rm = TRUE),
                by = seq_len(nrow(cuts_int_rem_dt))]


cuts_int_rem_dt[, stand_age := ifelse(oldest_cut > 2013, 1,
                                 ifelse(oldest_cut > 2003, 2,
                                      ifelse(oldest_cut > 1983,3,NA)))]
cuts_int_rem_dt <- cuts_int_rem_dt[!is.na(stand_age)]

table(cuts_int_rem_dt$stand_age, cuts_int_rem_dt$DN1_SILSYS)
table(cuts_int_rem_dt$stand_age, cuts_int_rem_dt$DN2_SILSYS)

table(cuts_int_rem_dt[partcut == "P"]$stand_age, cuts_int_rem_dt[partcut == "P"]$DN1_SILSYS)
table(cuts_int_rem_dt[partcut == "P"]$stand_age, cuts_int_rem_dt[partcut == "P"]$DN2_SILSYS)

cuts_int_rem_dt %>%
  count(stand_age, partcut, DN1_SILSYS) %>%
  tidyr::pivot_wider(names_from = DN1_SILSYS, values_from = n, values_fill = list(n = 0))



#road density -----------------------------------------------------------------------
rds <- read_sf("D:/Spatial Data/Roads/DRA_DGTL_ROAD_ATLAS_MPAR_SP/DRA_MPAR_line.shp")

#rds_int <- st_as_sf(rds) %>%
  #st_intersects(.,tsa_bounds, sparse = TRUE)

#rds_int_rem <- filter(st_as_sf(rds), lengths(rds_int) > 0)

# make raster grid of study area 
sa_bb <- st_bbox(tsa_bounds)

tsa_grid <- st_make_grid(st_as_sfc(sa_bb), n = rep(100, 2))
tsa_grid <- st_sf(tile_id = seq_along(tsa_grid), 
                   geometry = tsa_grid, crs = 3005)
plot(tsa_grid)

#get roads per tile:
rds_grid <- st_intersection(rds, tsa_grid)
rds_km <- rds_grid %>%
  group_by(tile_id) %>%
  summarise(total_length_km = sum(st_length(geometry)) / 1000) %>%
  st_drop_geometry()

#merge with grid to calculate kms of road/km2
tsa_rd_p <- merge(tsa_grid, rds_km, by = "tile_id", 
                     all.x = TRUE) %>%
  mutate(total_length_km = ifelse(is.na(total_length_km), 0, total_length_km)) %>%
  mutate(km2 = unclass(st_area(tsa_grid))/1000000) %>% #use km2 instead of ha?
  mutate(km_km2 = total_length_km/km2)

rast_grid <- rast(ext(tsa_grid), ncols = 100, nrows = 100)
crs(rast_grid) <- crs(tsa_grid)

tsa_rd_grid <- terra::rasterize(tsa_rd_p, rast_grid,
                                "km_km2")
plot(tsa_rd_grid)

#writeRaster(tsa_rd_grid,"tsa_rd_grid.tif", overwrite = TRUE)


# cuts -buffer ----------------------------------------------------
#get a point at the centre of each opening:
cuts_pts <- vect(st_centroid(st_as_sf(cuts_int_rem_dt)))

#write_sf(cuts_buffer,"cuts_buffer.gpkg")
buffer_distances <- c(1000, 5000, 20000, 50000)
rd_km_km2 <- list()
for(i in seq_along(buffer_distances)){
  cuts_buffer <- cuts_pts |> buffer(buffer_distances[i])
  rd_km_km2[[i]] <- extract(tsa_rd_grid, cuts_buffer, fun=mean, na.rm=TRUE)
  
}

cuts_int_rem_dt[, `:=`(rd_dens_1 = rd_km_km2[[1]]$km_km2,
                       rd_dens_5 = rd_km_km2[[2]]$km_km2,
                       rd_dens_20 = rd_km_km2[[3]]$km_km2,
                       rd_dens_50 = rd_km_km2[[4]]$km_km2)]

hist(cuts_int_rem_dt$rd_dens_1)

#using quantiles to bin
breaks_1 <- quantile(cuts_int_rem_dt$rd_dens_1, probs = c(0.25, 0.5, 0.75))
breaks_5 <- quantile(cuts_int_rem_dt$rd_dens_5, probs = c(0.25, 0.5, 0.75))
breaks_20 <- quantile(cuts_int_rem_dt$rd_dens_20, probs = c(0.25, 0.5, 0.75))
breaks_50 <- quantile(cuts_int_rem_dt$rd_dens_50, probs = c(0.25, 0.5, 0.75))


cuts_int_rem_dt[,`:=`(rd_level_1 = ifelse(rd_dens_1 > breaks_1[3], "high",
                                      ifelse(rd_dens_1 > breaks_1[2], "med","low")),
                      rd_level_5 = ifelse(rd_dens_5 > breaks_5[3], "high",
                                           ifelse(rd_dens_5 > breaks_5[2], "med","low")),
                      rd_level_20 = ifelse(rd_dens_20 > breaks_20[3], "high",
                                           ifelse(rd_dens_20 > breaks_20[2], "med","low")),
                      rd_level_50 = ifelse(rd_dens_50 > breaks_50[3], "high",
                                           ifelse(rd_dens_50 > breaks_50[2], "med","low")))]

ggplot(cuts_int_rem_dt)+
  geom_bar(aes(x = rd_level_5))

table(cuts_int_rem_dt[partcut == "P"]$rd_level_50, 
      cuts_int_rem_dt[partcut == "P"]$stand_age)

table(cuts_int_rem_dt[partcut == "P" & SPAC_COUNT != 0]$rd_level_1, 
      cuts_int_rem_dt[partcut == "P" & SPAC_COUNT != 0]$stand_age)

# lots of brushed clearcuts
# not a ton of spaced clearcuts, and no young ones
# lots prepped or not prepped
# not much fertilized

cuts_int_rem_dt[, treat_type := paste0(partcut,"_",stand_age,"_",rd_level_5)]

#get rid of ICH:
cuts_int_rem_dt <- cuts_int_rem_dt[G_BGC_ZONE != "ICH"]

# write it out
write_sf(st_as_sf(cuts_int_rem_dt),file.path(out_dir,"all_pot_polys.gpkg"))
write_sf(st_as_sf(cuts_int_rem_dt[partcut == "P"]),file.path(out_dir,"all_pc.gpkg"))


#exploration:

bcdc_search("RESULTS activities")

bcdc_describe_feature("results-activity-treatment-units")

bcdc_query_geodata('07cabbdf-d7bf-4c50-919d-5b7d80086ef5')|> 
  filter(SILV_BASE_CODE == "EP")


ep_dat <- bcdc_query_geodata('07cabbdf-d7bf-4c50-919d-5b7d80086ef5')|> 
  filter(SILV_BASE_CODE == "EP") |>
  collect()

ep_dat_dt <- data.table(ep_dat)





