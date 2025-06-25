#from all potential polygons, take samples
library(tidyverse)
library(terra)
library(sf)
library(data.table)

in_dir <- "01_cuts"
out_dir <- "02_samples"

#1. get all possible cutblocks----------------------------------------------
all_cuts <- st_read(file.path(in_dir,"all_pot_polys.gpkg")) 

area <- round(unclass(st_area(all_cuts)/10000),2)
all_cuts <- all_cuts %>%
  mutate(area = area)
all_cuts_dt <- as.data.table(all_cuts)


#4. where there are no patch cuts, or reserves explicitly part of the cut (pre-FPC), smaller cutblock size == patch cut
sm_cuts <- all_cuts_dt[oldest_cut < 1995 & area < 2]
write_sf(st_as_sf(sm_cuts), file.path(in_dir,"small_cuts.gpkg"))


#4b. where there are no patches, what about higher retention - 30%?
in_dir <- "D:/Spatial Data/RESULTS/RESULTS_silviculture/RSLT_FOREST_COVER_SILV_SVW"
silv_bc <- st_read(file.path(in_dir,"RSLT_FCSLV_polygon.shp"))
area <- round(unclass(st_area(silv_bc)/10000),2)
silv_bc <- silv_bc %>%
  mutate(area_calc = area) %>%
  select(OPEN_ID, SU_ID, RES_OBJ_CD, PLY_NO, PLY_AR, area_calc)

silv_bc %>% dplyr::filter(OPEN_ID == "1178635")
silv_bc %>% dplyr::filter(OPEN_ID == "1703922")
silv_bc %>% dplyr::filter(OPEN_ID == "6335")

silv_dt <- as.data.table(silv_bc)

prop_R <- silv_dt[, .(
  total_area = sum(area_calc),
  res_area = sum(ifelse(!is.na(RES_OBJ_CD), area_calc, 0))
), by = OPEN_ID]
prop_R[, prop_na := (res_area / total_area)*100]
hist(prop_R$prop_na)

poss_high_ret <- prop_R[prop_na >= 30]

high_ret_cuts <- all_cuts_dt[OPENING_ID %in% poss_high_ret$OPEN_ID]
write_sf(st_as_sf(high_ret_cuts), file.path(in_dir,"high_ret_cuts.gpkg"), append = FALSE)

#SW noralee:
write_sf(st_as_sf(silv_dt[OPEN_ID %in% c(1607244,1711838,1419252,15707,1684599)]),
         "D:/Sync/BVRC/SIP program/SIP02/04_mapping/2024_field_season/field_maps/May27-30/SW_Noralee_silv.gpkg")
write_sf(st_as_sf(silv_dt[OPEN_ID %in% c(35602,-581850000,15515,49320,15594,
                                         1719476,16108,-514970000)]),
         "D:/Sync/BVRC/SIP program/SIP02/04_mapping/2024_field_season/field_maps/May27-30/N_Noralee_silv.gpkg")
write_sf(st_as_sf(silv_dt[OPEN_ID %in% c(35602,98774,95196,103073,1726502,1726500,1726503)]),
         "D:/Sync/BVRC/SIP program/SIP02/04_mapping/2024_field_season/field_maps/May27-30/Noralee_to_Houston_silv.gpkg")
write_sf(st_as_sf(silv_dt[OPEN_ID %in% c(67844,1682223,1464311,78259,1118895,1650013,
                                         1695482,1665129,1073470)]),
         "D:/Sync/BVRC/SIP program/SIP02/04_mapping/2024_field_season/field_maps/June4-5/Morice_silv.gpkg")

write_sf(st_as_sf(silv_dt[OPEN_ID %in% c(15091,57515,15155,56876,1685494)]),
         "D:/Sync/BVRC/SIP program/SIP02/04_mapping/2024_field_season/field_maps/June10-11/Jonas_silv.gpkg")

write_sf(st_as_sf(silv_dt[OPEN_ID %in% c(46523,1759582,1729035,1385392)]),
         "D:/Sync/BVRC/SIP program/SIP02/04_mapping/2024_field_season/field_maps/June10-11/Fulton_silv.gpkg")

write_sf(st_as_sf(silv_dt[OPEN_ID %in% c(1745859,1251487,1441855,-50060000)]),
         "D:/Sync/BVRC/SIP program/SIP02/04_mapping/2024_field_season/field_maps/June12-13/Deep_silv.gpkg")

#and getting some dispersed retention over time:
in_dir <- "D:/Spatial Data/RESULTS/RESULTS_silviculture_reserve/RSLT_FOREST_COVER_RESERVE_SVW"
silv_rfc_bc <- st_read(file.path(in_dir,"RSLT_FCRES_polygon.shp"))

disp_res <- silv_rfc_bc %>% filter(RES_CD == "D")
all_disp_res <- all_cuts_dt[OPENING_ID %in% disp_res$OPEN_ID]
write_sf(st_as_sf(all_disp_res), file.path(in_dir,"disp_ret_cuts.gpkg"), append = FALSE)


sel_polys <- read_sf("D:/Sync/BVRC/SIP program/SIP02/2024 field season/Field_maps/May21-23/selected_polys.gpkg")
poly_ids <- sel_polys$OPENING_ID

silv_sel_polys <- silv_bc %>% filter(OPEN_ID %in% poly_ids)
write_sf(silv_sel_polys, "D:/Sync/BVRC/SIP program/SIP02/2024 field season/Field_maps/May21-23/silv_sel_polys.gpkg")


#what about uneven management - it seems we miss harvest types when there's more than 2 in RESULTS, so 
# lokoing to see if we can get at this through the silv layer
silv_uneven <- silv_dt[FC_SILV_TP == "UNEVEN"]
dim(silv_uneven)
silv_uneven[OPEN_ID == "1781104"]

all_cuts_dt[OPENING_ID %in% silv_uneven$OPEN_ID] #this doesn't look right.


#trying activities:
in_dir <- "D:/Spatial Data/RESULTS/RESULTS_activities_Jan 1 2024/RSLT_ACTIVITY_TREATMENT_SVW"
act_bc <- st_read(file.path(in_dir,"RSLT_ACTRT_polygon.shp"))
act_bc_dt <- data.table(act_bc)
act_bc_dt[OPEN_ID == "1710459"]


act_bc_dt_sa <- act_bc_dt[OPEN_ID %in% all_cuts_dt$OPENING_ID]
cut_types <- c("SELEC", "PATCT", "SHELT", "SEEDT", "RETEN")
act_bc_dt_sa[SYS_CD %in% cut_types | SYS_CD %in% cut_types, partcut := "P"]

unique(act_bc_dt_sa[partcut == "P"]$OPEN_ID) %in% unique(all_cuts_dt[partcut == "P"]$OPENING_ID)
miss_pcuts <- setdiff(unique(act_bc_dt_sa[partcut == "P"]$OPEN_ID),
        unique(all_cuts_dt[partcut == "P"]$OPENING_ID))

all_cuts_dt[OPENING_ID %in% miss_pcuts]
#2. bring in the possible field work areas and clip? ------------------------
poss_area_polys <- list.files(file.path(in_dir,"01a_poss_areas"), pattern = ".shp",
                         full.names = TRUE)
poss_area_names <- str_split(list.files(file.path(in_dir,"01a_poss_areas"), pattern = ".shp",
                              full.names = FALSE),".shp", simplify = TRUE)[,1]

poss_areas <- poss_area_polys %>%
  map2(poss_area_names, ~read_sf(.x) %>%
         mutate(area_name = .y)) %>%
  map(~st_transform(., crs = 3005)) %>%
  map(~select(., area_name, geometry)) %>%
  reduce(rbind)

polys_clip_poss <- st_join(all_cuts, poss_areas)


polys_clip_poss_sub <- polys_clip_poss %>%
  filter(., !is.na(area_name))
area <- round(unclass(st_area(polys_clip_poss_sub)/10000),2)

polys_clip_poss_sub <- polys_clip_poss_sub %>%
  mutate(area = area)

write_sf(polys_clip_poss_sub, file.path(in_dir,"polys_clip_poss_sub.gpkg"))
write_sf(polys_clip_poss_sub, file.path(in_dir,"polys_clip_poss_sub.kml"))

polys_poss_dt <- as.data.table(polys_clip_poss_sub)


#2. just for all partial cuts:
#get detailed spatial
pcut_id <- all_cuts_dt[partcut == "P"]$OPENING_ID

all_pcuts <- all_cuts %>%
  filter(OPENING_ID %in% pcut_id)
st_write(all_pcuts, file.path(out_dir, "all_pcuts.gpkg"))

#get the forest cover layer - silviculture
in_dir <- "D:/Spatial Data/RESULTS/RESULTS_silviculture/RSLT_FOREST_COVER_SILV_SVW"
silv_bc <- st_read(file.path(in_dir,"RSLT_FCSLV_polygon.shp"))
sil_lay <- silv_bc %>% 
  filter(OPEN_ID %in% pcut_id) %>%
  select(OPEN_ID, PLY_NO, SU_ID)


#get the forest cover layer - reserves
in_dir <- "D:/Spatial Data/RESULTS/RESULTS_silviculture_reserve/RSLT_FOREST_COVER_RESERVE_SVW"
silv_rfc_bc <- st_read(file.path(in_dir,"RSLT_FCRES_polygon.shp"))
res_lay <- silv_rfc_bc %>% 
  filter(OPEN_ID %in% pcut_id) %>%
  select(OPEN_ID, PLY_NO) %>%
  mutate(SU_ID = "NA")

silv_res <- rbind(sil_lay, res_lay)

st_write(silv_res, file.path(out_dir, "pc_silv_res.kml"))

pcuts <- all_cuts_dt[partcut == "P"]
pcuts_short <- pcuts[,.(OPENING_ID, DN1_SILSYS, DN1_year, DN2_SILSYS, 
                        DN2_year, PREP1_TECH, PREP2_TECH,
                        PLNT_COUNT, PLNT1_TECH, PLNT2_TECH, BRSH_COUNT,
                        SPAC_COUNT, FERT_COUNT)]

write.csv(pcuts_short, file.path(out_dir,"pcuts_short.csv"))
split_data <- split(polys_clip_poss_sub, polys_clip_poss_sub$treat_type)
split_data <- map(split_data, ~split(.x, .x$area_name))

# Write out each subset as a separate GeoPackage
lapply(names(split_data), function(t) {
  write_sf(split_data[[t]], file.path(in_dir, paste0("poss_", t, ".gpkg")))
})
lapply(names(split_data), function(t) {
  write_sf(split_data[[t]], file.path(in_dir, paste0("poss_", t, ".kml")))
})

# recce day:
recce <- st_read("D:/Sync/BVRC/SIP program/SIP02/2024 field season/Waypoints_07-MAY-24.gpx", 
        layer = "waypoints")
write_sf(recce, file.path(out_dir,"recce.kml"))

#3. start selection --------------------------------------------------------
#I want to make sure the treatments are spread out geographically (don't let random select all 2s in one area)

#treatments of interest for random sampling:
int_treats <- c(paste0("P","_",rep(seq(1,3),3),"_",c(rep("low",3),rep("med",3),rep("high",3))))

table(all_cuts_dt[treat_type %in% int_treats]$treat_type)
table(polys_poss_dt[treat_type %in% int_treats]$treat_type)

#so lacking in P2-low - where are they?
polys_poss_dt[treat_type == "P_2_low"] #all in the morice, one on s-side of Shelford hills -
#pick up on way to whitesail? So just randomly choose 2 from the Morice clump?

polys_poss_dt[area_name == "wetzinkwa_area" & partcut =="P"]
numSamples <- 6

wa_out <- polys_poss_dt[area_name == "wetzinkwa_area" & treat_type %in% int_treats,
                          .SD[sample(.N, min(numSamples,.N))],
                          by = treat_type]
st_write(wa_out, file.path(out_dir,"wz_selection.gpkg"), append = FALSE)

#
#for sure want the P_2 from here?

# cutblocks must be >2h in size (== FREP protocols)
#FREP only happens 1 - 3years post-harvest
#landscape context - get the GIS protocol to answer questions 1-4


#random sample
numSamples <- 30

#treatments of interest for random sampling:
int_treats <- c(paste0("N","_",rep(seq(1,3),3),"_",c(rep("low",3),rep("med",3),rep("high",3))),
                paste0("P","_",rep(seq(1,3),3),"_",c(rep("low",3),rep("med",3),rep("high",3))))

table(all_cuts_dt[treat_type %in% int_treats]$treat_type)
table(polys_clip_poss_dt[treat_type %in% int_treats]$treat_type)
ss_out <- cuts_int_rem_dt[treat_type %in% int_treats,
                          .SD[sample(.N, min(numSamples,.N))],
                          by = treat_type]
#
#so we get tight with P2-low, so could expand if needed. P3-med is also a bit tighter than it needs to be.
#I would maybe add southside before lake babine area....southside looks like it will pull in 
#the P3-med a bit better

#draw sample
ss_out <- cuts_int_rem_dt[treat_type %in% int_treats,
                          .SD[sample(.N, min(numSamples,.N))],
                          by = treat_type]
table(ss_out$treat_type)

#make sure sites are at least 1 km away?
ss_shp <- vect(sf::st_as_sf(ss_out))
distances <- as.matrix(terra::distance(ss_shp))
diag(distances) <- NA

ss_out[ , minDist := apply(distances, 2, min, na.rm = TRUE)/1000]

ss_out_mins <- ss_out[minDist > 1] #minimum distance is really going to vary by species
table(ss_out_mins$treat_type)


write_sf(ss_sf,file.path(out_dir,"samps_1.gpkg"))


# for the amount of young forest - use the 2023 vri - age raster and do similar to roads?

# forest cover after harvest:
fc_ss <- merge(ss_out_mins %>% select(-geom), res_bc_silv[,.(OPEN_ID, S_CC_PCT, 
                                                             S_BSL_AR, CVR_PAT_CD, REF_YR, S1_BSL_AR,
                                                             S1_TSP_HA, S2_TSP_HA,S3_TSP_HA,S4_TSP_HA)],
               by.x = "OPENING_ID", by.y = "OPEN_ID", all.x = TRUE)
fc_ss <- unique(fc_ss)
fc_ss[partcut == "P" & !is.na(CVR_PAT_CD )]

table(fc_ss$S3_TSP_HA)
#there's one retention harvest with 40 m2 basal area retained:
unique(fc_ss$S1_BSL_AR)
fc_ss[S1_BSL_AR == 40]
# reserves

fcr_ss <- merge(fc_ss, fcr_bc_silv[,.(OPEN_ID,PLY_AR,RES_CD,RES_OBJ_CD)],
                by.x = "OPENING_ID", by.y = "OPEN_ID", all.x = TRUE)
fcr_ss <- unique(fcr_ss)
fcr_ss[partcut == "P"]
# reserve code: G = group, D = dispersed. Grouped = mappable > 0.25 ha

#reserve objective:
table(fcr_ss$RES_OBJ_CD)


#I'll have to remove sites from partial treatment where reserve was for timber and then removed

select_polys <- merge(fcr_ss, ss_out_mins[,.(OPENING_ID, geom)], by = "OPENING_ID")
write_sf(st_as_sf(select_polys),file.path(out_dir,"samps_2.gpkg"))

