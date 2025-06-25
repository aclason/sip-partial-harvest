library(sf)
library(data.table)
library(dplyr)
library(purrr)

in_dir <- "data"
out_dir <- "03_sites"

dat <- fread(file.path(in_dir, "Install_data.csv"))

dat_min <- dat[,.(SiteID, Year_harvest, TYPE, Block_Descr,Latitude, 
                  Longitude = -Longitude, Elevation)]

dat_sf <- st_as_sf(dat_min, coords = c("Longitude","Latitude"), crs = 4326)
dat_sf <- st_transform(dat_sf, crs = 3005)
write_sf(dat_sf, file.path(out_dir, "install_locations.gpkg"))










dat_loc <- "D:/Github/sip-partial-harvest/data/locations-download-June9/May 11th-31st"
May11_31 <- list.files(dat_loc, full.names = TRUE)

dat_loc <- "D:/Github/sip-partial-harvest/data/locations-download-June9/June 1st-30th"
June1_30 <- list.files(dat_loc, full.names = TRUE)

may_pts <- map(May11_31, ~ read_sf(.x) %>% select(name, geometry)) %>%
  bind_rows()%>%
  filter(stringr::str_starts(name, "MV")) %>%
  arrange(name)

June_pts <- map(June1_30, ~ read_sf(.x) %>% select(name, geometry)) %>%
  bind_rows()%>%
  filter(stringr::str_starts(name, "MV")) %>%
  arrange(name)

may_june_pts <- bind_rows(may_pts, June_pts)

may_june_pts$name

#get missing file names from Avenza files:
dat_loc <- "D:/Github/sip-partial-harvest/data/Avenza plot locations compiled"
kml_files <- list.files(dat_loc, pattern = "\\.kml$", full.names = TRUE)
# rename:
#mv_files <- kml_files[grep("Mv", basename(kml_files))]
#new_filenames <- gsub("Mv", "MV", mv_files)
#file.rename(mv_files, new_filenames)
kml_files <- list.files(dat_loc, pattern = "\\.kml$", full.names = TRUE)
miss_files <- grep("MV07|MV09|MV10|MV11|MV12|MV62|MV63", kml_files, value = T)

miss_pts <- map(miss_files, ~ read_sf(.x) %>% select(Name, geometry)) %>%
  bind_rows()%>%
  st_zm(., drop=TRUE)%>%
  filter(stringr::str_starts(tolower(Name), "mv")) %>%
  mutate(Name = stringr::str_replace(Name, "^Mv", "MV")) %>%
  arrange(Name)%>%
  rename(name = Name)

may_june_pts <- bind_rows(may_june_pts, miss_pts)
may_june_pts <- may_june_pts %>%
  st_transform(crs = 3005)

dat_loc <- "D:/Github/sip-partial-harvest/data/Plot locations/"
all_plots <- list.files(dat_loc, full.names = TRUE)
all_plots_kml <- grep(".kml",all_plots, value = TRUE)      
miss_plot_pts <- map(all_plots_kml, ~ read_sf(.x) %>% select(Name, geometry)) %>%
  bind_rows()%>%
  st_zm(., drop=TRUE)%>%
  filter(grepl("_r",Name))%>%
  mutate(Name = stringr::str_replace(Name, "_retake", ""))%>%
  st_transform(crs = 3005)%>%
  rename(name = Name)

#drop 11, 12, 62, 63, 64
may_june_pts <- may_june_pts %>%
  filter(!name %in% c("MV11","MV12","MV62","MV63","MV64"))
  
#add them from the manual entries:
combo_pts <- bind_rows(may_june_pts,miss_plot_pts) %>%
  arrange(name)
combo_pts$name


write_sf(may_june_pts, "D:/Github/sip-partial-harvest/data/install_pts.gpkg", append = FALSE)



may_pts_not_mv <- map(May11_31, ~ read_sf(.x) %>% select(name, geometry)) %>%
  bind_rows()%>%
  filter(stringr::str_starts(name, "MV", negate = TRUE))

write_sf(may_pts_not_mv, "D:/Github/sip-partial-harvest/data/not_mv_pts.gpkg")

# Plot locations
dat_loc <- "D:/Github/sip-partial-harvest/data/Plot locations/"
all_plots <- list.files(dat_loc, full.names = TRUE)
all_plots <- all_plots[2:length(all_plots)]
all_plots_gpx <- grep(".gpx",all_plots, value = TRUE)

plot_pts <- map(all_plots_gpx, ~ read_sf(.x) %>% select(name, geometry)) %>%
  bind_rows()%>%
  st_zm(., drop=TRUE)%>%
  filter(stringr::str_starts(tolower(name), "mv"))%>%
  #filter(stringr::str_starts(name, "MV")) %>%
  arrange(name)
  

  plot_pts$name  

 
  
  
