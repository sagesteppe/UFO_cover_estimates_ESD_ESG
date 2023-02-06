library(tidyverse)
library(sf)
library(terra)

setwd('/media/sagesteppe/ExternalHD/UFO_cover_estimates_ESD_ESG/scripts')
praw <- '../data/raw'
f <- list.files(praw, recursive = T)
mode <- function(codes){as.numeric(which.max(tabulate(codes)))}

esg <- rast(file.path(praw, f[grep('ESGs.*tif$',f)]))

ecosites <- read.csv(file.path(praw, f[grep('Plot_Tracking',f)] )) %>% 
  filter(ECO.SITE.MATCHED == F) %>% 
  select(PLOT.ID)

plot_coords <- st_read('/media/sagesteppe/ExternalHD/UFO_cartography/AIM_plots_outcomes/plots_outcomes.shp', 
                    quiet = T) %>% # let us grab the spatial data from this real quick
  filter(Plt_Stt == 'sampled') %>% 
  select(PLOT.ID = Plot_ID) %>% 
  st_transform(26912) %>% 
  st_buffer(27.5) 

plots <- inner_join(ecosites, plot_coords, by = 'PLOT.ID') %>% 
  st_as_sf() %>% 
  vect() %>% 
  terra::project(., crs(esg))

rm(ecosites, plot_coords, esg)

sgu1  <- rast( file.path(praw, f[grep('SGU_1st.*tif$',f)]) )
sgu2  <- rast( file.path(praw, f[grep('SGU_2nd.*tif$',f)]) )
sgu3  <- rast( file.path(praw, f[grep('SGU_3rd.*tif$',f)]) )

c_zones <- rast( file.path(praw, f[grep('Zones.*tif$', f)]) )
cz_lkp <- data.frame(levels(c_zones)) 

sgu1_ex <- extract(sgu1, plots, method = 'simple') %>% 
  group_by(ID) %>% summarise(SGU1 = mode(SGU)) 
sgu2 <- extract(sgu2, plots, method = 'simple') %>% 
  group_by(ID) %>% summarise(SGU2 = mode(SGU)) 
sgu3 <- extract(sgu3, plots, method = 'simple') %>% 
  group_by(ID) %>%  summarise(SGU3 = mode(SGU)) 
sgu <- data.frame(sgu1_ex, 'SGU2' =  sgu2$SGU2, 'SGU3' = sgu3$SGU3, 
                  st_as_sf(plots) %>% select(PLOT.ID)) %>% 
  st_drop_geometry() %>% select(-geometry, -ID)

rm(sgu1_ex, sgu2, sgu3, f)

sgu <- sgu %>% 
  pivot_longer(!PLOT.ID, names_to = 'SGU', values_to = 'layer') %>% 
  group_by(PLOT.ID) %>% 
  add_count(layer) %>% 
  slice_max(n) %>% 
  distinct(PLOT.ID, layer, n, .keep_all = T) 

clean <- sgu %>% # 19 records with some degree of consensus
  filter(n >= 2)
sgu <- sgu %>% 
  filter(n == 1, SGU == 'SGU1') %>% 
  bind_rows(clean, .) %>% 
  select(PLOT.ID, Value = layer) %>% 
  mutate(Value = as.character(Value))

rm(clean)
  
sguvals <- data.frame(levels(sgu1)) %>% 
  mutate(Value = as.character(Value))

clim_zones <- extract(c_zones, plots, method = 'simple') %>% 
  group_by(ID) %>% summarise(ClimZone = mode(ClimZone)) %>% # mode of original input dataset
  left_join(., cz_lkp, by = c('ClimZone' = 'Value')) %>% 
  select(ID, ClimZone = ClimZone.y)  %>% 
  cbind(., st_as_sf(plots)) %>% 
  mutate(
    ClimZone = str_replace(ClimZone, 'Arid Warm', 'AW'), 
    ClimZone = str_replace(ClimZone, 'Semiarid Warm', 'SAW'),
    ClimZone = str_replace(ClimZone, 'Semiarid Cool', 'SAC')
  ) 

rm(c_zones, cz_lkp, sgu1, mode, plots)

sgu <- left_join(sgu, sguvals, by = 'Value') %>% 
  left_join(., clim_zones, by = 'PLOT.ID') %>% 
  unite('ESG', ClimZone, SGU, sep = '_-_') %>% 
  mutate(
    ESG = str_replace_all(ESG, ' ', "_"), 
    ESG = str_replace(ESG, ',_', '_and_'), 
    ESG = str_replace_all(ESG, ' ', '_'), 
    ESG = str_replace(ESG, 'SAW_-_Deep_Rocky', 'SAW_-_Shallow_and_Deep_Rocky'),
    ESG = str_replace(ESG, 'SAW_-_Loamy_Uplands', 'SAW_-_Sandy_Uplands_and_Loamy_Uplands')
    )

rm(clim_zones, sguvals)
