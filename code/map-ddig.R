
library(tidyverse)
library(sf)
library(tmap)

gps <- tribble(
  ~site,    ~latitude,   ~longitude,
  "Sokol Point",       47.964496,        -124.663549, 
  "Fogarty Creek",        44.839689,        -124.048811,
  "Strawberry Hill",   44.254589,        -124.111807,        
  "Van Damme State Park",     39.272849,        -123.792974,        
  "Bodega Marine Reserve",     38.318267,        -123.072199, 
  "Terrace Point",   37.030939,        -122.056166,        
  "Coal Oil Point",     34.407642,        -119.878323,        
  "Scripps Reef",     32.866401,        -117.254508,  
  "Cabrillo III", 32.668511, -117.244511
)

coord_gps <- st_bbox(gps %>%
                       st_as_sf(coords=c("longitude", "latitude"), crs=4326))

gps_sf <- gps %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)  #create sticky geometry for lat/long

tmap_mode("plot") #view = interactive
#create index map
tm_basemap("Esri.WorldImagery") +
  tm_shape(gps_sf, bbox = coord_gps) +
  tm_symbols(id="site", col="red", size=0.5) +
  tm_scale_bar(just="left")


growth <- tribble(
  ~`Origin site`, ~site_transplant,    ~growth,
  "VD",       "VD",        10, 
  "VD",        "BMR",        8,
  "VD",       "SR",        14, 
  "VD",        "CB3",        12,
  
  "BMR",       "VD",        8, 
  "BMR",        "BMR",        10,
  "BMR",       "SR",        13, 
  "BMR",        "CB3",        11,
  
  "SR",       "VD",        3, 
  "SR",        "BMR",        1,
  "SR",       "SR",        18, 
  "SR",        "CB3",       16,
  
  "CB3",       "VD",        4, 
  "CB3",        "BMR",        2,
  "CB3",       "SR",        16, 
  "CB3",        "CB3",       18,
  
)

growth_tidy <- growth %>% 
  mutate(site_trans = fct_relevel(site_transplant, "VD", "BMR", "SR", "CB3"))

ggplot(growth_tidy, aes(x=site_trans, y=growth, group=`Origin site`)) +
  geom_point(size=4, aes(color=`Origin site`, shape=`Origin site`)) +
  #geom_line(aes(color=site_origin)) +
  geom_segment(x=1, xend=2, y=4, yend=2, color="#7CAE00", linetype="dashed") + #CB3
  geom_segment(x=1, xend=2, y=3, yend=1, color="#00BFC4", linetype="dashed") + #SR
  geom_segment(x=1, xend=2, y=8, yend=10, color="#F8766D", linetype="dashed") + #BMR
  geom_segment(x=1, xend=2, y=10, yend=8, color="#C77CFF", linetype="dashed") + #VD
  
  geom_segment(x=3, xend=4, y=16, yend=18, color="#7CAE00", linetype="dotdash") + #CB3
  geom_segment(x=3, xend=4, y=18, yend=16, color="#00BFC4", linetype="dotdash") + #SR
  geom_segment(x=3, xend=4, y=13, yend=11, color="#F8766D", linetype="dotdash") + #BMR
  geom_segment(x=3, xend=4, y=14, yend=12, color="#C77CFF", linetype="dotdash") + #VD
  labs(x="Transplant site", y="Shell growth") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#BMR should have highest fitness at BMR b/c local adaptation
#CB3 should have highest fitness at CB3 b/c local adaptation