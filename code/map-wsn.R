library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

states <- map_data("state")
west_coast <- subset(states, region %in% c("california"))
sites <- tribble(
  ~name, ~lat, ~long, ~color,
  "Bodega Bay", 38.318757561054554, -123.07218556931106, "mediumblue",
  "Lompoc Landing", 34.7074307110824, -120.60051345263794, "mediumblue",
  "Alegria", 34.46907703880511, -120.27191157990546, "mediumblue", 
)

ggplot() + 
  geom_polygon(data=west_coast, aes(x = long, y = lat, group = group), fill = "grey", color = "gray42") + 
  coord_fixed(1.3) +
  xlim(-127,-114) +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_point(data=sites, aes(x=long, y=lat, color=name, size=6)) +
  scale_size_continuous(range = c(7, 7)) +
  scale_color_manual(values=c("#CC6677", "#6699CC", "#44AA99")) +
  geom_text(data=sites, aes(x=long, y=lat, label=name), size=6,
            hjust=ifelse(sites$name=='Alegria',1.25,
                         ifelse(sites$name=='Bodega Bay',-0.1, 1.05)), 
            vjust=ifelse(sites$name=='Alegria',1.25,
                         ifelse(sites$name=='Bodega Bay',-0.5, -0.7))) + 
  theme_minimal() +
  theme(legend.position="none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))

ggsave("figures/wsn-map.png", width=8, height=10)

