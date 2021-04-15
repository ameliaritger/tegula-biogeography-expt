# these are packages you will need, but probably already have.
# Don't bother installing if you already have them
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

states <- map_data("state")
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))
sites <- tribble(
  ~name, ~lat, ~long, ~color, ~shape,
  "SP", 47.965399673237194, -124.66343921807504, "red", "0",
  "FC", 44.83964859735996, -124.05205638812974, "red", "0",
  "BMR", 38.318757561054554, -123.07218556931106, "mediumblue", "0",
  "LL", 34.7074307110824, -120.60051345263794, "mediumblue", "0",
  "AL", 34.46907703880511, -120.27191157990546, "mediumblue", "5",
  "CNM", 32.66854992729292, -117.24431377177557, "red", "0"
)

ggplot() + 
  geom_polygon(data=west_coast, aes(x = long, y = lat, group = group), fill = "grey", color = "black") + 
  coord_fixed(1.3) +
  xlim(-127,-114) +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_point(data=sites, aes(x=long, y=lat, color=color, shape=shape), size=5) +
  geom_text(data=sites, aes(x=long, y=lat, label=name), size=6,
            hjust=ifelse(sites$name=='AL',1,1.25), 
            vjust=ifelse(sites$name=='AL',1.5,0)) +
  theme_minimal() +
  theme(legend.position="none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))
