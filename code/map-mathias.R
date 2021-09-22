library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

states <- map_data("state")
west_coast <- subset(states, region %in% c("california"))
sites <- tribble(
  ~name, ~lat, ~long, ~color, ~shape,
  "Bodega Marine Reserve", 38.318757561054554, -123.07218556931106, "red", "0",
  "LOL", 34.7074307110824, -120.60051345263794, "mediumblue", "0",
  "ALG", 34.46907703880511, -120.27191157990546, "mediumblue", "5",
  "Coal Oil Point Reserve", 34.4081, -119.8780, "red", "5"
)

ggplot() + 
  geom_rect(data=data.frame(ymin = 34.4486, ymax = 42, xmin = -Inf, xmax = Inf),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = "coral", alpha = 0.1) +
  geom_rect(data=data.frame(ymax = 34.4486, ymin = 32.5, xmin = -Inf, xmax = Inf),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = "cadetblue1", alpha = 0.2) +
  geom_polygon(data=west_coast, aes(x = long, y = lat, group = group), fill = "grey", color = "gray42") + 
  coord_fixed(1.3) +
  xlim(-127,-114) +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_point(data=sites, aes(x=long, y=lat, color=color, size=4)) +
  geom_text(data=sites, aes(x=long, y=lat, label=name), size=6,
            hjust=ifelse(sites$name=='ALG',1.25, 
                         ifelse(sites$name=='Coal Oil Point Reserve',-0.05, 
                                ifelse(sites$name=='Bodega Marine Reserve',-0.05, 1.25))), 
            vjust=ifelse(sites$name=='ALG',1.25,
                         ifelse(sites$name=='Coal Oil Point Reserve',-1, 
                                ifelse(sites$name=='Bodega Marine Reserve',-0.1, -0.5)))) + 
  geom_text(aes(label="Strong upwelling", y=34.7, x=-126.2)) +
  geom_text(aes(label="Weak upwelling", y=34.2, x=-126.2)) +
  theme_minimal() +
  theme(legend.position="none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))

ggsave("figures/fig2.png", width=6, height=10)

