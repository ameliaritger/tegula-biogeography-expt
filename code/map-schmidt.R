library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

states <- map_data("state")
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))
sites <- tribble(
  ~name, ~lat, ~long, ~color, ~shape,
  "SP", 47.965399673237194, -124.66343921807504, "#D55E00", "0",
  "FC", 44.83964859735996, -124.05205638812974, "#D55E00", "0",
  "BMR", 38.318757561054554, -123.07218556931106, "#0072B2", "5",
  "LOL", 34.7074307110824, -120.60051345263794, "#0072B2", "5",
  "ALG", 34.46907703880511, -120.27191157990546, "#0072B2", "5",
  "CNM", 32.66854992729292, -117.24431377177557, "#D55E00", "0"
)

ggplot() + 
  geom_polygon(data=west_coast, aes(x = long, y = lat, group = group), fill = "grey", color = "black") + 
  coord_fixed(1.3) +
  xlim(-127,-114) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(caption=str_wrap(expression(paste(bold("Figure 1."),"Figure 1. Map of study sites for Aim 2. Blue circles represent areas where community surveys were conducted. Red triangles represent areas where sensors were deployed and surveys were conducted.", 55)))) +
  geom_point(data=sites, aes(x=long, y=lat, color=name, shape=shape), size=4.5) +
  geom_text(data=sites, aes(x=long, y=lat, label=name), size=6,
            hjust=ifelse(sites$name=='ALG',1,1.25), 
            vjust=ifelse(sites$name=='ALG',1.5,0)) +
  scale_color_manual(values = c("orangered3", "orangered3", "royalblue4", "royalblue4", "orangered3","royalblue4")) +
  #coord_cartesian(clip="off") + #remove coordinates to allow increasing margin for caption
  theme_minimal() +
  theme(legend.position="none",
        #plot.margin=unit(c(0,0,0,0),"cm"), #create lower margin for caption
        axis.title=element_text(size=20),
        axis.text=element_text(size=15),
        plot.caption = element_text(hjust=0, size=14))


ggsave("figures/fig1.png", width=6, height=10)

