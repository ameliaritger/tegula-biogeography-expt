library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggtext) #for captions

states <- map_data("state")
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))
sites <- tribble(
  ~name, ~lat, ~long, ~color, ~shape,
  "Sokol \nPoint", 47.965399673237194, -124.66343921807504, "#D55E00", "0",
  "Fogarty \nCreek", 44.83964859735996, -124.05205638812974, "#D55E00", "0",
  "Bodega      \nMarine Reserve", 38.318757561054554, -123.07218556931106, "#0072B2", "5",
  "Lompoc Landing", 34.7074307110824, -120.60051345263794, "#0072B2", "5",
  "Alegria", 34.46907703880511, -120.27191157990546, "#0072B2", "5",
  "Cabrillo III", 32.66854992729292, -117.24431377177557, "#D55E00", "0"
)

ggplot() + 
  geom_rect(data=data.frame(ymin = 34.4486, ymax = 42.8376, xmin = -Inf, xmax = Inf),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = "#CC6677", alpha = 0.15) + #persistent upwelling
  geom_rect(data=data.frame(ymax = 34.4486, ymin = 32.5, xmin = -Inf, xmax = Inf),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = "#6699CC", alpha = 0.1) + #weak upwelling
  geom_rect(data=data.frame(ymax = 49, ymin = 42.8376, xmin = -Inf, xmax = Inf),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = "#44AA99", alpha = 0.1) + #intermittent upwelling
  geom_polygon(data=west_coast, aes(x = long, y = lat, group = group), fill = "grey", color = "black") + 
  coord_fixed(1.3) +
  xlim(-127,-114) +
  xlab("Longitude") +
  ylab("Latitude") +
  #labs(caption="<b>Figure 1.</b> Map of study sites for Aim 2. Blue circles represent areas where community surveys were conducted. Red triangles represent areas where both oceanographic sensors were deployed and community surveys were conducted.") +
  geom_point(data=sites, aes(x=long, y=lat, color=name, shape=shape), size=4.5) +
  geom_text(data=sites, aes(x=long, y=lat, label=name), size=6,
            hjust=ifelse(sites$name=='Alegria',1, ifelse(sites$name=='Bodega      \nMarine Reserve', 0.9, 1.1)), 
            vjust=ifelse(sites$name=='Alegria',1.5, ifelse(sites$name=='Fogarty \nCreek', 1, ifelse(sites$name=='Sokol \nPoint', 0.75, ifelse(sites$name=='Bodega      \nMarine Reserve', 1, 0))))) +
  scale_color_manual(values = c("orangered3", "orangered3", "royalblue4", "royalblue4", "orangered3","royalblue4")) +
  #coord_cartesian(clip="off") + #remove coordinates to allow increasing margin for caption
  theme_minimal() +
  theme(legend.position="none",
        #plot.margin=unit(c(0,0,1.8,0),"cm"), #create lower margin to fit caption
        axis.title=element_text(size=20),
        axis.text=element_text(size=15),
        plot.caption=element_textbox_simple(hjust=0.55, vjust=1.8, size=16, width=1.2)) #allow caption to wrap
          #plot.caption = element_text(hjust=0, size=14))

ggsave("figures/fig-prospectus2.png", width=6, height=10)

ggplot() + 
  geom_polygon(data=west_coast, aes(x = long, y = lat, group = group), 
               color = "black", 
               fill="antiquewhite")  +
  coord_fixed(1.3) +
  xlim(-127,-114) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.grid.major = element_line(color = gray(.7),
                                      linetype = "dashed", 
                                      size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

ggsave("figures/fig-conceptual.png", width=6, height=10)
