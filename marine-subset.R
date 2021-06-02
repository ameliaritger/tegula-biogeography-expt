library(tidyverse)
library(janitor)

#load data
marine <- read_csv("amelia_ritger_cbs_summary_2020_1005-quad.csv") %>% 
  clean_names()

#subset for san diego
marine_navy <- marine %>% 
  filter(georegion == "CA South") %>% 
  filter(intertidal_sitename == "Navy North")

#subset for tegula
marine_tegula <- marine %>% 
  filter(species_lump == "Tegula funebralis") %>% 
  filter(intertidal_sitename %in% c("Bodega", "Lompoc Landing", "Cabrillo III", "Alegria"))

ggplot(data=marine_tegula, aes(x=year, y=density_per_m2, fill=intertidal_sitename)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw()

ggsave("figures/tegula.png")
