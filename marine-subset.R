library(tidyverse)
library(janitor)

#load data
marine <- read_csv("amelia_ritger_cbs_summary_2020_1005.csv") %>% 
  clean_names()

#subset for san diego
marine_navy <- marine %>% 
  filter(georegion == "CA South") %>% 
  filter(intertidal_sitename == "Navy North")
