## Running a quick test to see if I can just average all of the "before" and "after" calibration values

# Load packages
library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# Load data
test <- read_csv(here("data","test.csv")) %>% 
  clean_names() %>%
  mutate(lub_date=mdy_hms(date)) %>% #apply lubridate to date column
  select(-date) #remove old date column

# plot data to compare old vs new vs mean calibration values
ggplot(test, aes(x=lub_date,y=p_h, group=group)) +
  geom_point(aes(color=group)) +
  geom_smooth(aes(color=group))
           
## Cool! It does look like simply taking the AVG() of the two calibration values provides a perfect middle ground of values