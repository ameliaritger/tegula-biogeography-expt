library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(scales) #date_format for x axis plot

bottom_temp <-  read_csv(here("data", "Bottom_temp_all_years_20201105.csv"), col_names = TRUE, col_types = cols(.default = col_character())) %>% 
  clean_names() %>%
  unite("date_time", "date_local", "time_local", sep="\ ", remove = TRUE) %>% #join together time and time zone
  mutate(temp_c=as.numeric(temp_c), #make temp column numeric (not character...)
         date_time=ymd_hm(date_time), #apply lubridate to date/time column
         year=format(date_time, '%Y'), #create only year column
         date=format(date_time, '%Y/%m/%d')) %>% #create only date column
  filter(date >= "2014/05/01" & date <= "2016/08/01") #look only at Blob timeframe

mohawk <- bottom_temp %>%
  filter(site=="MOHK")

naples <- bottom_temp %>%
  filter(site=="NAPL")

moh_nap <- bottom_temp %>%
  filter(site=="NAPL" | site=="MOHK")

max_mhk <- max(mohawk$temp_c)
max_nap <- max(naples$temp_c)
min_mhk <- min(mohawk$temp_c)

#plot up only mohawk
ggplot(mohawk, aes(x = date_time, y = temp_c)) +
  geom_point() +
  labs(x = "Time", y = "Bottom Temperature") +
  scale_x_datetime(breaks = scales::date_breaks("4 months"), 
                   labels = date_format("%m/%d %H:%m")) +
  scale_y_continuous(limits = c(10, 24), breaks = seq(10, 24, by = 2)) +
  theme_classic()

#plot up only naples
ggplot(naples, aes(x = date_time, y = temp_c)) +
  geom_point() +
  labs(x = "Time", y = "Bottom Temperature") +
  scale_x_datetime(breaks = scales::date_breaks("4 months"), 
                   labels = date_format("%m/%d %H:%m")) +
  scale_y_continuous(limits = c(10, 24), breaks = seq(10, 24, by = 2)) +
  theme_classic()

#compare naples and mohawk
ggplot(moh_nap, aes(x = date_time, y = temp_c, group=site)) +
  geom_point(aes(color=site)) +
  labs(x = "Time", y = "Bottom Temperature") +
  scale_x_datetime(breaks = scales::date_breaks("4 months"), 
                   labels = date_format("%m/%d %H:%m")) +
  scale_y_continuous(limits = c(10, 24), breaks = seq(10, 24, by = 2)) +
  theme_classic()
