library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(scales) #date_format for x axis plot
library(heatwaveR)

bottom_temp <-  read_csv(here("data", "Bottom_temp_all_years_20201105.csv"), col_names = TRUE, col_types = cols(.default = col_character())) %>% 
  clean_names() %>%
  unite("date_time", "date_local", "time_local", sep="\ ", remove = TRUE) %>% #join together time and time zone
  mutate(temp_c=as.numeric(temp_c), #make temp column numeric (not character...)
         date_time=ymd_hm(date_time), #apply lubridate to date/time column
         year=format(date_time, '%Y'), #create only year column
         date=format(date_time, '%Y/%m/%d')) %>% #create only date column
  arrange(date)

tail(bottom_temp)

bottom_blob <- bottom_temp %>%
  filter(date >= "2014/01/01" & date <= "2016/08/01") #look only at Blob timeframe

mohawk <- bottom_temp %>%
  filter(site=="MOHK")

naples <- bottom_temp %>%
  filter(site=="NAPL")

moh_nap <- bottom_blob %>%
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

##### Assess MHW

# Reformat df to run heatwaveR
site_mhw <- mohawk %>%
  mutate(date=as.Date(date)) %>%
  rename(t=date,
         temp=temp_c) %>%
  select(-site, -serial, -year)

# Detect the events in a time series
ts <- heatwaveR::ts2clm(site_mhw, x=t, y=temp, climatologyPeriod = c("2003-01-01", "2020-01-01"))
mhw <- heatwaveR::detect_event(ts)

#identify MHW events
mhw$event %>%
   dplyr::ungroup() %>%
   dplyr::select(event_no, duration, date_start, date_peak, date_end, intensity_max, intensity_mean, intensity_cumulative) %>%
   dplyr::arrange(-intensity_max) %>%
   head(5)

#basic plots
event_line(mhw, spread = 365, metric = "intensity_max",
           start_date = "2015-10-06", end_date = "2015-10-19")

lolli_plot(mhw, metric = "intensity_max")
lolli_plot(mhw, metric = "duration")

ggplot(mhw$event, aes(x = date_start, y = intensity_max)) +
  geom_lolli(colour = "salmon", colour_n = "red", n = 3) +
  geom_text(colour = "black", aes(x = as.Date("2006-08-01"), y = 5,
                                  label = "")) +
  labs(y = expression(paste("Max. intensity [", degree, "C]")), x = NULL)

#subset for dates of interest
mhw2 <- mhw$climatology %>% 
  slice(480000:535000) 

#NAPLES: 450000:520000 for 2014 to 2016, 468000:486000 for Oct 2014 to May 2015

head(mhw2)
tail(mhw2)

#create another "threshold" line for prospectus...
mhw3 <- mhw2 %>%
  mutate(thresh2=thresh+2,
         thresh3=thresh+3)

#give geom_flame() at least one row on either side of the event in order to calculate the polygon corners smoothly
mhw_top <- mhw3 %>% 
  slice(5:111)

#fancy mhw plot
ggplot(mhw3, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = F, alpha=0.8) +
  geom_flame(data = mhw_top, aes(y = temp, y2 = thresh, fill = "top"),  show.legend = F) +
  geom_line(aes(y = temp, colour = "temp"), alpha=0.4) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  geom_line(aes(y = thresh3, colour = "thresh2"), size = 1.0) +
  geom_line(aes(y = seas, colour = "seas"), size = 1.2) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", 
                                 "thresh" =  "chartreuse4", 
                                 "thresh2" =  "red4", 
                                 "seas" = "dodgerblue4")) +
  scale_fill_manual(name = "Event Colour", 
                    values = c("all" = "salmon", 
                               "top" = "red")) +
  scale_x_date(date_labels = "%b %Y",
               breaks = scales::date_breaks("4 months")) +
  scale_y_continuous(limits = c(11, 24), breaks = seq(11, 24, by = 2)) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature (", degree, "C)")), x = NULL) +
  theme_bw() +
  theme(legend.position="none",
        axis.text.x=element_text(size=22),
        axis.title.y=element_text(size=25),
        axis.text.y=element_text(size=22))

ggsave("figures/mhwk_mhw.png", height=20, width=30, units="cm")
