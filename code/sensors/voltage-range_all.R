library(tidyverse)
library(janitor)
library(here)
file_list <- list.files(here("data", "sensors", "raw"), pattern="*.csv", full.names=TRUE)
dataset <- data.frame()

for (i in 1:length(file_list)){
  print(file_list[i])
  temp_data <- read_csv(file_list[i]) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data <- slice(temp_data,-1:-6)
  colnames(temp_data) <- c("date","time","v1", "v2", "v3", "v4")
  dataset <- rbind(dataset, temp_data) #for each iteration, bind the new data to the building dataset
}

dataset$v1 <- na_if(dataset$v1, -160)

volt_summary <- dataset %>%
  mutate(v1 = as.numeric(v1),
         v2 = as.numeric(v2),
         v3 = as.numeric(v3),
         v4 = as.numeric(v4)) %>%
  filter(v1 > 1) %>%
  summarize(v1i = min(v1),
            v1a = max(v1),
            v2i = min(v2),
            v2a = max(v2),
            v3i = min(v3),
            v3a = max(v3),
            v4i = min(v4),
            v4a = max(v4))

# pH range = 3.77 - 111.8 mV
# bat 1 = 35.0 - 36.1 mV
# bat 2 = -35.5 - -34.6 mV
# temp range = -20.8 - 14.6 mV