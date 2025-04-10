######### Use calibration data (Tris) to calibrate intertidal pH sensor data ######### 
# Code written by Zoe Fung and edited/finalized by Amelia Ritger at UCSB
## Fall 2021/Winter 2022, R version 4.1.1

# Email questions to aritger@ucsb.edu
######################################################################################

##################  Before you begin ################## 
#1. Create a folder called "rawdata" and a folder called "calibrated"
#1a. Put sensor data (calibration and field data) in "rawdata" folder
#1b. Expect calibrated data to be populated in the "calibrated" folder
#1c. Make sure the input data are in the same location as this code (Creating an R Project is highly recommended)
#1d. Input data should be formatted as "date-location-sensor" (e.g. 01.01.2022-california-sensor1)

#2. Click "source" or ctrl-shift-s - follow prompts
#2a. Filename for calibration data must include ".csv" (e.g. FILENAME.csv)
#2b. Filename for field data must include ".csv" (e.g. FILENAME.csv)
#2c. Filename for output data must include ".csv" (e.g. FILENAME.csv)
#2d. Entering sensor, location name, and calibration date is not essential to run the code - if not desired, put "NA"
######################################################################################

################## Things to know about the code ##################  
# First 20 minutes of calibration data are removed from analysis
# Values used to calibrate field data can be found in the "calibrated" folder (e.g. "calib_FILENAME")

######################################################################################

library(tidyverse)
library(readr)
library(janitor)
library(here)
library(lubridate)

#Ask for inputs
tris=(readline(prompt="Enter calibration data filename (include .csv): "))
raw=(readline(prompt="Enter field data filename (include .csv): "))
sens=(readline(prompt="Enter name of sensor: "))
sit=(readline(prompt="Enter name of deployment site: "))
cali=(readline(prompt="Enter date of calibration: "))
filename=(readline(prompt="Enter output data filename (include .csv): "))

#Run the code
trisdata <- read_csv(here("rawdata", tris)) %>% # read in tris data excel file
    filter(row_number()!=c(1:5)) %>% # remove the first 5 rows
    row_to_names(row_number=1) %>% # make the top row the column headers
    rename("Date"=1,
           "Time"=2) %>% # rename date and time columns
    clean_names() %>% # clean up column header names
    slice(12:(n()-12)) %>% # remove first 2 and last 2 minutes of data (12 rows)
    mutate(date_lubridate=mdy(date)) %>% # tell R "date" is a DATE
    mutate(time_lubridate=hms(time)) %>% # tell R "time" is a TIME
    unite(date_time, c(date, time), sep = " ", remove = TRUE) %>% # combine the date and time columns
    mutate(date_time_lubridate=mdy_hms(date_time)) # tell R "date_time" is a DATE and a TIME
  
tris_voltages <- data.frame(trisdata$voltage_1_m_v,trisdata$voltage_2_m_v,trisdata$voltage_3_m_v,trisdata$voltage_4_m_v) %>% # create OMEGAS data frame with the tris voltages
  mutate(mv1=as.numeric(trisdata.voltage_1_m_v),
         mv2=as.numeric(trisdata.voltage_2_m_v),
         mv3=as.numeric(trisdata.voltage_3_m_v),
         mv4=as.numeric(trisdata.voltage_4_m_v)) %>% # changing data to numeric arguments
  select(mv1, mv2, mv3, mv4) # cleaning data frame

omegas_temp_tris <- tris_voltages %>% # create new data frame using omegas values
  mutate(v1=(mv1/1000),
         v2=(mv2/1000),
         v3=(mv3/1000),
         v4=(mv4/1000)) %>% # converting the mV to volts
  mutate(vbattery=((v2/100)*101.57)) %>% # calculate vbattery
  mutate(z=(v4/100)+vbattery*((15000*0.99)/(1000000+(15000*0.99)))) %>% # calculate column k
  mutate(y=(z*1000000)) %>% # calculate column l
  mutate(rthermistor=(y/(vbattery-z))) %>% # calculate Rthermistor
  mutate(sh_a=as.numeric(0.00106329736674527)) %>% # create column with Steinhart constant A
  mutate(sh_b=as.numeric(0.000251377462346306)) %>% # create column with Steinhart constant B
  mutate(sh_c=as.numeric(2.55455247726963E-08)) %>% # create column with Steinhart constant C
  mutate(k=(1/(sh_a+sh_b*log(rthermistor)+sh_c*(log(rthermistor))^3))) %>% # create column to calculate intermediate value K
  mutate(durafet=as.numeric((k-273.15))) %>% # create new column to calculate durafet therm 
  mutate(calt=as.numeric(1.0086*durafet+0.3157)) # create Cal_T column (does not get used for further calibration)

temp <- c(omegas_temp_tris$durafet) # copy durafet therm into tris pH template
b <- as.numeric(rep("11911.08",length(temp))) # create vector for tris pH template column b
c <- as.numeric(rep("18.2499",length(temp))) # create vector for tris pH template column c
d <- as.numeric(rep("0.039336",length(temp))) # create vector for tris pH template column d
e <- as.numeric(rep("366.27059",length(temp))) # create vector for tris pH template column e
f <- as.numeric(rep("0.53993607",length(temp))) # create vector for tris pH template column f
g <- as.numeric(rep("0.00016329",length(temp))) # create vector for tris pH template column g
h <- as.numeric(rep("64.52243",length(temp))) # create vector for tris pH template column h
i <- as.numeric(rep("0.084041",length(temp))) # create vector for tris pH template column i
j <- as.numeric(rep("0.11149858",length(temp))) # create vector for tris pH template column j
k <- as.numeric(rep("273.15",length(temp))) # create vector for tris pH template column k
t <- as.numeric(rep(k+temp),length(temp)) # create vector for tris pH template column t
m <- as.numeric(rep("35",length(temp))) # create vector for tris pH template column m

tris_ph_template <- data.frame(temp,b,c,d,e,f,g,h,i,j,t,k,m)%>% # create tris pH template as a data frame with all above columns
  mutate(n=(b-(c*m)-(d*m*m))*1/t) %>% # create column n
  mutate(o=(-e+(f*m)+(g*m*m))) %>% # create column o
  mutate(p=(h-(i*m))*log(t)-(j*(t))) %>% # create column p
  mutate(trisph=(n+o+p)) # calculate tris pH, add as a new column to tris pH template data frame

omegas_ph_tris <- data.frame(trisdata$date_time_lubridate,omegas_temp_tris$durafet,omegas_temp_tris$v1) %>% # create data frame with durafet therm and voltage 1 values
  rename(date_time=trisdata.date_time_lubridate) %>% # rename date/time column from trisdata data frame
  mutate(trisph=tris_ph_template$trisph) %>% # add tris pH values into calibration dataframe 
  mutate(tk=omegas_temp_tris$durafet+273.15) %>% # add TK column to data frame
  mutate(st=(8.31451*tk/96487*log(10))) %>% # add S(T) column to data frame
  mutate(eot=(-0.374911706-(0.001*(tk-290.2751473)))) %>% # add Eo(T) column to data frame
  mutate(omegasph=(omegas_temp_tris$v1-eot)/st) %>% # add pH column to data frame
  mutate(diff=(trisph-omegasph)) %>% # add difference column (tris pH - omegas pH) to data frame
  mutate(tempdiff=abs(omegas_temp_tris.durafet-lag(omegas_temp_tris.durafet,n=5,default=first(omegas_temp_tris.durafet)))) %>% # find difference between two temp measurements 5 rows apart to identify where temperature stabilizes
  mutate(phdiff=abs(omegasph-lag(omegasph,n=5,default=first(omegasph)))) # find difference between two pH measurements 5 rows apart to identify where pH stabilizes

calibration_values <- omegas_ph_tris %>% # work in omegas_calibration dataframe
  filter(row_number()!=c(1:100)) %>% # remove the first 100 rows (first 20 minutes of data)
  slice_min(tempdiff,n=10) %>% # select the rows that contains the 10 minimum values of change in temperature over time 
  slice_min(phdiff,n=1) # select the row that contains the minimum value of change in pH over time in within the 10 lowest temp differences

rawdata <- read_csv(here("rawdata", raw)) %>% # read in raw data excel file
  filter(row_number()!=c(1:5)) %>% # remove the first 5 rows
  row_to_names(row_number=1) %>% # make the top row the column headers
  rename("Date"=1,
         "Time"=2) %>%
  clean_names() %>% # clean up column header names
  mutate(date_lubridate=mdy(date)) %>% # tell R "date" is a DATE
  mutate(time_lubridate=hms(time)) %>% # tell R "time" is a TIME
  unite(date_time, c(date,time), sep = " ", remove = FALSE) %>% # combine the date and time columns
  mutate(date_time_lubridate=mdy_hms(date_time)) # tell R "date_time" is a DATE and a TIME

#Populate sensor number, location, date based on filename
rawdata <- rawdata %>%
  mutate(tmp = gsub("(\\./data/|\\.txt)", "", filename)) %>%
  separate(tmp, into = c("month", "day", "year", "id")) %>%
  mutate(newcol=strsplit(id, split="(?<=[a-zA-Z])\\s*(?=[0-9])", perl=TRUE)) %>%
  separate(newcol, c("c", "location", "sensor"), remove=TRUE) %>%
  unite(sensor_recovery_date, c(month,day,year), sep="/",remove=TRUE) %>%
  select(-id, -c) %>%
  mutate(sensor=sens,
         location=sit,
         sensor_recovery_date=cali)
omegas_temp_raw <- data.frame(rawdata$voltage_1_m_v,rawdata$voltage_2_m_v,rawdata$voltage_3_m_v,rawdata$voltage_4_m_v) %>% # create OMEGAS data frame with the raw voltages
  mutate(mv1=as.numeric(rawdata.voltage_1_m_v),
         mv2=as.numeric(rawdata.voltage_2_m_v),
         mv3=as.numeric(rawdata.voltage_3_m_v),
         mv4=as.numeric(rawdata.voltage_4_m_v)) %>% # changing data to numeric arguments
  select(mv1, mv2, mv3, mv4) %>% # cleaning data frame 
  mutate(v1=(mv1/1000),
         v2=(mv2/1000),
         v3=(mv3/1000),
         v4=(mv4/1000)) %>% # converting the mV to volts
  mutate(vbattery=((v2/100)*101.57))%>% # calculate vbattery
  mutate(z=(v4/100)+vbattery*((15000*0.99)/(1000000+(15000*0.99)))) %>% # calculate column k
  mutate(y=(z*1000000)) %>% # calculate column l
  mutate(rthermistor=(y/(vbattery-z))) %>% # calculate Rthermistor
  mutate(sh_a=as.numeric(0.00106329736674527)) %>% # create column with Steinhart constant A
  mutate(sh_b=as.numeric(0.000251377462346306)) %>% # create column with Steinhart constant B
  mutate(sh_c=as.numeric(2.55455247726963E-08)) %>% # create column with Steinhart constant C
  mutate(k=(1/(sh_a+sh_b*log(rthermistor)+sh_c*(log(rthermistor))^3))) %>% # create column to calculate intermediate value K
  mutate(durafet=as.numeric((k-273.15))) %>% # create new column to calculate durafet therm 
  mutate(calt=as.numeric(1.0086*durafet+0.3157)) # create Cal_T column (does not get used for further calibration)

temp <- c(omegas_temp_raw$durafet) # copy durafet therm into tris pH template
b <- as.numeric(rep("11911.08",length(temp))) # create vector for tris pH template column b
c <- as.numeric(rep("18.2499",length(temp))) # create vector for tris pH template column c
d <- as.numeric(rep("0.039336",length(temp))) # create vector for tris pH template column d
e <- as.numeric(rep("366.27059",length(temp))) # create vector for tris pH template column e
f <- as.numeric(rep("0.53993607",length(temp))) # create vector for tris pH template column f
g <- as.numeric(rep("0.00016329",length(temp))) # create vector for tris pH template column g
h <- as.numeric(rep("64.52243",length(temp))) # create vector for tris pH template column h
i <- as.numeric(rep("0.084041",length(temp))) # create vector for tris pH template column i
j <- as.numeric(rep("0.11149858",length(temp))) # create vector for tris pH template column j
k <- as.numeric(rep("273.15",length(temp))) # create vector for tris pH template column k
t <- as.numeric(rep(k+temp),length(temp)) # create vector for tris pH template column t
m <- as.numeric(rep("35",length(temp))) # create vector for tris pH template column m

raw_tris_ph <- data.frame(temp,b,c,d,e,f,g,h,i,j,t,k,m) %>% # create tris pH template as a data frame with all above columns
  mutate(n=(b-(c*m)-(d*m*m))*1/t) %>% # create column n
  mutate(o=(-e+(f*m)+(g*m*m))) %>% # create column o
  mutate(p=(h-(i*m))*log(t)-(j*(t))) %>% # create column p
  mutate(trisph=(n+o+p)) # calculate tris pH, add as a new column to tris pH template data frame

omegas_ph_raw <- data.frame(rawdata$sensor_recovery_date, rawdata$location, rawdata$sensor, rawdata$date_time_lubridate,rawdata$date,rawdata$time,omegas_temp_raw$durafet,omegas_temp_raw$v1) %>% # create data frame with durafet therm and voltage 1 values
  rename(date_time=rawdata.date_time_lubridate,
         date=rawdata.date,
         time=rawdata.time) %>% # rename date/time column from trisdata data frame
  mutate(trisph=raw_tris_ph$trisph) %>% # add tris pH values into calibration dataframe TRIS PH IS NOT THE SAME LENGTH AS THE RAW DATA
  mutate(tk=omegas_temp_raw$durafet+273.15) %>% # add TK column to data frame
  mutate(st=(8.31451*tk/96487*log(10))) %>% # add S(T) column to data frame
  mutate(eo=((calibration_values$omegas_temp_tris.v1)-(calibration_values$trisph)*8.31451*(273.15+calibration_values$omegas_temp_tris.durafet)*log(10)/96487)) %>% # calculate eo value using calibration values
  mutate(eot=(eo-(0.001*(tk-(273.15+calibration_values$omegas_temp_tris.durafet))))) %>% # add Eo(T) column to data frame
  mutate(omegasrawph=(omegas_temp_raw$v1-eot)/st) %>% # add pH column to data frame
  mutate(diff=(trisph-omegasrawph)) # add difference column (tris pH - omegas pH) to data frame

raw_calibrated <- omegas_ph_raw %>% # create new dataframe using omegasph_raw data
  select(rawdata.location, rawdata.sensor, rawdata.sensor_recovery_date, date_time,date, time, omegas_temp_raw.durafet,omegasrawph) %>% # keep only date/time, temperature, and pH columns
  rename("Location"=1,
         "Sensor"=2,
         "Calibration Date"=3,
         "Date & Time"=4,
         "Date"=5,
         "Time"=6,
         "Temperature"=7,
         "pH"=8)

#Write out .csv files
write.csv(raw_calibrated, here("calibrated", filename), row.names = FALSE) # create csv with final processed data
write.csv(calibration_values, here("calibrated", paste("calib_",filename,sep="")),row.names=FALSE) # create csv with calibration values
