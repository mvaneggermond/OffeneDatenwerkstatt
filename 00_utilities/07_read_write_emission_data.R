# Libraries
library(tidyverse)
###
# Functions ----
###
clean_column_names <- function(df)
{
  
  names(df) <- tolower(names(df))
  
  names(df) <- gsub(x = names(df),
                    pattern = "\\ ",
                    replacement = "_")
  
  names(df) <- gsub(x = names(df),
                    pattern = "\\/",
                    replacement = "")
  
  names(df) <- gsub(x = names(df),
                    pattern = "\\(",
                    replacement = "")
  
  names(df) <- gsub(x = names(df),
                    pattern = "\\)",
                    replacement = "")
  
  names(df) <- gsub(x = names(df),
                    
                    pattern = "\\-",
                    
                    replacement = "_")
  names(df) <- gsub(x = names(df),
                    pattern = "\\.",
                    replacement = "")
  names(df) <- gsub(x = names(df),
                    pattern = "\\ü",
                    replacement = "ue")
  
  names(df) <- gsub(x = names(df),
                    pattern = "\\ä",
                    replacement = "ae")
  
  names(df) <- gsub(x = names(df),
                    
                    pattern = "\\+",
                    
                    replacement = "plus")
  
  names(df) <- gsub(x = names(df),
                    pattern = "\\[",
                    replacement = "")
  
  names(df) <- gsub(x = names(df),
                    
                    pattern = "\\]",
                    
                    replacement = "")
  
  names(df) <- gsub(x = names(df),
                    
                    pattern = "\\m³",
                    
                    replacement = "m3")
  
  
  names(df) <- gsub(x = names(df),
                    
                    pattern = "\\µ",
                    
                    replacement = "u")
  
  
  return (df)
}
df_feldberg$`PM10 (Stundenmittelwerte  [µg/m³])`
# Read the data
df_feldberg <- readr::read_delim("data/basel_feinstaub/100050_feldbergstrasse.csv",delim=';',
                                col_types=cols(
  `Datum/Zeit` = col_datetime(format = ""),
  timestamp_text = col_datetime(format = ""),
  `PM10 (Stundenmittelwerte  [µg/m³])` = col_double(),
  `PM2.5 (Stundenmittelwerte  [µg/m³])` = col_double(),
  `NO2 (Stundenmittelwerte  [µg/m³])` = col_double(),
  geo_point_2d = col_character()
))%>% mutate(site='Feldbergstrasse')


# Read the data
df_johann <- readr::read_delim("data/basel_feinstaub/100049_st_johann.csv",delim=';',
                               col_types=cols(
                                 `Datum/Zeit` = col_datetime(format = ""),
                                 timestamp_text = col_datetime(format = ""),
                                 `PM10 (Stundenmittelwerte)` = col_double(),
                                 `PM2.5 (Stundenmittelwerte)` = col_double(),
                                 `O3 (Stundenmittelwerte)` = col_double(),
                                 geo_point_2d = col_character()
                               ))%>% mutate(site='St. Johann')


# Clean the data
# Feldbergstrasse
df_feldberg_clean <- clean_column_names(df_feldberg)
df_feldberg_clean_filter <- df_feldberg_clean %>% 
  filter(!is.na(pm25_stundenmittelwerte__ugm3)|!is.na(pm10_stundenmittelwerte__ugm3))%>%
  arrange(datumzeit)%>%
  mutate(year=lubridate::year(datumzeit),
         month=lubridate::month(datumzeit),
         week=lubridate::isoweek(datumzeit),
         weekalt=lubridate::week(datumzeit),
         day=lubridate::day(datumzeit),
         hour=lubridate::hour(datumzeit))%>%
  relocate(year,month,week,weekalt,day,hour)

# St Johann
df_johann_clean <- clean_column_names(df_johann)
df_johann_clean_filter <- df_johann_clean %>% 
  filter(!is.na(pm25_stundenmittelwerte)|!is.na(pm10_stundenmittelwerte))%>%
  arrange(datumzeit)%>%
  mutate(year=lubridate::year(datumzeit),
         month=lubridate::month(datumzeit),
         week=lubridate::isoweek(datumzeit),
         weekalt=lubridate::week(datumzeit),
         day=lubridate::day(datumzeit),
         hour=lubridate::hour(datumzeit))%>%
  relocate(year,month,week,weekalt,day,hour)%>%
  rename(pm25_stundenmittelwerte__ugm3=pm25_stundenmittelwerte,pm10_stundenmittelwerte__ugm3=pm10_stundenmittelwerte)%>%
  mutate(no2_stundenmittelwerte__ugm3=o3_stundenmittelwerte)

df_all_emission <- bind_rows(df_feldberg_clean_filter,df_johann_clean_filter)
         
write_csv(df_all_emission,"offene_datenwerkstatt_feinstaub.csv")
