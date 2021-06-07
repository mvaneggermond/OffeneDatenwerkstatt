####
# Load the libraries -----
###
library(tidyverse)
library(lubridate)


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

  

  return (df)
}

####
# Read data -----
####
#df_google_mobility
#df_covid <- readxl::read_xlsx("covid_19_basel_27_02.2020_per_day.xlsx")
#df_wetter <- readxl::read_xlsx("temperture_precipitation_1864-2021_per_month.xlsx")
#df_velo_zahldaten <- readxl::read_xlsx("velo_wettstein_per_hour.xlsx")

# MIV ----
# Read the data
df_miv_zahldaten <- readr::read_csv2("data/basel_miv/converted_MIV_Class_10_1.csv")
# Clean column names
df_miv_zahldaten_clean <- clean_column_names(df_miv_zahldaten) 
# Filter the following locations out of the data set
filter_miv <- c("235 A3-A35, Grenze CH-F","659 Schlachthofstrasse",
                "670 Zoll CH-D, Hiltalinger",
                "671 Zoll CH-D, Freiburger", "672 Zoll CH-D, Weilstrasse", 
                "673 Zoll CH-D, Lörracher","674 Zoll CH-D, Grenzacherstrasse","416 J. Burckhardt-Strasse 85", "416 J. Burckhardt-Strasse 85 (2-spurig bis Mai 2016)")

df_miv_zahldaten_clean_filter <- df_miv_zahldaten_clean %>% 
  filter(!sitename%in%filter_miv)%>%
  filter(year>2014)

df_miv_zahldaten_clean_filter

df_miv_zahldaten_group<- df_miv_zahldaten_clean_filter %>% 
  mutate(first_dow = lubridate::floor_date(datetimefrom,unit='week', week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(first_dow)%>%
  summarise(total_fahrzeug=sum(total))%>%
  mutate(weeknr=lubridate::isoweek(first_dow))

df_miv_zahldaten_group_month<- df_miv_zahldaten_clean_filter %>% 
  mutate(first_dow = lubridate::floor_date(datetimefrom,unit='week', week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(year,month)%>%
  summarise(total_fahrzeug=sum(total))%>%
  mutate(year_month=lubridate::make_date(year=year, month=month,day=1))

head(df_miv_zahldaten_group_month)

df_miv_zahldaten_per_site<- df_miv_zahldaten_clean_filter %>% 
  mutate(first_dow = lubridate::floor_date(datetimefrom,unit='week', week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(first_dow,sitecode,sitename)%>%
  summarise(total_fahrzeug=sum(total))%>%
  mutate(weeknr=lubridate::isoweek(first_dow))%>%
  mutate(sitecode=as.numeric(sitecode))%>%
  mutate(traffictype='car / truck')

df_miv_zahldaten_per_site_month<- df_miv_zahldaten_clean_filter %>% 
  mutate(first_dow = lubridate::floor_date(datetimefrom,unit='week', week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(first_dow,sitecode,sitename)%>%
  summarise(total_fahrzeug=sum(total))%>%
  mutate(weeknr=lubridate::isoweek(first_dow))%>%
  mutate(sitecode=as.numeric(sitecode))%>%
  mutate(traffictype='car / truck')


# OV ---
df_oev_zahldaten <- readr::read_csv2("data/basel_oev/100075.csv") %>% 
  select(-Kalenderwoche)
df_oev_zahldaten_clean <- clean_column_names(df_oev_zahldaten)

df_oev_zahldaten_clean_month_start <- df_oev_zahldaten_clean %>%
  mutate(enddatum_woche=startdatum_woche+lubridate::days(7))%>%
  mutate(end_of_month=lubridate::make_date(year=lubridate::year(startdatum_woche),month=lubridate::month(startdatum_woche),
                                        day=lubridate::days_in_month(lubridate::month(startdatum_woche))))%>%
  mutate(perc_week=case_when(lubridate::month(startdatum_woche)==lubridate::month(enddatum_woche) ~  ((enddatum_woche - startdatum_woche)/ ddays(1))/7.0,
                                 TRUE ~ (((end_of_month - startdatum_woche)/ ddays(1))+1)/7.0))

df_oev_zahldaten_clean_month_other <- df_oev_zahldaten_clean_month_start %>%
  filter(perc_week<1)%>%
  mutate(startdatum_woche=lubridate::make_date(year=lubridate::year(enddatum_woche),month=lubridate::month(enddatum_woche),
                                           day=1))%>%
  mutate(enddatum_woche=enddatum_woche)%>%
  mutate(perc_week=1-perc_week)%>%
  mutate(fahrgaeste_einsteiger=fahrgaeste_einsteiger*perc_week)%>%
  select(startdatum_woche,fahrgaeste_einsteiger)


df_oev_zahldaten_start <- df_oev_zahldaten_clean_month_start %>%
  filter(perc_week==1)%>%
  mutate(fahrgaeste_einsteiger=fahrgaeste_einsteiger*perc_week)%>%
  select(startdatum_woche,fahrgaeste_einsteiger)

df_oev_zahldaten_start_month <-
  bind_rows(df_oev_zahldaten_start,df_oev_zahldaten_clean_month_other)%>%
  mutate(year=lubridate::year(startdatum_woche),month=lubridate::month(startdatum_woche))%>%
  group_by(year,month)%>%
  summarise(total_oev=sum(fahrgaeste_einsteiger))

View(df_oev_zahldaten_start_month)

# Fuss, Velo -----
df_fuss_velo_zahldaten <- readr::read_csv2("data/basel_fuss_velo/converted_Velo_Fuss_Count.csv")
df_fuss_velo_zahldaten_clean <- clean_column_names(df_fuss_velo_zahldaten)

filter_velo <- c("804 Rosentalstrasse 29/28",
                "903 Äussere Baselstrasse 381 (Riehen)", "917 Schwarzwaldbrücke")

#View(df_fuss_velo_zahldaten_clean %>% group_by(sitename,year) %>% summarise(n=n()) %>%arrange(sitename,year))

df_fuss_velo_zahldaten_group <-df_fuss_velo_zahldaten_clean %>%
  filter(!sitename%in%filter_velo)%>%
  mutate(first_dow = lubridate::floor_date(datetimefrom,unit='week', week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(first_dow,traffictype)%>%
  summarise(total=sum(total,na.rm = T))%>%
  mutate(traffictype=tolower(traffictype))%>%
  filter(!is.na(traffictype))%>%
  filter(lubridate::year(first_dow)>2014)%>%
  mutate(traffictype=if_else(traffictype=='velo','total_velo','total_fuss'))%>%
  pivot_wider(names_from = traffictype,values_from = total)

df_fuss_velo_zahldaten_group_month <-df_fuss_velo_zahldaten_clean %>%
  filter(!sitename%in%filter_velo)%>%
  mutate(first_dow = lubridate::floor_date(datetimefrom,unit='week', week_start = getOption("lubridate.week.start", 1))) %>%
  filter(lubridate::year(first_dow)>2014)%>%
  group_by(year,month,traffictype)%>%
  summarise(total=sum(total,na.rm = T))%>%
  mutate(traffictype=tolower(traffictype))%>%
  filter(!is.na(traffictype))%>%
  mutate(traffictype=if_else(traffictype=='velo','total_velo','total_fuss'))%>%
  pivot_wider(names_from = traffictype,values_from = total)%>%
  mutate(year_month=lubridate::make_date(year=year, month=month,day=1))
  
df_fuss_velo_zahldaten_per_site<- df_fuss_velo_zahldaten_clean %>% 
  mutate(first_dow = lubridate::floor_date(datetimefrom,unit='week', week_start = getOption("lubridate.week.start", 1))) %>%
  mutate(sitecode=case_when(sitecode==902~404,
                            sitecode==952~352,
                            sitecode==812~354,
                            sitecode==803~352,
                            TRUE ~ sitecode))%>%
  group_by(first_dow,sitecode,sitename,traffictype)%>%
  summarise(total=sum(total,na.rm = T))%>%
  mutate(traffictype=tolower(traffictype))%>%
  filter(!is.na(traffictype))%>%
  filter(lubridate::year(first_dow)>2014)%>%
  mutate(traffictype=if_else(traffictype=='velo','velo','fuss'))%>%
  mutate(weeknr=lubridate::isoweek(first_dow))%>%
  pivot_wider(names_from = traffictype,values_from = total)

# Read the temperature
df_weather <- readr::read_csv("data/wetter/temp_basel_converted.csv")
df_weather_clean <- clean_column_names(df_weather)


# Read google data
df_google <- readr::read_csv("data/google_mobility_reports/2020_2021_CH_Region_Mobility_Report.csv") %>%
  select(date:residential_percent_change_from_baseline)%>%
  mutate(first_dow = lubridate::floor_date(date,unit='week', week_start = getOption("lubridate.week.start", 1)))%>%
  group_by(first_dow)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

df_google_month <- readr::read_csv("data/google_mobility_reports/2020_2021_CH_Region_Mobility_Report.csv") %>%
  select(date:residential_percent_change_from_baseline)%>%
  mutate(year = lubridate::year(date),month=lubridate::month(date))%>%
  group_by(year,month)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))%>%
  mutate(year_month=lubridate::make_date(year=year, month=month,day=1))


# Read COVID
df_covid <- readr::read_csv2("data/basel_covid/100073.csv")

df_covid_clean <- clean_column_names(df_covid)

df_covid_group <- df_covid_clean %>%
  mutate(first_dow = lubridate::floor_date(datum,unit='week', week_start = getOption("lubridate.week.start", 1)))%>%
  group_by(first_dow)%>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
  select(first_dow,covid_cases_bs=differenz_faelle_mit_wohnsitz_bs)

df_covid_group_month <- df_covid_clean %>%
  mutate(year = lubridate::year(datum),month=lubridate::month(datum))%>%
  group_by(year,month)%>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
  mutate(year_month=lubridate::make_date(year=year, month=month,day=1))%>%
  select(year,month,year_month,covid_cases_bs=differenz_faelle_mit_wohnsitz_bs)

# Read feinstaub
df_smart_climate <- readr::read_delim("data/basel_smart_climate/100081.csv",delim=";")


# Smart climate data
df_smart_climate_clean <- clean_column_names(df_smart_climate)%>%
  mutate(first_dow = lubridate::floor_date(zeitstempel,unit='week', week_start = getOption("lubridate.week.start", 1)))%>%
  group_by(first_dow)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

# Indices
df_covid_indices <- read_csv("offene_datenwerkstatt_indices.csv")

df_covid_indices_select <- df_covid_indices %>% select(date,stringency_index,containment_health_index)

df_covid_indices_month <- df_covid_indices_select %>%
  mutate(year = lubridate::year(date),month=lubridate::month(date))%>%
  group_by(year,month)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))%>%
  mutate(year_month=lubridate::make_date(year=year, month=month,day=1))
  

  
# Join data sets ----
df_join <- df_miv_zahldaten_group %>%
  left_join(df_oev_zahldaten_clean,by=c("first_dow"="startdatum_woche"))%>%
  left_join(df_fuss_velo_zahldaten_group,by=c("first_dow"="first_dow"))%>%
  left_join(df_google, by = "first_dow")%>%
  left_join(df_covid_group, by = "first_dow")%>%
  left_join(df_smart_climate_clean, by = "first_dow")%>%
  mutate(year=lubridate::year(first_dow))%>%
  mutate(month=lubridate::month(first_dow))%>%
  mutate(day=lubridate::day(first_dow))%>%
  relocate(year,month,day,weeknr,.after=first_dow)%>%
  left_join(df_weather_clean, by = c("year", "month"))%>% 
  rename(total_oev=fahrgaeste_einsteiger)%>%
  mutate(total_transport=total_fahrzeug+coalesce(total_oev,0)+coalesce(total_fuss,0)+coalesce(total_velo,0))%>%
  rename(date=first_dow)
  

# Write data ----
write.csv(df_join,"offene_datenwerkstatt.csv",row.names = FALSE,na = "")

# Join data sets month
df_join_month <- df_miv_zahldaten_group_month %>%
  left_join(df_oev_zahldaten_start_month)%>%
  left_join(df_fuss_velo_zahldaten_group_month)%>%
  left_join(df_weather_clean)%>%
  left_join(df_google_month)%>%
  left_join(df_covid_group_month)%>%
  left_join(df_covid_indices_month)

# Pivot transport
df_join_month_long <- df_join_month %>%
  relocate(year_month,.before=total_fahrzeug)%>%
  rename(car=total_fahrzeug)%>%
  rename(fuss=total_fuss)%>%
  rename(velo=total_velo)%>%
  rename(oev=total_oev)%>%
  pivot_longer(cols=c(car,fuss,velo,oev),names_to = "mode",values_to = "total_per_mode")
# Pivot Google
df_join_month_long_google <- df_join_month %>%
  relocate(year_month,.before=total_fahrzeug)%>%
  rename(car=total_fahrzeug)%>%
  rename(fuss=total_fuss)%>%
  rename(velo=total_velo)%>%
  rename(oev=total_oev)%>%
  pivot_longer(cols=c(retail_and_recreation_percent_change_from_baseline,
                      residential_percent_change_from_baseline,
                      workplaces_percent_change_from_baseline,
                      parks_percent_change_from_baseline,
                      transit_stations_percent_change_from_baseline,
                      grocery_and_pharmacy_percent_change_from_baseline),names_to = "google_change_type",values_to = "google_change_value")

View(df_join_month_long)  
write.csv(df_join_month,"offene_datenwerkstatt_month.csv",row.names = FALSE,na = "")
write.csv(df_join_month_long,"offene_datenwerkstatt_month_long.csv",row.names = FALSE,na = "")
write.csv(df_join_month_long_google,"offene_datenwerkstatt_month_google_long.csv",row.names = FALSE,na = "")


# Union disagg data ----
df_join_disagg <- df_miv_zahldaten_per_site %>%
  dplyr::full_join(df_fuss_velo_zahldaten_per_site, by = c("first_dow", "sitecode", "weeknr"))%>%
  select(-traffictype)%>%
  mutate(sitename=coalesce(sitename.x,sitename.y))%>%
  rename(car=total_fahrzeug)%>%
  mutate(year=lubridate::year(first_dow))%>%
  mutate(month=lubridate::month(first_dow))%>%
  mutate(day=lubridate::day(first_dow))%>%
  left_join(df_weather_clean, by = c("year", "month"))%>%
  left_join(df_google, by = "first_dow")%>%
  left_join(df_covid_group, by = "first_dow")%>%
  left_join(df_smart_climate_clean, by = "first_dow")%>%
  relocate(year,month,day,weeknr,sitename,.after=first_dow)%>%
  select(-sitename.x,-sitename.y) %>%
  pivot_longer(cols=c(car,fuss,velo),names_to = "mode",values_to = "total_per_mode")%>%
  rename(date=first_dow)
  
# Write data disagg ----
write.csv(df_join_disagg,"offene_datenwerkstatt_site_long.csv",row.names = FALSE,na = "")

# Pivot to long ---
df_long <- df_join %>% select(-total_transport) %>%
  pivot_longer(cols=c(total_fahrzeug,total_oev,total_fuss,total_velo),names_to = "mode",values_to = "total_per_mode")
# Write data long ----
write.csv(df_long,"offene_datenwerkstatt_long.csv",row.names = FALSE,na = "")


# Read zahlstellen ----
sf_zahlstellen <- sf::read_sf("data/basel_zahlstellen/100038.shp")
df_zahlstellen <- sf_zahlstellen %>% as.data.frame() %>% select(-geometry)

write.csv(df_zahlstellen,"offene_datenwerkstatt_zahlstellen.csv",row.names = FALSE,na = "")

