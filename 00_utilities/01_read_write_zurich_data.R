library(tidyverse)
library(lubridate)

# Empty the environment
rm(list=ls(all=TRUE))

# Read the count locations
df_count_locations <- readr::read_csv("data/zurich_zahlstellen/taz.view_eco_standorte.csv", guess_max = 100000)
# Count locations selection
df_count_locations_sel <- df_count_locations %>% dplyr::select(id1,korrekturfaktor,bezeichnung,geometry)

# Base directory for the counts
base_dir <- "data/zrh_verkehrszaehlungen_werte_fussgaenger_velo/"

# Create a list of the files from the target directory
df_of_files <- list.files(path=base_dir, recursive = TRUE) %>% 
  data.frame() %>%
  rename(file_name=1) # Rename the first column to 

df_of_files <- df_of_files %>%
  filter(!grepl('~$', file_name, fixed=TRUE)) %>%
  filter(!grepl('Record', file_name, fixed=TRUE))%>%
  data.frame()
# Parse the data frame to a list for the loop
list_of_files <- as.list(df_of_files$file_name)

# Read the first file
# This is to have a base file to append the remaining files
file <- list_of_files[1]

f <- as.character(list_of_files[1]) 
print(paste("Reading",f,sep=" "))

col_types <- cols(
  FK_ZAEHLER = col_character(),
  FK_STANDORT = col_double(),
  DATUM = col_datetime(format = ""),
  VELO_IN = col_double(),
  VELO_OUT = col_double(),
  FUSS_IN = col_double(),
  FUSS_OUT = col_double(),
  OST = col_double(),
  NORD = col_double()
)

# Read the first file
# Set the guess of the data types to a high column
df <- readr::read_csv(paste0(base_dir,f), guess_max = 100000, col_types = col_types)



# Loop over the files
for (i in 2:length(list_of_files)){
  
  # Get the file from lists
  file <- list_of_files[i]
  f <- as.character(file[1]) 
  print(paste("Reading",f,sep=" "))
  
  df_loop <- readr::read_csv(paste0(base_dir,f), guess_max = 100000, col_types = col_types)
  
  df <- bind_rows(df,df_loop)
}

## Get the locations with only bicycle counts
df_velo <- df %>% 
  filter((VELO_IN+VELO_OUT)>0) %>%
  group_by(FK_ZAEHLER) %>%
  summarise(n=n())

# Aggregate the counts
df_agg_counts <- df %>%
  mutate(VELO_IN = na_if(VELO_IN, 0))%>%
  mutate(VELO_OUT = na_if(VELO_OUT, 0))%>%
  mutate(FUSS_IN = na_if(FUSS_IN, 0))%>%
  mutate(FUSS_OUT = na_if(FUSS_OUT, 0))%>%
  mutate(total_fuss=FUSS_IN,FUSS_OUT)%>%
  mutate(total_velo=VELO_IN,VELO_OUT)%>%
  mutate(year=lubridate::year(DATUM),
         month=lubridate::month(DATUM),
         day=lubridate::day(DATUM))%>%
  group_by(FK_STANDORT,FK_ZAEHLER,year,month,day)%>%
  summarise(total_fuss=sum(total_fuss,na.rm = T),
            total_velo=sum(total_velo,na.rm = T))%>% 
  mutate(datum = lubridate::make_datetime(year, month, day))

# Have a look at the data set
head(df_agg_counts)

# Write out the count locations
df_agg_counts_filter <- df_agg_counts %>% filter(year>2017)

# Join the data from count_locations
df_agg_counts_filter <- df_agg_counts_filter %>% inner_join(df_count_locations_sel,by=c("FK_STANDORT"="id1"))
head(df_agg_counts_filter)

#####
# Write out the demo data set
####
readr::write_csv(df_agg_counts_filter,"data/zurich_aggregated_2018_2021.csv")


# Aggregate the counts
df_agg_counts <- df %>%
  mutate(VELO_IN = na_if(VELO_IN, 0))%>%
  mutate(VELO_OUT = na_if(VELO_OUT, 0))%>%
  mutate(FUSS_IN = na_if(FUSS_IN, 0))%>%
  mutate(FUSS_OUT = na_if(FUSS_OUT, 0))%>%
  mutate(total_fuss=FUSS_IN,FUSS_OUT)%>%
  mutate(total_velo=VELO_IN,VELO_OUT)%>%
  mutate(year=lubridate::year(DATUM),
         month=lubridate::month(DATUM),
         day=lubridate::day(DATUM),
         hour=lubridate::hour(DATUM))%>%
  group_by(FK_STANDORT,FK_ZAEHLER,year,month,day,hour)%>%
  summarise(total_fuss=sum(total_fuss,na.rm = T),
            total_velo=sum(total_velo,na.rm = T))%>% 
  mutate(datum = lubridate::make_datetime(year, month, day, hour))

# Have a look at the data set
head(df_agg_counts)

# Write out the count locations
df_agg_counts_filter <- df_agg_counts %>% filter(year>2017)

# Join the data from count_locations
df_agg_counts_filter <- df_agg_counts_filter %>% inner_join(df_count_locations_sel,by=c("FK_STANDORT"="id1"))
head(df_agg_counts_filter)

#####
# Write out the demo data set
####
readr::write_csv(df_agg_counts_filter,"data/zurich_aggregated_2018_2021_hour.csv")

#####
# Write out aggregated yearly data sets
####
# Loop over the files
for (i in 1:length(list_of_files)){
  
  # Get the file from lists
  file <- list_of_files[i]
  f <- as.character(file[1]) 
  print(paste("Reading",f,sep=" "))
  
  df_loop <- readr::read_csv(paste0(base_dir,f), guess_max = 100000, col_types = col_types)
  
  df_agg_counts <- df_loop %>%
    #filter(FK_ZAEHLER%in%df_velo$FK_ZAEHLER)%>%
    mutate(VELO_IN = na_if(VELO_IN, 0))%>%
    mutate(VELO_OUT = na_if(VELO_OUT, 0))%>%
    mutate(FUSS_IN = na_if(FUSS_IN, 0))%>%
    mutate(FUSS_OUT = na_if(FUSS_OUT, 0))%>%
    mutate(total_fuss=FUSS_IN,FUSS_OUT)%>%
    mutate(total_velo=VELO_IN,VELO_OUT)%>%
    mutate(year=lubridate::year(DATUM),
           month=lubridate::month(DATUM),
           day=lubridate::day(DATUM),
           hour=lubridate::hour(DATUM))%>%
    group_by(FK_ZAEHLER,FK_STANDORT,year,month,day,hour)%>%
    summarise(total_fuss=sum(total_fuss,na.rm = T),
              total_velo=sum(total_velo,na.rm = T))%>% 
    mutate(datum = lubridate::make_datetime(year, month, day, hour))
  
  # Write the file
  readr::write_csv(df_agg_counts,paste0("data/zurich_processed/aggregated_",f))
  

}




