library(tidyverse)
library(lubridate)

# Empty the environment
rm(list=ls(all=TRUE))


# Base directory for the counts
base_dir <- "data/zrh_miv/"

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
  .default = col_character(),
  EKoord = col_double(),
  NKoord = col_double(),
  Knummer = col_double(),
  AnzDetektoren = col_double(),
  D1ID = col_double(),
  MessungDatZeit = col_datetime(format = ""),
  LieferDat = col_date(format = ""),
  AnzFahrzeuge = col_double()
)

# Read the first file
# Set the guess of the data types to a high column
df <- readr::read_csv(paste0(base_dir,f), guess_max = 100000,col_types=col_types)



# Loop over the files
for (i in 2:length(list_of_files)){
  
  # Get the file from lists
  file <- list_of_files[i]
  f <- as.character(file[1]) 
  print(paste("Reading",f,sep=" "))
  
  df_loop <- readr::read_csv(paste0(base_dir,f), guess_max = 100000, col_types = col_types)
  
  df <- bind_rows(df,df_loop)
}


# Write out the count locations
df_agg_counts_filter <- df %>% 
  dplyr::mutate(year=lubridate::year(LieferDat)) %>% 
  dplyr::filter(year>2009)



#####
# Write out the demo data set
####
readr::write_csv(df_agg_counts_filter,"data/zurich_miv_2012_2021.csv")
# Write out the count locations
df_agg_counts_filter <- df %>% 
  dplyr::mutate(year=lubridate::year(MessungDatZeit)) %>% 
  dplyr::filter(year>2017)

df_agg_counts_filter %>% group_by(year)%>% summarise(n=n())
nrow(df_agg_counts_filter)
readr::write_csv(df_agg_counts_filter,"data/zurich_miv_2017_2021.csv")

