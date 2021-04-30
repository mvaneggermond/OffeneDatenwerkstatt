library(tidyverse)

# Read the data
file_name <- homog_mo_BAS.txt
df <- readr::read_table("data/wetter/homog_mo_BAS.txt",skip = 27)

readr::write_csv(df,"data/wetter/temp_basel_converted.csv")

