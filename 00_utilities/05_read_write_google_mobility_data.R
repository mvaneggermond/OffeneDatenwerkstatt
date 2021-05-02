library(tidyverse)

# Empty the environment
rm(list=ls(all=TRUE))

# Read the google data
df_2020 <- readr::read_csv("data/google_mobility_reports/2020_CH_Region_Mobility_Report.csv",
                           guess_max=10000,
                           col_types = cols(
                             country_region_code = col_character(),
                             country_region = col_character(),
                             sub_region_1 = col_character(),
                             sub_region_2 = col_logical(),
                             metro_area = col_logical(),
                             iso_3166_2_code = col_character(),
                             census_fips_code = col_logical(),
                             place_id = col_character(),
                             date = col_date(),
                             retail_and_recreation_percent_change_from_baseline = col_double(),
                             grocery_and_pharmacy_percent_change_from_baseline = col_double(),
                             parks_percent_change_from_baseline = col_double(),
                             transit_stations_percent_change_from_baseline = col_double(),
                             workplaces_percent_change_from_baseline = col_double(),
                             residential_percent_change_from_baseline = col_double()
                           ))
df_2021 <- readr::read_csv("data/google_mobility_reports/2021_CH_Region_Mobility_Report.csv",guess_max=10000,
                           col_types = cols(
                             country_region_code = col_character(),
                             country_region = col_character(),
                             sub_region_1 = col_character(),
                             sub_region_2 = col_logical(),
                             metro_area = col_logical(),
                             iso_3166_2_code = col_character(),
                             census_fips_code = col_logical(),
                             place_id = col_character(),
                             date = col_date(),
                             retail_and_recreation_percent_change_from_baseline = col_double(),
                             grocery_and_pharmacy_percent_change_from_baseline = col_double(),
                             parks_percent_change_from_baseline = col_double(),
                             transit_stations_percent_change_from_baseline = col_double(),
                             workplaces_percent_change_from_baseline = col_double(),
                             residential_percent_change_from_baseline = col_double()
                           ))

# Bind the data
df_all <- dplyr::bind_rows(df_2020,df_2021)
# Write the data
write_csv(df_all,"data/google_mobility_reports/2020_2021_CH_Region_Mobility_Report.csv")

