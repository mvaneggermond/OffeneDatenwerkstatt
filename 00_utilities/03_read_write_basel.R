library(tidyverse)

df <- readr::read_csv2("data/basel_fuss_velo/converted_Velo_Fuss_Count.csv")

df_filter <- df %>% filter(Year>2018,Year<2021)

readr::write_csv(df_filter,"data/basel_fuss_velo/2019_2020_converted_Velo_Fuss_Count.csv" )

df_filter_agg <- df %>% filter(Year>2018,Year<2021) %>%
  dplyr::group_by(SiteCode,SiteName,DirectionName,LaneCode,LaneName,Date,Year,Month,Day,Weekday) %>%
  dplyr::summarise(ValuesApproved=sum(Total,na.rm = T))

readr::write_csv(df_filter_agg,"data/basel_fuss_velo/2019_2020_converted_Velo_Fuss_Count_daily.csv" )
