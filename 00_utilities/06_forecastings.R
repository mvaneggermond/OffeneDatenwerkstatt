# Read the libraries
# 
library(forecast)
library(tidyverse)

# Weekly forecast ------
df <- readr::read_csv("offene_datenwerkstatt.csv") %>%
  mutate(week_alt=paste0("w_",lubridate::week(date)))

df_velo <-  df %>%
  filter(year>2014,year<2020)%>%
  dplyr::select(total_velo)


# See: https://otexts.com/fpp2/weekly.html
ts_velo <- ts(df_velo[,1],
          freq=365.25/7, start=2015)

ts_velo <- ts(df_velo[,1],
              freq=365, start=2015)

# Estimate a model ()
estimate_arima_weekly_model <- function(ts,max_K)
{
  bestfit <- list(aicc=Inf)
  for(K in seq(max_K)) {
    fit <- auto.arima(ts_velo, xreg=fourier(ts_velo, K=K),
                      seasonal=FALSE,  allowdrift = TRUE)
    if(fit[["aicc"]] < bestfit[["aicc"]]) {
      bestfit <- fit
      bestK <- K
    }
  }
  fc <- forecast(bestfit,
                 xreg=fourier(ts_velo, K=bestK, h=104))
  
  return(fc)
}

forecast_velo <- estimate_arima_weekly_model(ts_velo,25)
plot(forecast_velo)

forecast_velo_no_fourier <- auto.arima(ts_velo)
plot(forecast(forecast_velo_no_fourier))

df_forecast_velo <- forecast_velo$mean 

df_forecast_velo <- df_forecast_velo %>% as.data.frame() %>%
  mutate(rn=row_number()) %>%
  mutate(year=if_else(rn<53,2020,2021))%>%
  group_by(year)%>%
  mutate(week=row_number())

# Monthly forecast ------

df_month <- readr::read_csv("offene_datenwerkstatt_month.csv") 
## Velo
df_month_velo <-  df_month %>%
  filter(year>2014,year<2020)%>%
  dplyr::select(total_velo)

ts_velo_month <- ts(df_month_velo[,1],
              freq=12, start=2015)

fit_month_velo <- auto.arima(ts_velo_month, 
                  seasonal=TRUE,  allowdrift = TRUE)
fc_velo <- forecast(fit_month_velo)

plot(fc_velo)

## MIV
df_month_miv <-  df_month %>%
  filter(year>2014,year<2020)%>%
  dplyr::select(total_fahrzeug)

ts_miv_month <- ts(df_month_miv[,1],
                    freq=12, start=2015)

fit_month_miv <- auto.arima(ts_miv_month, 
                             seasonal=TRUE,  allowdrift = TRUE)
fc_miv <- forecast(fit_month_miv)

plot(fc_miv)


## Fuss
df_month_fuss <-  df_month %>%
  filter(year>2014,year<2020)%>%
  dplyr::select(total_fuss)

ts_fuss_month <- ts(df_month_fuss[,1],
                   freq=12, start=2015)

fit_month_fuss <- auto.arima(ts_fuss_month, 
                            seasonal=TRUE,  allowdrift = TRUE)
fc_fuss <- forecast(fit_month_fuss)

plot(fc_fuss)

# Use the forecasts
# Velo
df_forecast_velo_month <- fc_velo$mean 

df_forecast_velo_month <- df_forecast_velo_month %>% as.data.frame() %>%
  mutate(rn=row_number()) %>%
  mutate(year=if_else(rn<13,2020,2021))%>%
  group_by(year)%>%
  mutate(month=row_number())%>%
  select(year,month,forecast_velo=x)

# MIV
df_forecast_miv_month <- fc_miv$mean 

df_forecast_miv_month <- df_forecast_miv_month %>% as.data.frame() %>%
  mutate(rn=row_number()) %>%
  mutate(year=if_else(rn<13,2020,2021))%>%
  group_by(year)%>%
  mutate(month=row_number())%>%
  select(year,month,forecast_miv=x)

# Fuss
df_forecast_fuss_month <- fc_fuss$mean 

df_forecast_fuss_month <- df_forecast_fuss_month %>% as.data.frame() %>%
  mutate(rn=row_number()) %>%
  mutate(year=if_else(rn<13,2020,2021))%>%
  group_by(year)%>%
  mutate(month=row_number())%>%
  select(year,month,forecast_fuss=x)



# Join the forecast, calculate indicators
df_month_forecast_actual <- 
  df_month %>%
  filter(year>2019)%>%
  inner_join(df_forecast_velo_month)%>%
  inner_join(df_forecast_miv_month)%>%
  inner_join(df_forecast_fuss_month)%>%
  mutate(diff_velo=total_velo-forecast_velo,
         diff_miv=total_fahrzeug-forecast_miv,
         diff_fuss=total_fuss-forecast_fuss,
         diff_oev=total_oev-7524000)%>%
  mutate(perc_velo=100*diff_velo/forecast_velo)%>%
  mutate(perc_miv=100*diff_miv/forecast_miv)%>% 
  mutate(perc_fuss=100*diff_fuss/forecast_fuss)%>% 
  mutate(perc_oev=100*diff_oev/7524000)

df_month_forecast_actual_fuss <- df_month_forecast_actual %>% 
                                      select(-perc_oev,-perc_miv,-perc_velo,
                                           -diff_velo,-diff_miv,-diff_oev,
                                           -total_fahrzeug,total_oev,total_velo)%>%
                                      rename(total=total_fuss,perc=perc_fuss,diff=diff_fuss)%>%
                                      mutate(transport_mode='Fuss')

df_month_forecast_actual_miv <- df_month_forecast_actual %>% 
  select(-perc_oev,-perc_fuss,-perc_velo,
         -diff_velo,-diff_fuss,-diff_oev,
         -total_fuss,total_oev,total_velo)%>%
  rename(total=total_fahrzeug,perc=perc_miv,diff=diff_miv)%>%
  mutate(transport_mode='MIV')

df_month_forecast_actual_oev <- df_month_forecast_actual %>% 
  select(-perc_miv,-perc_fuss,-perc_velo,
         -diff_velo,-diff_fuss,-diff_miv,
         -total_fuss,total_fahrzeug,total_velo)%>%
  rename(total=total_fahrzeug,perc=perc_oev,diff=diff_oev)%>%
  mutate(transport_mode='ÖV')

df_month_forecast_actual_velo <- df_month_forecast_actual %>% 
  select(-perc_miv,-perc_fuss,-perc_oev,
         -diff_oev,-diff_fuss,-diff_miv,
         -total_fuss,total_fahrzeug,total_oev)%>%
  rename(total=total_velo,perc=perc_velo,diff=diff_velo)%>%
  mutate(transport_mode='Velo')
  View(df_month_forecast_actual_fuss)

df_long <- bind_rows(df_month_forecast_actual_fuss,df_month_forecast_actual_miv,df_month_forecast_actual_oev,df_month_forecast_actual_velo)
write.csv(df_month_forecast_actual,"offene_datenwerkstatt_forecast.csv")

write.csv(df_long,"offene_datenwerkstatt_forecast_long.csv")

