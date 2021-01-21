library(tidyverse)
library(lubridate)
library(zoo)
setwd("contagi/province")
casi <- read.csv("https://github.com/pcm-dpc/COVID-19/raw/master/dati-province/dpc-covid19-ita-province.csv")

province_fvg <- casi %>% 
  filter(
    denominazione_regione == "Friuli Venezia Giulia"
  )

str(province_fvg)

province_fvg <- province_fvg %>% 
  select(
    data,
    codice_provincia,
    denominazione_provincia,
    sigla_provincia,
    totale_casi
  )

province_fvg$data <- ymd_hms(province_fvg$data)
province_fvg$data <- as.Date(province_fvg$data)

province_fvg <- province_fvg %>% 
  filter(
    denominazione_provincia != "In fase di definizione/aggiornamento"
  )

province_fvg <- province_fvg %>% 
  group_by(denominazione_provincia) %>%
  mutate(
    casi_giornalieri = totale_casi - lag(totale_casi),
    nuovi_positivi_media_mobile = round(rollmeanr(casi_giornalieri, k = 7, fill = 0))
  )

oggi = Sys.Date()
province_fvg_oggi <- province_fvg %>% 
  filter(
    data == oggi
  )



#wide format con i dati giornalieri
province_fvg_wide <- province_fvg %>% 
  select(
    data,
    denominazione_provincia,
    casi_giornalieri
  ) %>% 
  filter(
    denominazione_provincia != "Fuori Regione / Provincia Autonoma",
    data > Sys.Date()-31
  ) %>% 
  spread(denominazione_provincia, casi_giornalieri)

#wide format con le medie mobili
province_fvg_wide_mediemobili <- province_fvg %>% 
  select(
    data,
    denominazione_provincia,
    nuovi_positivi_media_mobile
  ) %>% 
  filter(
    denominazione_provincia != "Fuori Regione / Provincia Autonoma",
    data > Sys.Date()-31
  ) %>% 
  spread(denominazione_provincia, nuovi_positivi_media_mobile)

#renaming columns in wide_mediemobili
province_fvg_wide_mediemobili <- province_fvg_wide_mediemobili %>% 
  rename(
    Gorizia_mediamobile = Gorizia,
    Pordenone_mediamobile = Pordenone,
    Udine_mediamobile = Udine,
    Trieste_mediamobile = Trieste
  )

province_fvg_wide <- left_join(province_fvg_wide,province_fvg_wide_mediemobili, by = "data")
province_fvg_wide <- province_fvg_wide %>% 
  select(
    data,
    Udine,
    Udine_mediamobile,
    Pordenone,
    Pordenone_mediamobile,
    Trieste,
    Trieste_mediamobile,
    Gorizia,
    Gorizia_mediamobile
  )

write_csv(province_fvg_wide, file = "data/province_fvg_last30days_wide.csv")
write_csv(province_fvg_oggi, file = "data/province_fvg_latest.csv")
write_csv(province_fvg, file = "data/province_fvg_complessivo.csv")
