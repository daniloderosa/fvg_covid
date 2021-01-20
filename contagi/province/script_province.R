library(tidyverse)
library(lubridate)
library(zoo)
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

write_csv(province_fvg_oggi, file = "data/province_fvg_latest.csv")
write_csv(province_fvg, file = "data/province_fvg_complessivo.csv")
