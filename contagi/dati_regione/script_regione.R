library(tidyverse)
library(lubridate)
library(zoo)
casi <- read.csv("https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni.csv")

fvg <- casi %>% 
  filter(
    denominazione_regione == "Friuli Venezia Giulia"
  )

str(fvg)

fvg$data <- ymd_hms(fvg$data)
fvg$data <- as.Date(fvg$data)
fvg_backup <- fvg

#fvg_from_oct <- fvg %>% 
  filter(
    data >= "2020-10-01"
  )

fvg <- fvg %>% 
  select(
    data,
    ricoverati_con_sintomi:casi_testati,
    ingressi_terapia_intensiva,
    totale_positivi_test_molecolare:tamponi_test_antigenico_rapido
  )

fvg <- fvg %>% 
  mutate(
    tamponi_giornaliero = tamponi - lag(tamponi),
    tamponi_molecolare_giornaliero = tamponi_test_molecolare - lag(tamponi_test_molecolare),
    tamponi_rapido_giornaliero = tamponi_test_antigenico_rapido - lag(tamponi_test_antigenico_rapido),
    positivi_molecolare_giornaliero = totale_positivi_test_molecolare - lag(totale_positivi_test_molecolare),
    positivi_rapido_giornaliero = totale_positivi_test_antigenico_rapido - lag(totale_positivi_test_antigenico_rapido),
    Tasso_positivi = round((nuovi_positivi/tamponi_giornaliero)*100,2),
    tasso_positivi_molecolare = round((positivi_molecolare_giornaliero/tamponi_molecolare_giornaliero)*100,2),
    tasso_positivi_rapido = round((positivi_rapido_giornaliero/tamponi_rapido_giornaliero)*100,2),
    nuovi_positivi_media_mobile = round(rollmeanr(nuovi_positivi, k = 7, fill = 0))
  )


ieri = Sys.Date()-1
fvg_ieri <- fvg %>% 
  filter(
    data == ieri
  )

write_csv(fvg_ieri, file = paste0("data/fvg_", ieri, ".csv"))
write_csv(fvg, file = "data/fvg_complessivo.csv")
