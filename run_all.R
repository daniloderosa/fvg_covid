library(tidyverse)
library(lubridate)
library(zoo)
#regioni
setwd("contagi/dati_regione")
casi <- read.csv("https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni.csv")

fvg <- casi %>% 
  filter(
    denominazione_regione == "Friuli Venezia Giulia"
  )

str(fvg)

fvg$data <- ymd_hms(fvg$data)
fvg$data <- as.Date(fvg$data)

fvg <- fvg %>% 
  select(
    data,
    ricoverati_con_sintomi:casi_testati,
    ingressi_terapia_intensiva,
    totale_positivi_test_molecolare:tamponi_test_antigenico_rapido
  )

fvg <- fvg %>% 
  mutate(
    casi_testati_giornaliero = casi_testati - lag(casi_testati),
    tamponi_giornaliero = tamponi - lag(tamponi),
    tamponi_molecolare_giornaliero = tamponi_test_molecolare - lag(tamponi_test_molecolare),
    tamponi_rapido_giornaliero = tamponi_test_antigenico_rapido - lag(tamponi_test_antigenico_rapido),
    positivi_molecolare_giornaliero = totale_positivi_test_molecolare - lag(totale_positivi_test_molecolare),
    positivi_rapido_giornaliero = totale_positivi_test_antigenico_rapido - lag(totale_positivi_test_antigenico_rapido),
    Tasso_positivi = round((nuovi_positivi/tamponi_giornaliero)*100,2),
    tasso_positivi_molecolare = round((positivi_molecolare_giornaliero/tamponi_molecolare_giornaliero)*100,2),
    tasso_positivi_rapido = round((positivi_rapido_giornaliero/tamponi_rapido_giornaliero)*100,2),
    nuovi_positivi_media_mobile = round(rollmeanr(nuovi_positivi, k = 7, fill = 0)),
    deceduti_giornalieri = deceduti - lag(deceduti)
  )


oggi = Sys.Date()
fvg_oggi <- fvg %>% 
  filter(
    data == oggi
  )

fvg_30days <- fvg %>% 
  filter(
    data > Sys.Date()-31
  ) %>% 
  select(
    data,
    nuovi_positivi,
    nuovi_positivi_media_mobile,
    deceduti_giornalieri,
    terapia_intensiva,
    ingressi_terapia_intensiva,
    Tasso_positivi,
    tasso_positivi_molecolare,
    tasso_positivi_rapido
  )

write_csv(fvg_oggi, file = "data/fvg_latest.csv")
write_csv(fvg_30days, file = "data/fvg_last30days.csv")
write_csv(fvg, file = "data/fvg_complessivo.csv")


#province
setwd("../province")
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


# Incidenza
popolazione_province <- readxl::read_xlsx("../../../Dataset Popolazione/Popolazione province.xlsx")

incidenza <- province_fvg_wide %>% 
  select(data, Udine, Pordenone, Trieste, Gorizia) %>% 
  mutate(nuovi_positivi_Udine = round(rollsumr(Udine, k = 7, fill = 0)),
         nuovi_positivi_Pordenone = round(rollsumr(Pordenone, k = 7, fill = 0)),
         nuovi_positivi_Trieste = round(rollsumr(Trieste, k = 7, fill = 0)),
         nuovi_positivi_Gorizia = round(rollsumr(Gorizia, k = 7, fill = 0)),
         incidenza_Udine = round((nuovi_positivi_Udine/526474)*100000),
         incidenza_Pordenone = round((nuovi_positivi_Pordenone/310502)*100000),
         incidenza_Trieste = round((nuovi_positivi_Trieste/231445)*100000),
         incidenza_Gorizia = round((nuovi_positivi_Gorizia/137795)*100000)) %>% 
  filter(data > Sys.Date() - 24)
write_csv(incidenza, file = "data/incidenza_province_fvg.csv")



# Vaccini
# setwd("../../vaccini")
# 
# età <- read_csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv")
# 
# fvg_età <- età %>% 
#   filter(
#     area == "FVG"
#   )
# 
# fvg_filter_età <- fvg_età %>% 
#   select(
#     data_somministrazione,
#     fascia_anagrafica,
#     sesso_femminile,
#     sesso_maschile
#   )
# 
# fvg_filter_età <- fvg_filter_età %>% 
#   group_by(fascia_anagrafica) %>% 
#   summarise(
#     donne = sum(sesso_femminile),
#     uomini = sum(sesso_maschile) 
#   )
# 
# 
# fvg_vaccini_età_giornaliero <- fvg_età
# 
# 
# Johnson_Johnson <- fvg_vaccini_età_giornaliero %>% 
#   filter(fornitore == "Janssen")
# 
# Johnson_Johnson$seconda_dose = Johnson_Johnson$prima_dose
# Johnson_Johnson$prima_dose = 0
# 
# nonJohnson_Johnson <- fvg_vaccini_età_giornaliero %>% 
#   filter(fornitore != "Janssen")
# 
# fvg_vaccini_età_giornaliero <- rbind(nonJohnson_Johnson, Johnson_Johnson)
# 
# write_csv(fvg_filter_età, file = "eta_sesso.csv")
# write_csv(fvg_vaccini_età_giornaliero, file = "vaccini_fvg_giornaliero_età.csv")
