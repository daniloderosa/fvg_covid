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


#dual axis nuovi positivi e tasso positività: malino, rivedere
# fvg_from_oct %>% 
#   filter(
#     data >= "2020-12-02"
#   ) %>% 
#   ggplot(
#     aes(x=data)
#   )+
#   geom_bar(aes(y = nuovi_positivi), stat = "identity")+
#   geom_line(aes(y=Tasso_positivi*100), color="blue")+
#   geom_line(aes(y=tasso_positivi_molecolare*100), color="red")+
#   scale_y_continuous(
#     name = "First axis",
#     sec.axis = sec_axis( trans=~./100, name="Second Axis")
#   )

#viz nuovi positivi e media mobile
# fvg %>%
#   filter(
#     data >= "2020-12-02"
#   ) %>%
#   ggplot(
#     aes(x=data)
#   )+
#   geom_bar(aes(y = nuovi_positivi), stat = "identity")+
#   geom_line(aes(y=nuovi_positivi_media_mobile), color="blue")

#viz tassi di positività
# fvg_from_oct %>% 
#   filter(
#     data >= "2020-12-02"
#   ) %>% 
#   ggplot(
#     aes(x=data)
#   )+
#   geom_line(aes(y = Tasso_positivi), color = "blue")+
#   geom_line(aes(y = tasso_positivi_molecolare), color = "red")+
#   geom_line(aes(y = tasso_positivi_rapido), color = "green")

# #unire tassi di posività per long format
# fvg_gather <- fvg_from_oct %>% 
#   select(
#     data,
#     Tasso_positivi,
#     tasso_positivi_molecolare,
#     tasso_positivi_rapido
#   ) %>% 
#   gather(
#     key = "Tasso",
#     value = "Percentuale",
#     Tasso_positivi:tasso_positivi_rapido
#   )

#viz tassi di posività con legenda
# fvg_gather %>% 
#   filter(
#     data >= "2020-12-02"
#   ) %>% 
#   ggplot(
#     aes(x=data)
#   )+
#   geom_line(aes(y = Percentuale, color = Tasso))+
#   scale_color_manual(values=c("steelblue4", "tan2", "thistle4"))

ieri = Sys.Date()-1
fvg_ieri <- fvg %>% 
  filter(
    data == ieri
  )

write_csv(fvg_ieri, file = paste0("data/fvg_", ieri, ".csv"))
write_csv(fvg, file = "data/fvg_complessivo.csv")
