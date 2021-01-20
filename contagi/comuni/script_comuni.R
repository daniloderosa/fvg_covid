library(tidyverse)
library(lubridate)
fvg_vecchi <- read_csv("https://covid19map.protezionecivile.fvg.it/history.csv")
fvg_nuovi <- read_csv("https://covid19map.protezionecivile.fvg.it/data.csv")



fvg_nuovi <- fvg_nuovi %>% 
  select(
    -(Indice)
  )
fvg_nuovi$Aggiornamento <- dmy_hm(fvg_nuovi$Aggiornamento)
fvg_nuovi$Aggiornamento <- floor_date(fvg_nuovi$Aggiornamento, "days")
fvg_vecchi$Aggiornamento <- dmy_hm(fvg_vecchi$Aggiornamento)
fvg_vecchi$Aggiornamento <- floor_date(fvg_vecchi$Aggiornamento, "days")
fvg_vecchi$Data <- dmy(fvg_vecchi$Data)
fvg_vecchi$Aggiornamento <- as.Date(fvg_vecchi$Aggiornamento)
fvg_nuovi$Aggiornamento <- as.Date(fvg_nuovi$Aggiornamento)
fvg_nuovi$Data <- fvg_nuovi$Aggiornamento

fvg_nuovi_senza_popolazione <- fvg_nuovi %>% 
  select(
    -(Popolazione)
  )

fvg_popolazione <- fvg_nuovi %>% 
  select(
    ISTAT,
    Popolazione
  )

fvg <- bind_rows(fvg_nuovi_senza_popolazione, fvg_vecchi)
fvg <- fvg %>% 
  arrange(
    Comune,
    Data
  )
rm(fvg_vecchi)
rm(fvg_nuovi)

fvgjoin <- left_join(fvg,fvg_popolazione, by="ISTAT")
fvg <- fvgjoin
rm(list = c("fvg_nuovi_senza_popolazione","fvg_popolazione","fvgjoin"))

write_csv(fvg, file=paste0("data/comuni_fvg_",ieri,".csv"), na="")
#write_delim(fvg, file="fvg_casi.csv", delim = ";", na="")