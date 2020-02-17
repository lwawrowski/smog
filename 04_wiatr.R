library(tidyverse)
library(lubridate)

# https://dane.imgw.pl/datastore
# https://dane.imgw.pl/datastore/getfiledown/Arch/Telemetria/Meteo/2019/Meteo_2019-12.zip

wczytaj_pliki <- function(nazwa){
  
  d <- read_csv2(str_c("wiatr/",nazwa), col_names = F) %>% 
    filter(X1==249180010)
  
  return(d)
}

# kierunek wiatru

pliki_wiatr_kierunek <- str_c(sort(rep(str_c("B00202A_",2015:2019),12)),"_",sprintf("%02d",1:12),".csv")

wiatr_kierunek <- map_df(pliki_wiatr_kierunek, wczytaj_pliki)

summary(wiatr_kierunek)

wiatr_kierunek <- wiatr_kierunek %>% 
  select(data_time=X3, kierunek=X4) %>% 
  mutate(data=as_date(data_time),
         kierunek8=cut(x=kierunek, breaks = seq(from = 0, to = 360, by = 22.5), 
                       labels = c("N", "NE", "NE", "E", "E", "SE", "SE", "S", "S", "SW", "SW", "W", "W", "NW", "NW", "N")))

# siła wiatru

pliki_wiatr_sila <- str_c(sort(rep(str_c("B00702A_",2015:2019),12)),"_",sprintf("%02d",1:12),".csv")

wiatr_sila <- map_df(pliki_wiatr_sila, wczytaj_pliki)

wiatr_sila <- wiatr_sila %>% 
  select(data_time=X3, sila=X4)

wiatr_kierunek_sila <- inner_join(wiatr_kierunek, wiatr_sila, by = "data_time")

save(wiatr_kierunek_sila, file="data/wiatr_kierunek_sila.RData")

# dominanta dzienna

wiatr_kierunek_dzien_dom <- wiatr_kierunek_sila %>% 
  group_by(data,kierunek8) %>% 
  summarise(n=n(),
            sila_min=min(sila),
            sila_sr=mean(sila),
            sila_med=median(sila),
            sila_max=max(sila)) %>% 
  group_by(data) %>% 
  top_n(1,n) %>% 
  top_n(1,sila_sr) %>% 
  ungroup()
  
wiatr_kierunek_dzien_med <- wiatr_kierunek_sila %>% 
  group_by(data) %>% 
  summarise(kierunek_min=min(kierunek),
            kierunek_med=median(kierunek),
            kierunek_max=max(kierunek)) %>% 
  mutate(kierunek8=cut(x=kierunek_med, breaks = seq(from = 0, to = 360, by = 22.5), 
                       labels = c("N", "NE", "NE", "E", "E", "SE", "SE", "S", "S", "SW", "SW", "W", "W", "NW", "NW", "N")))
  
table(wiatr_kierunek_dzien_dom$kierunek8);table(wiatr_kierunek_dzien_med$kierunek8)
table(wiatr_kierunek_dzien_dom$kierunek8, wiatr_kierunek_dzien_med$kierunek8)

ggplot(wiatr_kierunek_dzien_dom, aes(x=data, y=sila_med, color=kierunek8)) +
  geom_point()

# kierunek wiatru a przekroczenia

load("data/pm10.RData")

pm10_wiatr <- inner_join(pm10_pszczyna, wiatr_kierunek_dzien_dom, by="data") 

ggplot(pm10_wiatr, aes(x=rok, y=sila_med, fill=kwartal)) + geom_boxplot()

kwartal_wiatr <- pm10_wiatr %>% 
  count(kwartal, kierunek8)

treemap::treemap(kwartal_wiatr, index = c("kwartal", "kierunek8"), vSize = "n")

table(pm10_wiatr$poziom, pm10_wiatr$kierunek8)

table(pm10_wiatr$indeks, pm10_wiatr$kierunek8)

ggplot(pm10_wiatr, aes(y=pm10, x=sila_med, color=kierunek8)) +
  geom_point()

indeks_wiatr <- pm10_wiatr %>% 
  count(indeks, kierunek8)

treemap::treemap(indeks_wiatr, index = c("indeks", "kierunek8"), vSize = "n")

poziom_wiatr <- pm10_wiatr %>% 
  count(poziom, kierunek8)

treemap::treemap(poziom_wiatr, index = c("poziom", "kierunek8"), vSize = "n")

pm10_wiatr %>% 
  count(indeks, kierunek8) %>% 
  group_by(indeks) %>% 
  mutate(proc=n/n())
