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

wiatr_kierunek <- wiatr_kierunek %>% 
  select(data_time=X3, kierunek=X4)

save(wiatr_kierunek, file="data/wiatr_kierunek.RData")

wiatr_kierunek_dzien <- wiatr_kierunek %>% 
  mutate(data=as_date(data_time)) %>% 
  group_by(data) %>% 
  summarise(kierunek_min=min(kierunek),
            kierunek_med=median(kierunek),
            kierunek_max=max(kierunek)) 
  
summary(wiatr_kierunek_dzien)

ggplot(wiatr_kierunek_dzien, aes(x=data, y=kierunek_med)) +
  geom_point()
