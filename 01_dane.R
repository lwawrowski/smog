library(tidyverse)
library(readxl)
library(lubridate)
# http://powietrze.gios.gov.pl/pjp/archives

pm10_2015 <- read_xlsx("data/pm10_pszczyna.xlsx", sheet = "2015")
pm10_2016 <- read_xlsx("data/pm10_pszczyna.xlsx", sheet = "2016")
pm10_2017 <- read_xlsx("data/pm10_pszczyna.xlsx", sheet = "2017")
pm10_2018 <- read_xlsx("data/pm10_pszczyna.xlsx", sheet = "2018")

# http://www.gios.gov.pl/pl/aktualnosci/294-normy-dla-pylow-drobnych-w-polsce

pm10_pszczyna <- union_all(pm10_2015, pm10_2016) %>% 
  union_all(., pm10_2017) %>% 
  union_all(., pm10_2018) %>% 
  mutate(data=as_date(data),
         dzien_tyg=factor(wday(data),
                          levels = 1:7, 
                          labels = c("Pn", "Wt", "Śr", "Czw", "Pt", "Sb", "Nd"),
                          ordered = T),
         kwartal=str_c("Kwartał ", quarter(data)),
         rok=as.factor(year(data)),
         poziom=cut(x = pm10,
                    breaks = c(0,50,200,300,Inf), 
                    labels = c("w normie", "dopuszczalny", "informowania", "alarmowy"), 
                    ordered_result = T))

library(imgw)

d <- meteo_daily(rank = "climate", year = 2015:2018)

imgw_pszczyna <- d %>% 
  filter(station == "PSZCZYNA") %>% 
  mutate(data=as_date(str_c(yy,"-",mm,"-",day)))