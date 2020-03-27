library(tidyverse)
library(lubridate)

# http://looko2.com/Archives/

pobierz_looko2 <- function(){
  
  dni <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="+1 day")
  
  for(dzien in 1:length(dni)){
    
    try(download.file(url = paste0("http://looko2.com/Archives/Archives_",dni[dzien],".csv"), destfile = paste0("looko2/d_",dni[dzien],".csv")))
    Sys.sleep(1)
    
  }
  
}

pobierz_looko2()

# liczba kolumn

pliki <- list.files("looko2/")

n_col <- function(file){
  
  dir <- paste0("looko2/", file)
  
  d <- read_csv(dir, col_names = F, col_types = cols())
  
  return(ncol(d))
}

kolumny <- map(pliki, n_col)

kolumny_df <- data.frame(plik=pliki, kol=unlist(kolumny))

# 17.02.2018 - 11 kolumn
# 9.05.2018 - 13 kolumn

d10 <- read_csv("looko2/d_2017-01-01.csv", col_names = F, col_types = cols())

d11 <- read_csv("looko2/d_2018-02-17.csv", col_names = F, col_types = cols())

d13 <- read_csv("looko2/d_2018-05-09.csv", col_names = F, col_types = cols())

head(d10)
head(d11)
head(d13)

# wczytanie danych

read_file_o2 <- function(file){
  
  # file <- "d_2018-02-17.csv"
  
  date_file <- as_date(str_sub(file, 3, 12))
  
  dir <- paste0("looko2/", file)
  
  if(date_file < "2018-02-17"){
    
    d <- read_csv(dir, col_names = F, col_types = cols())
    
  } else if(date_file > "2018-05-08"){
    
    d <- read_csv(dir, col_names = F, col_types = cols()) %>% 
      select(-c(10,12,13)) %>% 
      rename(X10=X11)
    
  } else {
    
    d <- read_csv(dir, col_names = F, col_types = cols()) %>% 
      select(-10) %>% 
      rename(X10=X11)
    
  }
  
  return(d)
  
}

looko2_smog <- map_df(pliki, read_file_o2)

colnames(looko2_smog) <- c("data", "godzina", "dzien", "miesiac", "rok", "id_czujnika", "srednia_pm1", "srednia_pm2_5", "srednia_pm10", "nazwa")

save(looko2_smog, file = "data/looko2_smog.RData")

looko2_pszczyna <- looko2_smog %>% 
  filter(nazwa == "PSZCZYNA_Bielska10")

save(looko2_pszczyna, file = "data/looko2_pszczyna.RData")
