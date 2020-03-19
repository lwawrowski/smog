library(tidyverse)

# http://looko2.com/Archives/

d <- read_csv("looko2/Archives_2019-01-01.csv", col_names = F) %>% 
  filter(X11 == "PSZCZYNA_Bielska10")

d <- read_csv("looko2/Archives_2017-01-01.csv", col_names = F) %>% 
  filter(X10 == "PSZCZYNA_Bielska10")

try(download.file(url = "http://looko2.com/Archives/2017/Archives_2017-01-02.csv", destfile = "looko2/Archives_2017-01-02.csv"))

pobierz_looko2 <- function(){
  
  dni <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="+1 day")
  
  for(dzien in 1:length(dni)){
    
    try(download.file(url = paste0("http://looko2.com/Archives/Archives_",dni[dzien],".csv"), destfile = paste0("looko2/d_",dni[dzien],".csv")))
    Sys.sleep(1)
    
  }
  
}

pobierz_looko2()
