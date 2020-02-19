library(tidyverse)

# http://looko2.com/Archives/

d <- read_csv("looko2/Archives_2019-01-01.csv", col_names = F) %>% 
  filter(X11 == "PSZCZYNA_Bielska10")

d <- read_csv("looko2/Archives_2017-01-01.csv", col_names = F) %>% 
  filter(X10 == "PSZCZYNA_Bielska10")
