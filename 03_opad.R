library(tidyverse)

load("data/pm10.RData")
load("data/imgw.RData")

opad <- imgw_pszczyna %>% 
  count(data,rr_type) %>% 
  filter(rr_type!="")

ggplot(opad, aes(x=data, y=n, fill=rr_type)) +
  geom_col()

ggplot(imgw_pszczyna, aes(x=data, y=rr_daily, fill=rr_type)) +
  geom_col()

# wiatr

ggplot(imgw_pszczyna, aes(x=data, y=ws_mean_daily)) +
  geom_col()
