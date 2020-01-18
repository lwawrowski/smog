library(tidyverse)

# gioś
pm10_pszczyna %>% 
  filter(!is.na(pm10)) %>% 
  ggplot(aes(x=data, y = pm10, color = poziom)) +
  geom_point() +
  scale_color_manual(values = c("#1a9850", "#fdae61", "#d73027", "#67001f"))

pm10_pszczyna %>% 
  filter(!is.na(pm10)) %>% 
  count(rok, poziom)

pm10_pszczyna %>% 
  group_by(rok, dzien_tyg) %>% 
  summarise(srednia = mean(pm10, na.rm=T)) %>% 
  ggplot(aes(x=dzien_tyg, y=srednia, fill=rok)) +
  geom_col(position="dodge")

pm10_pszczyna %>% 
  group_by(rok, kwartal) %>% 
  summarise(srednia = mean(pm10, na.rm=T)) %>% 
  ggplot(aes(x=kwartal, y=srednia, fill=rok)) +
  geom_col(position="dodge")

ggplot(pm10_pszczyna, aes(x=dzien_tyg, y=pm10, fill=rok)) +
  geom_boxplot()

ggplot(pm10_pszczyna, aes(x=kwartal, y=pm10, fill=rok)) +
  geom_boxplot()

# imgw
ggplot(imgw_pszczyna, aes(x=data, y=t2m_mean_daily)) +
  geom_point() +
  geom_smooth()

pm10_imgw <- select(imgw_pszczyna, data, t2m_mean_daily) %>% 
  inner_join(., select(pm10_pszczyna, data, pm10)) %>% 
  pivot_longer(-data)

ggplot(pm10_imgw, aes(x=data, y=value, color=name)) +
  geom_point() +
  geom_smooth()

pm10_imgw_std <- select(imgw_pszczyna, data, t2m_mean_daily) %>% 
  inner_join(., select(pm10_pszczyna, data, pm10)) %>% 
  mutate_if(is.numeric, scale) %>% 
  pivot_longer(-data)

ggplot(pm10_imgw_std, aes(x=data, y=value, color=name)) +
  geom_point() +
  geom_smooth()

