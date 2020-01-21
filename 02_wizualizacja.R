library(tidyverse)

load("data/pm10.RData")
load("data/imgw.RData")

# gioś
pm10_pszczyna %>% 
  filter(!is.na(pm10)) %>% 
  ggplot(aes(x=data, y = pm10, color = poziom)) +
  geom_point() +
  scale_color_manual(name = "Poziom", values = c("#1a9850", "#fdae61", "#d73027", "#67001f")) +
  xlab("Data") +
  ylab("Stężenie PM10") +
  ggtitle("Stężenie pyłu PM10 w Pszczynie w latach 2015-2019") +
  labs(caption = "Łukasz Wawrowski - wawrowski.edu.pl") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.caption = element_text(color = "grey80"))

# fill with zeros
pm10_pszczyna %>% 
  filter(!is.na(pm10)) %>% 
  count(rok, poziom) %>% 
  complete(rok, poziom, fill = list(n = 0)) %>% 
  ggplot(aes(x = poziom, y = rok, fill = n)) +
  geom_tile() +
  geom_text(aes(label=n)) +
  scale_fill_gradientn(colours = terrain.colors(4), name = "Liczba dni") +
  xlab("Poziom") +
  ylab("Rok") +
  ggtitle("Liczba dni w roku z danym poziomem stężenia pyłu PM10") +
  labs(caption = "Łukasz Wawrowski - wawrowski.edu.pl") +
  theme_light() +
  theme(plot.caption = element_text(color = "grey80"))

pm10_pszczyna %>% 
  filter(!is.na(pm10)) %>% 
  count(rok, poziom) %>% 
  complete(rok, poziom, fill = list(n = 0)) %>% 
  group_by(rok) %>% 
  mutate(proc=round(n/sum(n)*100,1)) %>% 
  ggplot(aes(x = poziom, y = rok, fill = proc)) +
  geom_tile() +
  geom_text(aes(label=proc)) +
  scale_fill_gradientn(colours = terrain.colors(4), name = "Odsetek") +
  xlab("Poziom") +
  ylab("Rok") +
  ggtitle("Odsetek dni w roku z danym poziomem stężenia pyłu PM10") +
  labs(caption = "Łukasz Wawrowski - wawrowski.edu.pl") +
  theme_light() +
  theme(plot.caption = element_text(color = "grey80"))

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

ggplot(pm10_pszczyna, aes(x=rok, y=pm10, fill=kwartal)) +
  geom_boxplot() +
  scale_fill_discrete(name = "Kwartał") +
  xlab("Rok") +
  ylab("Poziom PM10") +
  ggtitle("Rozkład stężenia pyłu PM10 w ujęciu kwartalnym w latach 2015-2019") +
  labs(caption = "Łukasz Wawrowski - wawrowski.edu.pl") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.caption = element_text(color = "grey80"))

# imgw
ggplot(imgw_pszczyna, aes(x=data, y=t2m_mean_daily)) +
  geom_point(color = "deepskyblue2") +
  xlab("Rok") +
  ylab("Średnia dobowa temperatura") +
  ggtitle("Średnia dobowa temperatura w latach 2015-2019") +
  labs(caption = "Łukasz Wawrowski - wawrowski.edu.pl") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.caption = element_text(color = "grey80"))

pm10_imgw <- select(imgw_pszczyna, data, t2m_mean_daily) %>% 
  inner_join(., select(pm10_pszczyna, data, pm10))

ggplot(pm10_imgw, aes(x=pm10, y=t2m_mean_daily)) +
  geom_point()

pm10_imgw %>% 
  pivot_longer(-data) %>% 
  ggplot(aes(x=data, y=value, color=name)) +
  geom_point() +
  geom_smooth()

pm10_imgw_std <- select(imgw_pszczyna, data, t2m_mean_daily) %>% 
  inner_join(., select(pm10_pszczyna, data, pm10)) %>% 
  mutate_if(is.numeric, scale) %>% 
  pivot_longer(-data)

ggplot(pm10_imgw_std, aes(x=data, y=value, color=name)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, size = 2)

# iloraz szans

pm10_imgw_50 <- pm10_imgw %>% 
  filter(pm10 > 50)

temp <- -5:25
or <- numeric(length(temp))

for(i in 1:length(or)){
  
  pm10_imgw_cat <- pm10_imgw %>% 
    filter(!is.na(pm10)) %>% 
    mutate(temp2=cut(x = t2m_mean_daily, breaks = c(-Inf, temp[i], Inf), labels = c("Poniżej 10 stopni", "Powyżej 10 stopni")),
           pm102=cut(x = pm10, breaks = c(0,50,Inf), labels = c("W normie", "Przekroczony")))
  
  mm <- table(pm10_imgw_cat$pm102, pm10_imgw_cat$temp2)
  mm
  
  or[i] <- (mm[2,1]/mm[1,1])/(mm[2,2]/mm[1,2])
  
}

pm10_imgw_cat <- pm10_imgw %>% 
  filter(!is.na(pm10)) %>% 
  mutate(temp2=cut(x = t2m_mean_daily, breaks = c(-Inf, 10, Inf), labels = c("Poniżej 10 stopni", "Powyżej 10 stopni")),
         pm102=cut(x = pm10, breaks = c(0,50,Inf), labels = c("W normie", "Przekroczony")))

m2 <- pm10_imgw_cat %>% 
  count(temp2,pm102)

mm <- table(pm10_imgw_cat$pm102, pm10_imgw_cat$temp2)
mm

or <- (mm[2,1]/mm[1,1])/(mm[2,2]/mm[1,2])
# szansa na przekroczenie normy poniżej 10 stopni jest 9 razy wyższe

ggplot(m2, aes(x=temp2, y=pm102, fill=n)) + 
  geom_tile() +
  geom_text(aes(label=n), color = "white")

summary(pm10_imgw_cat)