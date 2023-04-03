# Packages ####
rm(list=ls(all=TRUE))
`%ni%` <- Negate(`%in%`)
library(tidyverse)


# Data ####


fish <- readRDS("data/Length.Age.all.species.rds")

lakes <- readRDS("data/StudyAreaLake.rds") %>% 
  dplyr::select(Lake.ID, state, usgs.id, x_UTM.state, y_UTM.state)

temp <- rbind(readRDS("data/Lake.Surface.Temperature01.rds"), readRDS("data/Lake.Surface.Temperature02.rds"))


## data combine ####
dat <- inner_join(inner_join(lakes, temp, by = c("usgs.id" = "site.id")),
                  fish, by = c("Lake.ID" = "Lake.ID", "year" = "year.b")) %>% 
  group_by(Lake.ID, state.y, year, species1, age) %>% 
  summarise(length.mean = mean(Length.mm), 
            x_UTM.state = first(x_UTM.state), 
            y_UTM.state = first(y_UTM.state),
            mean.3yr.temp = first(mean.3yr.temp), 
            mean.3yr.temp.ggd  = first(mean.3yr.temp.ggd),
            GGD.mean = first(GGD.mean), 
            GGD.sum = first(GGD.sum))


dat.2 <- dat %>% filter(age %in% c(3))
dat.3 <- dat %>% filter(age %in% c(1:10))
graphics.off()
quartz(height = 9, width = 9)

len.3yr <- ggplot() +
  geom_point(data = dat.2, aes(y = length.mean, x = mean.3yr.temp, color = year)) +
  ylab("Length at 3 yrs") + xlab("Mean 3 yr temperature") +
  viridis::scale_color_viridis() +
  geom_smooth(data = dat.2, aes(y = length.mean, x = mean.3yr.temp), method = "loess", se = FALSE, size = 2, color = "black")+
  facet_wrap(~species1, scales = "free") +
  theme_classic()
ggsave("Figures/Length.3yr.m.temp.pdf", len.3yr)

len.ggd.sum <-ggplot() +
  geom_point(data = dat.2, aes(y = length.mean, x = GGD.sum, color = year)) +
  viridis::scale_color_viridis() +
  ylab("Length at 3 yrs") + xlab("Cumulative Growing Degree Days over a 3 yr period (> 5 C)") +
  geom_smooth(data = dat.2, aes(y = length.mean, x = GGD.sum), method = "loess", se = FALSE, size = 2, color = "black")+
  facet_wrap(~species1, scales = "free") +
  theme_classic()
ggsave("Figures/Length.Cmlt.GDD.pdf", len.ggd.sum)

len.1_5yr <- ggplot() +
  # geom_point(data = dat, aes(y = Length.mm, x = mean.3yr.temp, color = year)) +
  geom_smooth(data = dat.3, aes(y = length.mean, x = mean.3yr.temp, color = as.factor(age)), method = "loess", se = FALSE, size = 1.2)+
  viridis::scale_color_viridis(discrete = T) +
  ylab("Length") + xlab("Mean 3 yr temperature from age = 0") +
  facet_wrap(~species1, scales = "free") +
  theme_classic()
ggsave("Figures/Length.temp.age1_10.pdf", len.1_5yr)


len.cohrot <- ggplot() +
  # geom_point(data = dat, aes(y = Length.mm, x = mean.3yr.temp, color = year)) +
  geom_smooth(data = dat.3, aes(y = length.mean, x = year, color = as.factor(age)), method = "loess", se = FALSE, size = 1.2)+
  viridis::scale_color_viridis(discrete = T) +
  ylab("Length") + xlab("Year where age = 0") +
  facet_wrap(~species1, scales = "free") +
  theme_classic()
ggsave("Figures/Length.year.age1_10.pdf", len.cohrot)


wae_nop <- ggplot() +
  geom_point(data = filter(dat.2, species1 %in% c("walleye", "northern_pike")), aes(y = length.mean, x = mean.3yr.temp, color = year)) +
  viridis::scale_color_viridis() +
  ylab("Length at 3 yrs") + xlab("Mean 3 yr temperature") +
  geom_smooth(data = filter(dat.2, species1 %in% c("walleye", "northern_pike")), aes(y = length.mean, x = mean.3yr.temp), method = "loess", se = FALSE, size = 2, color = "black")+
  facet_wrap(~species1) +
  theme_classic()

wae_nop2 <- ggplot() +
  geom_point(data = filter(dat.2, species1 %in% c("walleye", "northern_pike")), aes(y = length.mean, color = mean.3yr.temp, x = year)) +
  viridis::scale_color_viridis() +
  ylab("Length at 3 yrs") + xlab("Year (age = 0)") +
  geom_smooth(data = filter(dat.2, species1 %in% c("walleye", "northern_pike")), aes(y = length.mean, x = year), method = "loess", se = FALSE, size = 2, color = "black")+
  facet_wrap(~species1) +
  theme_classic()

graphics.off()
quartz(height = 9, width = 9)
fig_wae_nop <- ggpubr::ggarrange(wae_nop, wae_nop2, nrow = 2)
ggsave("Figures/WAE_NOP.pdf", fig_wae_nop)


# ggplot() +
#   geom_point(data = dat.2, aes(y = Length.mm, x = GGD.mean), color = "grey") +
#   geom_smooth(data = dat.2, aes(y = Length.mm, x = GGD.mean), method = "loess", se = FALSE)+
#   facet_wrap(~species1) +
#   theme_classic()
# 
# ggplot() +
#   geom_point(data = dat.2, aes(y = Length.mm, x = mean.3yr.temp.ggd), color = "grey") +
#   geom_smooth(data = dat.2, aes(y = Length.mm, x = mean.3yr.temp.ggd), method = "loess", se = FALSE)+
#   facet_wrap(~species1) +
#   theme_classic()

#time
time.lngth <- ggplot() +
  geom_point(data = dat.2, aes(y =length.mean, x = year, color = mean.3yr.temp)) +
  viridis::scale_color_viridis() +
  ylab("Length at 3 yrs") + xlab("Year (age = 0)") +
  geom_smooth(data = dat.2, aes(y = length.mean, x = year), method = "loess", se = FALSE, color = "black", size = 2)+
  # geom_smooth(data = dat.2, aes(y = Length.mm, x = year, color = state.y), method = "loess", se = FALSE)+
  facet_wrap(~species1, scales = "free") +
  theme_classic()
ggsave("Figures/Length.year.3yr.pdf", time.lngth)

