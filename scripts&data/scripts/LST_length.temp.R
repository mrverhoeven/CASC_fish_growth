# library ####
`%ni%` <- Negate(`%in%`)
library(tidyverse)
library(readxl)

## spatial packages ####
library(sf)
library(USAboundaries)



# data ####
## fish ####
dat.fish <- readRDS(file = "data/Length.Age.all.species.rds") %>% 
  dplyr::select(Lake.ID, state, lat, long) %>% distinct() %>% 
  dplyr::rename(y_DD5 = lat, x_DD5 = long) %>% 
  filter(y_DD5 %ni% NA, x_DD5 %ni% NA)

## USGS lake data ####
lake.usgs <- read.csv("data/lake_metadata.csv", header = T) %>% 
  dplyr::select(site_id, elevation_m, area_m2, lake_lon_deg, lake_lat_deg, group_id) %>% 
  dplyr::rename(x_DD5 = lake_lon_deg, y_DD5 = lake_lat_deg)



MainStates <- map_data("state")
FocalStates <- map_data("state") %>% filter(., region %in% c("illinois","arkansas","minnesota", "michigan", "north dakota", "iowa", "wisconsin", "missouri", "south dakota", "nebraska"))
study.Area <- ggplot() +
  geom_point(data = dat.fish, aes(x = x_DD5, y = y_DD5)) +
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="grey", fill="NA", size = 0.75) +
  geom_polygon(data=FocalStates, aes(x=long, y=lat, group=group),
               color="blue", fill="NA", size = 1.25) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

ggsave("Figures/StudyArea.pdf", study.Area)


# Spatial Join ####
## projections ####
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
utm15 <- st_crs("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")

## points to shapefiles ####
## convert to UTM's 
lakes.shp <- st_transform(st_as_sf(dat.fish, coords = c("x_DD5", "y_DD5"), crs = wgs84), crs = utm15)

l.usgs.shp <- st_transform(st_as_sf(lake.usgs, coords = c("x_DD5", "y_DD5"), crs = wgs84), crs = utm15)

## state import ####
states <- st_transform(st_read("Shapefiles/cb_2018_us_state_500k/cb_2018_us_state_500k.shp") %>% 
                         filter(NAME %in% c("Wisconsin", "Minnesota", "South Dakota", "Michigan")) %>% 
                         dplyr::select(NAME), crs = utm15)

# ## intersection between sta
# usgs.int <- st_intersection(states, l.usgs.shp)

## buffer of lake sites
l.buff <- st_buffer(lakes.shp, dist = 10000)
## intersection between lake buffer and usgs lakes
usgs.int <- st_intersection(l.buff, l.usgs.shp)

## distance matrix between all lakes
dist.all <- st_distance(usgs.int,lakes.shp)

## loop to identify matching lakes 
dat.final <- data.frame(matrix(nrow = 0, ncol = 2))
names(dat.final) <- c("lake.number", "usgs.number")
i <- 51
for(i in 1:nrow(lakes.shp)) {
  row.number <- which(dist.all[,i] == min(dist.all[,i])) %>% first()
  dat.final1 <- data.frame(lake.number = i, usgs.number = row.number)
  dat.final <- rbind(dat.final, dat.final1)
  
}

match.lakes <- data.frame(matrix(nrow = 0, ncol = 10))
names(match.lakes) <- c("Lake.ID", "state","usgs.id", "elevation_m", "area.m", "x_UTM.usgs", "y_UTM.usgs", "x_UTM.state", "y_UTM.state", "group_id")
i <- 30
for(i in 1:length(dat.final[,1])) {
  usgs.dat <- usgs.int[dat.final[i,"usgs.number"], ]
  coord <- st_coordinates(usgs.dat) %>% data.frame()
  names(coord) <- c("x_UTM.usgs", "y_UTM.usgs")
  study.l <- lakes.shp[dat.final[i,"lake.number"], ]
  coord2 <- st_coordinates(study.l) %>% data.frame()
  names(coord2) <- c("x_UTM.state", "y_UTM.state")
  match.lakes1 <- data.frame(Lake.ID = data.frame(study.l)[1,"Lake.ID"],
                             #Lake.name = data.frame(study.l)[1,"Lake.Name"],
                             state = data.frame(study.l)[1,"state"],
                             usgs.id = data.frame(usgs.dat)[1,"site_id"],
                             elevation_m = data.frame(usgs.dat)[1,"elevation_m"],
                             area_m2 = data.frame(usgs.dat)[1,"area_m2"],
                             coord,
                             coord2, 
                             group_id = data.frame(usgs.dat)[1,"group_id"])
  match.lakes <- rbind(match.lakes, match.lakes1)
  
}


saveRDS(match.lakes, "data/StudyAreaLake.rds")

dist.all[,36]

ggplot() + 
  # geom_sf(data = states) +
  geom_sf(data = lakes.shp[51, ], color = "red") +
  geom_sf(data = usgs.int[2766:2767, ], alpha = 0.5)








