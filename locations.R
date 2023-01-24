library(sf)
library(tidyverse)
library(readxl)

locs <- read.table("localities.csv", sep=";", header=F)

sites <- read.table("GIS/Yellowhammer/coordinates.csv", sep=",", header=T)


apply(sites, 1, function(site) site %>% strsplit(";") %>% "[["(1) %>% "["(2) %>% substr(1,4)) %>% table %>% length

names(df)
table(df$code)


df$loc_code <- df$code %>% substr(1,4)


df.mp <- read_xlsx("strnad_data_vsechna_marek.xlsx", sheet = 1)
names(df.mp)
summary(df.mp)
table(df.mp$`kód ptáka`)
table(df.mp$`kód ptáka`) %>% length

df.mp.sf <- df.mp %>%
  rename(c("code"="kód ptáka", "coords"="lokalita(GPS)")) %>% 
  distinct(code, .keep_all = T) %>% 
  select(code, coords) %>% 
  mutate(coords = str_replace_all(coords, '"', "''"),
         latstr = str_split_i(coords, ", ", 1),
         lonstr = str_split_i(coords, ", ", 2),
         londeg = str_split_i(lonstr, "°", 1) %>% as.numeric,
         lonmin_pre = str_split_i(lonstr, "°", 2),
         lonmin = str_split_i(lonmin_pre, "'", 1) %>% as.numeric,
         lonsec = str_split_i(lonstr, "'", 2) %>% as.numeric,
         latdeg = str_split_i(latstr, "°", 1) %>% as.numeric,
         latdeg = 50,
         latmin_pre = str_split_i(latstr, "°", 2),
         latmin = str_split_i(latmin_pre, "'", 1) %>% as.numeric,
         latsec = str_split_i(latstr, "'", 2) %>% as.numeric,
         lon = londeg + lonmin/60 + lonsec/3600,
         lat = latdeg + latmin/60 + latsec/3600,
         code_base = substr(code, 1,4)) %>%
  st_as_sf(coords = c("lon","lat"), crs=4326)

plot(df.mp.sf["code_base"])

  
df %>% distinct(code, .keep_all = T) %>% pull(loc_code) %>% table
table(df.mp.sf$code_base)



sites <- read.table("sites_MP.csv", sep=",", header=T) %>%
  mutate(coords = str_replace_all(coords, '"', "''"),
         latstr = str_split_i(coords, ", ", 1),
         lonstr = str_split_i(coords, ", ", 2),
         londeg = 14,
         lonmin_pre = str_split_i(lonstr, "°", 2),
         lonmin = str_split_i(lonmin_pre, "'", 1) %>% as.numeric,
         lonsec = str_split_i(lonstr, "'", 2) %>% as.numeric,
         latdeg = 50,
         latmin_pre = str_split_i(latstr, "°", 2),
         latmin = str_split_i(latmin_pre, "'", 1) %>% as.numeric,
         latsec = str_split_i(latstr, "'", 2) %>% as.numeric,
         lon = londeg + lonmin/60 + lonsec/3600,
         lat = latdeg + latmin/60 + latsec/3600) %>%
  st_as_sf(coords = c("lon","lat"), crs=4326)

plot(sites["location"])
sites %>% st_transform(5514) %>% st_write("GIS/sites_MP.shp")
