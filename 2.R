library(dplyr)
library(ggplot2)
library(lubridate)
library(grid)
library(scales)
library(RColorBrewer)
library(tidyr)
library(dendsort)
library(vegan)


load("movementsFri.RData")
load("movementsSat.RData")
load("movementsSun.RData")
attractions <- read.csv("attractions.csv", quote="\"", header = TRUE)
attractions <- attractions %>%
  mutate(pos = paste(X, Y, sep="_"))


partFri <- friday %>%
  filter(speed < 0.01) %>%
  rename(u_id = id) %>%
  mutate(pos = paste(from_x, from_y, sep="_")) %>%
  left_join(attractions[, c("id", "name", "pos", "type", "area")], by="pos") %>%
  filter(!is.na(id)) %>%
  mutate(day = "friday") %>%
  mutate(time = from_t)
partSat <- saturday %>%
  filter(speed < 0.01) %>%
  rename(u_id = id) %>%
  mutate(pos = paste(from_x, from_y, sep="_")) %>%
  left_join(attractions[, c("id", "name", "pos", "type", "area")], by="pos") %>%
  filter(!is.na(id)) %>%
  mutate(day = "saturday") %>%
  mutate(time = from_t - days(1))
partSun <- sunday %>%
  filter(speed < 0.01) %>%
  rename(u_id = id) %>%
  mutate(pos = paste(from_x, from_y, sep="_")) %>%
  left_join(attractions[, c("id", "name", "pos", "type", "area")], by="pos") %>%
  filter(!is.na(id)) %>%
  mutate(day = "sunday")%>%
  mutate(time = from_t - days(2))
all_set <- bind_rows(partFri, partSat, partSun)
ggplot(all_set) +geom_path(aes(x= time, color=day), stat="bin", binwidth=60*60) +facet_wrap(~name, scales="free_y", ncol = 5) 


all_set <- all_set %>%
  mutate(minutes = duration/60)
ggplot(all_set) +geom_path(aes(x= minutes, color=day), stat="bin", binwidth=0.5) +facet_wrap(~name, scales="free", ncol =5) 
