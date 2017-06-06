library(dplyr)
library(dendsort)
library(vegan)
library(ggplot2)
library(lubridate)
library(grid)
library(scales)
library(RColorBrewer)
library(tidyr)



load("movementsFri.RData")
load("movementssaturday.RData")
load("movementsSun.RData")

attractions <- read.csv("attractions.csv", quote="\"", header = TRUE)
attractions <- attractions %>%
  mutate(pos = paste(X, Y, sep="_"))



partFri <- friday %>%
  filter(speed < 0.01) %>%
  rename(u_id = id) %>%
  mutate(pos = paste(from_x, from_y, sep="_")) %>%
  left_join(attractions[, c("id", "name", "pos")], by="pos") %>%
  filter(!is.na(id))
partsaturday <- saturday %>%
  filter(speed < 0.01) %>%
  rename(u_id = id) %>%
  mutate(pos = paste(from_x, from_y, sep="_")) %>%
  left_join(attractions[, c("id", "name", "pos")], by="pos") %>%
  filter(!is.na(id))
partSun <- sunday %>%
  filter(speed < 0.01) %>%
  rename(u_id = id) %>%
  mutate(pos = paste(from_x, from_y, sep="_")) %>%
  left_join(attractions[, c("id", "name", "pos")], by="pos") %>%
  filter(!is.na(id))


friday_table <- partFri %>%
  count(u_id, id) %>%
  spread(id, n)%>%
  replace(is.na(.), 0)
m <- friday_table[, -1]
rownames(m) <- friday_table$u_id
friday_table = m
fri_seq <- as.data.frame( apply(friday_table[, 1:length(friday_table)], 1, paste, collapse=""), friday_table$u_id)
colnames(fri_seq) <- c("sequence")
fri_seq$id <- rownames(friday_table)

colnames(friday_table) =attractions$name[match(colnames(friday_table), attractions$id)]


summary <- fri_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))


saturday_table <- partsaturday %>%
  count(u_id, id) %>%
  spread(id, n)%>%
  replace(is.na(.), 0)
m <- saturday_table[, -1]
rownames(m) <- saturday_table$u_id
saturday_table = m
saturday_seq <- as.data.frame( apply(saturday_table[, 1:length(saturday_table)], 1, paste, collapse=""), saturday_table$u_id)
colnames(saturday_seq) <- c("sequence")
saturday_seq$id <- rownames(saturday_table)

saturday_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  head(n = 20)

sunday_table <- partSun %>%
  count(u_id, id) %>%
  spread(id, n)%>%
  replace(is.na(.), 0)
m <- sunday_table[, -1]
rownames(m) <- sunday_table$u_id
sunday_table = m
sun_seq <- as.data.frame( apply(sunday_table[, 1:length(sunday_table)], 1, paste, collapse=""), sunday_table$u_id)
colnames(sun_seq) <- c("sequence")
sun_seq$id <- rownames(sunday_table)

sun_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  head(n = 20)

friday_group <- fri_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(day = "friday") 
saturday_group <- saturday_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(day = "saturday") 
sunday_group <- sun_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(day = "sunday") 
groups <- bind_rows(friday_group, saturday_group, sunday_group)
annotation <- groups %>%
  group_by(day, count) %>%
  summarise(overlaps = n()) %>%
  ungroup() %>%
  mutate(label = ifelse(count >28, as.character(overlaps), "")) %>%
  ungroup() %>%
  select(day, count, overlaps, label) %>%
  distinct()

ggplot(groups, aes(x= count*2)) + geom_histogram(binwidth =0.05) + 
  geom_density(aes(y=2 * ..count..))+
  geom_text(data = annotation,  aes(x = count, y = overlaps, label = ifelse(overlaps < 100, overlaps, "")), hjust =0, vjust=0, cex = 4, color= "red")+
  facet_grid(day ~.) +theme_bw() + xlab("size") +ggtitle("Group sizes")

