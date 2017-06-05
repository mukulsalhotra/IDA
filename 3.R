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
  left_join(attractions[, c("id", "name", "pos")], by="pos") %>%
  filter(!is.na(id))
partSat <- saturday %>%
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

fri_table <- partFri %>%
  count(u_id, id) %>%
  spread(id, n)%>%
  replace(is.na(.), 0)
m <- fri_table[, -1]
rownames(m) <- fri_table$u_id
fri_table = m
fri_seq <- as.data.frame( apply(fri_table[, 1:length(fri_table)], 1, paste, collapse=""), fri_table$u_id)
colnames(fri_seq) <- c("sequence")
fri_seq$id <- rownames(fri_table)


sat_table <- partSat %>%
  count(u_id, id) %>%
  spread(id, n)%>%
  replace(is.na(.), 0)
m <- sat_table[, -1]
rownames(m) <- sat_table$u_id
sat_table = m
sat_seq <- as.data.frame( apply(sat_table[, 1:length(sat_table)], 1, paste, collapse=""), sat_table$u_id)
colnames(sat_seq) <- c("sequence")
sat_seq$id <- rownames(sat_table)

sun_table <- partSun %>%
  count(u_id, id) %>%
  spread(id, n)%>%
  replace(is.na(.), 0)
m <- sun_table[, -1]
rownames(m) <- sun_table$u_id
sun_table = m
sun_seq <- as.data.frame( apply(sun_table[, 1:length(sun_table)], 1, paste, collapse=""), sun_table$u_id)
colnames(sun_seq) <- c("sequence")
sun_seq$id <- rownames(sun_table)

fri_group <- fri_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(day = "friday") 
sat_group <- sat_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(day = "saturday") 
sun_group <- sun_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(day = "sunday") 
groups <- bind_rows(fri_group, sat_group, sun_group)

save(groups, file="all_groups.RData")

largeFriday <- fri_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count >28) %>%
  arrange(desc(count)) 
largeFriday$index  = 1:nrow(largeFriday)
largeFriday$g_id = paste("friday", largeFriday$index, sep = "_")
largeFriday$day = "friday"
#combine with seq data
fri_l <- fri_seq %>%
  left_join(largeFriday, by = "sequence") %>% 
  filter(!is.na(g_id)) %>%
  arrange(g_id)

largeSaturday <- sat_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count >28) %>%
  arrange(desc(count)) 
largeSaturday$index  = 1:nrow(largeSaturday)
largeSaturday$g_id = paste("saturday", largeSaturday$index, sep = "_")
largeSaturday$day = "saturday"

sat_l <- sat_seq %>%
  left_join(largeSaturday, by = "sequence") %>% 
  filter(!is.na(g_id)) %>%
  arrange(g_id)

largeSunday <- sun_seq %>%
  group_by(sequence) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count >28) %>%
  arrange(desc(count)) 
largeSunday$index  = 1:nrow(largeSunday)
largeSunday$g_id = paste("sunday", largeSunday$index, sep = "_")
largeSunday$day = "sunday"

sun_l <- sun_seq %>%
  left_join(largeSunday, by = "sequence") %>% 
  filter(!is.na(g_id)) %>%
  arrange(g_id)

#combine
all_l <- bind_rows(fri_l, sat_l, sun_l)
save(all_l, file = "all_large.RData")

load("all_large.RData")

table <- all_l%>%
  select(id, index, g_id, day)
fri_table <- table %>%
  filter(day == "friday")
fri_mov <- friday %>%
  filter(id %in% fri_table$id) %>%
  transform(id = as.character(id)) %>%
  left_join(fri_table, by = "id") %>%
  mutate(pos = paste(from_x, from_y, sep="_")) %>%
  left_join(attractions %>% select(pos, name, type, area), by ="pos") %>%
  group_by(id) %>%
  mutate(count = sum(!is.na(name))) %>%
  ungroup()

fri_group_t <- fri_mov %>%
  group_by(g_id) %>%
  summarise(min_t = min(from_t),
            max_t = max(to_t)) %>%
  ungroup() %>%
  arrange(min_t)

fri_group_t$new_index <- 1:nrow(fri_group_t)
fri_group_t <- fri_group_t %>%
  arrange(max_t)
fri_group_t$leave_index <- 1:nrow(fri_group_t)

fri_mov <- fri_mov %>%
  left_join(fri_group_t, by="g_id") 

fri_mov <-fri_mov %>%
  arrange(desc(new_index))

fri_ids <- fri_mov %>%
  select(id) %>%
  distinct() %>%
  t() %>%
  as.vector() %>%
  as.character()
fri_mov <- fri_mov %>%
  mutate(yindex = match(id, fri_ids))

annotation <- fri_mov %>%
  group_by(g_id) %>%
  summarise(count = length(unique(id)),
            last_x = max(to_t),
            top_y = max(yindex)) %>%
  mutate(label = paste0(g_id, "(",count,")"))

colours = c("Beer Gardends" ="red", 
            "Entrance"= "blue", "Food"="green", "Information and Assistance"="pink", "Kiddie Rides"="yellow", 
            "Restrooms"="grey", "Rides for Everyone"="brown", "Shoping"="black", "Shows and Entertainment"="darkgreen", 
            "Thrill Rides"="darkblue")


sat_table <- table %>%
  filter(day == "saturday")
sat_mov <- saturday %>%
  filter(id %in% sat_table$id) %>%
  transform(id = as.character(id)) %>%
  left_join(sat_table, by = "id") %>%
  mutate(pos = paste(from_x, from_y, sep="_")) %>%
  left_join(attractions %>% select(pos, name, type, area), by ="pos") %>%
  group_by(id) %>%
  mutate(count = sum(!is.na(name))) %>%
  ungroup()

sat_group_t <- sat_mov %>%
  group_by(g_id) %>%
  summarise(min_t = min(from_t),
            max_t = max(to_t)) %>%
  ungroup() %>%
  arrange(min_t)

sat_group_t$new_index <- 1:nrow(sat_group_t)
sat_group_t <- sat_group_t %>%
  arrange(max_t)
sat_group_t$leave_index <- 1:nrow(sat_group_t)

sat_mov <- sat_mov %>%
  left_join(sat_group_t, by="g_id") 

sat_mov <-sat_mov %>%
  arrange(desc(new_index))

sat_ids <- sat_mov %>%
  select(id) %>%
  distinct() %>%
  t() %>%
  as.vector() %>%
  as.character()


sat_mov <- sat_mov %>%
  mutate(yindex = match(id, sat_ids))
annotation <- sat_mov %>%
  group_by(g_id) %>%
  summarise(count = length(unique(id)),
            last_x = max(to_t),
            top_y = max(yindex)) %>%
  mutate(label = paste0(g_id, "(",count,")"))

colours = c("Beer Gardends" ="red", 
            "Entrance"= "blue", "Food"="green", "Information and Assistance"="pink", "Kiddie Rides"="yellow", 
            "Restrooms"="grey", "Rides for Everyone"="brown", "Shoping"="black", "Shows and Entertainment"="darkgreen", 
            "Thrill Rides"="darkblue")

sun_table <- table %>%
  filter(day == "sunday")
sun_mov <- sunday %>%
  filter(id %in% sun_table$id) %>%
  transform(id = as.character(id)) %>%
  left_join(sun_table, by = "id") %>%
  mutate(pos = paste(from_x, from_y, sep="_")) %>%
  left_join(attractions %>% select(pos, name, type, area), by ="pos") %>%
  group_by(id) %>%
  mutate(count = sum(!is.na(name))) %>%
  ungroup()

sun_group_t <- sun_mov %>%
  group_by(g_id) %>%
  summarise(min_t = min(from_t),
            max_t = max(to_t)) %>%
  ungroup() %>%
  arrange(min_t)

sun_group_t$new_index <- 1:nrow(sun_group_t)
sun_group_t <- sun_group_t %>%
  arrange(max_t)
sun_group_t$leave_index <- 1:nrow(sun_group_t)

sun_mov <- sun_mov %>%
  left_join(sun_group_t, by="g_id") 

sun_mov <-sun_mov %>%
  arrange(desc(new_index))
#   arrange(leave_index)
sun_ids <- sun_mov %>%
  select(id) %>%
  distinct() %>%
  t() %>%
  as.vector() %>%
  as.character()
sun_mov <- sun_mov %>%
  mutate(yindex = match(id, sun_ids))

annotation <- sun_mov %>%
  group_by(g_id) %>%
  summarise(count = length(unique(id)),
            last_x = max(to_t),
            top_y = max(yindex)) %>%
  mutate(label = paste0(g_id, "(",count,")"))

colours = c("Beer Gardends" ="red", 
            "Entrance"= "blue", "Food"="green", "Information and Assistance"="pink", "Kiddie Rides"="yellow", 
            "Restrooms"="grey", "Rides for Everyone"="brown", "Shoping"="black", "Shows and Entertainment"="darkgreen", 
            "Thrill Rides"="darkblue")

ggplot(fri_mov) + geom_rect(aes(xmin = from_t, xmax=to_t, ymin = yindex, ymax=yindex+1, fill = type.y )) + 
  theme_bw()+
  theme(legend.position="bottom", axis.title.x=element_blank())  + guides(fill=guide_legend(nrow=2, byrow=TRUE))+ 
  ggtitle("Large Groups on Friday")+
  scale_fill_manual(values = colours, name ="", na.value="grey90")+
  coord_cartesian(xlim = c(ymd_hm("2014-06-06 08:00"), ymd_hm("2014-06-06 23:50"))) +
  annotate("text", x= annotation$last_x, y=annotation$top_y, label= annotation$label, vjust=1, hjust=-0.1, cex= 4 )

ggplot(sat_mov) + geom_point(aes(xmin = from_t, xmax=to_t, ymin = yindex, ymax=yindex+1, fill = type.y )) + theme_bw()+
  theme(legend.position="bottom", axis.title.x=element_blank())  + guides(fill=guide_legend(nrow=2, byrow=TRUE))+ 
  ggtitle("Large Groups on Saturday")+
  scale_fill_manual(values = colours, name ="", na.value="grey90")+
  coord_cartesian(xlim = c(ymd_hm("2014-06-07 08:00"), ymd_hm("2014-06-07 23:59"))) +
  annotate("text", x= annotation$last_x, y=annotation$top_y, label= annotation$label, vjust=1, hjust=-0.1, cex= 4 )




ggplot(sun_mov) + geom_rect(aes(xmin = from_t, xmax=to_t, ymin = yindex, ymax=yindex+1, fill = type.y )) + theme_bw()+
  theme(legend.position="bottom", axis.title.x=element_blank())  + guides(fill=guide_legend(nrow=2, byrow=TRUE))+ 
  ggtitle("Large Groups on Sunday")+
  scale_fill_manual(values = colours, name ="", na.value="grey90")+
  coord_cartesian(xlim = c(ymd_hm("2014-06-08 08:00"), ymd_hm("2014-06-08 23:59"))) +
  annotate("text", x= annotation$last_x, y=annotation$top_y, label= annotation$label, vjust=1, hjust=-0.1, cex= 4 )
