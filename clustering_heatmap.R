## Run Cleaning File ####

library(clustMixType)

## Create Two Smaller Datasets ####
facts <- c("black","fund", "charis", "bagain", "evan", "traditional", "mainline", "progress", "nontrad", "secular", "agnostic", "atheist", "butnot", "none", "reborn")


clust <- anes %>% 
  select(black, att, bible, relguide, fund, charis, bagain, evan, traditional, mainline, progress, nontrad, secular, agnostic, atheist, butnot, none, reborn, discrim_muslim, discrim_xtn, therm_fundie, therm_pope, therm_muslim, therm_jews, therm_xtn, moral) %>% 
  mutate_each_(funs(factor(.)), facts) %>% 
  na.omit()

clust <- as.data.frame(clust)

idclust <- anes %>% 
  select(V160001, black, att, bible, relguide, fund, charis, bagain, evan, traditional, mainline, progress, nontrad, secular, agnostic, atheist, butnot, none, reborn, discrim_muslim, discrim_xtn, therm_fundie, therm_pope, therm_muslim, therm_jews, therm_xtn, moral) %>% 
  mutate_each_(funs(factor(.)), facts) %>% 
  na.omit()


## Making the Elbow Graph ####

set.seed(90210)

k.max <- 15

wss <- sapply(1:k.max, 
              function(k){kproto(clust, k)$tot.withinss})

elbow <- as.tibble(wss) %>% mutate(cluster = seq(from = 1, to = 15, by= 1))

ggplot(elbow, aes(x=cluster, y= value)) + 
  geom_point(size =3.5) + 
  geom_line(size = 1.5) + 
  theme_gg("Abel") + 
  scale_x_continuous(breaks = round(seq(min(elbow$cluster), max(elbow$cluster), by = 1),1)) +
  labs(x= "Number of Clusters", y = "Within Groups Sum of Squares", title = "Elbow Method for Choosing Number of Clusters") +
  ggsave("elbow_graph.png")

## Actual Clustering Here ####

k<-kproto(clust, k=5,iter.max=100000)

## Then joining it back to the IDs and RELTRADS ####

cluster <- as_tibble(clust) %>% 
  add_column(clusters = factor(k$cluster)) %>% bind_cols(idclust) 

rel <- anes %>% 
  select(V160001, reltrad)

cluster <- left_join(cluster, rel)

## Making the Crosstab Dataset ####

xtab <- cluster %>% filter(reltrad !=0) %>% 
  mutate(reltrad = as.numeric(reltrad)) %>% 
  mutate(reltrad = recode(reltrad, "1= 'Mainline'; 2 = 'Evangelical'; 3 = 'Black Prot.'; 4 = 'Catholic'; 5 = 'Christian'; 6 = 'Jewish'; 7 = 'Other Faith'; 8 = 'No Faith'")) %>% 
  mutate(clusters = recode(clusters, "1= 'Cluster 1'; 2 = 'Cluster 2'; 3 ='Cluster 3'; 4= 'Cluster 4'; 5 = 'Cluster 5'; 6 ='Cluster 6'; 7 = 'Cluster 7'; 8 = 'Cluster 8'; 9 = 'Cluster 9'")) %>% 
  group_by(reltrad) %>%
  ct(clusters) %>% 
  ungroup(reltrad) %>% 
  add_row(reltrad = "No Faith", clusters = "Cluster 2", n = 0, pct = 0) %>% 
  add_row(reltrad = "Jewish", clusters = "Cluster 2", n = 0, pct = 0) 

### Heatmapping the Crosstab ####

xtab %>% 
  ggplot(., aes(x= clusters, y = reltrad)) +
  geom_tile(aes(fill = pct), color = "black") +
  scale_fill_gradient(low = "azure3", high = "#E94057") +
  theme_gg("Abel") + 
  scale_x_discrete(position = "top")  +
  geom_text(aes(x= clusters, y = reltrad, label = paste0(pct*100, '%')), size = 4, family = "font") +
  labs(x= "", y = "", title = "Distribution of Religious Groups Across Clusters", subtitle = "", caption = "Data: ANES (2016)") +
  ggsave("cluster_heatmap.png", width = 5)

mean <- as.data.frame(k$centers)

write_csv(mean, "mean_table.csv")
