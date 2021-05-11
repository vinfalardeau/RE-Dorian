library(rtweet)
library(igraph)
library(dplyr)
library(tidytext)
library(tm)
library(tidyr)
library(ggraph)
library(tidycensus)
library(ggplot2)
library(RPostgres)
library(RColorBrewer)
library(DBI)
library(rccmisc)
library(sf)
library(spdep)
library(here)

sf::sf_use_s2(TRUE)

tornadoesTweetsByHour <- ts_data(tornadoes, by="hours")
ts_plot(tornadoes, by="hours")
byhour = ts_plot(tornado, by="hours")

ggsave(here("results","figures","tornadoByHour.svg"),plot = byhour, width = 6, height = 5, units = "in")

tornadoesText = tornadoes %>% select(text) %>% plain_tweets()
tornadoesWords = tornadoesText %>% unnest_tokens(word, text)
count(tornadoesWords)
stop_words = stop_words %>%
  add_row(word="t.co",lexicon = "SMART") %>%
  add_row(word="tornado",lexicon = "Search")
tornadoesWords =  tornadoesWords %>% anti_join(stop_words)

twc = tornadoesWords %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Unique words",
       y = "Count",
       title = "Count of unique words found in tweets")

ggsave(here("results","figures","tornadowordcounts.svg"),plot = twc, width = 5, height = 5, units = "in")

tornadoesWordPairs = tornadoesText %>%
  mutate(text = removeWords(tolower(text), stop_words$word)) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2) %>%
  separate(paired_words, c("word1", "word2"),sep=" ") %>%
  count(word1, word2, sort=TRUE)

twp = tornadoesWordPairs %>%
  filter(n >= 150 & !is.na(word1) & !is.na(word2)) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network of Tweets about Tornadoes",
       x = "", y = "") +
  theme_void()

ggsave(here("results","figures","tornadowordpairs.svg"), plot = twp, width = 11,height=8.5, units = "in")

counties2 <- get_estimates("county",
                          product="population",
                          output="wide",
                          geometry=TRUE, 
                          keep_geo_vars=TRUE)

counties2 = counties = filter(counties2,
                              STATEFP %in% c('35','08','48','40','20','31',
                                             '19','29','05','22','17','18',
                                             '39','21','54','51','47','28',
                                             '37','45','01','13','12') )

saveRDS(counties2, here("data","derived","public","counties2.RDS"))



########################

tornado_sf = tornado %>%
  st_as_sf(coords = c("lng","lat"), crs=4326) %>%  # make point geometries
  st_transform(4269) %>%  # transform to NAD 1983
  st_join(select(counties2,GEOID))  # spatially join counties to each tweet

tornado_by_county = tornado_sf %>%
  st_drop_geometry() %>%   # drop geometry / make simple table
  group_by(GEOID) %>%      # group by county using GEOID
  summarise(tornado = n())  # count # of tweets

counties2 = counties2 %>%
  left_join(tornado_by_county, by="GEOID") %>% # join count of tweets to counties
  mutate(tornado = replace_na(tornado,0))       # replace nulls with 0's

rm(tornado_by_county)

#######################

baseline_by_county = baseline %>% 
  st_as_sf(coords = c("lng","lat"), crs=4326) %>%
  st_transform(4269) %>%
  st_join(select(counties,GEOID)) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>% 
  summarise(baseline = n())

counties2 = counties2 %>%
  left_join(baseline_by_county, by="GEOID") %>%
  mutate(baseline = replace_na(baseline,0))

counties2 = counties2 %>%
  mutate(torrate = tornado / POP * 10000) %>%  # dorrate is tweets per 10,000
  mutate(ndti = (tornado - baseline) / (tornado + baseline)) %>%  # normalized tweet diff
  mutate(ndti = replace_na(ndti,0))   # replace NULLs with 0's

rm(baseline_by_county)

saveRDS(counties2,here("data","derived","public","counties_tornado_counts.RDS"))

########################

thresdist2 = counties2 %>% 
  st_centroid() %>%     # convert polygons to centroid points
  st_coordinates() %>%  # convert to simple x,y coordinates to play with stdep
  dnearneigh(0, 110, longlat = TRUE) %>%  # use geodesic distance of 110km
  # distance should be long enough for every feature to have >= one neighbor
  include.self()       # include a county in its own neighborhood (for G*)

dwm2 = nb2listw(thresdist2, zero.policy = T)

counties2$locG = as.vector(localG(counties2$torrate, listw = dwm2, 
                                 zero.policy = TRUE))

summary(counties2$locG)

siglevel = c(1.15,1.95)
counties2 = counties2 %>% 
  mutate(sig = cut(locG, c(min(counties2$locG),
                           siglevel[2]*-1,
                           siglevel[1]*-1,
                           siglevel[1],
                           siglevel[2],
                           max(counties2$locG))))
rm(siglevel)

states <- get_estimates("state",
                          product="population",
                          output="wide",
                          geometry=TRUE, keep_geo_vars=TRUE)

states = filter(states,
                  STATEFP %in% c('35','08','48','40','20','31',
                                 '19','29','05','22','17','18',
                                 '39','21','54','51','47','28',
                                 '37','45','01','13','12') )

ggplot() +
  geom_sf(data=counties2, aes(fill=sig), color="white", lwd=0.07)+
  scale_fill_manual(
    values = c("#0000FF80", "#8080FF80", "#FFFFFF80", "#FF808080", "#FF000080"),
    labels = c("very low","low", "insignificant","high"),
    aesthetics = "fill"
  ) +
  geom_sf(data=states, fill = NA, color = "black", lwd=0.12)+
  guides(fill=guide_legend(title="Hot Spots"))+
  labs(title = "Clusters of Tornado-Related Twitter Activity")+
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggplot()+
  geom_sf(data=counties2, aes(fill=ndti), color="white", lwd=0.07)+
  scale_fill_gradient2(low = "#ff5555",
                      high = "#5555ff",
                      mid = "#cccccc",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill")+
  geom_sf(data=states, fill = NA, color = "black", lwd=0.12)
