library(rtweet)
library(here)
library(dplyr)
library(rehydratoR)

write.table(tornadoes$status_id,
            here("data","raw","public","tornadoesids.txt"),
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)

tornadoesids =
  data.frame(read.table(here("data","raw","public","tornadoesids.txt"),
                        numerals = 'no.loss'))

tornadoes_raw = rehydratoR(twitter_token$app$key, twitter_token$app$secret,
                        twitter_token$credentials$oauth_token,
                        twitter_token$credentials$oauth_secret, tornadoesids,
                        base_path = NULL, group_start = 1)

count(tornadoes, place_type)

tornado = lat_lng(tornadoes, coords=c("coords_coords"))

tornado = subset(tornado,
                 place_type == 'city'| place_type == 'neighborhood'|
                   place_type == 'poi' | !is.na(lat))

tornado = lat_lng(tornado,coords=c("bbox_coords"))

count(tornado, place_type)

write.table(tornado$status_id,
            here("data","derived","public","tornadoids.txt"),
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)


