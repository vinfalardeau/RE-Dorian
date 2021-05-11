library(rtweet)
library(here)
library(dplyr)
library(rehydratoR)

twitter_token = create_token(
  app = "spatial_hazards_visualization",                     #enter your app name in quotes
  consumer_key = "",  		      #enter your consumer key in quotes
  consumer_secret = "",         #enter your consumer secret in quotes
  access_token = NULL,
  access_secret = NULL
)

# 6 geo-tagged tweets about the Hualapai Mountains Fire
hualapai <- search_tweets("hualapai OR flag fire OR #flagfire OR arizona fire OR new mexico fire",
              n=10000, include_rts=FALSE,
              token=twitter_token,
              geocode="35,-114,1000mi",
              retryonratelimit=TRUE)

hualapai2 <- search_tweets("hualapai OR wildfire OR arizona fire OR new mexico fire",
                          n=10000, include_rts=FALSE,
                          token=twitter_token,
                          geocode="35,-114,1000mi",
                          retryonratelimit=TRUE)

pats <- search_tweets("patriots draft OR pats draft OR mac jones draft ",
                      n=1000, include_rts=FALSE,
                      token=twitter_token,
                      geocode="37,-83,1000mi",
                      retryonratelimit=TRUE )

unique(pats$place_type)
count(pats, place_type)
pats <- lat_lng(pats,coords=c("coords_coords"))
pats <- subset(pats, place_type == 'city'| place_type == 'neighborhood'| place_type == 'poi' | !is.na(lat))
pats <- lat_lng(pats,coords=c("bbox_coords"))


tornadoes <- search_tweets("tornado",
                           n=200000, include_rts=FALSE,
                           token=twitter_token,
                           geocode="34.75,-92.29,1000mi",
                           retryonratelimit=TRUE)

saveRDS(tornadoes, here("data","derived","private","tornadoes.RDS"))

baseline <- search_tweets("",
                          n=37295, include_rts=FALSE,
                          token=twitter_token,
                          geocode="34.75,-92.29,1000mi",
                          retryonratelimit=TRUE)

saveRDS(baseline,here("data","derived","private","baseline.RDS") )
