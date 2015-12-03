## Playing around with streamR and Twitter API ## 
library(streamR)
library(hadleyverse)
library(maps)

load("my_oauth.Rdata")
setwd("")

# grab data for 90 minutes (12/2 - 10:40am)
filterStream("tweetsUS90.json", locations = c(-125, 25, -66, 50), timeout = 5400, 
              oauth = my_oauth)
tweets_df <- parseTweets("tweetsUS90.json", verbose = FALSE)
points <- data.frame(x = as.numeric(tweets_df$place_lon), y = as.numeric(tweets_df$place_lat))
points <- points[points$y > 25, ]

## plotting out the usage on a map
xlim <- c(-124.738281, -66.601563)
ylim <- c(24.039321, 50.856229)

map("world", col="#E8E8E8", fill=TRUE, bg="white", lwd=0.4, xlim=xlim, ylim=ylim, interior=TRUE)
points(points, pch=16, cex=.10, col="red")
map("state", fill=FALSE, bg="white", add = TRUE)

library(hadleyverse)
# ------------------ #  
# What are people using to tweet?
# will need these eventually
state1 <- data.frame(state.name, state.abb, stringsAsFactors = FALSE)
state2 <- data.frame(state.abb, state.region, stringsAsFactors = FALSE)

# clean source of tweet
type <- data.frame(tweets_df$source, stringsAsFactors = FALSE)
type <- type %>% mutate(tweets_df.source = str_replace_all(tweets_df.source, '.*">', ""),
                        tweets_df.source = str_replace_all(tweets_df.source, '</a>', ""),
                        tweets_df.source = str_replace_all(tweets_df.source, 'SafeTweet by ', ""))

# find regions
local <- tweets_df$full_name
local <- str_split(local, ", ")
local <- do.call(rbind, local)
local <- local[,1:2]

local <- data.frame(local, stringsAsFactors = FALSE)
# merge in state.abb, convert USA to state, drop any that aren't states
regions <-  left_join(local, state1, by = c("X1" = "state.name")) %>%
  mutate(X2 = ifelse(X2 == "USA", state.abb, X2)) %>% 
  select(-state.abb) %>% data.frame(type) %>% 
  filter(!str_length(X2) > 2) %>% 
  left_join(state2, by = c("X2" = "state.abb"))
colnames(regions) <- c("area", "st_abb", "source", "region")

# regions are NA
summary(regions[is.na(regions$region),])  # DC 
# code DC as Northeast
regions$region[is.na(regions$region)] <- "Northeast"

# summarize and put into dotplot
df_regions <- regions %>%
  group_by(region, source) %>% 
  summarise(total = n()) %>% data.frame() %>%
  arrange(desc(total))

df_regions %>% 
  group_by(region) %>% 
  summarise(tweets = sum(total)) # south more than doubles the rest of country

df_states <- regions %>% 
  group_by(st_abb) %>% 
  summarise(tweets = n()) %>%
  data.frame() %>% 
  arrange(desc(tweets))

top5 <- df_regions %>%
  group_by(region) %>%
  top_n(n = 5) %>%
  arrange(region)

ggplot(top5, aes(x = total, y = region, colour = source)) +
  geom_point(size=5) + 
  labs(title = "How do people Tweet", x = "Total", y = "Region") + theme_bw()

# ---------------- # 
summary(tweets_df$followers_count)
quantile(tweets_df$followers_count, p = seq(0,.95, .05))

#density on y-axis with density plot
g3<-ggplot(tweets_df, aes(followers_count)) + 
  geom_histogram(aes(y = ..density..), fill=NA, color="black", binwidth=50) + 
  xlim(0, 10000) + geom_density(color="blue") +
  theme_bw()   #nicer looking 
g3    

popular <- tweets_df %>%
  distinct(screen_name) %>%
  filter(followers_count > quantile(followers_count, p=.9)) %>% 
  mutate(twit_ratio = followers_count / statuses_count) %>%
  arrange(desc(followers_count)) %>% 
  data.frame()

efficiency <- tweets_df %>%
  distinct(screen_name) %>%
  mutate(twit_ratio = followers_count / statuses_count) %>%
  arrange(desc(twit_ratio)) %>% 
  data.frame()

efficiency$date <- strptime(efficiency$user_created_at, format =  "%a %b %d %H:%M:%S %z %Y")
efficiency$acct_age <- (as.numeric(now() - efficiency$date))/24/7 # age of account in weeks
efficiency$percentile <- ecdf(efficiency$followers_count)(efficiency$followers_count) # add in efficiency percentiles

g6 <- ggplot(data = efficiency, aes(x = statuses_count, y = followers_count)) + 
  geom_point(size = 1.2, alph = .6, col="#4e4e4e") + ylim(0,4000) + 
  xlim(0,40000) + stat_smooth(size = 2) + theme_bw() #90th percentile for status updates
g6


# ------------ # 
# find those who were tweeted at in reply to previous tweeet 
# and who allow geolocation
to <- tweets_df$in_reply_to_screen_name
sent_to <- to %in% tweets_df$screen_name
receive <- to[sent_to] # 5655 tweets to someone in the data

s_df <- tweets_df[tweets_df$in_reply_to_screen_name %in% receive,] # geolocation of those who sent tweets
r_df <- tweets_df[tweets_df$screen_name %in% receive,] # geolocation of those who received tweets

geo <- data.frame(s_df$place_lat, s_df$place_lon, s_df$screen_name, 
                  s_df$in_reply_to_screen_name, s_df$id_str, stringsAsFactors = FALSE)

geo_receive <- data.frame(r_df$place_lat, r_df$place_lon, 
                          r_df$screen_name, stringsAsFactors = FALSE)

inter_df <- left_join(geo, geo_receive, by = c("s_df.in_reply_to_screen_name" = "r_df.screen_name"))
`%notin%` <- function(x,y) !(x %in% y)
inter_df <- inter_df[inter_df$s_df.screen_name %notin% inter_df$s_df.in_reply_to_screen_name,]

inter_df <- inter_df %>% distinct(s_df.id_str)

colnames(inter_df) <- c("from_lat", "from_lon", "at", "from", 'id', "to_lat", "to_lon")

## plot the data 
library(maps)
library(geosphere)

# Calculate distance in kilometers between two points (this is rough, but works for our purposes)
earth.dist <- function (long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

pal <- colorRampPalette(c("#d9d9d9", "white"))
colors <- pal(100)
xlim <- c(-124.738281, -66.601563)
ylim <- c(24.039321, 50.856229)
map("world", col="#232323", fill=TRUE, bg="black", lwd=0.01, xlim=xlim, ylim=ylim, interior=FALSE)

for (i in 1:3) { 
  for (j in 1:dim(inter_df)[1]) {
    inter <- gcIntermediate(c(inter_df[j,]$to_lon, inter_df[j,]$to_lat), c(inter_df[j,]$from_lon, inter_df[j,]$from_lat), 
                            n=100, addStartEnd = TRUE)
    colindex <- round(earth.dist(inter_df[j,2], inter_df[j,1], inter_df[j,7], inter_df[j,6]) / 4435 * length(colors) +1 )
    lines(inter, col=colors[colindex], lwd=0.05)
  }
}

 





