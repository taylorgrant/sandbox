# surfbot 2 - reply to DMs directed at @ElPortoSurf
library(twitteR)
library(dplyr)
library(readr)
library(stringr)
library(rvest)

# load api and credentials
api_key <- Sys.getenv("twitter_api_key")
api_secret <- Sys.getenv("twitter_api_secret")
access_token <- Sys.getenv("twitter_access_token")
access_token_secret <- Sys.getenv("twitter_access_token_secret")
options(httr_oauth_cache = TRUE)
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# genesis for data ID
#dms <- dmGet()
#x <- dms[[1]]
#id1 <- data.frame(id1 = as.numeric(x$getId()))
#write_tsv(id1, "id1")

# read in current id of last dm responded to
curr_id <- as.numeric(read_table("/Users/taylorgrant38/Documents/R Resources/Twitter/SurfBot/id1"))
words <- c('current', 'conditions')

# gather dms and pull out unique tweet ids
dms <- dmGet(sinceID = curr_id)
test_id <- sapply(dms, function(x) as.vector(as.numeric(x$getId())))

# test to see if there are any new dms
new <- length(test_id[test_id > curr_id])

if (new == 0) {
  NULL
}
if (new > 0) {
  # pull out screen_name of sender and text
  test_name <- sapply(dms, function(x) as.vector(x$getSenderSN()))[c(seq(new))]
  test_text <- sapply(dms, function(x) as.vector(x$getText()))[c(seq(new))]
  
  # test to see if DM matches our search phrase, keep those names that do
  final_name <- as.vector(test_name[which(str_detect(str_to_lower(test_text), 
                                                     str_to_lower(paste(words, collapse = ' '))) == TRUE)])
  
  # scraping current conditions
  if (length(final_name) > 0) {
    current_url <- "http://magicseaweed.com/El-Porto-Beach-Surf-Report/2677/"
    
    current <- read_html(current_url) %>%
      html_nodes(".msw-fc-current-v0 .row") %>%
      html_text()  %>% str_split("        ")
    current <- unlist(current)
    
    # break into lists for easy use (drop Wind Swell and Secondary Swell)
    cond <- current
    cond <- cond[!str_detect(cond, "Wind Swell")]
    cond <- cond[!str_detect(cond, "Secondary Swell")]
    cond[cond==""] <- NA
    cond <- cond[complete.cases(cond)]
    cond <- str_trim(cond, "both") %>%
      str_split(" ")
    
    waves <- cond[[1]]
    wind <- cond[[2]]
    swell <- cond[[3]]
    weather <- cond[[4]]
    
    current_surf <- str_c(strftime(Sys.time(),"%I:%M %p"),":"," Waves ", waves, " with a ", swell[2], " of ", 
                          swell[6], " at ", swell[8], ". ", wind[3], " wind at ", wind[1], ". ", weather[1], " and ",
                          weather[3], " Water temp: ", weather[5], weather[6], ". #elporto #surf")
    current_surf
    
    # make sure not sending multiple DMs to same account
    final_name <- unique(final_name)
   
     # send DM response to each account
    for (i in 1:length(final_name)) {
      dmSend(current_surf, final_name[i])
    }
  }
  else if (length(final_name) == 0) {
    NULL
  }
}
tmp_id <- data.frame(test_id[1])
write_tsv(tmp_id, "/Users/taylorgrant38/Documents/R Resources/Twitter/SurfBot/id1")
