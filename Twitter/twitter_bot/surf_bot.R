# -------------------------------------# 
# A twitter bot that periodically tweets 
# out the surf conditions in El Porto, CA. 
# After sunset, the bot tweets out the 6AM
# surf forecast for the next day. 
#     @ElPortoSurf
# -------------------------------------#

#devtools::install_github("geoffjentry/twitteR")
library(twitteR)
library(httr)
library(base64enc)
library(rvest)
library(lubridate)
library(stringr)
library(dplyr)

# using this to store the API locally
# http://www.r-datacollection.com/blog/How-to-conduct-a-tombola-with-R/
# # ---------------------- #
# credentials <- c(
#   "twitter_api_key=blahblahblah_blah",
#   "twitter_api_secret=blahblahblah_blahblahblahblah_blah",
#   "twitter_access_token=blahblahblah_blahblahblahblah_blah",
#   "twitter_access_token_secret=blahblahblah_blah"
# )
# 
# fname <- paste0(normalizePath("~/"),"/.Renviron")
# writeLines(credentials, fname)
# 
# browseURL(fname)
# ---------------------- #

# setup authentication
api_key <- Sys.getenv("twitter_api_key")
api_secret <- Sys.getenv("twitter_api_secret")
access_token <- Sys.getenv("twitter_access_token")
access_token_secret <- Sys.getenv("twitter_access_token_secret")

options(httr_oauth_cache = TRUE)

# httr:::guess_cache()
# httr:::use_cache()
# getwd()

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# --------------------------- # 
# Scrape Data: Surf Report for El Porto (surfline.com) # 
url <- "http://www.surfline.com/surf-report/el-porto-southern-california_4900/"

# --------------------------- #
## grab sunrise and sunset
rise_set <- read_html(url) %>% 
  html_nodes("div:nth-child(16) span") %>% 
  html_text()

sunrise <- str_extract(rise_set, "([1-9][:][0-5][0-9][A][M])")
sunrise <- strptime(sunrise, "%I:%M %p")
sunset <- str_extract(rise_set, "([1-9][:][0-5][0-9][P][M])")
sunset <- strptime(sunset, "%I:%M %p")

cur_time <- now()
day_plus1 <- cur_time + days(1)
weekday <- as.character(wday(day_plus1, label=TRUE, abbr=FALSE))
month <- month(day_plus1)
day <- day(day_plus1)

# --------------------------- #
# Morning - Current conditions
if (cur_time > sunrise & cur_time < sunrise + hours(2)) {
  ## Current Conditions
  cast <- read_html(url) %>%
    html_nodes("#observed-spot-conditions , #observed-wave-description, #observed-wave-range") %>%
    html_text() 
  
  # clean up conditions
  wave <- cast[[1]] %>% str_replace("m", "") %>% str_c("ft")
  height <- cast[[2]] %>% str_replace_all(("\n"), "") %>% str_replace("-", "") %>%
    str_trim("both") 
  conditions <- cast[[3]] %>% str_replace("Conditions", "") %>% str_trim("right")
  
  ## Time of conditions 
  full <- read_html(url) %>%
    html_nodes("strong") %>%
    html_text()
  
  # clean up time 
  date <- gsub("\n","", full[11])
  time <- str_extract(date, "((1)?[0-9][:][0-9][0-9][a-p][m])")
  
  # change display time so that the reporting time is 
  # not always the same 
  time2 <- strptime(time, '%R')
  dis_time <- ifelse((time2 - Sys.time() > -1), strftime(Sys.time(),"%I:%M %p"), paste("As of", time, ""))
  
  ## Water temp
  h20temp <- read_html(url) %>%
    html_nodes(":nth-child(7) div:nth-child(2) span:nth-child(5)") %>% 
    html_text() %>%
    str_replace_all("\n", "") %>% 
    str_trim("both")
  
  morning_surf <- str_c(dis_time,": ", "Surf conditions are ", conditions,". ", 
                        "Waves are ", height,": ", wave,". ", "Water Temp: ",h20temp, ". #elporto #surf", "")
  morning_surf
  tweet(morning_surf)
}

# --------------------------- #
## Early Morning / Afternoon - Current Conditions 
if (cur_time > sunrise + hours(2) & cur_time < sunset) {
  current_url <- "http://magicseaweed.com/El-Porto-Beach-Surf-Report/2677/"
  current_url <- "http://magicseaweed.com/South-Beach-Surf-Report/298/"
  current <- read_html(current_url) %>%
    html_nodes(".msw-fc-current-v0 .row") %>%
    html_text()  %>% str_split("        ")
  
  current <- unlist(current)
  
  # break into lists for easy use
  # first drop "Wind Swell"
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
  tweet(current_surf)
}
# --------------------------- #
# Predicted Conditions - for the scheduled tweet
if (cur_time > sunset & cur_time < sunrise + days(1)) {
  root <- "http://magicseaweed.com/El-Porto-Beach-Surf-Report/2677/#" 
  pred_url <- str_c(root, weekday, day, month,"")
  
  pred <- read_html(pred_url) %>%
    html_nodes("table") %>%
    .[[3]] %>%
    html_table(fill=TRUE, header = TRUE )
  
  ## want the prediction of the day in question for 
  ## the 6am time
  pred_tbl <- pred[22, c(1,2,5,6,7,14,16:18)]
  colnames(pred_tbl) <- c("Time", "Surf", "Swell", "Period", "Direction", "Wind", "Weather", 'Temp', 'Prob')
  onshore <- ifelse(pred_tbl$Direction > 210 & pred_tbl$Direction < 345, "Onshore", NULL)
  
  ## generate tweet text
  pred_surf <- str_c(pred_tbl$Time, " forecast for ", month,"/", day , ": ", "Surf ", pred_tbl$Surf, "; ", onshore, " Swell ", 
                     pred_tbl$Swell, " w/ ", pred_tbl$Period, " period; ", pred_tbl$Weather, " & ", pred_tbl$Temp, 
                     " #elporto #surf" )
  
  pred_surf
  tweet(pred_surf)
}

# # 
# # # create log entry
# line_c <- paste( as.character(Sys.time()), current_surf ,sep="\t" )
# line_p <- paste( as.character(Sys.time()), pred_surf ,sep="\t" )
# write(line_c, line_p, file="tweets.log", ncol=2, append=TRUE)


