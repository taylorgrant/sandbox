# MA.BABIP
# BABIP (batting average on balls in play) for the 2014 season. 
# How do players fare over specific durations during the season 
# and how do these periods compare to career means? 
# Evidence of streakiness, luck, both?  
# 2014 play-by-play data from retrosheet.org, in the data folder. 

lahman.player.search <- function(lastname, firstname) {
  # use to get appropriate IDs for player
  require(Lahman)
  if (is.null(firstname)) {
    Master %>% filter_(~str_detect(nameLast, fixed(lastname, ignore_case = TRUE))) %>%
      mutate(MLByear = ifelse(birthMonth <= 6, birthYear, birthYear + 1))
  } 
  else {
    Master %>% filter_(~str_detect(nameLast, fixed(lastname, ignore_case = TRUE)),
                       (~str_detect(nameFirst, fixed(firstname, ignore_case = TRUE)))) %>%
      mutate(MLByear = ifelse(birthMonth <= 6, birthYear, birthYear + 1))
  } 
}


MA.babip <- function(lastname, firstname, window) { 
  # ---------------------------------------------- #
  # Inputs : lastname, firstname of MLB player
  #           window: the number of games to include in 
  #                  moving average calculation
  #
  # Output: game stats for selected player
  #         MA over selected game window
  #         Career BABIP
  #         Plot of MA BABIP
  # ---------------------------------------------- #
  library(hadleyverse)
  library(Lahman)
  library(rvest)
  
  ## read in the data
  pbp2014 <- read.csv("pbp2014.csv", header=TRUE)
  
  pbp2014$date <- substr(pbp2014$GAME_ID, 4, 12)
  
  ## Get appropriate PIDs for given player
  player.info <- lahman.player.search(lastname, firstname)
  if (dim(player.info)[1] > 1) 
    stop("Too many results, please include full first and last name.")
  PID <- c(player.info$retroID, player.info$bbrefID) #[1] retrosheet; [2] baseball reference
  
  ## scrape career BABIP from Baseball-Reference.com
  letter1 <- substr(PID[2], 1,1)
  suff <- paste(PID[2],"-bat", sep="")
  url <- paste("http://www.baseball-reference.com/players/", letter1,"/", suff,".shtml", sep = "")
  career.babip <- read_html(url) %>%
    html_nodes("#batting_advanced .stat_total:nth-child(1) td:nth-child(7)") %>%
    html_text
  
  ## Gather data to estimate player BABIP
  p1 <- pbp2014 %>%
    filter_(~BAT_ID == PID[1]) %>%
    mutate(H = ifelse(H_FL > 0, 1, 0),
           HR = ifelse(EVENT_CD == 23, 1, 0),
           AB = ifelse(AB_FL == TRUE, 1, 0),
           K = ifelse(EVENT_CD == 3, 1, 0), 
           SF = ifelse(SF_FL == TRUE, 1, 0))
  ## summarise by individual game
  p1 <- p1 %>%
    mutate(ID = id(p1["date"], drop = TRUE)) %>% 
    select_(~date, ~H, ~HR, ~AB, ~K, ~SF, ~ID) %>%
    group_by_(~ID) %>%
    summarise_(
      date = ~date[1], s.H = ~sum(H), s.HR = ~sum(HR), s.AB = ~sum(AB),
              s.K = ~sum(K), s.SF = ~sum(SF),
              num = (~s.H - s.HR), 
              denom = (~s.AB - s.K - s.HR + s.SF),
              BABIP = ~num / denom)
  
  ## function
  MA_fun <- function(num, denom, window){
    require(TTR)
    game <- 1 : length(num)
    P <- data.frame(Game=SMA(game, window),
                    Average=SMA(num, window) / SMA(denom, window))
    P[complete.cases(P), ]
  }
  
  player.babip <- MA_fun(p1$num, p1$denom, window)
  playername <- paste(player.info$nameFirst, player.info$nameLast, sep = " ")
  
  ## Plot Moving Average. Add in career BABIP for comparison
  c.babip <- data.frame(yintercept=as.numeric(career.babip), c.babip=factor(career.babip))
  p <- ggplot(player.babip, (aes(Game, Average)))
  print(p + geom_line() +
          geom_hline(data=c.babip, aes(yintercept = yintercept, fill = "Career BABIP"), 
                     color = "steelblue", linetype = "dashed", show_guide = TRUE) + 
          guides(fill = guide_legend(title = NULL)) + 
          xlab("Game Number") + ylab("Average BABIP") +
          scale_x_continuous(breaks=seq(0,162,10)) + 
          ggtitle(paste(window, "Game Moving Average of 2014 BABIP:", playername, sep = " ")))
  
  my_list <- list(p1, player.babip, career.babip)
}

# ----------------------------- # 
# Example 
# MA.babip("trout", "mike", 15)
# MA.babip("stubbs", "drew", 15)
