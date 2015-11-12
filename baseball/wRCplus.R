## --------------------------------------------------------- #
# wRC+ - Comparing similar players
#
# wRC+ is an index rating players total offensive value
# by runs created, while also accounting for park effects.
# a wRC+ value of 100 is league average, for every point
# > 100, the player is a percentage point > league average,
# e.g., a player with wRC+ of 115, created 15% more runs than 
# the league average. 
# 
# This function takes any offensive player from 1941 through 2014,
# calculates a similarity ranking based on Bill James similarity scores,
# and plots each players' wRC+ as a function of age. The plot includes 
# a quadratic age function. wRC+ is comparable regardless of  position, 
# but the James similarity scores account for position played. As a result,
# most comparisons will be of players from similar position.
#
# INPUT: lastname, firstname, number of similar players to find (n-1)
#
# OUTPUT: career statistics for each player, year by year statistics for
#         each player, plot of wRC+ as function of age, model fits
#         for each player's quadratic function with predicted peak
#         age and max wRC+.
#
# Note: may take a minute to run based on internet speed and number 
# of similar players. Most data is gathered from the web, however
# one file ("wRC_PA.league.csv") is in data folder.
## --------------------------------------------------------- #
require(Lahman)
install.packages("devtools")
library(devtools)
install_github("aaboyles/hadleyverse")
library(hadleyverse)
require(rvest)
require(car)

# ----- # 
lahman.player.search <- function(lastname, firstname=NULL) {
  require(Lahman)
  require(car)
  require(hadleyverse)
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

# ----- # 
similar <- function(p, data, number=10){
  # Bill James similarity Scores
  P <- subset(data, playerID == p)
  data$SS <- with(data,
                  1000 -
                    floor(abs(C.G - P$C.G) / 20) -
                    floor(abs(C.AB - P$C.AB) / 75) -
                    floor(abs(C.R - P$C.R) / 10) -
                    floor(abs(C.H - P$C.H) / 15) -
                    floor(abs(C.2B - P$C.2B) / 5) -
                    floor(abs(C.3B - P$C.3B) / 4) -
                    floor(abs(C.HR - P$C.HR) / 2) -
                    floor(abs(C.RBI - P$C.RBI) / 10) -
                    floor(abs(C.BB - P$C.BB) / 25) -
                    floor(abs(C.SO - P$C.SO) / 150) -
                    floor(abs(C.SB - P$C.SB) / 20) -
                    floor(abs(C.AVG - P$C.AVG) / 0.001) -
                    floor(abs(C.SLG - P$C.SLG) / 0.002) -
                    abs(Value.POS - P$Value.POS))
  data <- data[order(data$SS, decreasing=TRUE), ]
  data[1:number, ]
}

# ----- # 
wRCplus <- function(lastname, firstname=NULL, number=10) {
  
  # --------------------------------------- #
  # Data for similarity scores (including position) 
  # --------------------------------------- #
  ## player primary position (most often played)
  player.pos2 <- Fielding %>%
    filter(yearID > 1941) %>%
  group_by(playerID) %>%
  filter(G = min_rank(desc(G))==1) %>%
  select(playerID, POS) %>% distinct(playerID)

  ## career totals for each player
  df.career <- Batting %>% filter_(~yearID > 1941) %>%
    group_by_(~playerID) %>% 
    summarise(C.G = sum(G), C.AB = sum(AB), C.R=sum(R), C.H=sum(H), 
              C.2B=sum(X2B), C.3B=sum(X3B), 
              C.HR=sum(HR), C.RBI=sum(RBI), C.SB = sum(SB), C.CS = sum(CS),
              C.BB = sum(BB), C.SO = sum(SO), C.IBB = sum(IBB), C.HBP = sum(HBP),
              C.SH = sum(SH), C.SF = sum(SF), C.GIDP = sum(GIDP)) %>% 
    mutate(C.AVG = round((C.H / C.AB), 3), 
           C.SLG = round(((C.H - C.2B - C.3B - C.HR + 2 * C.2B 
                           + 3 * C.3B + 4 * C.HR) / C.AB),3),
           C.OBP = round((C.H + C.BB + C.HBP) / (C.AB + C.BB + C.HBP + C.SF)),3)

  ## join player position and career totals
  df.career <- inner_join(df.career, player.pos2)

  # add in Bill James position value
  df.career$Value.POS <- with(df.career,
                              ifelse(POS == "C", 240,
                                     ifelse(POS == "SS", 168,
                                            ifelse(POS == "2B", 132,
                                                   ifelse(POS == "3B", 84,
                                                          ifelse(POS == "OF", 48,
                                                                 ifelse(POS == "1B", 12, 0)))))))

  ## collect playerID 
  player.info <- lahman.player.search(lastname, firstname)
  PID <- as.vector(player.info$playerID)

  ## similarity rankings
  similarity <- similar(PID, df.career, number)
  sim.pid <- as.vector(similarity$playerID)

  ## player name and birth year for proper age
  sim.info <- Master %>% filter_(~playerID %in% sim.pid) %>%
    mutate(MLByear = ifelse(birthMonth <= 6, birthYear, birthYear + 1),
           Name = paste(nameFirst, nameLast, "")) %>%
    select_(~playerID, ~MLByear, ~Name)

  # --------------------------------------- #
  # Similar players - year on year stats 
  # --------------------------------------- #
  df <- Batting %>% 
    filter_(~playerID %in% sim.pid, ~yearID > 1941) %>%
    mutate(HBP = recode(HBP, 'NA=0'), SF = recode(SF, "NA = 0"), IBB = recode(IBB, 'NA=0'), 
           AVG = round((H / AB), 3),
           SLG = round(((H - X2B - X3B - HR + 2 * X2B + 3 * X3B + 4 * HR) / AB), 3),
           OBP = round(((H + BB + HBP) / (AB + BB + HBP + SF)), 3),
           OPS = round((SLG + OBP), 3))
  # adding age
  df <- df %>% inner_join(sim.info) %>% 
    mutate(Age = yearID - MLByear) %>%
    arrange_(~yearID, ~playerID)

  # --------------------------------------- #
  # Get data to calculate wRC+ 
  # --------------------------------------- #
  ## FanGraphs for constant values
  k_wOBA <- read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>%
    html_nodes("table") %>% 
    .[[16]] %>% 
    html_table()
  
  names(k_wOBA)[1] <- "yearID"
  names(k_wOBA)[2] <- "lg.wOBA"
  names(k_wOBA)[12] <- "R.PA"
  names(k_wOBA)[13] <- "R.W"
  
  ## FanGraphs Park Factor Data 
  getData <- function(url){
    require(rvest)
    park <- read_html(url) %>%
      html_nodes("table")  %>% 
      .[[18]] %>% 
      html_table()
  }
  
  getUrls <- function(yrs){
    root = "http://www.fangraphs.com/guts.aspx?type=pf&season="
    urls <- NULL
    for (year in first(yrs):last(yrs)){
      urls <- c(urls,(paste(root,year,"&teamid=0",sep="")))
    }
    return(urls)
  }
  
  yrs <- unique(df$yearID) 
  urls <- getUrls(yrs)
  
  park.factor=NULL
  for (url in urls){
    park.factor <- rbind(park.factor,getData(url))
  }
  
  ## AL/NL Batting Totals and wRC 
  # this takes too long to read in from the web
  wRC_PA.league <- read.csv("data/wRC_PA.league.csv", header = TRUE)
  

  # -------------------------------------------- #
  # Clean up discrepancy in Team Names and Parks
  # -------------------------------------------- #
  franchises <- Teams %>% filter(yearID > 1941)
  ## change labels of park.factor to franchID
  lut <- c("Angels" = "LAA", "Astros"  = "HOU", "Athletics" = "OAK", "Blue Jays" = "TOR", "Diamondbacks" = "ARI",
           "Braves" = "ATL", "Brewers" = "MIL", "Browns" = "BAL", "Cardinals" = "STL", "Colt .45's" = "HOU", 
           "Cubs" = "CHC", "Devil Rays" = "TBD", "Dodgers" = "LAD", "Expos" = "WSN", "Giants" = "SFG", 
           "Indians" = "CLE", "Mariners" = "SEA", "Marlins" = "FLA", "Mets" = "NYM", "Nationals" = "WSN",
           "Orioles" = "BAL", "Padres" = "SDP", "Phillies" = "PHI", "Pilots" = "MIL", "Pirates" = "PIT",
           "Rangers" = "TEX", "Rays" = "TBD", "Red Sox" = "BOS", "Redlegs" = "CIN", "Reds" = "CIN", 
           "Rockies" = "COL", "Royals" = "KCR", "Senators" = "MIN", "Tigers" = "DET", "Twins" = "MIN",
           "White Sox" = "CHW", "Yankees" = "NYY")
  # Use lut to translate the Team column park.factor
  park.factor$Team <- lut[park.factor$Team]

  # -------------------------------------------- #
  # Merge necessary data 
  # -------------------------------------------- #
  ## common franchise ID
  df <- merge(df, franchises[c("yearID", "teamID", "franchID")], by = c("yearID", "teamID"))
  
  ## park.factor for each team and year
  names(park.factor)[1] <- "yearID"
  names(park.factor)[2] <- "franchID"
  df <- merge(df, park.factor[c("yearID", "franchID", "Basic")], by = c("yearID", "franchID"))
  
  ## league PA and wRC
  df <- merge(df, wRC_PA.league[c("yearID", "lgID", "PA", "wRC")], by = c("yearID", "lgID"))

  ## constants for calculating wOBA and wRC+
  df <- inner_join(df, k_wOBA, by = 'yearID')
  df <- df %>% mutate(B1 = H - X2B - X3B - HR)

  # -------------------------------------------- #
  # Calculate wOBA and wRC+
  # -------------------------------------------- #
  df <- df %>% mutate(wOBA = round((wBB*(BB-IBB) + wHBP*HBP + w1B*B1 + w2B*X2B + w3B*X3B + wHR*HR) / 
                              (AB + BB - IBB + SF + HBP),3),
                            wRCplus = (((((wOBA-lg.wOBA)/wOBAScale) + R.PA) + 
                                          (R.PA-((Basic/100)*R.PA)))/(wRC/PA))*100,
                      wRCplus = round(wRCplus,0)) %>%
    arrange_(~playerID, ~yearID) %>% 
    select_(~MLByear, ~yearID, ~playerID, ~Name, ~Age, ~lgID, ~franchID, ~teamID, ~stint, ~G, ~AB, ~R, ~H,
           ~X2B, ~X3B, ~HR, ~RBI, ~SB, ~CS, ~BB, ~SO, ~IBB, ~HBP, ~SH, ~SF, ~GIDP, ~AVG, ~SLG, ~OBP,
           ~OPS, ~wOBA, ~wRCplus)

  ## crude accounting for trades - estimate simple mean for season duration...
   if (is.element(2, df$stint)) {
     df <- df %>% group_by_(~playerID, ~yearID) %>%
       summarise_(MLByear = ~first(MLByear),  Name = ~first(Name),
                  Age=~floor(mean(Age)), lgID = ~first(lgID), 
                  franchID = ~first(franchID),
                  teamID = ~first(teamID), G = ~sum(G), AB=~sum(AB),R=~sum(R),H=~sum(H),
                  X2B=~sum(X2B),X3B=~sum(X3B),HR=~sum(HR),RBI=~sum(RBI),SB=~sum(SB),CS=~sum(CS), 
                  BB=~sum(BB),SO=~sum(SO),IBB=~sum(IBB), HBP=~sum(HBP),SH=~sum(SH),SF=~sum(SF),
                  GIDP=~sum(GIDP), 
                  SLG = ~mean(round(SLG,3)),
                  OBP = ~mean(round(OBP,3)),
                  OPS = ~sum(SLG,OBP),
                  wOBA = ~mean(round(wOBA,3)),
                  wRCplus = ~mean(round(wRCplus,3))) %>%
       arrange_(~playerID, ~yearID) 
   } else {
     df == df
   }

  # --------------------------------------- #
  # Estimate Quadratic Model, plot, fit 
  # --------------------------------------- #
  models <- df %>% group_by_(~playerID) %>%
    do({mod = lm(wRCplus ~ I(Age - 30) + I((Age - 30)^2), data = .)
    pred <- predict(mod, newdata = .["Age"], se.fit=TRUE)
    data.frame(.,pred)})

  models <- models %>% mutate(lci = fit - 1.96 * se.fit,
                              uci = fit + 1.96 * se.fit)

  ## Plotting the fits
  print(ggplot(models, aes(x = Age, y = fit)) + geom_line() + 
          geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") + 
          geom_point(data = models, aes(x = Age, y = wRCplus)) + 
          facet_wrap(~ Name))

  sum.fit <- function(d){
    b <- coef(lm(wRCplus ~ I(Age - 30) + I((Age - 30) ^ 2), data=d))
    data.frame(A=b[1], B=b[2], C=b[3])
  }
  
  fits <- df %>% group_by_(~playerID) %>%
    do(sum.fit(.)) %>% 
    mutate(peak.age = round((30 - B / 2 / C),1),
           max = round((A - B^2 / C / 4 ),3),
           decline = round(C,5))
  
  my_list <- list(similarity, models, fits)

}

## Examples
# wRCplus("burks", 'ellis", 6)
# wRCplus("reynolds", "harold", 6)
