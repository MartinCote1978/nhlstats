#--------------------------------------------------------------
# scratch code for verifying hockey reference urls
#
# box score URL (specific game): http://www.hockey-reference.com/boxscores/201201050LAK.html
# box score URL (date): http://www.hockey-reference.com/boxscores/index.cgi?month=1&day=5&year=2012
# team URL: http://www.hockey-reference.com/teams/LAK
# skaters URL: http://www.hockey-reference.com/teams/LAK/skaters.html
# goalies URL: http://www.hockey-reference.com/teams/LAK/goalies.html
# coaches URL: http://www.hockey-reference.com/teams/LAK/coaches.html
# draft URL: http://www.hockey-reference.com/teams/LAK/draft.html
# captain URL: http://www.hockey-reference.com/teams/LAK/captains.html
# h2h URL: http://www.hockey-reference.com/teams/LAK/head2head.html
# roster stats for team and year: http://www.hockey-reference.com/teams/LAK/1968.html
# schedule stats for team and year: http://www.hockey-reference.com/teams/LAK/1968_games.html
# season stats for a given year:  http://www.hockey-reference.com/leagues/NHL_1968_games.html
#--------------------------------------------------------------

# compress mightily on save
options(save.defaults=list(compress="bzip2", compression_level=9))

library(RCurl)
library(stringr)
library(XML)
library(plyr)
library(lubridate)
library(scrapeR)

url.checker <- function(url) {
  fn <- try(suppressWarnings(readLines(con <- url(url))), silent=TRUE)
  close(con)
  !inherits(fn, "try-error")
}

hrefFun <- function(x){
  xpathSApply(x,'./a',xmlAttrs)  
}

hrefFun2 <- function(x){
  xpathSApply(x,'./strong /a',xmlAttrs)  
}

base.url <- "http://www.hockey-reference.com"

#------------------------------------------------------------------------------
# extract unique hockey ref coaches IDs from hockey ref coaches index
#------------------------------------------------------------------------------

coach.url <- "http://www.hockey-reference.com/coaches"
page.source <- scrape(url=coach.url, verbose=FALSE, follow=TRUE)
coach.table <- readHTMLTable(page.source[[1]], stringsAsFactors=FALSE)[[1]]
names(coach.table) <- tolower(names(coach.table))
coach.table$coach <- str_replace_all(coach.table$coach, "\\*", "")
coach.table$firstName <- sapply(str_split(coach.table$coach, pattern=" "), "[", 1)
coach.table$lastName <- sapply(str_split(coach.table$coach, pattern=" "), "[", 2)
coach.table <- coach.table[, c("coach", "firstName", "lastName")]

# import additional copies of table to obtain embedded HTML links to each player
# need to import twice due to some player links embedded in bold 
coach.table.url <- readHTMLTable(page.source[[1]], elFun = hrefFun, header=FALSE, stringsAsFactors=FALSE)[[1]]
coach.table.url2 <- readHTMLTable(page.source[[1]], elFun = hrefFun2, header=FALSE, stringsAsFactors=FALSE)[[1]]

# clean up urls and extract unique coach ID to coach table
url.table <- as.data.frame(cbind(coach.table.url[,1], coach.table.url2[,1]), stringsAsFactors=FALSE)
url.table[url.table$V1 == "list()", "V1"] <- url.table[url.table$V1 == "list()", "V2"]
url.table$V2 <- unlist(str_split(url.table$V1, pattern="/"))[4]
url.table$V2 <- str_replace_all(sapply(str_split(url.table$V1, pattern="/"), "[", 3), "\\.html", "")
coach.table$url_coach <- paste0("http://www.hockey-reference.com", url.table$V1)
coach.table$hrefID <- url.table$V2

#------------------------------------------------------------------------------
# extract unique hockey ref player IDs from hockey refplayer index
#------------------------------------------------------------------------------

name.urls <- paste0("http://www.hockey-reference.com/players/", letters)

page.source <- scrape(url=name.urls, verbose=FALSE, follow=TRUE)
id.list <- vector("list", length(name.urls))

for(i in 1:length(name.urls)) {
  message(paste0("processing player IDs with last name starting with ", letters[i]))
  letter.table <- readHTMLTable(page.source[[i]], stringsAsFactors=FALSE)
  
  # check to make sure there is a table with player names starting with this letter
  # if not, make list element missing and go to next letter
  if(!any(names(letter.table) %in% c("players"))) {
    id.list[[i]] <- NA
    next
  }
  
  # import table and remove asterisk from player name
  letter.table <- letter.table[[1]]
  names(letter.table) <- tolower(names(letter.table))
  letter.table$player <- str_replace_all(letter.table$player, "\\*", "")
  letter.table$firstName <- sapply(str_split(letter.table$player, pattern=" "), "[", 1)
  letter.table$lastName <- sapply(str_split(letter.table$player, pattern=" "), "[", 2)
  letter.table <- letter.table[, c("player", "firstName", "lastName")]
  
  # import additional copies of table to obtain embedded HTML links to each player
  # need to import twice due to some player links embedded in bold 
  letter.table.url <- readHTMLTable(page.source[[i]], elFun = hrefFun, header=FALSE, stringsAsFactors=FALSE)[[1]]
  letter.table.url2 <- readHTMLTable(page.source[[i]], elFun = hrefFun2, header=FALSE, stringsAsFactors=FALSE)[[1]]
  
  # clean up urls and extract unique player ID to letter table
  url.table <- as.data.frame(cbind(letter.table.url[,1], letter.table.url2[,1]), stringsAsFactors=FALSE)
  url.table[url.table$V1 == "list()", "V1"] <- url.table[url.table$V1 == "list()", "V2"]
  url.table$V2 <- unlist(str_split(url.table$V1, pattern="/"))[4]
  url.table$V2 <- str_replace_all(sapply(str_split(url.table$V1, pattern="/"), "[", 4), "\\.html", "")
  letter.table$url_player <- paste0("http://www.hockey-reference.com", url.table$V1)
  letter.table$hrefID <- url.table$V2
  
  id.list[[i]] <- letter.table
}

# combine into one data frame using rbind.fill
id_data <- rbind.fill(id.list[!is.na(id.list)])

readHTMLTable(page.source[[24]], stringsAsFactors=FALSE)

letter.tables <- readHTMLTable(page.source[[1]], stringsAsFactors=FALSE)
letter.tables.url <- readHTMLTable(page.source[[1]], elFun = hrefFun, header=FALSE, stringsAsFactors=FALSE)[[1]]
letter.tables.url2 <- readHTMLTable(page.source[[1]], elFun = hrefFun2, header=FALSE, stringsAsFactors=FALSE)[[1]]

url.table <- as.data.frame(cbind(letter.tables.url[,1], letter.tables.url2[,1]), stringsAsFactors=FALSE)

str(url.table)

url.table[url.table$V1 == "list()", "V1"] <- url.table[url.table$V1 == "list()", "V2"]
url.table$V2 <- unlist(str_split(url.table$V1, pattern="/"))[4]
url.table$V2 <- str_replace_all(sapply(str_split(url.table$V1, pattern="/"), "[", 4), "\\.html", "")




#---------------------------------------------------------------------------------
# box score URL logic
# 1. assemble URL for a particular date based on month, day, year
# 2. check that the URL itself is valid using url.checker function
# - if not, go to next date
# 3. import tables from url using readHTMLTable
# - check to make sure the table called "stats" was imported.  If not, go to next date
# 4. import another copy of table but using hrefFun custom fuction to get the embedded HTML link
#    in the date column
# 5. add the URL column from second HTML import to the first table imported
# 6. clean up the table 
# - from master team long/short name table, add in short team name columns for home and away
# - add in new columns for season, month, day, year
# 
# put all of these tables into an empty list and then use plyr and rbind.fill to put them into
# one big table
#---------------------------------------------------------------------------------

start <- as.POSIXct("1987-10-08")
end <- as.POSIXct("2013-10-10")
start.year <- as.numeric(substr(start, 0, 4))

if (start.year < 1987) {
  warning("Box score data are not available before 1987.  ")
  start <- as.POSIXct("1987-10-08")
}

if (end > as.POSIXct(Sys.Date())) warning("Sorry, I can't access box score data in the future!")

base.url <- "http://www.hockey-reference.com"

years <- 1987:2013

months <- c(9:12, 1:6)
days <- 1:31
# months <- sapply(c(9:12, 1:6), function(x) ifelse(str_length(x)==1, paste0(0, x), x))
# days <- sapply(1:31, function(x) ifelse(str_length(x)==1, paste0(0, x), x))

boxscore.input <- expand.grid(year=years, month=months, day=days, stringsAsFactors=FALSE)
boxscore.input <- mutate(boxscore.input, 
                         month.text = ifelse(str_length(month) == 1, paste0(0, month), month),
                         day.text =  ifelse(str_length(day)==1, paste0(0, day), day),
                         date = paste(year, month.text, day.text, sep="-"),
                         date.parsed = ymd(date),
                         url_date = paste0(base.url, "/boxscores/index.cgi?month=", month, "&day=", day, "&year=", year))

boxscore.input <- boxscore.input[!is.na(boxscore.input$date.parsed),]
boxscore.input <- arrange(boxscore.input, date.parsed)
boxscore.input <- subset(boxscore.input,
                         subset=(date.parsed >= ymd("1987-10-08") & date.parsed < ymd(as.character(Sys.Date()))))

boxscore.input$season <- ifelse(boxscore.input$month <= 6,
                                paste(boxscore.input$year-1, boxscore.input$year, sep="-"),
                                paste(boxscore.input$year, boxscore.input$year+1, sep="-"))

#tmp <- unique(boxscore.input[, c("month", "year", "season")])

#boxscore.input[which(boxscore.input$date=="1987-12-25"),]

#boxscore.input <- boxscore.input[290:300,]
#message("test")
#size <- 10
size <- nrow(boxscore.input)
#boxscore.urls.list <- vector("list", length(290:298))
boxscore.urls.list <- vector("list", size)

message(paste("import started on", Sys.time()))
import.startime <- Sys.time()

# start to process dates in the boxscore.input data frame
for(i in 1:size) {
  message("date is ", boxscore.input[i, "date"])
  full.url <- boxscore.input[i, "url_date"]
  season <- boxscore.input[i, "season"]
  date <- boxscore.input[i, "date"]
  table.stats <- readHTMLTable(full.url, header=TRUE, stringsAsFactors=FALSE)
  if(!any(names(table.stats) %in% c("stats"))) {
    #print(paste0("no games for ", boxscore.input[i, "date"]))
    boxscore.urls.list[[i]] <- NA
    next
  }
  table.stats2 <- readHTMLTable(full.url, elFun = hrefFun, header=FALSE, stringsAsFactors=FALSE)
  
  # subset to only take the columns with the date, visitor team name, and home team name
  games.set <- table.stats[[1]][, c(1,2,4)]
  names(games.set) <- c("date.site", "visitor.long", "home.long")
  games.set$url_game <- paste0(base.url, table.stats2[[1]][,1])
  games.set$date <- date
  games.set$season <- season
  
  #print(head(games.set))
  boxscore.urls.list[[i]] <- games.set
}

message(paste("import ended on", Sys.time()))
import.endtime <- Sys.time()

# combine into one data frame using rbind.fill
urls.boxscore <- rbind.fill(boxscore.urls.list[!is.na(boxscore.urls.list)])

# additional diagnostics on urls.boxscore



#names(urls.boxscore) <- c("date", "visitor.long", "home.long", "url_game")

#sort(unique(urls.boxscore$visitor.long))


urls.boxscore$visitor.short <- revalue(urls.boxscore$visitor.long,
                                      c("Anaheim Ducks" = "ANA",
                                        "Atlanta Thrashers" = "ATL",
                                        "Boston Bruins" = "BOS",
                                        "Buffalo Sabres" = "BUF",
                                        "Calgary Flames" = "CGY",
                                        "Carolina Hurricanes" = "CAR",
                                        "Chicago Blackhawks" = "CHI",
                                        "Colorado Avalanche" = "COL",
                                        "Columbus Blue Jackets" = "CBJ",
                                        "Dallas Stars" = "DAL",
                                        "Detroit Red Wings" = "DET",
                                        "Edmonton Oilers" = "EDM",
                                        "Florida Panthers" = "FLA",
                                        "Hartford Whalers" = "HAR",
                                        "Los Angeles Kings" = "LAK",
                                        "Mighty Ducks of Anaheim" = "MDA",
                                        "Minnesota North Stars" = "MNS",
                                        "Minnesota Wild" = "MIN",
                                        "Montreal Canadiens" = "MON",
                                        "Nashville Predators" = "NSH",
                                        "New Jersey Devils" = "NJD",
                                        "New York Islanders" = "NYI",
                                        "New York Rangers" = "NYR",
                                        "Ottawa Senators" = "OTT",
                                        "Philadelphia Flyers" = "PHI",
                                        "Phoenix Coyotes" = "PHX",
                                        "Pittsburgh Penguins" = "PIT",
                                        "Quebec Nordiques" = "QUE",
                                        "San Jose Sharks" = "SJS",
                                        "St. Louis Blues" = "STL",
                                        "Tampa Bay Lightning" = "TBL",
                                        "Toronto Maple Leafs" = "TOR",
                                        "Vancouver Canucks" = "VAN",
                                        "Washington Capitals" = "WSH",
                                        "Winnipeg Jets" = "WPG"))

urls.boxscore$home.short <- revalue(urls.boxscore$home.long,
                                      c("Anaheim Ducks" = "ANA",
                                        "Atlanta Thrashers" = "ATL",
                                        "Boston Bruins" = "BOS",
                                        "Buffalo Sabres" = "BUF",
                                        "Calgary Flames" = "CGY",
                                        "Carolina Hurricanes" = "CAR",
                                        "Chicago Blackhawks" = "CHI",
                                        "Colorado Avalanche" = "COL",
                                        "Columbus Blue Jackets" = "CBJ",
                                        "Dallas Stars" = "DAL",
                                        "Detroit Red Wings" = "DET",
                                        "Edmonton Oilers" = "EDM",
                                        "Florida Panthers" = "FLA",
                                        "Hartford Whalers" = "HAR",
                                        "Los Angeles Kings" = "LAK",
                                        "Mighty Ducks of Anaheim" = "MDA",
                                        "Minnesota North Stars" = "MNS",
                                        "Minnesota Wild" = "MIN",
                                        "Montreal Canadiens" = "MON",
                                        "Nashville Predators" = "NSH",
                                        "New Jersey Devils" = "NJD",
                                        "New York Islanders" = "NYI",
                                        "New York Rangers" = "NYR",
                                        "Ottawa Senators" = "OTT",
                                        "Philadelphia Flyers" = "PHI",
                                        "Phoenix Coyotes" = "PHX",
                                        "Pittsburgh Penguins" = "PIT",
                                        "Quebec Nordiques" = "QUE",
                                        "San Jose Sharks" = "SJS",
                                        "St. Louis Blues" = "STL",
                                        "Tampa Bay Lightning" = "TBL",
                                        "Toronto Maple Leafs" = "TOR",
                                        "Vancouver Canucks" = "VAN",
                                        "Washington Capitals" = "WSH",
                                        "Winnipeg Jets" = "WPG"))



save(urls.boxscore,
     file="urls.boxscore.RData")

#---------------------------------------------------------------------------------
# overall team stats URL logic
# 1. Use character vector with all short team names
# 2. Define all 7 URLs corresponding to summary type
# 2. check that the team summary URL itself exists
# - if not, go to next team name
# 3. Create URLs based on team name and populate URL data set
#
# NOTE: need to omit ATL since it is redundant with WPG
#---------------------------------------------------------------------------------

teams <- c("ANA", "ATF", "BOS", "BRO", "BUF", "CGY", "CAR", "CBH", "CGS", "CHI", "CLE", "CLR", 
           "COL", "CBJ", "DAL", "DET", "DTC", "DTF", "EDM", "FLA", "HAM", "HAR", "KCS", "LAK", "MDA", 
           "MIN", "MNS", "MTL", "MTM", "MTW", "NSH", "NJD", "NYA", "NYI", "NYR", "OAK", "OTS", "OTT", 
           "PHI", "PHQ", "PHX", "PIT", "PTP", "QBC", "QUE", "SJS", "STE", "STL", "TBL", "TOR", "TRA", 
           "TRS", "VAN", "WIN", "WSH", "WPG")

# define data.frame to host urls
urls_team_overall <- data.frame(team_name = teams,
                                url_summary = rep(NA, length(teams)),
                                url_skaters = rep(NA, length(teams)),
                                url_goalies = rep(NA, length(teams)),
                                url_coaches = rep(NA, length(teams)),
                                url_draft = rep(NA, length(teams)),
                                url_captains = rep(NA, length(teams)),
                                url_head2head = rep(NA, length(teams)))

for(i in 1:nrow(urls_team_overall)) {
  team <- urls_team_overall[i, "team_name"]
  
  message(paste("assembling URLs for team", team))
  
  #url_summary <- "http://www.hockey-reference.com//teams/ATF"
  url_summary <- paste0(base.url, "/teams/", team)
  
  # if team summary page does not exist, then skip and go to next team
  if(!url.checker(url=url_summary)) next
  
  # add urls to data frame
  urls_team_overall[i, "url_summary"] <- paste0(base.url, "/teams/", team)
  urls_team_overall[i, "url_skaters"] <- paste0(base.url, "/teams/", team, "/skaters.html")
  urls_team_overall[i, "url_goalies"] <- paste0(base.url, "/teams/", team, "/goalies.html")
  urls_team_overall[i, "url_coaches"] <- paste0(base.url, "/teams/", team, "/coaches.html")
  urls_team_overall[i, "url_draft"] <- paste0(base.url, "/teams/", team, "/draft.html")
  urls_team_overall[i, "url_captains"] <- paste0(base.url, "/teams/", team, "/captains.html")
  urls_team_overall[i, "url_head2head"] <- paste0(base.url, "/teams/", team, "/head2head.html")
}

# omit rows with NA in urls (if one is missing, all are missing)
urls_team_overall <- urls_team_overall[!is.na(urls_team_overall$url_summary),]

# save to a workspace
save(urls_team_overall,
     file="urls_team_overall.RData")

#------------------------------------------------------------------------------------
# overall team specific stats grabber
#------------------------------------------------------------------------------------

skaters_names <- c("rk",
                   "player",
                   "from",
                   "to",
                   "yrs",
                   "pos",
                   "gp",
                   "goals",
                   "assists",
                   "points",
                   "goals_created",
                   "plus_minus",
                   "pim",
                   "goals_even",
                   "goals_pp",
                   "goals_sh",
                   "goals_gw",
                   "assists_even",
                   "assists_sh",
                   "assists_pp",
                   "shots",
                   "shooting_percent",
                   "time_on_ice",
                   "avg_time_on_ice")

goalies_names <- c("rk",
                   "player",
                   "from",
                   "to",
                   "yrs",
                   "gp",
                   "w",
                   "l",
                   "tot",
                   "ga",
                   "sa",
                   "sv",
                   "sv_percent",
                   "gaa",
                   "so",
                   "min",
                   "ga_pct",
                   "gsaa",
                   "goals",
                   "assists",
                   "points",
                   "pim")

coaches_names <- c("rk",
                    "coach",
                    "from",
                    "to",
                    "yrs",
                    "reg_gp",
                    "reg_w",
                    "reg_l",
                    "reg_t",
                    "reg_ol",
                    "reg_points",
                    "reg_points_percent",
                    "playoff_g",
                    "playoff_w",
                    "playoff_l",
                    "playoff_t",
                    "playoff_w_l_percent")

draft_names <- c("year",
                  "league",
                  "draft",
                  "round",
                  "overall",
                  "player",
                  "amateur_team",
                  "skater_gp",
                  "skater_goals",
                  "skater_assists",
                  "skater_points",
                  "skater_plus_minus",
                  "skater_pim",
                  "goalie_gp",
                  "goalie_wins",
                  "goalie_losses",
                  "goalie_tot",
                  "goalie_save_percent",
                  "goalie_gaa")

captains_names <- c("rk",
                    "season",
                    "player",
                    "skater_gp",
                    "skater_goals",
                    "skater_assists",
                    "skater_points",
                    "skater_plus_minus",
                    "skater_pim",
                    "goalie_gp",
                    "goalie_wins",
                    "goalie_losses",
                    "goalie_tot",
                    "goalie_save_percent",
                    "goalie_gaa")

head2head_names <- c("rk",
                    "opponent",
                    "gp",
                    "wins",
                    "losses",
                    "ties",
                    "overtime_losses",
                    "points",
                    "points_percent",
                    "goals_for",
                    "goals_against",
                    "goals_for_per_game",
                    "goals_against_per_game")

load("urls_team_overall.RData")

# set up lists for the data frames
skaters_list <- goalies_list <- coaches_list <- draft_list <- captains_list <- head2head_list <- vector("list", nrow(urls_team_overall))

for(i in 1:nrow(urls_team_overall)) {
  
  message(paste("importing team statistics for", urls_team_overall[i, "team_name"]))
          
  # import skaters table
  # issue: Atlanta skater page does not have even strength assists, short-handed assists, or power play assists
  
  if(urls_team_overall[i, "team_name"] == "ATL") {
    skaters_names_interim <- skaters_names[c(1:17, 21:24)]
    skaters_classes <- c("integer", 
                         "character", 
                         rep("integer", 3),
                         "character",
                         rep("integer", 12),
                         "numeric",
                         "integer",
                         "character")
    
    # this is the name order from the web site for ATL
    goalies_names_interim <- goalies_names[c(1:15, 22, 16, 19:21)]
    
    # this is what the final order should look like
    goalies_names <- goalies_names[c(1:16, 19:22)]
    
    # classes of columns in ATL table
    goalies_classes <- c("integer", 
                         "character", 
                         rep("integer", 8),
                         rep("numeric", 2),
                         rep("integer", 6))
    
  } else {
    skaters_names_interim <- skaters_names
    skaters_classes <- c("integer", 
                         "character", 
                         rep("integer", 3),
                         "character",
                         rep("integer", 15),
                         "numeric",
                         "integer",
                         "character")
    
    goalies_names_interim <- goalies_names
    
    goalies_classes <- c("integer", 
                         "character", 
                         rep("integer", 10),
                         rep("numeric", 2),
                         rep("integer", 2),
                         rep("numeric", 2),
                         rep("integer", 4))
      
  }
  
  skaters_check <- readHTMLTable(urls_team_overall[i, "url_skaters"], header=FALSE)
  if(!any(names(skaters_check) %in% c("skaters"))) {
    skaters_list[[i]] <- NA
  } else {
    skaters_table <- readHTMLTable(urls_team_overall[i, "url_skaters"],
                                   header=FALSE,
                                   stringsAsFactors=FALSE,
                                   colClasses=skaters_classes)[[1]]
    
    names(skaters_table) <- skaters_names_interim
    skaters_table <- skaters_table[!is.na(skaters_table$rk),]
    skaters_table$team_short <- urls_team_overall[i, "team_name"]
    
    skaters_list[[i]] <- skaters_table
  }
  
  
  # import goalies table
  #browser()
  goalies_check <- readHTMLTable(urls_team_overall[i, "url_goalies"], header=FALSE)
  if(!any(names(goalies_check) %in% c("goalies"))) {
    goalies_list[[i]] <- NA
  } else {
    goalies_table <- readHTMLTable(urls_team_overall[i, "url_goalies"],
                                   header=FALSE,
                                   stringsAsFactors=FALSE,
                                   colClasses=goalies_classes)[[1]]
    
    names(goalies_table) <- goalies_names_interim
    goalies_table <- goalies_table[!is.na(goalies_table$rk),]
    goalies_table$team_short <- urls_team_overall[i, "team_name"]
    goalies_table <- goalies_table[,c(goalies_names_interim, "team_short")]
    
    goalies_list[[i]] <- goalies_table
  }


  
  # import coaches table
  
  coaches_check <- readHTMLTable(urls_team_overall[i, "url_coaches"], header=FALSE)
  if(!any(names(coaches_check) %in% c("coaches"))) {
    coaches_list[[i]] <- NA
  } else {
    coaches_table <- readHTMLTable(urls_team_overall[i, "url_coaches"],
                                   header=FALSE,
                                   stringsAsFactors=FALSE,
                                   colClasses=c("integer", 
                                                "character", 
                                                rep("integer", 9),
                                                "numeric",
                                                rep("integer", 4),
                                                "numeric"))[[1]]
    
    names(coaches_table) <- coaches_names
    coaches_table <- coaches_table[!is.na(coaches_table$rk),]
    coaches_table$team_short <- urls_team_overall[i, "team_name"]
    
    coaches_list[[i]] <- coaches_table
  }

  
  # import draft table
  
  draft_check <- readHTMLTable(urls_team_overall[i, "url_draft"], header=FALSE)
  if(!any(names(draft_check) %in% c("stats"))) {
    draft_list[[i]] <- NA
  } else {
    draft_table <- readHTMLTable(urls_team_overall[i, "url_draft"],
                                 header=FALSE,
                                 stringsAsFactors=FALSE,
                                 colClasses=c("integer", 
                                              rep("character", 2), 
                                              rep("integer", 2),
                                              rep("character", 2),
                                              rep("integer", 10),
                                              rep("numeric", 2)))[[1]]
    
    names(draft_table) <- draft_names
    draft_table <- draft_table[!is.na(draft_table$year),]
    draft_table$team_short <- urls_team_overall[i, "team_name"]
    
    draft_list[[i]] <- draft_table
  }

  
  # import captains table
  
  captains_check <- readHTMLTable(urls_team_overall[i, "url_captains"], header=FALSE)
  if(!any(names(captains_check) %in% c("captains"))) {
    captains_list[[i]] <- NA
  } else {
    captains_table <- readHTMLTable(urls_team_overall[i, "url_captains"],
                                    header=FALSE,
                                    stringsAsFactors=FALSE,
                                    colClasses=c("integer", 
                                                 rep("character", 2), 
                                                 rep("integer", 12),
                                                 rep("numeric", 2)))[[1]]
    
    names(captains_table) <- captains_names
    captains_table <- captains_table[!is.na(captains_table$rk),]
    captains_table$team_short <- urls_team_overall[i, "team_name"]
    
    captains_list[[i]] <- captains_table
  }
  

  
  # import head2head table
  
  head2head_check <- readHTMLTable(urls_team_overall[i, "url_head2head"], header=FALSE)
  if(!any(names(head2head_check) %in% c("head2head"))) {
    head2head_list[[i]] <- NA
  } else {
    head2head_table <- readHTMLTable(urls_team_overall[i, "url_head2head"],
                                     header=FALSE,
                                     stringsAsFactors=FALSE,
                                     colClasses=c("integer",
                                                  "character",
                                                  rep("integer", 6),
                                                  "numeric",
                                                  rep("integer", 2),
                                                  rep("numeric", 2)))[[1]]
    
    names(head2head_table) <- head2head_names
    head2head_table <- head2head_table[!is.na(head2head_table$rk),]
    head2head_table$team_short <- urls_team_overall[i, "team_name"]
    
    head2head_table$opponent_short <- revalue(head2head_table$opponent,
                                              c("Anaheim Ducks" = "ANA",
                                                "Atlanta Thrashers" = "ATL",
                                                "Boston Bruins" = "BOS",
                                                "Buffalo Sabres" = "BUF",
                                                "Calgary Flames" = "CGY",
                                                "Carolina Hurricanes" = "CAR",
                                                "Chicago Blackhawks" = "CHI",
                                                "Colorado Avalanche" = "COL",
                                                "Columbus Blue Jackets" = "CBJ",
                                                "Dallas Stars" = "DAL",
                                                "Detroit Red Wings" = "DET",
                                                "Edmonton Oilers" = "EDM",
                                                "Florida Panthers" = "FLA",
                                                "Hartford Whalers" = "HAR",
                                                "Los Angeles Kings" = "LAK",
                                                "Mighty Ducks of Anaheim" = "MDA",
                                                "Minnesota North Stars" = "MNS",
                                                "Minnesota Wild" = "MIN",
                                                "Montreal Canadiens" = "MON",
                                                "Nashville Predators" = "NSH",
                                                "New Jersey Devils" = "NJD",
                                                "New York Islanders" = "NYI",
                                                "New York Rangers" = "NYR",
                                                "Ottawa Senators" = "OTT",
                                                "Philadelphia Flyers" = "PHI",
                                                "Phoenix Coyotes" = "PHX",
                                                "Pittsburgh Penguins" = "PIT",
                                                "Quebec Nordiques" = "QUE",
                                                "San Jose Sharks" = "SJS",
                                                "St. Louis Blues" = "STL",
                                                "Tampa Bay Lightning" = "TBL",
                                                "Toronto Maple Leafs" = "TOR",
                                                "Vancouver Canucks" = "VAN",
                                                "Washington Capitals" = "WSH",
                                                "Winnipeg Jets" = "WPG"),
                                              warn_missing=FALSE)
    
    head2head_list[[i]] <- head2head_table
  }

}

# combine individual data frames into one data frame using rbind.fill
head2head_overall <- rbind.fill(head2head_list[!is.na(head2head_list)])
save(head2head_overall, file="head2head_overall.RData")

skaters_overall <- rbind.fill(skaters_list[!is.na(skaters_list)])
save(skaters_overall, file="skaters_overall.RData")

goalies_overall <- rbind.fill(goalies_list[!is.na(goalies_list)])
save(goalies_overall, file="goalies_overall.RData")

captains_overall <- rbind.fill(captains_list[!is.na(captains_list)])
save(captains_overall, file="captains_overall.RData")

coaches_overall <- rbind.fill(coaches_list[!is.na(coaches_list)])
save(coaches_overall, file="coaches_overall.RData")

draft_overall <- rbind.fill(draft_list[!is.na(draft_list)])
save(draft_overall, file="draft_overall.RData")


#---------------------------------------------------------------------------------
# yearly team stats URL grabber logic
# 1. Use urls_team_overall as input (select only the team name and overall summary url)
# 2. Extract the overall team summary table using url_summary value
# 3. Extract another version of team summary table with url value for year team page
#
# TO DO: extract number out of "finish" column (i.e. get 4 out of 4th)
#---------------------------------------------------------------------------------

#load("urls_team_overall.RData")

input.set <- urls_team_overall[, c("team_name", "url_summary")]

team_summary_data_list <- vector("list", nrow(input.set))

# define column names for overall team stats dataset
team_summary_names <- c("season",
                        "league",
                        "team_long",
                        "gp",
                        "wins",
                        "losses",
                        "ties",
                        "overtime_losses",
                        "points",
                        "points_percent",
                        "srs",
                        "sos",
                        "finish",
                        "playoff_notes",
                        "coaches")

for(i in 1:nrow(input.set)) {
  
  message(paste("importing overall team summary for", input.set[i, "team_name"]))
  #i <- 1
  
  # import summary stats table (need to check if table exists first)
  
  #browser()
  table_check <- readHTMLTable(input.set[i, "url_summary"], header=FALSE, stringsAsFactors=FALSE)
  
  if(!any(names(table_check) %in% input.set[i, "team_name"])) {
    team_summary_data_list[[i]] <- NA
  } else {
    table.stats <- readHTMLTable(input.set[i, "url_summary"], header=TRUE, stringsAsFactors=FALSE)
    team_summary <- table.stats[[1]]
    
    # rename columns
    names(team_summary) <- team_summary_names
    
    # define indicator for whether team made playoffs based on asterisk in team name
    team_summary$playoff_ind <- str_detect(team_summary$team_long, "\\*")
    team_summary$team_long <- str_replace_all(team_summary$team_long, "\\*", "")
    team_summary$team_short <- input.set[i, "team_name"]
    
    # import additional version to extract year url
    table.stats2 <- readHTMLTable(input.set[i, "url_summary"], elFun = hrefFun, header=FALSE, stringsAsFactors=FALSE)
    team_summary$url_season <- paste0(base.url, table.stats2[[1]][,1])
    team_summary$url_schedule <- str_replace_all(team_summary$url_season, pattern="\\.html", replacement="\\_games\\.html")
    
    #print(head(games.set))
    team_summary_data_list[[i]] <- team_summary
  }
}

# combine into one data frame using rbind.fill
team_summary_data <- rbind.fill(team_summary_data_list[!is.na(team_summary_data_list)])

save(team_summary_data,
     file="team_summary_data.RData")

load("team_summary_data.RData")

#-----------------------------------------------------------------------------
# import team-specific yearly statistics for players and goalies
#------------------------------------------------------------------------------

load("team_summary_data.RData")

input_set <- team_summary_data[, c("team_short", "team_long", "season", "playoff_ind", "url_season")]

# need to skip current season (find a way to standardize this)
input_set <- subset(input_set, season != "2013-14")

skaters_reg_colnames <- c("rk",
                       "player",
                       "pos",
                       "age",
                       "gp",
                       "goals",
                       "assists",
                       "points",
                       "goals_created",
                       "plus_minus",
                       "pim",
                       "goals_even",
                       "goals_pp",
                       "goals_sh",
                       "goals_gw",
                       "assists_even",
                       "assists_sh",
                       "assists_pp",
                       "shots",
                       "shooting_percent",
                       "time_on_ice",
                       "avg_time_on_ice",
                       "ops",
                       "dps",
                       "ps")

skaters_playoff_colnames <- skaters_reg_colnames[1:22]

goalies_reg_colnames <- c("team",
                          "season",
                          "rk",
                          "player",
                          "pos",
                          "age",
                          "gp",
                          "w",
                          "l",
                          "tot",
                          "ga",
                          "sa",
                          "sv",
                          "sv_percent",
                          "gaa",
                          "so",
                          "min",
                          "gps")

goalies_playoff_colnames <- goalies_reg_colnames[1:17]

skaters_season_reg_list <- vector("list", nrow(input_set)) 
goalies_season_reg_list <- vector("list", nrow(input_set)) 
skaters_season_playoff_list <- vector("list", nrow(input_set)) 
goalies_season_playoff_list <- vector("list", nrow(input_set)) 


for(i in 1:nrow(input_set)) {

  message(paste("importing player statistics for team", input_set[i, "team_short"], input_set[i, "season"]))
  
  #browser()
  
  # alternative way to read in tables
  # table order: team_stats, roster, skaters, goalies, skaters_playoffs, goalies_playoffs
  doc = htmlParse(input_set[i, c("url_season")])
  tableNodes = getNodeSet(doc, "//table")
  
  skaters_reg_table <- readHTMLTable(tableNodes[[3]],
                                     header=TRUE,
                                     stringsAsFactors=FALSE,
                                     colClasses=c("integer",
                                                  rep("character", 2),
                                                  rep("integer", 16),
                                                  "numeric",
                                                  "integer",
                                                  "character",
                                                  rep("numeric", 3)))
  
  names(skaters_reg_table) <- skaters_reg_colnames
  skaters_reg_table$team_short <- input_set[i, "team_short"]
  skaters_reg_table$season <- input_set[i, "season"]
  
  skaters_season_reg_list[[i]] <- skaters_reg_table
  
  goalies_reg_table <- readHTMLTable(tableNodes[[4]],
                                     header=TRUE,
                                     stringsAsFactors=FALSE,
                                     colClasses=c("integer",
                                                  rep("character", 2),
                                                  rep("integer", 8),
                                                  rep("numeric", 2),
                                                  rep("integer",2),
                                                  "numeric"))
  
  names(goalies_reg_table) <- goalies_reg_colnames
  goalies_reg_table$team_short <- input_set[i, "team_short"]
  goalies_reg_table$season <- input_set[i, "season"]
  
  goalies_season_reg_list[[i]] <- goalies_reg_table
  
  if(length(tableNodes) < 6) {
    skaters_season_playoff_list[[i]] <- NA
    goalies_season_playoff_list[[i]] <- NA
  } else {
    skaters_playoff_table <- readHTMLTable(tableNodes[[5]],
                                           header=TRUE,
                                           stringsAsFactors=FALSE,
                                           colClasses=c("integer",
                                                        rep("character", 2),
                                                        rep("integer", 16),
                                                        "numeric",
                                                        "integer",
                                                        "character",
                                                        rep("numeric", 2)))
    names(skaters_playoff_table) <- skaters_playoff_colnames
    skaters_playoff_table$team_short <- input_set[i, "team_short"]
    skaters_playoff_table$season <- input_set[i, "season"]
    
    skaters_season_playoff_list[[i]] <- skaters_playoff_table
    
    goalies_playoff_table <- readHTMLTable(tableNodes[[6]],
                                           header=TRUE,
                                           stringsAsFactors=FALSE,
                                           colClasses=c("integer",
                                                        rep("character", 2),
                                                        rep("integer", 8),
                                                        rep("numeric", 2),
                                                        rep("integer",2)))
    
    names(goalies_playoff_table) <- goalies_playoff_colnames
    goalies_playoff_table$team_short <- input_set[i, "team_short"]
    goalies_playoff_table$season <- input_set[i, "season"]
    
    goalies_season_playoff_list[[i]] <- goalies_playoff_table
  }
}

# combine individual data frames into one data frame using rbind.fill
skaters_season_reg_overall <- rbind.fill(skaters_season_reg_list[!is.na(skaters_season_reg_list)])
save(skaters_season_reg_overall, file="skaters_season_reg_overall.RData")

skaters_season_playoff_overall <- rbind.fill(skaters_season_playoff_list[!is.na(skaters_season_playoff_list)])
save(skaters_season_playoff_overall, file="skaters_season_playoff_overall.RData")

goalies_season_reg_overall <- rbind.fill(goalies_season_reg_list[!is.na(goalies_season_reg_list)])
save(goalies_season_reg_overall, file="goalies_season_reg_overall.RData")

goalies_season_playoff_overall <- rbind.fill(goalies_season_playoff_list[!is.na(goalies_season_playoff_list)])
save(goalies_season_playoff_overall, file="goalies_season_playoff_overall.RData")

#------------------------------------------------------------------------------
# set up NHL index data set to grab season skater and goalie statistics
# aggregated across all teams
#------------------------------------------------------------------------------

index_url <- "http://www.hockey-reference.com/leagues/"
season_index_input <- readHTMLTable(index_url, header=TRUE, stringsAsFactors=FALSE)[[1]]
names(season_index_input) <- tolower(names(season_index_input))

season_index_tmp <- readHTMLTable(index_url, elFun = hrefFun, header=FALSE, stringsAsFactors=FALSE)[[1]]

season_index_input$url_summary <- paste0(base.url, season_index_tmp[,1])
season_index_input$url_schedule <- str_replace_all(season_index_input$url_summary, "\\.html", "\\_games\\.html")
season_index_input$url_skaters <- str_replace_all(season_index_input$url_summary, "\\.html", "\\_skaters\\.html")
season_index_input$url_goalies <- str_replace_all(season_index_input$url_summary, "\\.html", "\\_goalies\\.html")

# remove 2004-05 season since it was cancelled
season_index_input <- subset(season_index_input, subset=(season != "2004-05" & lg == "NHL"))

#------------------------------------------------------------------------------
# process yearly NHL schedule results
#------------------------------------------------------------------------------

schedule_results_names <- c("date",
                            "team_away",
                            "goals_away",
                            "team_home",
                            "goals_home",
                            "outcome_type",
                            "notes")

reg_results_list <- playoff_results_list <- vector("list", nrow(season_index_input))


for(i in 1:nrow(season_index_input)) {
  
  #browser()
  message(paste("importing schedule results for", season_index_input[i, "season"]))
  
  # import tables
  schedule_tables <- readHTMLTable(season_index_input[i, "url_schedule"], 
                                   header=TRUE, 
                                   stringsAsFactors=FALSE,
                                   colClasses=c(rep("character", 2),
                                                "integer",
                                                "character",
                                                "integer",
                                                rep("character", 2)))
  
  reg_table <- schedule_tables[[1]]
  
  # force to only have first 9 columns, since 2013-14 table has column for tickets at the end
  reg_table <- reg_table[, 1:7]
  names(reg_table) <- schedule_results_names
  reg_table$date_clean <- strptime(reg_table$date, "%a, %b %d, %Y")
  reg_table$season <- season_index_input[i, "season"]
  reg_table$game_number <- 1:nrow(reg_table)
  reg_table$game_id <- paste(reg_table$season, reg_table$game_number, sep="-")
  
  reg_results_list[[i]] <- reg_table
    
  # if only one table returned, then skip playoff input
  if(length(schedule_tables) < 2) {
    playoff_results_list[[i]] <- NA
  } else {
    playoff_table <- schedule_tables[[2]]
    names(playoff_table) <- schedule_results_names
    playoff_table$date_clean <- strptime(playoff_table$date, "%a, %b %d, %Y")
    playoff_table$season <- season_index_input[i, "season"]
    playoff_table$game_number <- 1:nrow(playoff_table)
    playoff_table$game_id <- paste(playoff_table$season, playoff_table$game_number, sep="-")
    
    playoff_results_list[[i]] <- playoff_table
  }
}

# combine individual data frames into one data frame using rbind.fill
reg_overall <- rbind.fill(reg_results_list[!is.na(reg_results_list)])
save(reg_overall, file="reg_overall.RData")

playoff_overall <- rbind.fill(playoff_results_list[!is.na(playoff_results_list)])
save(playoff_overall, file="playoff_overall.RData")
