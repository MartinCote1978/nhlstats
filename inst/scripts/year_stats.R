# compress mightily on save
options(save.defaults=list(compress="bzip2", compression_level=9))

library(RCurl)
library(stringr)
library(XML)
library(plyr)
library(lubridate)

url.checker <- function(url) {
  fn <- try(suppressWarnings(readLines(con <- url(url))), silent=TRUE)
  close(con)
  !inherits(fn, "try-error")
}

hrefFun <- function(x){
  xpathSApply(x,'./a',xmlAttrs)  
}

base.url <- "http://www.hockey-reference.com"

#------------------------------------------------------------------------------
# set up NHL index data set to grab schedule, reg season skater and goalie statistics
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
# obtain regular season statistics for skaters and goalies by season
#-------------------------------------------------------------------------------

skaters_reg_colnames <- c("rk",
                          "player",
                          "age",
                          "team_short",
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
                          "shooting_pct",
                          "time_on_ice",
                          "avg_time_on_ice")

skaters_reg_colclasses <- c("integer",
                            "character",
                            "integer",
                            rep("character", 2),
                            rep("integer", 15),
                            "numeric",
                            "integer",
                            "character")

goalies_reg_colnames <- c("rk",
                          "player",
                          "age",
                          "team_short",
                          "gp",
                          "w",
                          "l",
                          "tot",
                          "ga",
                          "sa",
                          "sv",
                          "sv_pct",
                          "gaa",
                          "so",
                          "min",
                          "ga_pct",
                          "gsaa",
                          "goals",
                          "assists",
                          "points",
                          "pim")

goalies_reg_colclasses <- c("integer",
                            "character",
                            "integer",
                            "character",
                            rep("integer", 7),
                            rep("numeric", 2),
                            rep("integer", 2),
                            rep("numeric", 2),
                            rep("integer", 4))

skaters_season_reg_list <- vector("list", nrow(season_index_input)) 
goalies_season_reg_list <- vector("list", nrow(season_index_input)) 

for(i in 1:nrow(season_index_input)) {
  #browser()
  message(paste("importing skater and goalie stats for", season_index_input[i, "season"]))
  
  skaters_table <- readHTMLTable(season_index_input[i, "url_skaters"], 
                                   header=FALSE, 
                                   stringsAsFactors=FALSE,
                                   colClasses=skaters_reg_colclasses)[[1]]
  
  names(skaters_table) <- skaters_reg_colnames
  
  # remove empty rows from table
  skaters_table <- skaters_table[!is.na(skaters_table$rk),]
  skaters_table$season <- season_index_input[i, "season"]
  skaters_season_reg_list[[i]] <- skaters_table
  
  goalies_table <- readHTMLTable(season_index_input[i, "url_goalies"], 
                                 header=FALSE, 
                                 stringsAsFactors=FALSE,
                                 colClasses=goalies_reg_colclasses)[[1]]
  
  names(goalies_table) <- goalies_reg_colnames
  
  # remove empty rows from table
  goalies_table <- goalies_table[!is.na(goalies_table$rk),]
  goalies_table$season <- season_index_input[i, "season"]
  goalies_season_reg_list[[i]] <- goalies_table
}

# combine individual data frames into one data frame using rbind.fill
skaters_season_reg_overall <- rbind.fill(skaters_season_reg_list[!is.na(skaters_season_reg_list)])
save(skaters_season_reg_overall, file="skaters_season_reg_overall.RData")

goalies_season_reg_overall <- rbind.fill(goalies_season_reg_list[!is.na(goalies_season_reg_list)])
save(goalies_season_reg_overall, file="goalies_season_reg_overall.RData")





#------------------------------------------------------------------------------
# obtain playoff season statistics for skaters and goalies by season
#-------------------------------------------------------------------------------

load("team_summary_data.RData")
input_set <- team_summary_data[, c("team_short", "team_long", "season", "playoff_ind", "url_season")]

# need to skip current season (find a way to standardize this)
#input_set <- subset(input_set, season != "2013-14")

skaters_playoff_colnames <- c("rk",
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
                              "avg_time_on_ice")

skaters_playoff_colclasses <- c("integer",
                                rep("character", 2),
                                rep("integer", 16),
                                "numeric",
                                "integer",
                                "character",
                                rep("numeric", 2))

goalies_playoff_colnames <- c("rk",
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
                              "min")

goalies_playoff_colclasses <- c("integer",
                                rep("character", 2),
                                rep("integer", 8),
                                rep("numeric", 2),
                                rep("integer",2))

skaters_season_playoff_list <- vector("list", nrow(input_set)) 
goalies_season_playoff_list <- vector("list", nrow(input_set)) 


for(i in 1:nrow(input_set)) {
  
  #browser()
  message(paste("importing playoff skater and goalie statistics for", input_set[i, "team_short"], input_set[i, "season"]))
  
  # alternative way to read in tables
  # table order: team_stats, roster, skaters, goalies, skaters_playoffs, goalies_playoffs
  doc = htmlParse(input_set[i, c("url_season")])
  tableNodes = getNodeSet(doc, "//table")
  
  if(!input_set[i, "playoff_ind"]) {
    skaters_season_playoff_list[[i]] <- NA
    goalies_season_playoff_list[[i]] <- NA
  } else {
    skaters_playoff_table <- readHTMLTable(tableNodes[[5]],
                                           header=FALSE,
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
    
    # logic to check whether there are more columns than expected in goalie table
    n.cols <- ncol(readHTMLTable(tableNodes[[6]], header=FALSE))
    
    if(n.cols > length(goalies_playoff_colnames)) {
      n.cols.diff <- n.cols - length(goalies_playoff_colnames)
      goalies_playoff_table <- readHTMLTable(tableNodes[[6]],
                                             header=FALSE,
                                             stringsAsFactors=FALSE,
                                             colClasses=c(goalies_playoff_colclasses,
                                                          rep("character", n.cols.diff)))
      
      goalies_playoff_table <- goalies_playoff_table[, 1:length(goalies_playoff_colnames)]
      
      
    } else {
      goalies_playoff_table <- readHTMLTable(tableNodes[[6]],
                                             header=FALSE,
                                             stringsAsFactors=FALSE,
                                             colClasses=goalies_playoff_colclasses)
    }
    
    names(goalies_playoff_table) <- goalies_playoff_colnames
    goalies_playoff_table$team_short <- input_set[i, "team_short"]
    goalies_playoff_table$season <- input_set[i, "season"]
    
    goalies_season_playoff_list[[i]] <- goalies_playoff_table
  }
}


# check for missing position values
tmp <- subset(skaters_season_playoff_overall,
              select=c("team_short", "season", "player", "pos"),
              subset=(pos==""))

tmp <- subset(skaters_season_reg_overall,
              select=c("team_short", "season", "player", "pos"),
              subset=(pos==""))

# combine individual data frames into one data frame using rbind.fill
skaters_season_playoff_overall <- rbind.fill(skaters_season_playoff_list[!is.na(skaters_season_playoff_list)])
save(skaters_season_playoff_overall, file="skaters_season_playoff_overall.RData")

goalies_season_playoff_overall <- rbind.fill(goalies_season_playoff_list[!is.na(goalies_season_playoff_list)])
save(goalies_season_playoff_overall, file="goalies_season_playoff_overall.RData")
