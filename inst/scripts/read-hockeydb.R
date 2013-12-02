#------------------------------------------------------------------------------
# File: read-hockeydb.R
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/
# Email: theRcast@gmail.com
# Purpose: Import and process hockey database files for nhlstats package
#------------------------------------------------------------------------------

library(Rd2roxygen)

setwd("~/rpodcast_code/nhlstats")

# directory where the .csv files will be extracted
indir <- "~/scratch/hockeydb2013/"

# directory where the .RData files will be created
outdir <- "data"

# directory for the temporary man files
dir.create("man_tmp")
man_tmp <- "man_tmp"

# directory to store the R oxygen version of documentation files
man_final <- "R"

# download the .zip archive containing the .csv files from http://sports.groups.yahoo.com/group/hockey-databank/files/
# need to be a member of the yahoo group to download files
#2012-2013
#zipfile <- "hdb-2013-07-07.zip"
#unzip(file.path(indir, "hdb-2013-07-07.zip" ), exdir=indir)

#2011-2012 
# zipfile <- "http://f1.grp.yahoofs.com/v1/kJmxURGXN7S1T-hXGeGW98kgBcNbHZwGQ8Dbb5GSMSgJIki2AWSXx2DKlkWJzAgP6DvlHsHAg5eQfFJA-qZ-M1HCGgi3E-QNCYFL676U1LOs/hdb-2012-06-23.zip"
#download.file(zipfile, file.path(indir, "hdb-2012-06-23.zip"))
#unzip(file.path(indir, "hdb-2012-06-23.zip" ), exdir=indir)

# for 2011-2012: Download the fixes archive: contains revised Master.csv, HOF.csv, Scoring.csv, Goalies.csv
# see forum post at http://sports.groups.yahoo.com/group/hockey-databank/message/191
#zipfile <- "http://f1.grp.yahoofs.com/v1/SKWxUSOi2aOvPCe2jlVN5WdPJh3LfZz7tHSj6MCgn8a6f0wwQhDxmSE98Tl2dP7MJGTozXZSwq6KrMwWY9GYez3ONahDKO0y7PKyhr6SK3f6/fixes20120718.zip"
#download.file(zipfile, file.path(indir, "fixes20120718.zip"))
#unzip(file.path(indir, "fixes20120718.zip" ), exdir=indir)

# Read the Hockey database .csv files and create .RData and .Rd files

(files <- list.files(path=indir,pattern="*.csv"))
for (i in 1:length(files)) {
  inp <- read.csv(file=file.path(indir, files[i]), header=TRUE, stringsAsFactors=FALSE, na.strings="")
  cat("Read:", files[i], "\trows: ", nrow(inp), " cols: ", ncol(inp), "\n")
  
  # make a few variables factors
  if("lgID" %in% names(inp)) inp$lgID <- factor(inp$lgID)
  if("tmID" %in% names(inp)) inp$tmID <- factor(inp$tmID)
  if("category" %in% names(inp)) inp$category <- factor(inp$category)
  if("franchID" %in% names(inp)) inp$franchID <- factor(inp$franchID)
  if("lgIDwinner" %in% names(inp)) inp$lgIDwinner <- factor(inp$lgIDwinner)
  if("lgIDloser" %in% names(inp)) inp$lgIDloser <- factor(inp$lgIDloser)
  if("tmIDwinner" %in% names(inp)) inp$teamIDwinner <- factor(inp$tmIDwinner)
  if("tmIDloser" %in% names(inp)) inp$teamIDloser <- factor(inp$tmIDloser)
  
  cname <- name <- sub(".csv", "", files[i])
  
  # fix a few problems with column names
  if(cname == "Scoring") {
    message("hit Scoring")
    colnames(inp)[12] <- 'PlusMinus'
    colnames(inp)[25] <- 'PostPlusMinus'
  }
  
#   old_doc <- sub(".csv", ".Rd", files[i])
#   new_doc <- sub(".csv", ".R", files[i])
  
  assign( name, inp)
  
  # only run this once since I needed to manually edit the doc files afterwards
#   if(TRUE) {
#     browser()
#     promptData(inp,      filename=file.path(man_tmp, old_doc))
#     parse_and_save(path=file.path(man_tmp, old_doc), file=file.path(man_final, new_doc))
#   }
}

# compress mightily on save
options(save.defaults=list(compress="bzip2", compression_level=9))

save(AwardsCoaches,      file=file.path(outdir, "AwardsCoaches.RData"))     
save(AwardsPlayers,       file=file.path(outdir, "AwardsPlayers.RData"))
save(AwardsMisc,       file=file.path(outdir, "AwardsMisc.RData"))
save(Coaches,            file=file.path(outdir, "Coaches.RData"))          
save(CombinedShutouts,    file=file.path(outdir, "CombinedShutouts.RData"))
save(Goalies,             file=file.path(outdir, "Goalies.RData"))
save(GoaliesSC,           file=file.path(outdir, "GoaliesSC.RData"))
save(GoaliesShootout,           file=file.path(outdir, "GoaliesShootout.RData"))
save(HOF,                 file=file.path(outdir, "HOF.RData"))
save(Master,              file=file.path(outdir, "Master.RData"))
save(Scoring,             file=file.path(outdir, "Scoring.RData"))
save(ScoringSC,             file=file.path(outdir, "ScoringSC.RData"))
save(ScoringShootout,             file=file.path(outdir, "ScoringShootout.RData"))
save(ScoringSup,             file=file.path(outdir, "ScoringSup.RData"))
save(SeriesPost,          file=file.path(outdir, "SeriesPost.RData"))
save(TeamSplits,               file=file.path(outdir, "TeamSplits.RData"))   
save(TeamVsTeam,               file=file.path(outdir, "TeamVsTeam.RData"))   
save(Teams,               file=file.path(outdir, "Teams.RData"))        
save(TeamsHalf,           file=file.path(outdir, "TeamsHalf.RData"))
save(TeamsPost,           file=file.path(outdir, "TeamsPost.RData"))
save(TeamsSC,           file=file.path(outdir, "TeamsSC.RData"))
save(abbrev,           file=file.path(outdir, "abbrev.RData"))  


# only ran this once, since all .Rd files were extensively edited

if(FALSE) {
  promptData(AwardsCoaches,      filename=file.path("man", "AwardsCoaches.Rd"))     
  promptData(AwardsPlayers,       filename=file.path("man", "AwardsPlayers.Rd"))
  promptData(AwardsMisc,       filename=file.path("man", "AwardsMisc.Rd"))
  promptData(Coaches_beta,            filename=file.path("man", "Coaches.Rd"))          
  promptData(CombinedShutouts,    filename=file.path("man", "CombinedShutouts.Rd"))
  promptData(Goalies,             filename=file.path("man", "Goalies.Rd"))
  promptData(GoaliesSC,           filename=file.path("man", "GoaliesSC.Rd"))
  promptData(GoaliesShootout,           filename=file.path("man", "GoaliesShootout.Rd"))
  promptData(HOF,                 filename=file.path("man", "HOF.Rd"))
  promptData(Master,              filename=file.path("man", "Master.Rd"))
  promptData(Scoring,             filename=file.path("man", "Scoring.Rd"))
  promptData(ScoringSC,             filename=file.path("man", "ScoringSC.Rd"))
  promptData(ScoringShootout,             filename=file.path("man", "ScoringShootout.Rd"))
  promptData(ScoringSup,             filename=file.path("man", "ScoringSup.Rd"))
  promptData(SeriesPost,          filename=file.path("man", "SeriesPost.Rd"))
  promptData(TeamSplits,               filename=file.path("man", "TeamSplits.Rd"))   
  promptData(TeamVsTeam,               filename=file.path("man", "TeamVsTeam.Rd"))   
  promptData(Teams,               filename=file.path("man", "Teams.Rd"))        
  promptData(TeamsHalf,           filename=file.path("man", "TeamsHalf.Rd"))
  promptData(TeamsPost,           filename=file.path("man", "TeamsPost.Rd"))
  promptData(TeamsSC,           filename=file.path("man", "TeamsSC.Rd"))
  promptData(abbrev,           filename=file.path("man", "abbrev.Rd"))
}
