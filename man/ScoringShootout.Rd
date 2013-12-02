\name{ScoringShootout}
\alias{ScoringShootout}
\docType{data}
\title{
ScoringShootout table
}
\description{
Scoring statistics for NHL regular season shootouts 
}
\usage{data(ScoringShootout)}
\format{
  A data frame with 2072 observations on the following 7 variables.
  \describe{
    \item{\code{playerID}}{Player ID}
    \item{\code{year}}{Year (2005-06 is listed as \code{2005})}
    \item{\code{stint}}{Stint (order of appearance in a season)}
    \item{\code{tmID}}{Team ID}
    \item{\code{S}}{Shots}
    \item{\code{G}}{Goals}
    \item{\code{GDG}}{Game-deciding goals}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(ScoringShootout)
data(Master)

# which player has the most shootout goals?
library(plyr)
scoringsc_totals <- ddply(ScoringShootout, .(playerID), summarize,
                         goals = sum(G))

scoringsc_totals  <- merge(scoringsc_totals,
                          subset(Master, 
                                 select = c("playerID", "lastName", "firstName"),
                                 subset = (!is.na(playerID) & pos != "G")),
                          by = "playerID")

arrange(scoringsc_totals, desc(goals))[1:5,]
}
\keyword{datasets}
