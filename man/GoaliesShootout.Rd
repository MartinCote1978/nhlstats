\name{GoaliesShootout}
\alias{GoaliesShootout}
\docType{data}
\title{
GoaliesShootout table
}
\description{
Goalie statistics for NHL regular season shootouts 
}
\usage{data(GoaliesShootout)}
\format{
  A data frame with 480 observations on the following 8 variables.
  \describe{
    \item{\code{playerID}}{Goalie ID}
    \item{\code{year}}{Year (2005-06 is listed as \code{2005})}
    \item{\code{stint}}{Stint (order of appearance in a season)}
    \item{\code{tmID}}{Team ID}
    \item{\code{W}}{Wins}
    \item{\code{L}}{Losses}
    \item{\code{SA}}{Shots against}
    \item{\code{GA}}{Goals against}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(GoaliesShootout)
data(Master)

# which goalie has the most shootout wins?
library(plyr)
goaliesc_totals <- ddply(GoaliesShootout, .(playerID), summarize,
                         w = sum(W),
                         l = sum(L))

goaliesc_totals  <- merge(goaliesc_totals,
                          subset(Master, 
                                 select = c("playerID", "lastName", "firstName"),
                                 subset = (!is.na(playerID) & pos == "G")),
                          by = "playerID")

arrange(goaliesc_totals, desc(w))
}
\keyword{datasets}
