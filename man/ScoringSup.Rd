\name{ScoringSup}
\alias{ScoringSup}
\docType{data}
\title{
ScoringSup Table
}
\description{
Table with power play and shorthanded assists records for players with 
multiple stints during particular seasons.  The corresponding \code{playerID} 
and \code{year} records in the \code{Scoring} table have missing values for
either of these statistics.
}
\usage{data(ScoringSup)}
\format{
  A data frame with 137 observations on the following 4 variables.
  \describe{
    \item{\code{playerID}}{Player ID}
    \item{\code{year}}{Year (2005-06 listed as "2005")}
    \item{\code{PPA}}{Power play assists}
    \item{\code{SHA}}{Shorthanded assists}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(ScoringSup)
data(Scoring)

# compute total PPA and SHA from Scoring table by player and season
# from Scoring table for players represented in ScoringSup table
require(plyr)


scoring_sup <- ddply(subset(Scoring, subset=(playerID \%in\% unique(ScoringSup$playerID))), 
                     c("playerID", "year"), 
                     summarize,
                     PPA = sum(PPA),
                     SHA = sum(SHA))

scoring_sup <- merge(scoring_sup,
                     ScoringSup,
                     by = c("playerID", "year", "PPA", "SHA"),
                     all.x=TRUE)

head(scoring_sup)
}
\keyword{datasets}
