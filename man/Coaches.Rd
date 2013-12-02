\name{Coaches}
\alias{Coaches}
\docType{data}
\title{
Coaches Table
}
\description{
Coaches table: information about invidual team coaches, teams they coached and
some basic statistics for those teams in each season.
}
\usage{data(Coaches_beta)}
\format{
  A data frame with 1812 observations on the following 15 variables.
  \describe{
    \item{\code{coachID}}{Coach ID}
    \item{\code{year}}{Year (2005-06 is listed as \code{2005})}
    \item{\code{tmID}}{Team ID}
    \item{\code{lgID}}{League ID}
    \item{\code{stint}}{Coaching order (1 for first coach, 2 for second, etc.)}
    \item{\code{notes}}{Miscellaneous comments}
    \item{\code{g}}{Games}
    \item{\code{w}}{Wins}
    \item{\code{l}}{Losses}
    \item{\code{t}}{Ties}
    \item{\code{otl}}{Overtime losses}
    \item{\code{postg}}{Postseason games}
    \item{\code{postw}}{Postseason wins}
    \item{\code{postl}}{Postseason losses}
    \item{\code{postt}}{Postseason ties}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(Coaches)
data(Master)
# career summaries by coach

library(plyr)
coach_totals <- ddply(Coaches, .(coachID), summarize,
                      nyear = length(unique(year)),
                      yearBegin = min(year),
                      yearEnd = max(year),
                      nTeams = length(unique(tmID)),
                      w = sum(w),
                      l = sum(l),
                      t = sum(t),
                      otl = sum(otl))

coach_totals <- merge(coach_totals,
                      subset(Master, 
                             select = c("coachID", "lastName", "firstName"),
                             subset = (!is.na(coachID))),
                      by = "coachID")
}
\keyword{datasets}
