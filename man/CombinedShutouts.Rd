\name{CombinedShutouts}
\alias{CombinedShutouts}
\docType{data}
\title{
CombinedShutouts table
}
\description{
Table with records for each game in which a team's goalies combined for a 
regular season or postseason shutout.
}
\usage{data(CombinedShutouts)}
\format{
  A data frame with 54 observations on the following 8 variables.
  \describe{
    \item{\code{year}}{Year (actual calendar year, not a season ID)}
    \item{\code{month}}{Month}
    \item{\code{date}}{Day of month}
    \item{\code{tmID}}{Team ID}
    \item{\code{oppID}}{Team ID of opponent}
    \item{\code{R.P}}{Season type indicator (\code{R} for regular season, 
                      \code{P} for postseason)}
    \item{\code{IDgoalie1}}{ID of first goalie}
    \item{\code{IDgoalie2}}{ID of second goalie}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(CombinedShutouts)

# find all postseason games with shutouts using two goalies
subset(CombinedShutouts,
       subset=(R.P == "P"))
}
\keyword{datasets}
