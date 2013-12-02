\name{TeamVsTeam}
\alias{TeamVsTeam}
\docType{data}
\title{
TeamVsTeam Table
}
\description{
Table with head-to-head team statistics
}
\usage{data(TeamVsTeam)}
\format{
  A data frame with 25602 observations on the following 8 variables.
  \describe{
    \item{\code{year}}{Year (2005-06 listed as "2005")}
    \item{\code{lgID}}{League ID}
    \item{\code{tmID}}{Team ID}
    \item{\code{oppID}}{Opponent ID}
    \item{\code{W}}{Wins}
    \item{\code{L}}{Losses}
    \item{\code{T}}{Ties}
    \item{\code{OTL}}{Overtime losses}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(TeamVsTeam)
## maybe str(TeamVsTeam) ; plot(TeamVsTeam) ...
}
\keyword{datasets}
