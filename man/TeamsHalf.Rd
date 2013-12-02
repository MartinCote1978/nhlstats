\name{TeamsHalf}
\alias{TeamsHalf}
\docType{data}
\title{
TeamsHalf Table
}
\description{
Team statistics grouped by first and second half of season for seasons
1917-18 through 1920-21.
}
\usage{data(TeamsHalf)}
\format{
  A data frame with 41 observations on the following 11 variables.
  \describe{
    \item{\code{year}}{Year (2005-06 listed as "2005")}
    \item{\code{lgID}}{League ID}
    \item{\code{tmID}}{Team ID}
    \item{\code{half}}{First or second half of season}
    \item{\code{rank}}{Final standing for particular season half}
    \item{\code{G}}{Games}
    \item{\code{W}}{Wins}
    \item{\code{L}}{Losses}
    \item{\code{T}}{Ties}
    \item{\code{GF}}{Goals for}
    \item{\code{GA}}{Goals against}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(TeamsHalf)
## maybe str(TeamsHalf) ; plot(TeamsHalf) ...
}
\keyword{datasets}
