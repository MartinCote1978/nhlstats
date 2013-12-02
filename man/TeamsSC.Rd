\name{TeamsSC}
\alias{TeamsSC}
\docType{data}
\title{
TeamsSC table
}
\description{
Table with team statistics from Stanley Cup finals from 1917-18 through 
1925-26.
}
\usage{data(TeamsSC)}
\format{
  A data frame with 30 observations on the following 10 variables.
  \describe{
    \item{\code{year}}{Year (2005-06 listed as "2005")}
    \item{\code{lgID}}{League ID}
    \item{\code{tmID}}{Team ID}
    \item{\code{G}}{Games}
    \item{\code{W}}{Wins}
    \item{\code{L}}{Losses}
    \item{\code{T}}{Ties}
    \item{\code{GF}}{Goals for}
    \item{\code{GA}}{Goals against}
    \item{\code{PIM}}{Penalty minutes}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(TeamsSC)
## maybe str(TeamsSC) ; plot(TeamsSC) ...
}
\keyword{datasets}
