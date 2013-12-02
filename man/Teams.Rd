\name{Teams}
\alias{Teams}
\docType{data}
\title{
Teams Table
}
\description{
Table with franchise statistics by season
}
\usage{data(Teams)}
\format{
  A data frame with 1519 observations on the following 27 variables.
  \describe{
    \item{\code{year}}{Year (2005-06 listed as "2005")}
    \item{\code{lgID}}{League ID}
    \item{\code{tmID}}{Team ID}
    \item{\code{franchID}}{Franchise ID}
    \item{\code{confID}}{Conference ID (see \code{\link{abbrev}} table for more details)}
    \item{\code{divID}}{Division ID (see \code{\link{abbrev}} table for more details)}
    \item{\code{rank}}{Final standing}
    \item{\code{playoff}}{Playoff result (see \code{\link{abbrev}} table for more details)}
    \item{\code{G}}{Games}
    \item{\code{W}}{Wins}
    \item{\code{L}}{Losses}
    \item{\code{T}}{Ties}
    \item{\code{OTL}}{Overtime losses}
    \item{\code{Pts}}{Points}
    \item{\code{SoW}}{Shootout wins}
    \item{\code{SoL}}{Shootout losses}
    \item{\code{GF}}{Goals for}
    \item{\code{GA}}{Goals against}
    \item{\code{name}}{Full team name}
    \item{\code{PIM}}{Penalty minutes}
    \item{\code{BenchMinor}}{Bench minor minutes}
    \item{\code{PPG}}{Power play goals for}
    \item{\code{PPC}}{Power play chances}
    \item{\code{SHA}}{Shorthanded goals against}
    \item{\code{PKG}}{Power play goals against}
    \item{\code{PKC}}{Penalty kill chances}
    \item{\code{SHF}}{Shorthanded goals for}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(Teams)
## maybe str(Teams) ; plot(Teams) ...
}
\keyword{datasets}
