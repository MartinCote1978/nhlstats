\name{ScoringSC}
\alias{ScoringSC}
\docType{data}
\title{
ScoringSC table
}
\description{
Table with scoring statistics from Stanley Cup Finals from 1917-18 through
1925-26.
}
\usage{data(ScoringSC)}
\format{
  A data frame with 284 observations on the following 10 variables.
  \describe{
    \item{\code{playerID}}{Player ID code}
    \item{\code{year}}{Year (2005-06 listed as "2005")}
    \item{\code{tmID}}{Team ID}
    \item{\code{lgID}}{League ID}
    \item{\code{pos}}{Position}
    \item{\code{GP}}{Games played}
    \item{\code{G}}{Goals}
    \item{\code{A}}{Assists}
    \item{\code{Pts}}{Points}
    \item{\code{PIM}}{Penalty minutes}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(ScoringSC)
## maybe str(ScoringSC) ; plot(ScoringSC) ...
}
\keyword{datasets}
