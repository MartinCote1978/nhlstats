\name{GoaliesSC}
\alias{GoaliesSC}
\docType{data}
\title{
GoaliesSC table
}
\description{
Table with goalie statistics during Stanley Cup finals from 1917-18 through 
1925-26
}
\usage{data(GoaliesSC)}
\format{
  A data frame with 31 observations on the following 11 variables.
  \describe{
    \item{\code{playerID}}{Goalie ID}
    \item{\code{year}}{Year (2005-06 is listed as \code{2005})}
    \item{\code{tmID}}{Team ID}
    \item{\code{lgID}}{League ID}
    \item{\code{GP}}{Games played}
    \item{\code{Min}}{Minutes}
    \item{\code{W}}{Wins}
    \item{\code{L}}{Losses}
    \item{\code{T}}{Ties}
    \item{\code{SHO}}{Shutouts}
    \item{\code{GA}}{Goals against}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(GoaliesSC)
## maybe str(GoaliesSC) ; plot(GoaliesSC) ...
}
\keyword{datasets}
