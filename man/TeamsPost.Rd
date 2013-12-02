\name{TeamsPost}
\alias{TeamsPost}
\docType{data}
\title{
TeamsPost Table
}
\description{
Table with postseason team statistics.
}
\usage{data(TeamsPost)}
\format{
  A data frame with 927 observations on the following 17 variables.
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
data(TeamsPost)
## maybe str(TeamsPost) ; plot(TeamsPost) ...
}
\keyword{datasets}
