\name{Goalies}
\alias{Goalies}
\docType{data}
\title{
Goalies Table
}
\description{
Table with goalie regular season and playoff statistics
}
\usage{data(Goalies)}
\format{
  A data frame with 4278 observations on the following 23 variables.
  \describe{
    \item{\code{playerID}}{Goalie ID}
    \item{\code{year}}{Year (2005-06 is listed as \code{2005})}
    \item{\code{stint}}{Stint (order of appearance in a season)}
    \item{\code{tmID}}{Team ID}
    \item{\code{lgID}}{League ID}
    \item{\code{GP}}{Games played}
    \item{\code{Min}}{Minutes}
    \item{\code{W}}{Wins}
    \item{\code{L}}{Losses}
    \item{\code{T.OL}}{Ties/overtime losses}
    \item{\code{ENG}}{Empty net goals}
    \item{\code{SHO}}{Shutouts}
    \item{\code{GA}}{Goals against}
    \item{\code{SA}}{Shots against}
    \item{\code{PostGP}}{Postseason games played}
    \item{\code{PostMin}}{Postseason minutes}
    \item{\code{PostW}}{Postseason wins}
    \item{\code{PostL}}{Postseason losses}
    \item{\code{PostT}}{Posteason ties}
    \item{\code{PostENG}}{Postseason empty net goals}
    \item{\code{PostSHO}}{Postseason shutouts}
    \item{\code{PostGA}}{Postseason goals against}
    \item{\code{PostSA}}{Postseason shots against}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
## add example using derived stats calculator
}
\keyword{datasets}
