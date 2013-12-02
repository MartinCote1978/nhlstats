\name{Scoring}
\alias{Scoring}
\docType{data}
\title{
Scoring table
}
\description{
Table with regular season and postseason scoring statistics 
}
\usage{data(Scoring)}
\format{
  A data frame with 45967 observations on the following 31 variables.
  \describe{
    \item{\code{playerID}}{Player ID code}
    \item{\code{year}}{Year (2005-06 listed as "2005")}
    \item{\code{stint}}{player's stint (order of appearances within a season)}
    \item{\code{tmID}}{Team ID}
    \item{\code{lgID}}{League ID}
    \item{\code{pos}}{Position}
    \item{\code{GP}}{Games played}
    \item{\code{G}}{Goals}
    \item{\code{A}}{Assists}
    \item{\code{Pts}}{Points}
    \item{\code{PIM}}{Penalty minutes}
    \item{\code{PlusMinus}}{Plus / minus}
    \item{\code{PPG}}{Power play goals}
    \item{\code{PPA}}{Power play assists}
    \item{\code{SHG}}{Shorthanded goals}
    \item{\code{SHA}}{Shorthanded assists}
    \item{\code{GWG}}{Game-winning goals}
    \item{\code{GTG}}{Game-tying goals}
    \item{\code{SOG}}{Shots on goal}
    \item{\code{PostGP}}{Postseason games played}
    \item{\code{PostG}}{Postseason goals}
    \item{\code{PostA}}{Postseason assists}
    \item{\code{PostPts}}{Postseason points}
    \item{\code{PostPIM}}{Postseason penalty minutes}
    \item{\code{PostPlusMinus}}{Postseason plus / minus}
    \item{\code{PostPPG}}{Postseason power play goals}
    \item{\code{PostPPA}}{Postseason power play assists}
    \item{\code{PostSHG}}{Postseason shorthanded goals}
    \item{\code{PostSHA}}{Postseason shorthanded assists}
    \item{\code{PostGWG}}{Postseason game-winning goals}
    \item{\code{PostSOG}}{Postseason shots on goals}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(Scoring)
## maybe str(Scoring) ; plot(Scoring) ...
}
\keyword{datasets}
