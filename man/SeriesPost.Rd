\name{SeriesPost}
\alias{SeriesPost}
\docType{data}
\title{
SeriesPost Table
}
\description{
Table with postseason series results grouped by year and round
}
\usage{data(SeriesPost)}
\format{
  A data frame with 832 observations on the following 13 variables.
  \describe{
    \item{\code{year}}{Year (1920-21 listed as "1920")}
    \item{\code{round}}{Round (see \code{\link{abbrev}} table for more details)}
    \item{\code{series}}{Letter designated to identify a particular series
          pairing within a round}
    \item{\code{tmIDWinner}}{Team ID of winner}
    \item{\code{lgIDWinner}}{League ID of winner}
    \item{\code{tmIDLoser}}{Team ID of loser}
    \item{\code{lgIDLoser}}{League ID of loser}
    \item{\code{W}}{Wins for winner}
    \item{\code{L}}{Losses for winner}
    \item{\code{T}}{Ties}
    \item{\code{GoalsWinner}}{Goals scored for winner}
    \item{\code{GoalsLoser}}{Goals scored for loser}
    \item{\code{note}}{Notes on series (\code{EX} = exhibition, 
          \code{ND} = no decision), \code{TG} = Total-goals series}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(SeriesPost)

# How many total goals were scored in the first round of playoffs since 
# the 2004-2005 NHL lockout?

require(plyr)
series_sub <- subset(SeriesPost,
                     subset=(round == "CQF" & year >= 2005))

series_sub <- mutate(series_sub,
                     total_goals = GoalsWinner + GoalsLoser)
                     
ddply(series_sub, .(year), summarize,
      total_goals = sum(total_goals))
}
\keyword{datasets}
