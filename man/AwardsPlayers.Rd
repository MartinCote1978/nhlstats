\name{AwardsPlayers}
\alias{AwardsPlayers}
\docType{data}
\title{
AwardsPlayers Table
}
\description{
Award information for players awards.
}
\usage{data(AwardsPlayers)}
\format{
  A data frame with observations on the following 6 variables.
  \describe{
    \item{\code{playerID}}{Player ID code}
    \item{\code{award}}{Name of award won}
    \item{\code{year}}{Year (2005-06 is listed as \code{2005})}
    \item{\code{lgID}}{League ID}
    \item{\code{note}}{Notes about the award}
    \item{\code{pos}}{Position (for all-star teams)}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(AwardsPlayers)

# which awards have been given and how many?
with(AwardsPlayers, table(award))

# which players have won at least 5 Art Ross awards?
art_ross <- subset(AwardsPlayers, award=="Art Ross")
ar_count <- table(art_ross$playerID)
sort(ar_count[ar_count >= 5], decreasing=TRUE)
}
\keyword{datasets}
