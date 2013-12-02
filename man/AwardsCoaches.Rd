\name{AwardsCoaches}
\alias{AwardsCoaches}
\docType{data}
\title{
AwardsCoaches table
}
\description{
Award information for coaches awards
}
\usage{data(AwardsCoaches)}
\format{
  A data frame with observations on the following 5 variables.
  \describe{
    \item{\code{coachID}}{Coach ID code}
    \item{\code{award}}{Name of award won}
    \item{\code{year}}{Year (2005-06 is listed as \code{2005})}
    \item{\code{lgID}}{League ID}
    \item{\code{note}}{Notes about the award}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
# Number of Jack Adams awards received by coach in NHL
data(AwardsCoaches)
ja_subset <- subset(AwardsCoaches, award=="Jack Adams")
with(ja_subset, table(coachID, award))
}
\keyword{datasets}
