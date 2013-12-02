\name{HOF}
\alias{HOF}
\docType{data}
\title{
HOF table
}
\description{
Records of Hockey Hall of Fame inductees
}
\usage{data(HOF)}
\format{
  A data frame with 370 observations on the following 4 variables.
  \describe{
    \item{\code{year}}{Year elected}
    \item{\code{hofID}}{Hall of Fame ID}
    \item{\code{name}}{Name of inductee}
    \item{\code{category}}{Category of inductee (\code{Builder} \code{Player}, or \code{Referee/Linesman})}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(HOF)

# summarize number of inductees by category
library(plyr)

ddply(HOF, .(category), summarize,
      n_inductees = length(hofID))
}
\keyword{datasets}
