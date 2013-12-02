\name{abbrev}
\alias{abbrev}
\docType{data}
\title{
Hockey Databank variable labels
}
\description{
This data frame provides descriptive labels for the variables in the 
\code{\link{Teams}} and \code{\link{SeriesPost}} data frames.  They are useful
for plots and other output using the \code{Label} function.
}
\usage{data(abbrev)}
\format{
  A data frame with observations on the following 3 variables.
  \describe{
    \item{\code{Type}}{variable name}
    \item{\code{Code}}{abbreviation code for variable}
    \item{\code{Fullname}}{full label for variable}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(abbrev)
str(abbrev)
}
\keyword{datasets}
