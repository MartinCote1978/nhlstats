\name{TeamSplits}
\alias{TeamSplits}
\docType{data}
\title{
TeamSplits Table
}
\description{
Table with team home/road and monthly splits during regular season.
}
\usage{data(TeamSplits)}
\format{
  A data frame with 1519 observations on the following 43 variables.
  \describe{
    \item{\code{year}}{Year (2005-06 listed as "2005")}
    \item{\code{lgID}}{League ID}
    \item{\code{tmID}}{Team ID}
    \item{\code{hW}}{Home wins}
    \item{\code{hL}}{Home losses}
    \item{\code{hT}}{Home ties}
    \item{\code{hOTL}}{Home overtime losses}
    \item{\code{rW}}{Road wins}
    \item{\code{rL}}{Road losses}
    \item{\code{rT}}{Road ties}
    \item{\code{rOTL}}{Road overtime losses}
    \item{\code{SepW}}{September wins}
    \item{\code{SepL}}{September losses}
    \item{\code{SepT}}{September ties}
    \item{\code{SepOL}}{September overtime losses}
    \item{\code{OctW}}{October wins}
    \item{\code{OctL}}{October losses}
    \item{\code{OctT}}{October ties}
    \item{\code{OctOL}}{October overtime losses}
    \item{\code{NovW}}{November wins}
    \item{\code{NovL}}{November losses}
    \item{\code{NovT}}{November ties}
    \item{\code{NovOL}}{November overtime losses}
    \item{\code{DecW}}{December wins}
    \item{\code{DecL}}{December losses}
    \item{\code{DecT}}{December ties}
    \item{\code{DecOL}}{December overtime losses}
    \item{\code{JanW}}{January wins}
    \item{\code{JanL}}{January losses}
    \item{\code{JanT}}{January ties}
    \item{\code{JanOL}}{January overtime losses}
    \item{\code{FebW}}{February wins}
    \item{\code{FebL}}{February losses}
    \item{\code{FebT}}{February ties}
    \item{\code{FebOL}}{February overtime losses}
    \item{\code{MarW}}{March wins}
    \item{\code{MarL}}{March losses}
    \item{\code{MarT}}{March ties}
    \item{\code{MarOL}}{March overtime losses}
    \item{\code{AprW}}{April wins}
    \item{\code{AprL}}{April losses}
    \item{\code{AprT}}{April ties}
    \item{\code{AprOL}}{April overtime losses}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(TeamSplits)

# which team had the most home wins since the 2004-05 NHL lockout?

home_sub <- subset(TeamSplits,
                   select=c("year", "tmID", "hW", "hL", "hOTL"),
                   subset=(year >= 2005))

home_best <- do.call("rbind", 
                     by(home_sub, home_sub$year, function(x) x[which.max(x$hW),]))

home_best
}
\keyword{datasets}
