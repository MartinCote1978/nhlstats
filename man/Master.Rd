\name{Master}
\alias{Master}
\docType{data}
\title{
Master table
}
\description{
Master table - Player names, DOB, and biographical info.  This file is to be used to get details
about players listed in the \code{\link{Scoring}}, \code{\link{Goalies}}, and other files
where players are identified only by \code{playerID}.
}
\usage{data(Master)}
\format{
  A data frame with 7763 observations on the following 31 variables.
  \describe{
    \item{\code{playerID}}{A unique code asssigned to each player.  The \code{playerID} links
                 the data in this file with records on players in the other files.}
    \item{\code{coachID}}{An ID for individuals who served as coaches. Links to information in the 
                 \code{\link{Coaches}}, \code{\link{AwardsCoaches}} and other files dealing with managers.}
    \item{\code{hofID}}{An ID for individuals who are in the hockey Hall of Fame.}
    \item{\code{firstName}}{Player's first name}
    \item{\code{lastName}}{Player's last name}
    \item{\code{nameNote}}{Note about player's name (usually signifying that they changed
                 their name or played under two different names)}
    \item{\code{nameGiven}}{Player's given name (typically first and middle)}
    \item{\code{nameNick}}{Player's nickname}
    \item{\code{height}}{Player's height in inches}
    \item{\code{weight}}{Player's weight in pounds}
    \item{\code{shootCatch}}{Shooting hand (catching hand for goalies)}
    \item{\code{legendsID}}{ID at Legends Of Hockey (www.legendsofhockey.net)}
    \item{\code{ihdbID}}{ID at the Internet Hockey Database}
    \item{\code{hrefID}}{ID at Hockey-Reference.com}
    \item{\code{firstNHL}}{First NHL season (2005-06 listed as "2005")}
    \item{\code{lastNHL}}{Last NHL season (2005-06 listed as "2005")}
    \item{\code{firstWHA}}{First WHA season (1975-76 listed as "1975")}
    \item{\code{lastWHA}}{Last WHA season (1975-76 listed as "1975")}
    \item{\code{pos}}{Position}
    \item{\code{birthYear}}{Year player was born}
    \item{\code{birthMon}}{Month player was born}
    \item{\code{birthDay}}{Day player was born}
    \item{\code{birthCountry}}{Country where player was born}
    \item{\code{birthState}}{State or province where player was born}
    \item{\code{birthCity}}{City where player was born}
    \item{\code{deathYear}}{Year player died}
    \item{\code{deathMon}}{Month player died}
    \item{\code{deathDay}}{Day player died}
    \item{\code{deathCountry}}{Country where player died}
    \item{\code{deathState}}{State or province where player died}
    \item{\code{deathCity}}{City where player died}
  }
}
\source{
Hockey Databank: \url{http://groups.yahoo.com/neo/groups/hockey-databank/info}
}
\examples{
data(Master)
## maybe str(Master) ; plot(Master) ...
}
\keyword{datasets}
