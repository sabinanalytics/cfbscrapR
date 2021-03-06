% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfb_stats_season_player.R
\name{cfb_stats_season_player}
\alias{cfb_stats_season_player}
\title{Get Season Statistics by Player}
\usage{
cfb_stats_season_player(
  year,
  season_type = "regular",
  team = NULL,
  conference = NULL,
  start_week = NULL,
  end_week = NULL,
  category = NULL
)
}
\arguments{
\item{year}{(\emph{Integer} required): Year, 4 digit format (\emph{YYYY})}

\item{season_type}{(\emph{String} default both): Select Season Type: regular, postseason, or both.}

\item{team}{(\emph{String} optional): D-I Team}

\item{conference}{(\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr}

\item{start_week}{(\emph{Integer} optional): Starting Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier}

\item{end_week}{(\emph{Integer} optional): Ending Week - values range fom 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier}

\item{category}{(\emph{String} optional): Category filter (e.g defensive)\cr
Offense: passing, receiving, rushing\cr
Defense: defensive, fumbles, interceptions\cr
Special Teams: punting, puntReturns, kicking, kickReturns\cr}
}
\description{
Get Season Statistics by Player
}
\examples{

cfb_stats_season_player(year=2018, conference = 'B12', start_week = 1,end_week = 7)

cfb_stats_season_player(2019, team = 'LSU', category = 'passing')

cfb_stats_season_player(2013, team = "Florida State", category = 'passing')

}
\keyword{Player}
\keyword{Season}
\keyword{Stats}
