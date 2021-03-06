% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfb_game_player_stats.R
\name{cfb_game_player_stats}
\alias{cfb_game_player_stats}
\title{Get results information from games}
\usage{
cfb_game_player_stats(
  year,
  week = NULL,
  season_type = "regular",
  team = NULL,
  conference = NULL,
  category = NULL,
  game_id = NULL
)
}
\arguments{
\item{year}{(\emph{Integer} required): Year, 4 digit format(\emph{YYYY})}

\item{week}{(\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)}

\item{season_type}{(\emph{String} default regular): Select Season Type: regular or postseason}

\item{team}{(\emph{String} optional): D-I Team}

\item{conference}{(\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr}

\item{category}{(\emph{String} optional): Category filter (e.g defensive)\cr
Offense: passing, receiving, rushing\cr
Defense: defensive, fumbles, interceptions\cr
Special Teams: punting, puntReturns, kicking, kickReturns\cr}

\item{game_id}{(\emph{Integer} optional): Game ID filter for querying a single game
Can be found using the \code{\link[cfbscrapR:cfb_game_info]{cfbscrapR::cfb_game_info()}} function}
}
\description{
Get results information from games
}
\examples{

cfb_game_player_stats(2018, week = 15, conference = 'Ind')

cfb_game_player_stats(2013, week=1, team = "Florida State", category = 'passing')

}
\keyword{Game}
\keyword{Info}
