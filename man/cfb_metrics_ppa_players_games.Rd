% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfb_metrics_ppa_players_games.R
\name{cfb_metrics_ppa_players_games}
\alias{cfb_metrics_ppa_players_games}
\title{Get player game averages for Predicted Points Added (PPA)}
\usage{
cfb_metrics_ppa_players_games(
  year = NULL,
  week = NULL,
  team = NULL,
  position = NULL,
  athlete_id = NULL,
  threshold = NULL,
  excl_garbage_time = FALSE
)
}
\arguments{
\item{year}{(\emph{Integer} required): Year, 4 digit format (\emph{YYYY})}

\item{week}{(\emph{Integer} optional): Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier}

\item{team}{(\emph{String} optional): D-I Team}

\item{position}{(\emph{string} optional): Position abbreviation of the player you are searching for.\cr
Position Group  - options include:\cr
 * Offense: QB, RB, FB, TE,  OL, G, OT, C, WR\cr
 * Defense: DB, CB, S, LB,  DE, DT, NT, DL\cr
 * Special Teams: K, P, LS, PK\cr}

\item{athlete_id}{(\emph{Integer} optional): Athlete ID filter for querying a single athlete\cr
Can be found using the \code{\link[cfbscrapR:cfb_player_info]{cfbscrapR::cfb_player_info()}} function.}

\item{threshold}{(\emph{Integer} optional): Minimum threshold of plays.}

\item{excl_garbage_time}{(\emph{Logical} default FALSE): Select whether to exclude Garbage Time (TRUE or FALSE)}
}
\description{
Get player game averages for Predicted Points Added (PPA)
}
\examples{



cfb_metrics_ppa_players_games(year = 2019,week=3, team = 'TCU')

}
\keyword{Players}
\keyword{Points}
\keyword{Predicted}
