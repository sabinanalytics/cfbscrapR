% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfb_metrics_wp_pregame.R
\name{cfb_metrics_wp_pregame}
\alias{cfb_metrics_wp_pregame}
\title{Get Pre-game Win Probability Data from API}
\usage{
cfb_metrics_wp_pregame(
  year = NULL,
  week = NULL,
  team = NULL,
  season_type = "regular"
)
}
\arguments{
\item{year}{(\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})}

\item{week}{(\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier}

\item{team}{(\emph{String} optional): D-I Team}

\item{season_type}{(\emph{String} default regular): Select Season Type: regular or postseason}
}
\description{
Get Pre-game Win Probability Data from API
}
\examples{

cfb_metrics_wp_pregame(year = 2019, week=9, team='Texas A&M')


}
\keyword{Data}
\keyword{Pre-game}
\keyword{Probability}
\keyword{Win}
