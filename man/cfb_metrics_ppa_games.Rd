% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfb_metrics_ppa_games.R
\name{cfb_metrics_ppa_games}
\alias{cfb_metrics_ppa_games}
\title{Get team game averages for Predicted Points Added (PPA)}
\usage{
cfb_metrics_ppa_games(
  year,
  week = NULL,
  team = NULL,
  conference = NULL,
  excl_garbage_time = FALSE
)
}
\arguments{
\item{year}{(\emph{Integer} required): Year, 4 digit format (\emph{YYYY})}

\item{week}{(\emph{Integer} optional): Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier}

\item{team}{(\emph{String} optional): D-I Team}

\item{conference}{(\emph{String} optional): Conference name - select a valid FBS conference\cr
Conference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\cr
Conference names G5 and FBS Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic\cr}

\item{excl_garbage_time}{(\emph{Logical} default FALSE): Select whether to exclude Garbage Time (TRUE or FALSE)}
}
\description{
Get team game averages for Predicted Points Added (PPA)
}
\examples{



cfb_metrics_ppa_games(year = 2019, team = 'TCU')

}
\keyword{Points}
\keyword{Predicted}
\keyword{Teams}
