% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfb_team_matchup.R
\name{cfb_team_matchup}
\alias{cfb_team_matchup}
\title{Get matchup history between two teams.}
\usage{
cfb_team_matchup(team1, team2, min_year = NULL, max_year = NULL)
}
\arguments{
\item{team1}{(\emph{String} required): D-I Team 1}

\item{team2}{(\emph{String} required): D-I Team 2}

\item{min_year}{(\emph{Integer} optional): Minimum of year range, 4 digit format (\emph{YYYY})}

\item{max_year}{(\emph{Integer} optional): Maximum of year range, 4 digit format (\emph{YYYY})}
}
\description{
Get matchup history between two teams.
}
\examples{

cfb_team_matchup('Texas','Oklahoma')

cfb_team_matchup('Texas A&M','TCU')

cfb_team_matchup('Texas A&M','TCU', min_year = 1975)

cfb_team_matchup('Florida State', 'Florida', min_year = 1975)

}
\keyword{Matchup}
\keyword{Team}
