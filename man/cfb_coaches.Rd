% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfb_coaches.R
\name{cfb_coaches}
\alias{cfb_coaches}
\title{Coach Information Search}
\usage{
cfb_coaches(
  first = NULL,
  last = NULL,
  team = NULL,
  year = NULL,
  min_year = NULL,
  max_year = NULL
)
}
\arguments{
\item{first}{(\emph{String} optional): First name for the coach you are trying to look up}

\item{last}{(\emph{String} optional): Last name for the coach you are trying to look up}

\item{team}{(\emph{String} optional): Team - Select a valid team, D1 football}

\item{year}{(\emph{Integer} optional): Year, 4 digit format (\emph{YYYY}).}

\item{min_year}{(\emph{Integer} optional): Minimum Year filter (inclusive), 4 digit format (\emph{YYYY}).}

\item{max_year}{(\emph{Integer} optional): Maximum Year filter (inclusive), 4 digit format (\emph{YYYY}).}
}
\description{
A coach search function which provides coaching records and school history for a given coach
}
\examples{

cfb_coaches(first = "Nick", last = "Saban",team='alabama')


}
\keyword{Recruiting}
