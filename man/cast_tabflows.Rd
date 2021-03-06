% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cast_tabflows.R
\name{cast_tabflows}
\alias{cast_tabflows}
\title{Create an OD square matrix}
\usage{
cast_tabflows(idpol, tabflows, idori, iddes, idflow)
}
\arguments{
\item{idpol}{A character vector with id values for the spatial units}

\item{tabflows}{A data.frame of flows between origins and destinations (long format matrix containing, at least, 3 column : origins, destinations, flows)}

\item{idori}{A character string giving the origin field name in tabflows}

\item{iddes}{A character string giving the destination field name in tabflows}

\item{idflow}{A character string giving the flow field name in tabflows}
}
\value{
A squared Origins (rows) and Destination (columns) matrix of flows
}
\description{
This function allows you to create an Origin/Destination squared matrix from a Origin/Destination long format matrix
}
