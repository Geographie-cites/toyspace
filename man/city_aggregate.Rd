% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/city_aggregate.R
\name{city_aggregate}
\alias{city_aggregate}
\title{Aggregate cities of a DF}
\usage{
city_aggregate(before, after, tabflows, idori, iddes)
}
\arguments{
\item{before}{A list of the identifiers to be replaced as numeric}

\item{after}{A number of the identifier replacement}

\item{tabflows}{A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)}

\item{idori}{A character string giving the origin field name in tabflows (numeric)}

\item{iddes}{A character string giving the destination field name in tabflows (numeric)}
}
\value{
A data.frame with the same variables as the shape, centroïds (lat,lon) and no geometries
}
\description{
This function allows you to aggregate cities of a dataframe from designed identifier
}
\examples{
# Import data
before <-  c(75101, 75102, 75103,75104, 75105,75106,75107, 75108, 75109,
             75110,75111, 75112, 75113, 75114,75115, 75116, 75117,75118, 75119, 75120)
after <- 75056
data(tabflows)
idori <- "ORI"
iddes <- "DES"

cityAggregate <- city_aggregate(before, after, tabflows, idori, iddes)

cityAggregate[1:10,]

}
