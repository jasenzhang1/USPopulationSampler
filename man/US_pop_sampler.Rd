% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/US_pop_sampler.R
\name{US_pop_sampler}
\alias{US_pop_sampler}
\title{Sampling in the United States by 2020 Census data}
\usage{
US_pop_sampler(N, area_fips, level)
}
\arguments{
\item{N}{number of points to draw}

\item{area_fips}{vector of FIPS (2 characters for states, 5 characters for counties)}

\item{level}{integer specifying granularity of population measurements. (1 = state, 2 = county, 3 = tract, 4 = BG)}
}
\value{
sampled points
}
\description{
Sampling in the United States by 2020 Census data
}
\examples{
US_pop_sampler(50, '06', 2)
}
