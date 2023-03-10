% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nTickets.R
\name{nTickets}
\alias{nTickets}
\title{Finds the optimal amount of plane tickets to sell on a flight given that some passengers won't show up.}
\usage{
nTickets(N, gamma, p)
}
\arguments{
\item{N}{Number of seats on the plane.}

\item{gamma}{Probability of overbooking.}

\item{p}{Probability that passengers show.}
}
\value{
A discrete plot of the showing the range of possible tickets which could be sold and the percentage of over bookings happening.

A continuous plot (acting as an approximation) show the range of possible tickets which could be sold and the percentage of over bookings happening.

A list which contains the optimal tickets to sell for the discrete (nd) & continuous (nc) calculations along with N, gamma, and p.
}
\description{
Finds the optimal amount of plane tickets to sell on a flight given that some passengers won't show up.
}
\examples{
nTickets(200, 0.02, 0.95)
}
