% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbsa.R
\name{rand_route}
\alias{rand_route}
\title{Random route selection}
\usage{
rand_route(fish_num, route_names, route_probs)
}
\arguments{
\item{fish_num}{Number of fish in the simulation}

\item{route_names}{Vector of route names through a project}

\item{route_probs}{Vector of probabilities that fish enters each route; should sum to one.}
}
\description{
Randomly select route for a fish through a project from a multinomial distribution based on routing probabilities.
}
