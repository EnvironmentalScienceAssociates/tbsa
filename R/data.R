#' Example Route Data
#'
#' Example route data used as input to the tbsa function. Includes all four route types and corresponds
#' to the "Francis, Kaplan and propeller w spill, gates and bypass" example in the spreadsheet model.
#'
#' @format A data frame with 9 rows and 16 variables:
#' \describe{
#'   \item{route_name}{Unique name of a route through a project}
#'   \item{route_prob}{Probability that a fish enters each route; should sum to one.}
#'   \item{route_type}{Route type should be one of Francis, Kaplan, propeller, or bypass.}
#'   \item{D}{Nominal diameter (ft) of runner}
#'   \item{N}{Number of blades}
#'   \item{B}{Runner height (ft) at inlet}
#'   \item{Q}{Turbine discharge (cfs)}
#'   \item{opt}{Ratio of turbine discharge at best efficiency to hydraulic capacity}
#'   \item{H}{Net head on the turbine (ft)}
#'   \item{rpm}{Runner revolutions per minute}
#'   \item{xi}{Ratio between Q with no exit swirl and Qopt}
#'   \item{lambda}{Actual mortality correlation; influenced by many factors including unit type and fish species}
#'   \item{D1}{Diameter (ft) at the intake of the runner}
#'   \item{D2}{Diameter (ft) at the outlet of the runner}
#'   \item{eta}{Turbine efficiency}
#'   \item{est_mortality}{Estimated mortality for routes without turbines, e.g., gates, spillways, fishways, etc.}
#' }
"route_data_ex"

