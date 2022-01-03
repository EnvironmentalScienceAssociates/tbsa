#' Rotational speed
#'
#' @md
#' @param rpm            Runner revolutions per minute
#' @export

rotational_speed <- function(rpm) {
  (rpm * 2 * pi)/60
}

#' Energy coefficient
#'
#' Non-dimensional energy coefficient term incorporated into strike equations
#'
#' @md
#' @param H              Net head on the turbine (ft)
#' @param D              Nominal diameter (ft) of runner
#' @param rpm            Runner revolutions per minute
#' @export

energy_coef <- function(H, D, rpm) {
  g = 32.17 # acceleration of gravity ft/s/s
  omega = rotational_speed(rpm)
  (g * H)/(omega * D)^2
}

#' Discharge coefficient
#'
#' Non-dimensional discharge coefficient term incorporated into strike equations
#'
#' @md
#' @param Q              Turbine discharge (cfs)
#' @param D              Nominal diameter (ft) of runner
#' @param rpm            Runner revolutions per minute
#' @export

discharge_coef <- function(Q, D, rpm) {
  omega = rotational_speed(rpm)
  Q/(omega * D^3)
}
