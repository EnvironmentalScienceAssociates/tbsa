#' Propeller blade strike probability
#'
#' Calculates leading-edge blade strike probability from a propeller turbine
#'
#' @md
#' @param Q              Turbine discharge (cfs)
#' @param H              Net head on the turbine (ft)
#' @param D              Nominal diameter (ft) of runner
#' @param rpm            Runner revolutions per minute
#' @param eta            Turbine efficiency
#' @param opt            Ratio of turbine discharge at best efficiency to hydraulic capacity
#' @param N              Number of blades
#' @param L              Fish length (ft)
#' @param lambda         Actual mortality correlation; influenced by many factors including unit type and fish species
#' @export

propeller_strike <- function(Q, H, D, rpm, eta, opt, N, L, lambda = 0.2) {
  R = 0.5 * D
  r = 0.75 * R  # Radius at mid blade (ft)
  dc = discharge_coef(Q, D, rpm)
  alpha = propeller_alpha(Q, H, D, rpm, eta, opt, r)
  # breaking the equation into a few pieces (abbreviated as pc)
  pc1 = (lambda * N * L)/D
  pc2 = cos(alpha)/(8*dc)
  pc3 = sin(alpha)/((pi * r)/R)
  pc1 * (pc2 + pc3)
}

#' Propeller alpha
#'
#' Angle (rad) to tangential of absolute flow upstream of runner
#'
#' @md
#' @param Q              Turbine discharge (cfs)
#' @param H              Net head on the turbine (ft)
#' @param D              Nominal diameter (ft) of runner
#' @param rpm            Runner revolutions per minute
#' @param eta            Turbine efficiency
#' @param opt            Ratio of turbine discharge at best efficiency to hydraulic capacity
#' @param r              Radius at mid blade (ft)
#' @export

propeller_alpha <- function(Q, H, D, rpm, eta, opt, r) {
  ec = energy_coef(H, D, rpm)
  dc = discharge_coef(Q, D, rpm)
  beta = propeller_beta(Q, D, rpm, opt, r)
  R = 0.5 * D
  num1 = (pi * ec * eta)/2
  den1 = (dc * r)/R
  num2 = (pi * r)/(8 * R)
  atan((num1/den1) + (num2/dc) - tan(beta))
}

#' Propeller beta
#'
#' Relative flow angle (rad) at turbine discharge; used in strike equations
#'
#' @md
#' @param Q              Turbine discharge (cfs)
#' @param D              Nominal diameter (ft) of runner
#' @param rpm            Runner revolutions per minute
#' @param opt            Ratio of turbine discharge at best efficiency to hydraulic capacity
#' @param r              Radius at mid blade (ft)
#' @export

propeller_beta <- function(Q, D, rpm, opt, r) {
  num = (pi * r)/(8 * 0.5 * D)
  den = discharge_coef(Q, D, rpm) * opt
  atan(num/den)
}
