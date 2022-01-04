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
#' @param radius_ratio   r/R where R = 0.5 * D; passage near hub (0.5), mid-blade (0.75), blade tip (1)
#' @export

propeller_strike <- function(Q, H, D, rpm, eta, opt, N, L, lambda = 0.2, radius_ratio = 0.75) {
  dc = discharge_coef(Q, D, rpm)
  alpha = propeller_alpha(Q, H, D, rpm, eta, opt, radius_ratio)
  # breaking the equation into a few pieces (abbreviated as pc)
  pc1 = (lambda * N * L)/D
  pc2 = cos(alpha)/(8*dc)
  pc3 = sin(alpha)/(pi * radius_ratio)
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
#' @param radius_ratio   r/R where R = 0.5 * D; passage near hub (0.5), mid-blade (0.75), blade tip (1)
#' @export

propeller_alpha <- function(Q, H, D, rpm, eta, opt, radius_ratio) {
  ec = energy_coef(H, D, rpm)
  dc = discharge_coef(Q, D, rpm)
  beta = propeller_beta(Q, D, rpm, opt, radius_ratio)
  num1 = (pi * ec * eta)/2
  den1 = dc * radius_ratio
  num2 = (pi / 8) * radius_ratio
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
#' @param radius_ratio   r/R where R = 0.5 * D; passage near hub (0.5), mid-blade (0.75), blade tip (1)
#' @export

propeller_beta <- function(Q, D, rpm, opt, radius_ratio) {
  num = (pi / 8) * radius_ratio
  den = discharge_coef(Q, D, rpm) * opt
  atan(num/den)
}
