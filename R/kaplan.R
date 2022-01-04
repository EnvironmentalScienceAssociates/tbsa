#' Kaplan blade strike probability
#'
#' Calculates leading-edge blade strike probability from Kaplan turbine
#'
#' @md
#' @param Q              Turbine discharge (cfs)
#' @param H              Net head on the turbine (ft)
#' @param D              Nominal diameter (ft) of runner
#' @param rpm            Runner revolutions per minute
#' @param eta            Turbine efficiency
#' @param N              Number of blades
#' @param L              Fish length (ft)
#' @param lambda         Actual mortality correlation; influenced by many factors including unit type and fish species
#' @param radius_ratio   r/R where R = 0.5 * D; passage near hub (0.5), mid-blade (0.75), blade tip (1)
#' @export

kaplan_strike <- function(Q, H, D, rpm, eta, N, L, lambda = 0.2, radius_ratio = 0.75) {
  dc = discharge_coef(Q, D, rpm)
  alpha = kaplan_alpha(Q, H, D, rpm, eta, radius_ratio)
  # breaking the equation into a few pieces (abbreviated as pc)
  pc1 = (lambda * N * L)/D
  pc2 = cos(alpha)/(8*dc)
  pc3 = sin(alpha)/(pi * radius_ratio)
  pc1 * (pc2 + pc3)
}

#' Kaplan alpha
#'
#' Angle (rad) to tangential of absolute flow upstream of runner
#'
#' @md
#' @param Q              Turbine discharge (cfs)
#' @param H              Net head on the turbine (ft)
#' @param D              Nominal diameter (ft) of runner
#' @param rpm            Runner revolutions per minute
#' @param eta            Turbine efficiency
#' @param radius_ratio   r/R where R = 0.5 * D; passage near hub (0.5), mid-blade (0.75), blade tip (1)
#' @export

kaplan_alpha <- function(Q, H, D, rpm, eta, radius_ratio) {
  ec = energy_coef(H, D, rpm)
  dc = discharge_coef(Q, D, rpm)
  num = pi * ec * eta
  den = 2 * dc * radius_ratio
  atan(num/den)
}
