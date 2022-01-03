#' Francis blade strike probability
#'
#' Calculates leading-edge blade strike probability from a Francis turbine
#'
#' @md
#' @param Q              Turbine discharge (cfs)
#' @param H              Net head on the turbine (ft)
#' @param D              Nominal diameter (ft) of runner
#' @param D1             Diameter (ft) at the intake of the runner
#' @param D2             Diameter (ft) at the outlet of the runner
#' @param rpm            Runner revolutions per minute
#' @param eta            Turbine efficiency
#' @param opt            Ratio of turbine discharge at best efficiency to hydraulic capacity
#' @param xi             Ratio between Q with no exit swirl and Qopt
#' @param B              Runner height (ft) at inlet
#' @param N              Number of blades
#' @param L              Fish length (ft)
#' @param lambda         Actual mortality correlation; influenced by many factors including unit type and fish species
#' @export

francis_strike <- function(Q, H, D, D1, D2, rpm, eta, opt, xi, B, N, L, lambda = 0.2) {
  R = 0.5 * D
  r = 0.75 * R  # Radius at mid blade (ft)
  dc = discharge_coef(Q, D, rpm)
  alpha = francis_alpha(Q, H, D, D1, D2, rpm, eta, opt, xi, B)
  # breaking the equation into a few pieces (abbreviated as pc)
  pc1 = (lambda * N * L)/D
  pc2 = (sin(alpha) * (B/D1))/(2*dc)
  pc3 = cos(alpha)/pi
  pc1 * (pc2 + pc3)
}

#' Francis alpha
#'
#' Angle (rad) to tangential of absolute flow upstream of runner
#'
#' @md
#' @param Q              Turbine discharge (cfs)
#' @param H              Net head on the turbine (ft)
#' @param D              Nominal diameter (ft) of runner
#' @param D1             Diameter (ft) at the intake of the runner
#' @param D2             Diameter (ft) at the outlet of the runner
#' @param rpm            Runner revolutions per minute
#' @param eta            Turbine efficiency
#' @param opt            Ratio of turbine discharge at best efficiency to hydraulic capacity
#' @param xi             Ratio between Q with no exit swirl and Qopt
#' @param B              Runner height (ft) at inlet
#' @export

francis_alpha <- function(Q, H, D, D1, D2, rpm, eta, opt, xi, B) {
  ec = energy_coef(H, D, rpm)
  dc = discharge_coef(Q, D, rpm)
  beta = francis_beta(Q, D, D1, D2, rpm, opt, xi)
  pc1 = (2 * pi * ec * eta * B)/(dc * D1)
  pc2 = ((pi * 0.707^2 * B)/(2 * dc * D1)) * (D2/D1)^2
  pc3 = 4 * 0.707 * tan(beta) * (B/D1) * (D1/D2)
  atan(1/(pc1 + pc2 - pc3))
}

#' Francis beta
#'
#' Relative flow angle (rad) at turbine discharge; used in strike equations
#'
#' @md
#' @param Q              Turbine discharge (cfs)
#' @param D              Nominal diameter (ft) of runner
#' @param D1             Diameter (ft) at the intake of the runner
#' @param D2             Diameter (ft) at the outlet of the runner
#' @param rpm            Runner revolutions per minute
#' @param opt            Ratio of turbine discharge at best efficiency to hydraulic capacity
#' @param xi             Ratio between Q with no exit swirl and Qopt
#' @export

francis_beta <- function(Q, D, D1, D2, rpm, opt, xi) {
  num = (0.707 * pi)/8
  den = xi * discharge_coef(Q, D, rpm) * opt * (D1/D2)^3
  atan(num/den)
}
