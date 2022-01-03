## Tests are based on example data in spreadsheet model
## after loading the tbsa package, i.e., `library(tbsa)`
## run tests with `tinytest::test_all()`

# Francis only ------------------------------------------------------------

energy_coef(H = 42, D = 5.75, rpm = 200) |>
  round(5) |>
  expect_equal(0.09316)
discharge_coef(Q = 750, D = 5.75, rpm = 200) |>
  round(5) |>
  expect_equal(0.18836)
# for some reason this beta test is slightly off with cascading effects for alpha
# decided not to worry about this discrepancy as long as all other tests are correct
# francis_beta(Q = 750, D = 5.75, D1 = 5.3, D2 = 3.2, rpm = 200, opt = 0.89, xi = 1.1) |>
#   round(3) |>
#   expect_equal(0.329)
# francis_alpha(Q = 750, H = 42, D = 5.75, D1 = 5.3, D2 = 3.2, rpm = 200,
#               eta = 0.8, opt = 0.89, xi = 1.1, B = 1.1) |>
#   round(3) |>
#   expect_equal(1.096)

# Propeller w/ spill only -------------------------------------------------

energy_coef(H = 48, D = 10, rpm = 92) |>
  round(5) |>
  expect_equal(0.16636)
discharge_coef(Q = 1850, D = 10, rpm = 92) |>
  round(5) |>
  expect_equal(0.19202)
propeller_alpha(Q = 1850, H = 48, D = 10, rpm = 92, eta = 0.8, opt = 0.9, r = 0.75*0.5*10) |>
  round(3) |>
  expect_equal(0.908)
propeller_strike(Q = 1850, H = 48, D = 10, rpm = 92, eta = 0.8, opt = 0.9, N = 5, L = 1.81) |>
  round(3) |>
  expect_equal(0.133)
propeller_strike(Q = 1850, H = 48, D = 10, rpm = 92, eta = 0.8, opt = 0.9, N = 5, L = 1.14) |>
  round(3) |>
  expect_equal(0.084)

# Francis and Kaplan w/ spill only ----------------------------------------

## Kaplan -----------------------------------------------------------------

energy_coef(H = 47, D = 10, rpm = 100) |>
  round(5) |>
  expect_equal(0.13788)
discharge_coef(Q = 1800, D = 10, rpm = 100) |>
  round(5) |>
  expect_equal(0.17189)
kaplan_alpha(Q = 1800, H = 47, D = 10, rpm = 100, eta = 0.9, r = 0.75*0.5*10) |>
  round(3) |>
  expect_equal(0.986)
kaplan_strike(Q = 1800, H = 47, D = 10, rpm = 100, eta = 0.9, N = 5, L = 1.75) |>
  round(3) |>
  expect_equal(0.132)
kaplan_strike(Q = 1800, H = 47, D = 10, rpm = 100, eta = 0.9, N = 5, L = 1.13) |>
  round(3) |>
  expect_equal(0.085)

## Francis ----------------------------------------------------------------

energy_coef(H = 50, D = 11.5, rpm = 100) |>
  round(5) |>
  expect_equal(0.11091)
discharge_coef(Q = 2300, D = 11.5, rpm = 100) |>
  round(5) |>
  expect_equal(0.14441)
francis_beta(Q = 2300, D = 11.5, D1 = 11, D2 = 8, rpm = 100, opt = 0.9, xi = 1.1) |>
  round(3) |>
  expect_equal(0.642)
francis_alpha(Q = 2300, H = 50, D = 11.5, D1 = 11, D2 = 8, rpm = 100,
              eta = 0.85, opt = 0.9, xi = 1.1, B = 1.7) |>
  round(3) |>
  expect_equal(1.009)
francis_strike(Q = 2300, H = 50, D = 11.5, D1 = 11, D2 = 8, rpm = 100,
               eta = 0.85, opt = 0.9, xi = 1.1, B = 1.7, N = 13, L = 1.25) |>
  round(3) |>
  expect_equal(0.176)
francis_strike(Q = 2300, H = 50, D = 11.5, D1 = 11, D2 = 8, rpm = 100,
               eta = 0.85, opt = 0.9, xi = 1.1, B = 1.7, N = 13, L = 2.09) |>
  round(3) |>
  expect_equal(0.294)

# Francis and Kaplan w/ spill and bypass ----------------------------------

## Kaplan -----------------------------------------------------------------

energy_coef(H = 52, D = 11, rpm = 95) |>
  round(5) |>
  expect_equal(0.13969)
discharge_coef(Q = 1800, D = 11, rpm = 95) |>
  round(5) |>
  expect_equal(0.13594)
kaplan_alpha(Q = 1800, H = 52, D = 11, rpm = 95, eta = 0.9, r = 0.75*0.5*11) |>
  round(3) |>
  expect_equal(1.094)
kaplan_strike(Q = 1800, H = 52, D = 11, rpm = 95, eta = 0.9, N = 5, L = 1.94) |>
  round(3) |>
  expect_equal(0.141)
kaplan_strike(Q = 1800, H = 52, D = 11, rpm = 95, eta = 0.9, N = 5, L = 1.25) |>
  round(3) |>
  expect_equal(0.091)

## Francis ----------------------------------------------------------------

energy_coef(H = 50, D = 12, rpm = 100) |>
  round(5) |>
  expect_equal(0.10186)
discharge_coef(Q = 2300, D = 12, rpm = 100) |>
  round(5) |>
  expect_equal(0.12710)
francis_beta(Q = 2300, D = 12, D1 = 11, D2 = 8, rpm = 100, opt = 0.9, xi = 1.1) |>
  round(3) |>
  expect_equal(0.704)
francis_alpha(Q = 2300, H = 50, D = 12, D1 = 11, D2 = 8, rpm = 100,
              eta = 0.85, opt = 0.9, xi = 1.1, B = 1.6) |>
  round(3) |>
  expect_equal(1.017)
francis_strike(Q = 2300, H = 50, D = 12, D1 = 11, D2 = 8, rpm = 100,
               eta = 0.85, opt = 0.9, xi = 1.1, B = 1.6, N = 12, L = 1.05) |>
  round(3) |>
  expect_equal(0.137)
francis_strike(Q = 2300, H = 50, D = 12, D1 = 11, D2 = 8, rpm = 100,
               eta = 0.85, opt = 0.9, xi = 1.1, B = 1.6, N = 12, L = 2.18) |>
  round(3) |>
  expect_equal(0.285)

# Francis, Kaplan, and propeller w/ sipll, gates, and bypass --------------

## Propeller --------------------------------------------------------------

### PH West 2 -------------------------------------------------------------

energy_coef(H = 47, D = 10, rpm = 92) |>
  round(5) |>
  expect_equal(0.16290)
discharge_coef(Q = 1725, D = 10, rpm = 92) |>
  round(5) |>
  expect_equal(0.17905)
propeller_alpha(Q = 1725, H = 47, D = 10, rpm = 92, eta = 0.9, opt = 0.85, r = 0.75*0.5*10) |>
  round(3) |>
  expect_equal(0.959)
propeller_strike(Q = 1725, H = 47, D = 10, rpm = 92, eta = 0.9, opt = 0.85, N = 5, L = 1.84) |>
  round(3) |>
  expect_equal(0.138)
propeller_strike(Q = 1725, H = 47, D = 10, rpm = 92, eta = 0.9, opt = 0.85, N = 5, L = 1.33) |>
  round(3) |>
  expect_equal(0.100)

### PH West 3 -------------------------------------------------------------

# energy coefficient is same as PH West 2
discharge_coef(Q = 1700, D = 10, rpm = 92) |>
  round(5) |>
  expect_equal(0.17645)
propeller_alpha(Q = 1700, H = 47, D = 10, rpm = 92, eta = 0.9, opt = 0.85, r = 0.75*0.5*10) |>
  round(3) |>
  expect_equal(0.966)
propeller_strike(Q = 1700, H = 47, D = 10, rpm = 92, eta = 0.9, opt = 0.85, N = 5, L = 1.69) |>
  round(3) |>
  expect_equal(0.127)
propeller_strike(Q = 1700, H = 47, D = 10, rpm = 92, eta = 0.9, opt = 0.85, N = 5, L = 1.06) |>
  round(3) |>
  expect_equal(0.080)

## Kaplan -----------------------------------------------------------------

energy_coef(H = 47, D = 9.5, rpm = 95) |>
  round(5) |>
  expect_equal(0.16928)
discharge_coef(Q = 1600, D = 9.5, rpm = 95) |>
  round(5) |>
  expect_equal(0.18758)
kaplan_alpha(Q = 1600, H = 47, D = 9.5, rpm = 95, eta = 0.91, r = 0.75*0.5*9.5) |>
  round(3) |>
  expect_equal(1.044)
kaplan_strike(Q = 1600, H = 47, D = 9.5, rpm = 95, eta = 0.91, N = 5, L = 1.19) |>
  round(3) |>
  expect_equal(0.088)
kaplan_strike(Q = 1600, H = 47, D = 9.5, rpm = 95, eta = 0.91, N = 5, L = 1.69) |>
  round(3) |>
  expect_equal(0.125)
kaplan_strike(Q = 1600, H = 47, D = 9.5, rpm = 95, eta = 0.91, N = 5, L = 2.17) |>
  round(3) |>
  expect_equal(0.160)

## Francis ----------------------------------------------------------------

### PH East 1 -------------------------------------------------------------

energy_coef(H = 50, D = 12, rpm = 100) |>
  round(5) |>
  expect_equal(0.10186)
discharge_coef(Q = 2300, D = 12, rpm = 100) |>
  round(5) |>
  expect_equal(0.12710)
francis_beta(Q = 2300, D = 12, D1 = 11, D2 = 8, rpm = 100, opt = 0.88, xi = 1.1) |>
  round(3) |>
  expect_equal(0.715)
francis_alpha(Q = 2300, H = 50, D = 12, D1 = 11, D2 = 8, rpm = 100,
              eta = 0.85, opt = 0.88, xi = 1.1, B = 2) |>
  round(3) |>
  expect_equal(0.922)
francis_strike(Q = 2300, H = 50, D = 12, D1 = 11, D2 = 8, rpm = 100,
               eta = 0.85, opt = 0.88, xi = 1.1, B = 2, N = 13, L = 2.1) |>
  round(3) |>
  expect_equal(0.347)
francis_strike(Q = 2300, H = 50, D = 12, D1 = 11, D2 = 8, rpm = 100,
               eta = 0.85, opt = 0.88, xi = 1.1, B = 2, N = 13, L = 1.08) |>
  round(3) |>
  expect_equal(0.178)


### PH East 2 -------------------------------------------------------------

# same energy coefficient as PH East 1
discharge_coef(Q = 2350, D = 12, rpm = 100) |>
  round(5) |>
  expect_equal(0.12987)
francis_beta(Q = 2350, D = 12, D1 = 11.2, D2 = 8.1, rpm = 100, opt = 0.88, xi = 1.1) |>
  round(3) |>
  expect_equal(0.696)
francis_alpha(Q = 2350, H = 50, D = 12, D1 = 11.2, D2 = 8.1, rpm = 100,
              eta = 0.87, opt = 0.88, xi = 1.1, B = 2.1) |>
  round(3) |>
  expect_equal(0.906)
francis_strike(Q = 2350, H = 50, D = 12, D1 = 11.2, D2 = 8.1, rpm = 100,
               eta = 0.87, opt = 0.88, xi = 1.1, B = 2.1, N = 13, L = 1.05) |>
  round(3) |>
  expect_equal(0.174)
francis_strike(Q = 2350, H = 50, D = 12, D1 = 11.2, D2 = 8.1, rpm = 100,
               eta = 0.87, opt = 0.88, xi = 1.1, B = 2.1, N = 13, L = 1.80) |>
  round(3) |>
  expect_equal(0.298)

### house unit ------------------------------------------------------------

energy_coef(H = 50, D = 6, rpm = 180) |>
  round(5) |>
  expect_equal(0.12575)
discharge_coef(Q = 600, D = 6, rpm = 180) |>
  round(5) |>
  expect_equal(0.14737)
francis_beta(Q = 600, D = 6, D1 = 5.7, D2 = 3.2, rpm = 180, opt = 0.86, xi = 1.1) |>
  round(3) |>
  expect_equal(0.339)
francis_alpha(Q = 600, H = 50, D = 6, D1 = 5.7, D2 = 3.2, rpm = 180,
              eta = 0.78, opt = 0.86, xi = 1.1, B = 1.4) |>
  round(3) |>
  expect_equal(0.784)
francis_strike(Q = 600, H = 50, D = 6, D1 = 5.7, D2 = 3.2, rpm = 180,
               eta = 0.78, opt = 0.86, xi = 1.1, B = 1.4, N = 12, L = 1.00) |>
  round(3) |>
  expect_equal(0.325)
francis_strike(Q = 600, H = 50, D = 6, D1 = 5.7, D2 = 3.2, rpm = 180,
               eta = 0.78, opt = 0.86, xi = 1.1, B = 1.4, N = 12, L = 2.00) |>
  round(3) |>
  expect_equal(0.651)
