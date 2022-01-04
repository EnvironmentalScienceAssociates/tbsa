#' Turbine blade strike analysis
#'
#' Runs stochastic simulation of turbine blade strike analysis based on input parameters in route_data
#'
#' @md
#' @param fish_num       Number of fish in the simulation
#' @param length_mean    Mean fish length (ft)
#' @param length_sd      Standard deviation of fish length (ft)
#' @param route_data     Data frame with input parameters for each route (see route_data_ex for example)
#' @export
#' @examples
#' tbsa(10, 1.5, 0.25, route_data_ex)

tbsa <- function(fish_num, length_mean, length_sd, route_data) {
  rd = route_data
  rd$route_type = tolower(rd$route_type)

  if(any(!(rd$route_type %in% c("francis", "kaplan", "propeller", "bypass"))))
    stop("Values in route_data$route_type must be one of Francis, Kaplan, propeller, or bypass")

  if(length(rd$route) != length(unique(rd$route)))
    stop("Values in route_data$route must be unique")

  if(any((rd$route_type == "bypass" & is.na(rd$est_mortality)) |
         (rd$route_type != "bypass" & !is.na(rd$est_mortality))))
    stop("Values in route_data$est_mortality should be NA unless route_data$route_type is bypass")

  if(sum(rd$route_prob) != 1)
    warning("Values in route_data$route_prob do not sum to one")

  rd$energy_coef = ifelse(rd$route_type == "bypass", NA_real_, energy_coef(rd$H, rd$D, rd$rpm))

  rd$discharge_coef = ifelse(rd$route_type == "bypass", NA_real_, discharge_coef(rd$Q, rd$D, rd$rpm))

  rd$alpha = ifelse(rd$route_type == "francis",
                    francis_alpha(rd$Q, rd$H, rd$D, rd$D1, rd$D2, rd$rpm, rd$eta, rd$opt, rd$xi, rd$B),
                    ifelse(rd$route_type == "kaplan",
                           kaplan_alpha(rd$Q, rd$H, rd$D, rd$rpm, rd$eta, 0.75*0.5*rd$D),
                           ifelse(rd$route_type == "propeller",
                                  propeller_alpha(rd$Q, rd$H, rd$D, rd$rpm, rd$eta, rd$opt, 0.75*0.5*rd$D),
                                  NA_real_)))

  rd$beta = ifelse(rd$route_type == "francis",
                   francis_beta(rd$Q, rd$D, rd$D1, rd$D2, rd$rpm, rd$opt, rd$xi),
                   ifelse(rd$route_type == "propeller",
                          propeller_beta(rd$Q, rd$D, rd$rpm, rd$opt, 0.75*0.5*rd$D),
                          NA_real_))

  rand_df = data.frame(route_name = rand_route(fish_num, rd$route_name, rd$route_prob),
                       # following spreadsheet model, it is possible to generate negative fish lengths
                       fish_length_ft = rnorm(fish_num, length_mean, length_sd),
                       rand_unif = runif(fish_num))

  out = dplyr::left_join(rd, rand_df, by = "route_name") |>
    # not all route names will end up in rand_dt; dropping rows with no fish lengths
    dplyr::filter(!is.na(fish_length_ft))

  out$strike_prob = ifelse(out$route_type == "francis",
                           francis_strike(out$Q, out$H, out$D, out$D1, out$D2, out$rpm, out$eta, out$opt,
                                          out$xi, out$B, out$N, out$fish_length_ft, out$lambda),
                           ifelse(out$route_type == "kaplan",
                                  kaplan_strike(out$Q, out$H, out$D, out$rpm, out$eta, out$N,
                                                out$fish_length_ft, out$lambda),
                                  ifelse(out$route_type == "propeller",
                                         propeller_strike(out$Q, out$H, out$D, out$rpm, out$eta, out$opt,
                                                          out$N, out$fish_length_ft, out$lambda),
                                         NA_real_)))

  out$result = ifelse(is.na(out$est_mortality),
                      ifelse(out$strike_prob > out$rand_unif, "strike", "pass"),
                      ifelse(out$est_mortality > out$rand_unif, "no pass", "pass"))

  out
}

#' Random route selection
#'
#' Randomly select route for a fish through a project from a multinomial distribution based on routing probabilities.
#'
#' @md
#' @param fish_num       Number of fish in the simulation
#' @param route_names    Vector of route names through a project
#' @param route_probs    Vector of probabilities that fish enters each route; should sum to one.
#' @export

rand_route <- function(fish_num, route_names, route_probs){
  multi_mat = rmultinom(fish_num, 1, route_probs)
  route_vec = vector("character", length = fish_num)
  for (i in 1:ncol(multi_mat)){
    route_vec[i] = route_names[multi_mat[,i] == 1]
  }
  route_vec
}


