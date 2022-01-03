route_data_ex = read.csv(file.path("data-raw", "francis_kaplan_propeller_bypass.csv"))
usethis::use_data(route_data_ex, overwrite = TRUE)
