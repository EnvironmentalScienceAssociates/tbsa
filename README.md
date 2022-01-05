## tbsa

R implementation of the [Turbine Blade Strike Analysis (TBSA) spreadsheet model](https://www.fws.gov/northeast/fisheries/fishpassageengineering.html) provided by the U.S. Fish & Wildlife Service (based on the equations in [Frank et al. 1997](https://digital.library.unt.edu/ark:/67531/metadc690969/m2/1/high_res_d/563213.pdf)) for performing leading-edge blade strike analysis of hydropower turbines on fish.

### Prerequisites

Installation requires the R package [`remotes`](https://remotes.r-lib.org).

```
install.packages("remotes")
```

### Installation

`tbsa` is only available through GitHub.

```
remotes::install_github("EnvironmentalScienceAssociates/tbsa")
```

### Usage

The `tbsa` function runs the TBSA model simulation based on a data frame with route data. Example route data is included with the package in the data frame `route_data_ex`.

```
tbsa(fish_num = 10, 
     length_mean = 1.5, 
     length_sd = 0.25, 
     route_data = route_data_ex)
```
