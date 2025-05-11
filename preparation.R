library(purrr)
library(dplyr)
library(leaflet)
library(fontawesome)


data <- spData::cycle_hire

coords <- map_dfr(data$geometry, ~ {
  tibble(
    longitude = .x[1],
    latitude  = .x[2]
  )
})


data <- bind_cols(data, coords)
data <- data %>% select(-geometry)


# hist(data$nbikes)
# median(data$nbikes)
# quantile(data$nbikes)

getColor <- function(bikes) {
  sapply(bikes, function(b) {
    if (b <= 7) {
      "red"
    } else if (b <= 15) {
      "orange"
    } else {
      "green"
    }
  })
}


metric_trans <- function(word) {
  if (word == "Number of Bikes") {
    return("nbikes")
  } else {
    return("nempty")
  }
}

icon_trans <- function(word) {
  if (word == "Number of Bikes") {
    return("bicycle")
  } else {
    return("square-parking")
  }
}
