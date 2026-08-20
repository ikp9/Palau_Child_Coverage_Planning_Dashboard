packages <- c(
  "shiny", "bslib", "bsicons", "dplyr", "readr", "tidyr", "stringr",
  "DT", "leaflet", "plotly", "here", "readxl", "tibble",
  "rio", "janitor", "lubridate", "purrr", "ggplot2", "sf",
  "htmltools", "htmlwidgets", "crosstalk", "RColorBrewer",
  "openxlsx", "writexl", "knitr", "kableExtra", "ggtext"
)
new <- packages[!packages %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
