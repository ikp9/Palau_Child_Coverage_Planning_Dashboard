packages <- c(
  "shiny", "bslib", "bsicons", "dplyr", "readr", "tidyr", "stringr",
  "DT", "leaflet", "plotly", "here", "readxl", "tibble",
  "rio", "janitor", "lubridate", "purrr", "ggplot2", "sf",
  "htmltools", "htmlwidgets", "crosstalk", "RColorBrewer",
  "openxlsx", "writexl", "knitr", "kableExtra", "ggtext", "rsconnect"
)
new <- packages[!packages %in% rownames(installed.packages())]
if (length(new)) install.packages(new)

#Code below was only for publishing - does not need to be run every time

install.packages(c("openssl", "rsconnect"), 
                 repos = "https://packagemanager.posit.co/cran/latest")

rsconnect::writeManifest(contentCategory = "site")