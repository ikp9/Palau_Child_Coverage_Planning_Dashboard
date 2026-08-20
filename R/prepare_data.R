# ==============================================================================
# Shore to Shot Dashboard - Palau
# Prepare patient-, community-, geography-, and vaccine-level dashboard datasets
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(readxl)
library(tibble)
library(here)

# ==============================================================================
# 1. Paths and settings
# ==============================================================================

project_dir <- here::here()
data_dir <- here::here("data")
patient_data_dir <- here::here("data", "Analytic Code Output")
geo_file_candidates <- c(
  here::here("data", "Palau_State_Population_Area_Density_2020.xlsx"),
  here::here("data", "Palau_County Coordinates.xlsx"),
  here::here("data", "Palau_County_Coordinates.xlsx")
)
existing_geo_files <- geo_file_candidates[file.exists(geo_file_candidates)]
geo_file <- if (length(existing_geo_files) > 0) existing_geo_files[[1]] else geo_file_candidates[[1]]

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

community_output_file <- file.path(data_dir, "community_summary.csv")
vaccine_output_file <- file.path(data_dir, "community_vaccine_needs.csv")
product_output_file <- file.path(data_dir, "community_product_needs.csv")
patient_output_file <- file.path(data_dir, "patient_dashboard.csv")
geography_qa_output_file <- file.path(data_dir, "qa_palau_geography_assignments.csv")
reminder_qa_output_file <- file.path(data_dir, "qa_reminder_coordinate_sources.csv")

high_individual_risk_threshold <- 70

# Provisional operational-access values used by the inherited priority score.
# Review with the Palau program before treating these as final travel categories.
palau_remoteness_lookup <- c(
  HATOHOBEI = 1,
  SONSOROL = 1,
  ANGAUR = 0.5,
  KAYANGEL = 0.5,
  PELELIU = 0.5
)

# ==============================================================================
# 2. Helpers
# ==============================================================================

normalize_join_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\u00A0", " ") %>%
    str_squish() %>%
    str_to_upper()
}

clean_display_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\u00A0", " ") %>%
    str_squish() %>%
    str_to_lower() %>%
    str_to_title()
}

clean_numeric <- function(x) {
  parse_number(str_replace_all(as.character(x), "\\u00A0", ""))
}

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

safe_quantile <- function(x, probability = 0.75) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  as.numeric(quantile(x, probs = probability, na.rm = TRUE, names = FALSE, type = 7))
}

rescale_100 <- function(x) {
  out <- rep(NA_real_, length(x))
  valid <- !is.na(x)
  if (!any(valid)) return(out)
  r <- range(x[valid])
  if (diff(r) == 0) {
    out[valid] <- 50
    return(out)
  }
  out[valid] <- 100 * (x[valid] - r[1]) / diff(r)
  out
}

percentile_100 <- function(x) {
  out <- rep(NA_real_, length(x))
  valid <- !is.na(x)
  if (!any(valid)) return(out)
  values <- x[valid]
  if (length(values) == 1 || length(unique(values)) == 1) {
    out[valid] <- 50
    return(out)
  }
  out[valid] <- 100 * percent_rank(values)
  out
}

resolve_patient_file <- function(directory) {
  if (!dir.exists(directory)) {
    stop("Analytic output directory not found: ", directory, call. = FALSE)
  }

  matching_files <- list.files(
    directory,
    pattern = "^Dataset 2_Palau_2-83 Mos_DeID_Full_Dataset_.*\\.(csv|xlsx|xls)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(matching_files) == 0) {
    stop(
      paste0(
        "No Palau deidentified patient dataset found in: ", directory,
        "\\nExpected a file named like Dataset 2_Palau_2-83 Mos_DeID_Full_Dataset_MMDDYY.xlsx"
      ),
      call. = FALSE
    )
  }

  info <- file.info(matching_files)
  matching_files[[order(info$mtime, decreasing = TRUE)[1]]]
}

read_patient_data <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "csv") {
    return(read_csv(path, locale = locale(encoding = "windows-1252"), show_col_types = FALSE))
  }
  if (extension %in% c("xlsx", "xls")) return(read_excel(path, guess_max = 10000))
  stop("Unsupported patient-data file type: ", extension, call. = FALSE)
}

# ==============================================================================
# 3. Palau county, coordinate, population, and area lookup
# ==============================================================================

if (!file.exists(geo_file)) {
  stop(
    paste0(
      "Missing Palau county coordinate file. Place one of these files in data/:\n  - ",
      paste(basename(geo_file_candidates), collapse = "\n  - ")
    ),
    call. = FALSE
  )
}

standardize_column_names <- function(data) {
  names(data) <- names(data) %>%
    str_replace_all("²", "2") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    make.unique(sep = "_")
  data
}

find_column <- function(data, candidates, required = TRUE) {
  hit <- intersect(candidates, names(data))
  if (length(hit) > 0) return(hit[[1]])
  if (!required) return(NULL)
  stop(
    "Could not find a required column. Expected one of: ",
    paste(candidates, collapse = ", "),
    call. = FALSE
  )
}

standardize_palau_county <- function(x) {
  value <- normalize_join_text(x)
  value[value %in% c("", "NA", "N/A", "UNKNOWN", "UNK", "ZZ-UNKNOWN-PALAU")] <- NA_character_
  recode(
    value,
    "NGARCHELONG" = "NGARCHELONG",
    "NGERCHELONG" = "NGARCHELONG",
    "NGARACHELONG" = "NGARCHELONG",
    "NGEREMLENGUI" = "NGAREMLENGUI"
  )
}

geo_sheets <- excel_sheets(geo_file)
geo_sheet <- case_when(
  "State_Data" %in% geo_sheets ~ "State_Data",
  "Sheet1" %in% geo_sheets ~ "Sheet1",
  TRUE ~ geo_sheets[[1]]
)

geo_raw <- read_excel(geo_file, sheet = geo_sheet) %>%
  standardize_column_names()

county_column <- find_column(geo_raw, c("county", "state"))
latitude_column <- find_column(geo_raw, c("latitude", "lat"))
longitude_column <- find_column(geo_raw, c("longitude", "lon", "long"))
population_column <- find_column(
  geo_raw,
  c("2020_population", "population_2020", "census_total", "population"),
  required = FALSE
)
area_column <- find_column(
  geo_raw,
  c("land_area_km2", "area_km2"),
  required = FALSE
)

county_lookup <- tibble(
  region = "PALAU",
  county_join = standardize_palau_county(geo_raw[[county_column]]),
  latitude_gis = clean_numeric(geo_raw[[latitude_column]]),
  longitude_gis = clean_numeric(geo_raw[[longitude_column]]),
  census_total = if (is.null(population_column)) NA_real_ else clean_numeric(geo_raw[[population_column]]),
  area_km2 = if (is.null(area_column)) NA_real_ else clean_numeric(geo_raw[[area_column]])
) %>%
  filter(!is.na(county_join)) %>%
  mutate(
    area_m2 = area_km2 * 1e6,
    population_density_km2 = if_else(
      !is.na(census_total) & !is.na(area_km2) & area_km2 > 0,
      census_total / area_km2,
      NA_real_
    )
  ) %>%
  distinct(county_join, .keep_all = TRUE)

valid_counties <- county_lookup$county_join

if (!"KOROR" %in% valid_counties) {
  stop("The Palau county coordinate file must contain KOROR for the default assignment rule.", call. = FALSE)
}

clinic_match_order <- valid_counties[order(nchar(valid_counties), decreasing = TRUE)]

infer_county_from_clinic <- function(clinic) {
  clinic_key <- normalize_join_text(clinic) %>%
    str_replace_all("\\bNGERCHELONG\\b|\\bNGARACHELONG\\b", "NGARCHELONG") %>%
    str_replace_all("\\bNGEREMLENGUI\\b", "NGAREMLENGUI")

  vapply(
    clinic_key,
    function(value) {
      if (is.na(value) || value == "") return(NA_character_)
      matches <- clinic_match_order[str_detect(value, fixed(clinic_match_order))]
      if (length(matches) == 0) return(NA_character_)
      matches[[1]]
    },
    character(1)
  )
}

# ==============================================================================
# 4. Population and land-area lookup from the same county workbook
# ==============================================================================

population_area_lookup <- county_lookup %>%
  select(county_join, census_total, area_m2, area_km2, population_density_km2)

# ==============================================================================
# 5. Read and standardize the newest patient dataset
# ==============================================================================

patient_file <- resolve_patient_file(patient_data_dir)
patient_raw <- read_patient_data(patient_file)

names_trimmed <- str_squish(names(patient_raw))
for (canonical in c("state", "region", "county", "rr_county", "clinic")) {
  hit <- which(str_to_lower(names_trimmed) == canonical)
  if (length(hit) == 1) names(patient_raw)[hit] <- canonical
}

required_patient_columns <- c(
  "patient_id", "county", "clinic", "agegroup", "age_months",
  "days_since_last_vax", "onreminder", "utd_no_mmr", "mmr_utd",
  "individual_risk_score",
  "dtap1utd", "dtap2utd", "dtap3utd", "dtap4utd", "dtap54utd",
  "ipv1utd", "ipv2utd", "ipv3utd", "ipv43utd",
  "mmr1utd", "mmr2utd",
  "hib1utd", "hib2utd", "hib3utd", "hib_utd",
  "hepb2utd", "hepb3utd",
  "pcv1utd", "pcv2utd", "pcv3utd", "pcv4utd", "pcv_utd"
)
missing_patient_columns <- setdiff(required_patient_columns, names(patient_raw))
if (length(missing_patient_columns) > 0) {
  stop(
    "Required patient columns are missing: ",
    paste(missing_patient_columns, collapse = ", "),
    call. = FALSE
  )
}

if (!"rr_county" %in% names(patient_raw)) patient_raw$rr_county <- NA_character_
if (!"state" %in% names(patient_raw)) patient_raw$state <- "PALAU"
if (!"region" %in% names(patient_raw)) patient_raw$region <- NA_character_

patient <- patient_raw %>%
  mutate(
    clinic_key = normalize_join_text(clinic),
    county_input = standardize_palau_county(county),
    inferred_county = infer_county_from_clinic(clinic),

    county_join = case_when(
      county_input %in% valid_counties ~ county_input,
      inferred_county %in% valid_counties ~ inferred_county,
      TRUE ~ "KOROR"
    ),
    geography_assignment_source = case_when(
      county_input %in% valid_counties ~ "Valid county field",
      inferred_county %in% valid_counties ~ "County name found in clinic",
      TRUE ~ "Koror default"
    ),
    county = county_join,
    state = "PALAU",
    region = "PALAU"
  ) %>%
  left_join(county_lookup, by = "county_join") %>%
  mutate(
    region = "PALAU",
    latitude = latitude_gis,
    longitude = longitude_gis,
    community_id = paste("PALAU", county_join, sep = "__"),

    # Recalculate UTD for age using the inherited age-specific dose rules.
    utd = case_when(
      agegroup == 1 & dtap1utd == 1 & ipv1utd == 1 & hib1utd == 1 & hepb2utd == 1 & pcv1utd == 1 ~ 1,
      agegroup == 2 & dtap2utd == 1 & ipv2utd == 1 & hib2utd == 1 & hepb2utd == 1 & pcv2utd == 1 ~ 1,
      agegroup == 3 & dtap3utd == 1 & ipv3utd == 1 & hib2utd == 1 & hepb3utd == 1 & pcv3utd == 1 ~ 1,
      agegroup == 4 & dtap4utd == 1 & ipv3utd == 1 & mmr1utd == 1 & hib3utd == 1 & hepb3utd == 1 & pcv4utd == 1 ~ 1,
      agegroup %in% c(5, 6) & dtap4utd == 1 & ipv3utd == 1 & mmr1utd == 1 & (hib3utd == 1 | hib_utd == 1) & hepb3utd == 1 & (pcv4utd == 1 | pcv_utd == 1) ~ 1,
      agegroup %in% c(7, 8, 9) & dtap54utd == 1 & ipv43utd == 1 & mmr2utd == 1 & (hib3utd == 1 | hib_utd == 1) & hepb3utd == 1 & (pcv4utd == 1 | pcv_utd == 1) ~ 1,
      TRUE ~ 0
    ),
    months_since_last_vax = if_else(is.na(days_since_last_vax), NA_real_, pmin(days_since_last_vax / 30.4375, 36)),
    not_utd = as.integer(utd == 0),
    not_utd_no_mmr = if_else(is.na(utd_no_mmr), NA_integer_, as.integer(utd_no_mmr == 0)),
    mmr_not_utd = if_else(is.na(mmr_utd), NA_integer_, as.integer(mmr_utd == 0)),
    high_individual_risk = if_else(is.na(individual_risk_score), NA_integer_, as.integer(individual_risk_score >= high_individual_risk_threshold)),
    reminder_location = clean_display_text(county),
    reminder_latitude = latitude,
    reminder_longitude = longitude,
    reminder_coordinate_source = if_else(
      !is.na(latitude) & !is.na(longitude),
      "Palau county coordinate workbook",
      "Missing"
    )
  ) %>%
  select(-region.x, -region.y)

# QA the county hierarchy and reminder coordinate sources.
write_csv(
  patient %>%
    count(geography_assignment_source, county, clinic, name = "children", sort = TRUE),
  geography_qa_output_file,
  na = ""
)
write_csv(
  patient %>% count(reminder_coordinate_source, reminder_location, name = "children", sort = TRUE),
  reminder_qa_output_file,
  na = ""
)

# ==============================================================================
# 6. Community summary
# ==============================================================================

community_metrics <- patient %>%
  filter(county_join %in% valid_counties) %>%
  group_by(
    community_id,
    state,
    region,
    county,
    county_join
  ) %>%
  summarise(
    child_population = n(),
    children_not_utd = sum(not_utd == 1, na.rm = TRUE),
    proportion_not_utd = 100 * safe_mean(not_utd),
    utd_coverage = 100 * safe_mean(utd),
    utd_no_mmr_coverage = 100 * safe_mean(utd_no_mmr),
    mmr_eligible_n = sum(!is.na(mmr_utd)),
    mmr_not_utd_n = sum(mmr_not_utd == 1, na.rm = TRUE),
    mmr_coverage = 100 * safe_mean(mmr_utd),
    
    median_months_since_vax = safe_median(
      if_else(
        not_utd == 1,
        months_since_last_vax,
        NA_real_
      )
    ),
    
    mean_individual_risk = safe_mean(individual_risk_score),
    median_individual_risk = safe_median(individual_risk_score),
    p75_individual_risk = safe_quantile(
      individual_risk_score,
      0.75
    ),
    
    high_risk_n = sum(
      high_individual_risk == 1,
      na.rm = TRUE
    ),
    
    high_risk_percent =
      100 * safe_mean(high_individual_risk),
    
    .groups = "drop"
  )

# Begin with the county workbook so selectors include every Palau county even
# when a county has no patients in the current analytic dataset.
community_summary <- county_lookup %>%
  transmute(
    community_id = paste("PALAU", county_join, sep = "__"),
    state = "PALAU",
    region = "PALAU",
    county = county_join,
    county_join,
    latitude = latitude_gis,
    longitude = longitude_gis,
    census_total,
    area_m2,
    area_km2,
    population_density_km2
  ) %>%
  left_join(
    community_metrics %>%
      select(-state, -region, -county),
    by = c("community_id", "county_join")
  ) %>%
  
  mutate(
    across(
      c(
        child_population, children_not_utd, mmr_eligible_n,
        mmr_not_utd_n, high_risk_n
      ),
      ~coalesce(.x, 0L)
    ),

    # Retained as empty compatibility fields for the dashboard score structure.
    census_under5 = NA_real_,
    under5_density_km2 = NA_real_,
    
    remoteness = coalesce(
      as.numeric(palau_remoteness_lookup[county_join]),
      0
    ),
    
    access_component = 100 * remoteness,
    
    median_risk_component =
      pmin(pmax(median_individual_risk, 0), 100),
    
    p75_risk_component =
      pmin(pmax(p75_individual_risk, 0), 100),
    
    high_risk_percent_component =
      pmin(pmax(high_risk_percent, 0), 100),
    
    community_patient_risk_component =
      0.60 * median_risk_component +
      0.20 * p75_risk_component +
      0.20 * high_risk_percent_component,
    
    # Compress large differences between densely and sparsely populated states.
    log_population_density = case_when(
      !is.na(population_density_km2) &
        population_density_km2 >= 0 ~
        log1p(population_density_km2),
      
      TRUE ~ NA_real_
    )
  ) %>%
  
  mutate(
    # Relative density ranking among Palau counties.
    density_percentile_component =
      percentile_100(log_population_density),
    
    # Sparse communities receive a higher operational-access score.
    low_density_component = case_when(
      !is.na(density_percentile_component) ~
        100 - density_percentile_component,
      
      TRUE ~ NA_real_
    ),
    
    # Dense communities receive a higher transmission-potential score.
    high_density_component = case_when(
      !is.na(density_percentile_component) ~
        density_percentile_component,
      
      TRUE ~ NA_real_
    ),
    
    population_density_missing =
      is.na(population_density_km2),
    
    # Neutral fallback only if a population/area match is missing.
    low_density_component_for_score =
      coalesce(low_density_component, 50),
    
    high_density_component_for_score =
      coalesce(high_density_component, 50),
    
    # Transmission potential is high only when BOTH:
    # 1. the community is relatively dense, and
    # 2. patient-level vaccination risk is high.
    transmission_component =
      high_density_component_for_score *
      community_patient_risk_component / 100,
    
    burden_component =
      rescale_100(log1p(high_risk_n)),
    
    priority_score = round(
      0.90 * community_patient_risk_component +
        0.010 * access_component +
        0.010 * low_density_component_for_score +
        0.040 * transmission_component +
        0.040 * burden_component,
      1
    ),
    
    priority_rank =
      min_rank(desc(priority_score))
  )

# ------------------------------------------------------------------------------
# Convert continuous priority scores to Low / Moderate / High groups
# ------------------------------------------------------------------------------

priority_cutpoints <- quantile(
  community_summary$priority_score,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE,
  names = FALSE
)

community_summary <- community_summary %>%
  mutate(
    priority_group = factor(
      case_when(
        is.na(priority_score) ~ NA_character_,

        # Highest score tertile.
        priority_score > priority_cutpoints[2] ~ "High",

        # Severe coverage gaps are always High.
        utd_coverage < 30 | mmr_coverage < 50 ~ "High",

        # Remote communities with substantial coverage gaps
        # cannot be classified as Low.
        remoteness == 1 &
          (utd_coverage < 50 | mmr_coverage < 50) ~ "Moderate",

        # Middle score tertile.
        priority_score > priority_cutpoints[1] ~ "Moderate",

        TRUE ~ "Low"
      ),
      levels = c("Low", "Moderate", "High"),
      ordered = TRUE
    ),

    # Population/area fields now participate in dashboard readiness QA.
    data_quality_flag = case_when(
      is.na(latitude) | is.na(longitude) ~ "Missing coordinates",
      is.na(census_total) ~ "Missing Census match",
      is.na(area_km2) ~ "Missing land area",
      is.na(population_density_km2) ~ "Missing population density",
      child_population < 5 ~ "Small denominator",
      TRUE ~ "Ready"
    ),

    # Routing itself only requires usable map coordinates.
    routing_ready = !is.na(latitude) & !is.na(longitude)
  ) %>%
  arrange(priority_rank, county)

# ==============================================================================
# 7. Vaccine and product needs
# ==============================================================================

vaccine_map <- c(DTaP = "dtap_utd", IPV = "ipv_utd", MMR = "mmr_utd", HepB = "hepb_utd", Hib = "hib_utd", PCV = "pcv_utd")
available_vaccine_map <- vaccine_map[vaccine_map %in% names(patient)]

community_vaccine_needs <- lapply(names(available_vaccine_map), function(vaccine_name) {
  vaccine_variable <- available_vaccine_map[[vaccine_name]]
  patient %>%
    filter(county_join %in% valid_counties) %>%
    group_by(community_id, state, region, county) %>%
    summarise(
      eligible_children = sum(!is.na(.data[[vaccine_variable]])),
      children_due = sum(.data[[vaccine_variable]] == 0, na.rm = TRUE),
      children_utd = sum(.data[[vaccine_variable]] == 1, na.rm = TRUE),
      vaccine_coverage = if_else(eligible_children > 0, 100 * children_utd / eligible_children, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(vaccine = vaccine_name)
}) %>%
  bind_rows() %>%
  select(community_id, state, region, county, vaccine, eligible_children, children_utd, children_due, vaccine_coverage) %>%
  arrange(county, vaccine)

product_order <- c("PedVaxHib", "Vaxelis", "Pediarix", "DTaP_Single", "IPV_Single", "MMR_Single", "Prevnar", "RotaTeq")
available_product_variables <- intersect(product_order, names(patient))

if (length(available_product_variables) > 0) {
  community_product_needs <- patient %>%
    filter(county_join %in% valid_counties) %>%
    select(community_id, state, region, county, all_of(available_product_variables)) %>%
    pivot_longer(cols = all_of(available_product_variables), names_to = "product", values_to = "product_needed") %>%
    group_by(community_id, state, region, county, product) %>%
    summarise(doses_needed = sum(product_needed == 1, na.rm = TRUE), .groups = "drop") %>%
    complete(nesting(community_id, state, region, county), product = product_order, fill = list(doses_needed = 0)) %>%
    mutate(product = factor(product, levels = product_order)) %>%
    arrange(county, product) %>%
    mutate(product = as.character(product))
} else {
  community_product_needs <- tibble(
    community_id = character(), state = character(), region = character(), county = character(),
    product = character(), doses_needed = numeric()
  )
}

# ==============================================================================
# 8. Patient dashboard dataset and outputs
# ==============================================================================

patient_dashboard <- patient

write_csv(community_summary, community_output_file, na = "")
write_csv(community_vaccine_needs, vaccine_output_file, na = "")
write_csv(community_product_needs, product_output_file, na = "")
write_csv(patient_dashboard, patient_output_file, na = "")

message("Palau data preparation complete.")
message("Using patient dataset: ", normalizePath(patient_file, winslash = "/", mustWork = FALSE))
message("Created: ", community_output_file)
message("Created: ", vaccine_output_file)
message("Created: ", product_output_file)
message("Created: ", patient_output_file)
message("QA: ", geography_qa_output_file)
message("QA: ", reminder_qa_output_file)
