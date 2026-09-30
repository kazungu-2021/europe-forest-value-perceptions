# ================================================================
# FOREST VALUE PERCEPTIONS ACROSS EUROPE
# FINAL CLEAN REPRODUCIBLE MAJOR-REVISION WORKFLOW
# ================================================================
# This script consolidates the validated corrected analysis only.
# Run from a fresh R session from the project root.
# ================================================================

# ================================================================
# 01. SETUP AND PACKAGES
# ================================================================

required_packages <- c(
  "dplyr",
  "tidyr",
  "lme4",
  "lmerTest",
  "broom.mixed",
  "openxlsx",
  "ggplot2",
  "clubSandwich",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "ggspatial",
  "psych"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "The following required packages are not installed: ",
    paste(missing_packages, collapse = ", ")
  )
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

set.seed(12345)

cat(
  "Setup complete.\n",
  "R version: ", R.version.string, "\n",
  "Required packages loaded: ", length(required_packages), "\n",
  sep = ""
)

# ================================================================
# 02. OUTPUT DIRECTORIES
# ================================================================

base_dir <- "major_revision"
final_dir <- file.path(base_dir, "final_results")
table_dir <- file.path(final_dir, "tables")
figure_dir <- file.path(final_dir, "figures")
supp_dir <- file.path(final_dir, "supplementary")
public_dir <- file.path(final_dir, "public_data")
model_dir <- file.path(base_dir, "models_corrected")

output_dirs <- c(
  base_dir,
  final_dir,
  table_dir,
  figure_dir,
  supp_dir,
  public_dir,
  model_dir
)

invisible(
  lapply(
    output_dirs,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

dir_check <- data.frame(
  directory = output_dirs,
  exists = dir.exists(output_dirs)
)

print(dir_check, row.names = FALSE)

if (!all(dir_check$exists)) {
  stop("One or more required output directories could not be created.")
}

cat("\nOutput directory structure successfully created.\n")

# ================================================================
# 03. IMPORT AND INITIAL VALIDATION
# ================================================================

data_file <- file.path(
  "new_2026 analysis",
  "combined_final_analysis_with_domains_paper1.rds"
)

if (!file.exists(data_file)) {
  stop(
    "Analytical data file not found at: ",
    data_file,
    "\nCheck the working directory with getwd()."
  )
}

combined <- readRDS(data_file)

cat("\nAnalytical dataset loaded successfully.\n")
cat("Stored rows:", nrow(combined), "\n")
cat("Stored variables:", ncol(combined), "\n")

required_vars <- c(
  "country",
  "region",
  "socio_cultural",
  "provisioning",
  "regulating",
  "weight",
  "weight_norm"
)

missing_vars <- setdiff(required_vars, names(combined))

if (length(missing_vars) > 0) {
  stop(
    "Required variables missing from source data: ",
    paste(missing_vars, collapse = ", ")
  )
}

cat("\nAll required core variables are present.\n")

cat("\nCountries stored in source data:\n")
print(sort(unique(as.character(combined$country))))

cat("\nMissingness in core analytical variables:\n")

core_missingness <- combined %>%
  summarise(
    socio_cultural_missing = sum(is.na(socio_cultural)),
    provisioning_missing = sum(is.na(provisioning)),
    regulating_missing = sum(is.na(regulating)),
    weight_norm_missing = sum(is.na(weight_norm))
  )

print(core_missingness)

cat(
  "\nRows with at least one missing outcome or missing normalised weight:",
  sum(
    is.na(combined$socio_cultural) |
      is.na(combined$provisioning) |
      is.na(combined$regulating) |
      is.na(combined$weight_norm)
  ),
  "\n"
)

# ================================================================
# 04. CREATE FINAL ANALYTICAL DATASET
# ================================================================

non_analytical_rows <- combined %>%
  filter(
    is.na(socio_cultural) |
      is.na(provisioning) |
      is.na(regulating) |
      is.na(weight_norm)
  )

cat("\nNon-analytical rows identified:", nrow(non_analytical_rows), "\n")

cat("\nDistribution of non-analytical rows by country:\n")
print(
  non_analytical_rows %>%
    count(country, name = "N")
)

analysis_data <- combined %>%
  filter(
    !is.na(socio_cultural),
    !is.na(provisioning),
    !is.na(regulating),
    !is.na(weight_norm)
  )

cat("\nFinal analytical N:", nrow(analysis_data), "\n")

country_n <- analysis_data %>%
  count(country, name = "N") %>%
  arrange(country)

cat("\nFinal analytical sample by country:\n")
print(country_n, n = Inf)

# Formal reproducibility checks
stopifnot(
  nrow(non_analytical_rows) == 12,
  nrow(analysis_data) == 12460,
  dplyr::n_distinct(analysis_data$country) == 12,
  !anyNA(analysis_data$socio_cultural),
  !anyNA(analysis_data$provisioning),
  !anyNA(analysis_data$regulating),
  !anyNA(analysis_data$weight_norm)
)

cat("\nAnalytical sample validation passed.\n")

# ================================================================
# 05. FOREST EUROPE FIVE-REGION CLASSIFICATION
# ================================================================

analysis_data <- analysis_data %>%
  mutate(
    # Retain original regional classification for traceability
    region_original = region,
    
    # Five Forest Europe macro-regions
    region_fe = case_when(
      country %in% c(
        "Denmark", "Sweden"
      ) ~ "North Europe",
      
      country %in% c(
        "France", "Germany", "Netherlands", "Scotland"
      ) ~ "Central-West Europe",
      
      country %in% c(
        "Czech", "Romania"
      ) ~ "Central-East Europe",
      
      country %in% c(
        "Italy", "Spain"
      ) ~ "South-West Europe",
      
      country %in% c(
        "Croatia", "Serbia"
      ) ~ "South-East Europe",
      
      TRUE ~ NA_character_
    ),
    
    region_fe = factor(
      region_fe,
      levels = c(
        "North Europe",
        "Central-West Europe",
        "Central-East Europe",
        "South-West Europe",
        "South-East Europe"
      )
    )
  )

# ------------------------------------------------
# Validate country-to-region mapping
# ------------------------------------------------

country_region_check <- analysis_data %>%
  distinct(country, region_fe) %>%
  arrange(region_fe, country)

cat("\nCountry-to-macro-region assignment:\n")
print(country_region_check, n = Inf)

# ------------------------------------------------
# Number of countries and respondents by region
# ------------------------------------------------

region_check <- analysis_data %>%
  group_by(region_fe) %>%
  summarise(
    countries = n_distinct(country),
    respondents = n(),
    .groups = "drop"
  )

cat("\nForest Europe macro-region structure:\n")
print(region_check, n = Inf)

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

stopifnot(
  !anyNA(analysis_data$region_fe),
  nlevels(analysis_data$region_fe) == 5,
  n_distinct(analysis_data$region_fe) == 5,
  
  sum(analysis_data$region_fe == "North Europe") == 2064,
  sum(analysis_data$region_fe == "Central-West Europe") == 4192,
  sum(analysis_data$region_fe == "Central-East Europe") == 2067,
  sum(analysis_data$region_fe == "South-West Europe") == 2048,
  sum(analysis_data$region_fe == "South-East Europe") == 2089
)

cat("\nFive-region classification validation passed.\n")

# ================================================================
# 06. CREATE FINAL MODEL PREDICTORS
# ================================================================

analysis_data <- analysis_data %>%
  mutate(
    
    # ------------------------------------------------------------
    # GENDER
    #
    # Coding differs across source datasets:
    # Denmark:  gender_dk    1 = Female, 2 = Male
    # Scotland: gender_clean 1 = Male,   2 = Female
    # Others:   gender       1 = Male,   2 = Female
    #
    # Reference = Male
    # ------------------------------------------------------------
    
    gender_model = case_when(
      
      country == "Denmark" &
        gender_dk == 1 ~ "Female",
      
      country == "Denmark" &
        gender_dk == 2 ~ "Male",
      
      country == "Scotland" &
        gender_clean == 1 ~ "Male",
      
      country == "Scotland" &
        gender_clean == 2 ~ "Female",
      
      !country %in% c("Denmark", "Scotland") &
        gender == 1 ~ "Male",
      
      !country %in% c("Denmark", "Scotland") &
        gender == 2 ~ "Female",
      
      TRUE ~ NA_character_
    ),
    
    gender_model = factor(
      gender_model,
      levels = c("Male", "Female")
    ),
    
    # ------------------------------------------------------------
    # AGE
    # Reference = 18-26 years
    # ------------------------------------------------------------
    
    age_model = case_when(
      age >= 18 & age <= 26 ~ "18-26",
      age >= 27 & age <= 37 ~ "27-37",
      age >= 38 & age <= 48 ~ "38-48",
      age >= 49 & age <= 59 ~ "49-59",
      age >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    
    age_model = factor(
      age_model,
      levels = c(
        "18-26",
        "27-37",
        "38-48",
        "49-59",
        "60+"
      )
    ),
    
    # ------------------------------------------------------------
    # EDUCATION
    # Reference = Level 1: no formal education
    # ------------------------------------------------------------
    
    education_model = case_when(
      as.character(education_rec) == "1" ~ "Level 1",
      as.character(education_rec) == "2" ~ "Level 2",
      as.character(education_rec) == "3" ~ "Level 3",
      as.character(education_rec) == "4" ~ "Level 4",
      as.character(education_rec) == "5" ~ "Level 5",
      as.character(education_rec) == "6" ~ "Level 6",
      TRUE ~ NA_character_
    ),
    
    education_model = factor(
      education_model,
      levels = c(
        "Level 1",
        "Level 2",
        "Level 3",
        "Level 4",
        "Level 5",
        "Level 6"
      )
    ),
    
    # ------------------------------------------------------------
    # LENGTH OF RESIDENCE
    #
    # 1 = 0-5 years
    # 2 = 6-10 years
    # 3 = 11-15 years
    # 4 = 16-20 years
    # 5 = 21-25 years
    # 6 = 26-30 years
    # 7 = >30 years
    #
    # Genuine nonresponse retained as "Missing"
    # Reference = 0-5 years
    # ------------------------------------------------------------
    
    residence_model = case_when(
      as.character(q24_yks) == "1" ~ "1",
      as.character(q24_yks) == "2" ~ "2",
      as.character(q24_yks) == "3" ~ "3",
      as.character(q24_yks) == "4" ~ "4",
      as.character(q24_yks) == "5" ~ "5",
      as.character(q24_yks) == "6" ~ "6",
      as.character(q24_yks) == "7" ~ "7",
      is.na(q24_yks) ~ "Missing",
      TRUE ~ NA_character_
    ),
    
    residence_model = factor(
      residence_model,
      levels = c(
        "1", "2", "3", "4",
        "5", "6", "7", "Missing"
      )
    ),
    
    # ------------------------------------------------------------
    # FOREST OWNERSHIP
    # 1 = Yes, 2 = No
    # Reference = No
    # ------------------------------------------------------------
    
    ownership_model = case_when(
      as.character(q25) == "1" ~ "Yes",
      as.character(q25) == "2" ~ "No",
      TRUE ~ NA_character_
    ),
    
    ownership_model = factor(
      ownership_model,
      levels = c("No", "Yes")
    ),
    
    # ------------------------------------------------------------
    # SOCIAL PROXIMITY TO FOREST OWNERSHIP
    # Relatives/friends own forest land
    # 1 = Yes, 2 = No, 97 = Don't know
    # Reference = No
    # ------------------------------------------------------------
    
    social_model = case_when(
      as.character(q26) == "1" ~ "Yes",
      as.character(q26) == "2" ~ "No",
      as.character(q26) == "97" ~ "Don't know",
      TRUE ~ NA_character_
    ),
    
    social_model = factor(
      social_model,
      levels = c(
        "No",
        "Yes",
        "Don't know"
      )
    )
  )

# ================================================================
# 06A. VALIDATE RECODED PREDICTORS
# ================================================================

cat("\nGender:\n")
print(table(analysis_data$gender_model, useNA = "ifany"))

cat("\nAge group:\n")
print(table(analysis_data$age_model, useNA = "ifany"))

cat("\nEducation:\n")
print(table(analysis_data$education_model, useNA = "ifany"))

cat("\nResidence:\n")
print(table(analysis_data$residence_model, useNA = "ifany"))

cat("\nForest ownership:\n")
print(table(analysis_data$ownership_model, useNA = "ifany"))

cat("\nSocial proximity to forest ownership:\n")
print(table(analysis_data$social_model, useNA = "ifany"))

cat("\nGender by country:\n")
print(
  table(
    analysis_data$country,
    analysis_data$gender_model,
    useNA = "ifany"
  )
)

# ------------------------------------------------------------
# Formal checks against validated coding
# ------------------------------------------------------------

stopifnot(
  sum(analysis_data$gender_model == "Male") == 5991,
  sum(analysis_data$gender_model == "Female") == 6469,
  sum(is.na(analysis_data$gender_model)) == 0,
  
  sum(is.na(analysis_data$age_model)) == 0,
  sum(is.na(analysis_data$education_model)) == 0,
  
  sum(analysis_data$residence_model == "Missing") == 1132,
  
  sum(analysis_data$social_model == "Yes") == 2668,
  sum(analysis_data$social_model == "No") == 8662,
  sum(analysis_data$social_model == "Don't know") == 1130
)

cat("\nFinal predictor recoding validation passed.\n")


# ================================================================
# 07. VERIFY COUNTRY-NORMALISED ANALYTICAL WEIGHTS
# ================================================================

weight_check <- analysis_data %>%
  group_by(country) %>%
  summarise(
    N = n(),
    mean_weight_norm = mean(weight_norm),
    sum_weight_norm = sum(weight_norm),
    min_weight_norm = min(weight_norm),
    max_weight_norm = max(weight_norm),
    .groups = "drop"
  )

cat("\nCountry-level normalised-weight verification:\n")
print(weight_check, n = Inf)

# Country-normalised weights should have mean 1
# and sum approximately to the observed country sample size.

mean_check <- all(
  abs(weight_check$mean_weight_norm - 1) < 1e-10
)

sum_check <- all(
  abs(weight_check$sum_weight_norm - weight_check$N) < 1e-8
)

positive_check <- all(
  analysis_data$weight_norm > 0
)

missing_check <- !anyNA(
  analysis_data$weight_norm
)

cat("\nMean weight = 1 within every country:", mean_check, "\n")
cat("Sum of weights = country N:", sum_check, "\n")
cat("All weights are positive:", positive_check, "\n")
cat("No missing analytical weights:", missing_check, "\n")

stopifnot(
  mean_check,
  sum_check,
  positive_check,
  missing_check
)

cat("\nCountry-normalised analytical-weight validation passed.\n")

# ================================================================
# 08. TABLE 1: WEIGHTED SAMPLE CHARACTERISTICS BY MACRO-REGION
# ================================================================

table1_data <- analysis_data %>%
  mutate(
    # Levels 5-6:
    # post-secondary non-tertiary + tertiary
    postsecondary_tertiary = if_else(
      as.character(education_rec) %in% c("5", "6"),
      1,
      0
    ),
    
    female = if_else(
      gender_model == "Female",
      1,
      0
    ),
    
    forest_owner = case_when(
      as.character(q25) == "1" ~ 1,
      as.character(q25) == "2" ~ 0,
      TRUE ~ NA_real_
    )
  )

# ------------------------------------------------
# Macro-region summaries
# ------------------------------------------------

table1_region <- table1_data %>%
  group_by(region_fe) %>%
  summarise(
    `Countries (n)` = n_distinct(country),
    
    `Respondents (n)` = n(),
    
    `Age mean` = weighted.mean(
      age,
      weight_norm,
      na.rm = TRUE
    ),
    
    `Age SD` = sqrt(
      weighted.mean(
        (
          age -
            weighted.mean(
              age,
              weight_norm,
              na.rm = TRUE
            )
        )^2,
        weight_norm,
        na.rm = TRUE
      )
    ),
    
    `Female (%)` =
      100 * weighted.mean(
        female,
        weight_norm,
        na.rm = TRUE
      ),
    
    `Post-secondary/tertiary education (%)` =
      100 * weighted.mean(
        postsecondary_tertiary,
        weight_norm,
        na.rm = TRUE
      ),
    
    `Forest owners (%)` =
      100 * weighted.mean(
        forest_owner,
        weight_norm,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

# ------------------------------------------------
# Overall sample
# ------------------------------------------------

table1_overall <- table1_data %>%
  summarise(
    region_fe = "Overall",
    
    `Countries (n)` = n_distinct(country),
    
    `Respondents (n)` = n(),
    
    `Age mean` = weighted.mean(
      age,
      weight_norm,
      na.rm = TRUE
    ),
    
    `Age SD` = sqrt(
      weighted.mean(
        (
          age -
            weighted.mean(
              age,
              weight_norm,
              na.rm = TRUE
            )
        )^2,
        weight_norm,
        na.rm = TRUE
      )
    ),
    
    `Female (%)` =
      100 * weighted.mean(
        female,
        weight_norm,
        na.rm = TRUE
      ),
    
    `Post-secondary/tertiary education (%)` =
      100 * weighted.mean(
        postsecondary_tertiary,
        weight_norm,
        na.rm = TRUE
      ),
    
    `Forest owners (%)` =
      100 * weighted.mean(
        forest_owner,
        weight_norm,
        na.rm = TRUE
      )
  )

# ------------------------------------------------
# Final manuscript Table 1
# ------------------------------------------------

table1_final <- bind_rows(
  table1_region %>%
    mutate(region_fe = as.character(region_fe)),
  table1_overall
) %>%
  mutate(
    `Age, mean (SD)` =
      sprintf(
        "%.1f (%.1f)",
        `Age mean`,
        `Age SD`
      ),
    
    `Female (%)` =
      round(`Female (%)`, 1),
    
    `Post-secondary/tertiary education (%)` =
      round(
        `Post-secondary/tertiary education (%)`,
        1
      ),
    
    `Forest owners (%)` =
      round(`Forest owners (%)`, 1)
  ) %>%
  select(
    `Macro-region` = region_fe,
    `Countries (n)`,
    `Respondents (n)`,
    `Age, mean (SD)`,
    `Female (%)`,
    `Post-secondary/tertiary education (%)`,
    `Forest owners (%)`
  )

cat("\nFINAL WEIGHTED TABLE 1\n\n")

print(
  as.data.frame(table1_final),
  row.names = FALSE
)

# ------------------------------------------------
# Reproducibility checks
# ------------------------------------------------

stopifnot(
  nrow(table1_final) == 6,
  
  table1_final$`Respondents (n)`[
    table1_final$`Macro-region` == "Overall"
  ] == 12460,
  
  table1_final$`Countries (n)`[
    table1_final$`Macro-region` == "Overall"
  ] == 12
)

cat("\nWeighted Table 1 constructed successfully.\n")

# ================================================================
# 08B. FINALISE AND EXPORT TABLE 1
# ================================================================

table1_final <- table1_final %>%
  rename(
    `Forest ownership (%)` = `Forest owners (%)`
  )

# ------------------------------------------------
# Save CSV
# ------------------------------------------------

table1_csv <- file.path(
  table_dir,
  "Table_1_sample_characteristics.csv"
)

write.csv(
  table1_final,
  table1_csv,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# ------------------------------------------------
# Save Excel
# ------------------------------------------------

table1_xlsx <- file.path(
  table_dir,
  "Table_1_sample_characteristics.xlsx"
)

openxlsx::write.xlsx(
  table1_final,
  table1_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------
# Verify saved files
# ------------------------------------------------

table1_files <- c(
  table1_csv,
  table1_xlsx
)

table1_file_check <- data.frame(
  file = table1_files,
  exists = file.exists(table1_files),
  size_bytes = file.info(table1_files)$size,
  modified = file.info(table1_files)$mtime
)

cat("\nTABLE 1 FILE VERIFICATION\n\n")
print(table1_file_check, row.names = FALSE)

stopifnot(
  all(table1_file_check$exists),
  all(table1_file_check$size_bytes > 0)
)

# ------------------------------------------------
# Read CSV back and verify key values
# ------------------------------------------------

table1_verify <- read.csv(
  table1_csv,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(table1_verify) == 6,
  table1_verify$`Respondents (n)`[
    table1_verify$`Macro-region` == "Overall"
  ] == 12460,
  table1_verify$`Female (%)`[
    table1_verify$`Macro-region` == "Overall"
  ] == 51.4,
  table1_verify$`Forest ownership (%)`[
    table1_verify$`Macro-region` == "South-East Europe"
  ] == 24.2
)

cat("\nTable 1 exported and read-back validation passed.\n")

# ================================================================
# 09. MACRO-REGIONAL FOREST-VALUE DESCRIPTIVE STATISTICS
# ================================================================

regional_values_long <- analysis_data %>%
  select(
    region_fe,
    country,
    weight_norm,
    socio_cultural,
    provisioning,
    regulating
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      socio_cultural,
      provisioning,
      regulating
    ),
    names_to = "domain",
    values_to = "score"
  ) %>%
  mutate(
    domain = factor(
      domain,
      levels = c(
        "socio_cultural",
        "provisioning",
        "regulating"
      ),
      labels = c(
        "Socio-cultural",
        "Provisioning",
        "Regulating/supporting"
      )
    )
  )

regional_value_stats <- regional_values_long %>%
  group_by(region_fe, domain) %>%
  summarise(
    n = sum(!is.na(score)),
    
    weighted_mean = weighted.mean(
      score,
      weight_norm,
      na.rm = TRUE
    ),
    
    weighted_sd = sqrt(
      weighted.mean(
        (
          score -
            weighted.mean(
              score,
              weight_norm,
              na.rm = TRUE
            )
        )^2,
        weight_norm,
        na.rm = TRUE
      )
    ),
    
    # Kish effective sample size
    effective_n =
      sum(weight_norm[!is.na(score)])^2 /
      sum(weight_norm[!is.na(score)]^2),
    
    .groups = "drop"
  ) %>%
  mutate(
    weighted_se =
      weighted_sd / sqrt(effective_n),
    
    ci_lower =
      weighted_mean - 1.96 * weighted_se,
    
    ci_upper =
      weighted_mean + 1.96 * weighted_se
  )

cat("\nMACRO-REGIONAL FOREST-VALUE STATISTICS\n\n")

print(
  regional_value_stats %>%
    select(
      region_fe,
      domain,
      n,
      weighted_mean,
      weighted_sd,
      effective_n,
      ci_lower,
      ci_upper
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Basic validation
# ------------------------------------------------

stopifnot(
  nrow(regional_value_stats) == 15,
  all(regional_value_stats$n ==
        rep(c(2064, 4192, 2067, 2048, 2089), each = 3)),
  !anyNA(regional_value_stats$weighted_mean),
  !anyNA(regional_value_stats$weighted_sd),
  !anyNA(regional_value_stats$ci_lower),
  !anyNA(regional_value_stats$ci_upper)
)

cat("\nMacro-regional descriptive-statistics validation passed.\n")


# ================================================================
# 10. COUNTRY-LEVEL FOREST-VALUE DESCRIPTIVE STATISTICS
# ================================================================

country_values_long <- analysis_data %>%
  select(
    region_fe,
    country,
    weight_norm,
    socio_cultural,
    provisioning,
    regulating
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      socio_cultural,
      provisioning,
      regulating
    ),
    names_to = "domain",
    values_to = "score"
  ) %>%
  mutate(
    domain = factor(
      domain,
      levels = c(
        "socio_cultural",
        "provisioning",
        "regulating"
      ),
      labels = c(
        "Socio-cultural",
        "Provisioning",
        "Regulating/supporting"
      )
    )
  )

country_value_stats <- country_values_long %>%
  group_by(
    region_fe,
    country,
    domain
  ) %>%
  summarise(
    n = sum(!is.na(score)),
    
    weighted_mean = weighted.mean(
      score,
      weight_norm,
      na.rm = TRUE
    ),
    
    weighted_sd = sqrt(
      weighted.mean(
        (
          score -
            weighted.mean(
              score,
              weight_norm,
              na.rm = TRUE
            )
        )^2,
        weight_norm,
        na.rm = TRUE
      )
    ),
    
    # Kish effective sample size
    effective_n =
      sum(weight_norm[!is.na(score)])^2 /
      sum(weight_norm[!is.na(score)]^2),
    
    .groups = "drop"
  ) %>%
  mutate(
    weighted_se =
      weighted_sd / sqrt(effective_n),
    
    ci_lower =
      weighted_mean - 1.96 * weighted_se,
    
    ci_upper =
      weighted_mean + 1.96 * weighted_se
  )

# ------------------------------------------------
# Display country means in compact form
# ------------------------------------------------

country_means_check <- country_value_stats %>%
  select(
    country,
    domain,
    weighted_mean
  ) %>%
  tidyr::pivot_wider(
    names_from = domain,
    values_from = weighted_mean
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

cat("\nCOUNTRY-LEVEL WEIGHTED MEANS\n\n")

print(
  country_means_check,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

expected_country_n <- c(
  Croatia = 1024,
  Czech = 1033,
  Denmark = 1032,
  France = 1028,
  Germany = 1011,
  Italy = 1042,
  Netherlands = 1145,
  Romania = 1034,
  Scotland = 1008,
  Serbia = 1065,
  Spain = 1006,
  Sweden = 1032
)

country_n_check <- country_value_stats %>%
  distinct(country, n) %>%
  mutate(
    expected_n =
      expected_country_n[as.character(country)]
  )

stopifnot(
  nrow(country_value_stats) == 36,
  nrow(country_n_check) == 12,
  all(country_n_check$n ==
        country_n_check$expected_n),
  !anyNA(country_value_stats$weighted_mean),
  !anyNA(country_value_stats$weighted_sd),
  !anyNA(country_value_stats$ci_lower),
  !anyNA(country_value_stats$ci_upper)
)

cat("\nCountry-level descriptive-statistics validation passed.\n")

# ================================================================
# 10B. SAVE VALIDATED DESCRIPTIVE STATISTICS
# ================================================================

regional_stats_file <- file.path(
  table_dir,
  "Regional_forest_value_statistics.csv"
)

country_stats_file <- file.path(
  table_dir,
  "Country_level_forest_value_statistics.csv"
)

# ------------------------------------------------
# Save full numerical results
# ------------------------------------------------

write.csv(
  regional_value_stats,
  regional_stats_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  country_value_stats,
  country_stats_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------
# Read files back
# ------------------------------------------------

regional_stats_verify <- read.csv(
  regional_stats_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

country_stats_verify <- read.csv(
  country_stats_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ------------------------------------------------
# File verification
# ------------------------------------------------

descriptive_files <- c(
  regional_stats_file,
  country_stats_file
)

descriptive_file_check <- data.frame(
  file = descriptive_files,
  exists = file.exists(descriptive_files),
  size_bytes = file.info(descriptive_files)$size,
  modified = file.info(descriptive_files)$mtime
)

cat("\nDESCRIPTIVE-STATISTICS FILE VERIFICATION\n\n")

print(
  descriptive_file_check,
  row.names = FALSE
)

# ------------------------------------------------
# Content verification
# ------------------------------------------------

stopifnot(
  all(descriptive_file_check$exists),
  all(descriptive_file_check$size_bytes > 0),
  
  nrow(regional_stats_verify) == 15,
  nrow(country_stats_verify) == 36,
  
  all(c(
    "weighted_mean",
    "weighted_sd",
    "effective_n",
    "ci_lower",
    "ci_upper"
  ) %in% names(regional_stats_verify)),
  
  all(c(
    "weighted_mean",
    "weighted_sd",
    "effective_n",
    "ci_lower",
    "ci_upper"
  ) %in% names(country_stats_verify))
)

# ------------------------------------------------
# Verify selected known estimates
# ------------------------------------------------

check_ce_prov <- regional_stats_verify %>%
  filter(
    region_fe == "Central-East Europe",
    domain == "Provisioning"
  ) %>%
  pull(weighted_mean)

check_romania_socio <- country_stats_verify %>%
  filter(
    country == "Romania",
    domain == "Socio-cultural"
  ) %>%
  pull(weighted_mean)

check_czech_prov <- country_stats_verify %>%
  filter(
    country == "Czech",
    domain == "Provisioning"
  ) %>%
  pull(weighted_mean)

stopifnot(
  length(check_ce_prov) == 1,
  length(check_romania_socio) == 1,
  length(check_czech_prov) == 1,
  
  abs(check_ce_prov - 3.742) < 0.001,
  abs(check_romania_socio - 4.336) < 0.001,
  abs(check_czech_prov - 4.031) < 0.001
)

cat(
  "\nRegional and country descriptive statistics ",
  "exported and read-back validation passed.\n",
  sep = ""
)

# ================================================================
# 11A. PREPARE DATA FOR FIGURE 1
# Spatial distribution of survey respondents
# ================================================================

# ------------------------------------------------
# Convert original coordinate fields to numeric
# using the final analytical sample
# ------------------------------------------------

map_dat <- analysis_data %>%
  mutate(
    latitude =
      suppressWarnings(
        as.numeric(map_var_lat_1)
      ),
    
    longitude =
      suppressWarnings(
        as.numeric(map_var_lng_1)
      )
  )

# ------------------------------------------------
# Coordinate availability
# ------------------------------------------------

coordinate_check <- map_dat %>%
  summarise(
    analytical_N = n(),
    
    latitude_available =
      sum(!is.na(latitude)),
    
    longitude_available =
      sum(!is.na(longitude)),
    
    complete_coordinates =
      sum(
        !is.na(latitude) &
          !is.na(longitude)
      )
  )

cat("\nCOORDINATE AVAILABILITY\n\n")

print(coordinate_check)

# ------------------------------------------------
# Final displayed geographic extent
#
# Latitude:   27 to 72
# Longitude: -20 to 35
#
# This is a display-window filter only.
# Respondents are NOT filtered against country polygons.
# ------------------------------------------------

figure1_points <- map_dat %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    latitude >= 27,
    latitude <= 72,
    longitude >= -20,
    longitude <= 35
  )

# ------------------------------------------------
# Plotted respondents by macro-region
# ------------------------------------------------

figure1_region_check <- figure1_points %>%
  count(
    region_fe,
    name = "plotted_respondents"
  ) %>%
  mutate(
    analytical_region_N = case_when(
      region_fe == "North Europe" ~ 2064L,
      region_fe == "Central-West Europe" ~ 4192L,
      region_fe == "Central-East Europe" ~ 2067L,
      region_fe == "South-West Europe" ~ 2048L,
      region_fe == "South-East Europe" ~ 2089L
    ),
    
    percent_with_coordinates =
      100 *
      plotted_respondents /
      analytical_region_N
  )

cat("\nFIGURE 1 RESPONDENTS BY MACRO-REGION\n\n")

print(
  figure1_region_check,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Overall Figure 1 coverage
# ------------------------------------------------

figure1_n <- nrow(figure1_points)

figure1_pct <-
  100 * figure1_n / nrow(analysis_data)

cat(
  "\nTotal respondent locations to be plotted:",
  figure1_n,
  "\n"
)

cat(
  "Percentage of full analytical sample:",
  round(figure1_pct, 1),
  "%\n"
)

# ------------------------------------------------
# Formal validation against approved Figure 1
# ------------------------------------------------

stopifnot(
  nrow(analysis_data) == 12460,
  nrow(figure1_points) == 10740,
  round(figure1_pct, 1) == 86.2,
  n_distinct(figure1_points$region_fe) == 5,
  all(
    figure1_points$latitude >= 27 &
      figure1_points$latitude <= 72
  ),
  all(
    figure1_points$longitude >= -20 &
      figure1_points$longitude <= 35
  )
)

cat(
  "\nFigure 1 spatial-data validation passed.\n"
)


# ================================================================
# 11B. CONSTRUCT FINAL FIGURE 1
# Spatial distribution of survey respondents
# ================================================================

# ------------------------------------------------
# Natural Earth European basemap
# ------------------------------------------------

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

europe_sf <- world[
  world$continent == "Europe" |
    world$name == "Turkey",
]

# ------------------------------------------------
# Validate basemap
# ------------------------------------------------

cat("\nFIGURE 1 BASEMAP CHECK\n\n")

cat(
  "Basemap class:",
  paste(class(europe_sf), collapse = ", "),
  "\n"
)

cat(
  "Number of basemap features:",
  nrow(europe_sf),
  "\n"
)

cat(
  "CRS:",
  sf::st_crs(europe_sf)$input,
  "\n"
)

stopifnot(
  inherits(europe_sf, "sf"),
  nrow(europe_sf) > 0,
  nrow(figure1_points) == 10740
)

# ------------------------------------------------
# Construct final Figure 1
# ------------------------------------------------

fig1 <- ggplot2::ggplot() +
  
  # European basemap
  ggplot2::geom_sf(
    data = europe_sf,
    fill = "grey95",
    colour = "grey70",
    linewidth = 0.30
  ) +
  
  # Respondent locations
  ggplot2::geom_point(
    data = figure1_points,
    ggplot2::aes(
      x = longitude,
      y = latitude,
      colour = region_fe
    ),
    size = 0.55,
    alpha = 0.60
  ) +
  
  # Geographic display extent
  ggplot2::coord_sf(
    xlim = c(-20, 35),
    ylim = c(27, 72),
    expand = FALSE
  ) +
  
  # Scale bar
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.8,
    line_width = 0.8
  ) +
  
  # North arrow
  ggspatial::annotation_north_arrow(
    location = "tl",
    which_north = "true",
    height = grid::unit(1.1, "cm"),
    width = grid::unit(1.1, "cm"),
    pad_x = grid::unit(0.35, "cm"),
    pad_y = grid::unit(0.35, "cm"),
    style =
      ggspatial::north_arrow_fancy_orienteering
  ) +
  
  # Labels
  ggplot2::labs(
    x = "Longitude",
    y = "Latitude",
    colour = "Forest Europe macro-region"
  ) +
  
  # Publication theme
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  
  ggplot2::theme(
    panel.grid.major =
      ggplot2::element_line(
        colour = "grey85",
        linewidth = 0.25
      ),
    
    panel.grid.minor =
      ggplot2::element_blank(),
    
    legend.position = "right",
    
    legend.title =
      ggplot2::element_text(
        size = 12,
        face = "bold"
      ),
    
    legend.text =
      ggplot2::element_text(
        size = 11
      ),
    
    axis.title =
      ggplot2::element_text(
        size = 12
      ),
    
    axis.text =
      ggplot2::element_text(
        size = 10
      ),
    
    plot.margin =
      ggplot2::margin(
        t = 8,
        r = 8,
        b = 8,
        l = 8
      )
  ) +
  
  # Larger symbols in legend only
  ggplot2::guides(
    colour =
      ggplot2::guide_legend(
        override.aes = list(
          size = 3.2,
          alpha = 1
        )
      )
  )

# ------------------------------------------------
# Display figure
# ------------------------------------------------

print(fig1)

cat(
  "\nFigure 1 object constructed successfully.\n"
)

# ================================================================
# 11C. EXPORT AND VERIFY FINAL FIGURE 1
# ================================================================

figure1_pdf <- file.path(
  figure_dir,
  "Figure_1_five_macroregions.pdf"
)

figure1_tiff <- file.path(
  figure_dir,
  "Figure_1_five_macroregions.tiff"
)

# ------------------------------------------------
# Save publication-quality PDF
# ------------------------------------------------

ggplot2::ggsave(
  filename = figure1_pdf,
  plot = fig1,
  width = 10,
  height = 7,
  units = "in",
  device = cairo_pdf
)

# ------------------------------------------------
# Save publication-quality TIFF
# 600 dpi with LZW compression
# ------------------------------------------------

ggplot2::ggsave(
  filename = figure1_tiff,
  plot = fig1,
  device = "tiff",
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# ------------------------------------------------
# Verify files
# ------------------------------------------------

figure1_files <- c(
  figure1_pdf,
  figure1_tiff
)

figure1_file_check <- data.frame(
  file = figure1_files,
  exists = file.exists(figure1_files),
  size_bytes = file.info(figure1_files)$size,
  size_mb = round(
    file.info(figure1_files)$size /
      (1024^2),
    2
  ),
  modified = file.info(figure1_files)$mtime
)

cat(
  "\nFIGURE 1 FILE VERIFICATION\n\n"
)

print(
  figure1_file_check,
  row.names = FALSE
)

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

stopifnot(
  all(figure1_file_check$exists),
  all(
    figure1_file_check$size_bytes > 0
  ),
  nrow(figure1_points) == 10740,
  round(
    100 *
      nrow(figure1_points) /
      nrow(analysis_data),
    1
  ) == 86.2
)

cat(
  "\nFigure 1 PDF and TIFF exported successfully.\n"
)

cat(
  "Plotted respondents:",
  nrow(figure1_points),
  "of",
  nrow(analysis_data),
  "(",
  round(
    100 *
      nrow(figure1_points) /
      nrow(analysis_data),
    1
  ),
  "% )\n"
)

# ================================================================
# 12A. CONSTRUCT FINAL FIGURE 2
# Macro-regional forest-value perceptions
# ================================================================

# ------------------------------------------------
# Prepare plotting object from already validated
# regional descriptive statistics
# ------------------------------------------------

figure2_data <- regional_value_stats %>%
  mutate(
    region_fe = factor(
      region_fe,
      levels = c(
        "North Europe",
        "Central-West Europe",
        "Central-East Europe",
        "South-West Europe",
        "South-East Europe"
      )
    ),
    domain = factor(
      domain,
      levels = c(
        "Socio-cultural",
        "Provisioning",
        "Regulating/supporting"
      )
    )
  )

# ------------------------------------------------
# Validate plotting data
# ------------------------------------------------

stopifnot(
  nrow(figure2_data) == 15,
  n_distinct(figure2_data$region_fe) == 5,
  n_distinct(figure2_data$domain) == 3,
  !anyNA(figure2_data$weighted_mean),
  !anyNA(figure2_data$ci_lower),
  !anyNA(figure2_data$ci_upper)
)

cat("\nFIGURE 2 DATA CHECK\n\n")

print(
  figure2_data %>%
    select(
      region_fe,
      domain,
      weighted_mean,
      ci_lower,
      ci_upper
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Construct final Figure 2
# ------------------------------------------------

figure2 <- ggplot2::ggplot(
  figure2_data,
  ggplot2::aes(
    x = region_fe,
    y = weighted_mean,
    shape = domain,
    group = domain
  )
) +
  
  ggplot2::geom_point(
    position =
      ggplot2::position_dodge(
        width = 0.55
      ),
    size = 3.2
  ) +
  
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    position =
      ggplot2::position_dodge(
        width = 0.55
      ),
    width = 0.15,
    linewidth = 0.7
  ) +
  
  ggplot2::scale_y_continuous(
    limits = c(1, 5),
    breaks = 1:5
  ) +
  
  ggplot2::labs(
    x = NULL,
    y = "Mean forest-value score",
    shape = "Forest-value domain"
  ) +
  
  ggplot2::theme_classic(
    base_size = 14
  ) +
  
  ggplot2::theme(
    axis.title.y =
      ggplot2::element_text(
        size = 14,
        face = "bold"
      ),
    
    axis.text.x =
      ggplot2::element_text(
        size = 14,
        angle = 30,
        hjust = 1
      ),
    
    axis.text.y =
      ggplot2::element_text(
        size = 14
      ),
    
    legend.position = "bottom",
    
    legend.title =
      ggplot2::element_text(
        size = 14,
        face = "bold"
      ),
    
    legend.text =
      ggplot2::element_text(
        size = 14
      )
  )

# ------------------------------------------------
# Display only
# ------------------------------------------------

print(figure2)

cat(
  "\nFigure 2 object constructed successfully.\n"
)

# ================================================================
# 12B. EXPORT AND VERIFY FINAL FIGURE 2
# ================================================================

figure2_pdf <- file.path(
  figure_dir,
  "Figure_2_macroregional_forest_values.pdf"
)

figure2_tiff <- file.path(
  figure_dir,
  "Figure_2_macroregional_forest_values.tiff"
)

# ------------------------------------------------
# Export PDF
# ------------------------------------------------

ggplot2::ggsave(
  filename = figure2_pdf,
  plot = figure2,
  width = 8.5,
  height = 5.8,
  units = "in"
)

# ------------------------------------------------
# Export TIFF
# ------------------------------------------------

ggplot2::ggsave(
  filename = figure2_tiff,
  plot = figure2,
  device = "tiff",
  width = 8.5,
  height = 5.8,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# ------------------------------------------------
# Verify exported files
# ------------------------------------------------

figure2_files <- c(
  figure2_pdf,
  figure2_tiff
)

figure2_file_check <- data.frame(
  file = figure2_files,
  exists = file.exists(figure2_files),
  size_bytes = file.info(figure2_files)$size,
  size_mb = round(
    file.info(figure2_files)$size /
      (1024^2),
    2
  ),
  modified = file.info(figure2_files)$mtime
)

cat(
  "\nFIGURE 2 FILE VERIFICATION\n\n"
)

print(
  figure2_file_check,
  row.names = FALSE
)

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

stopifnot(
  all(figure2_file_check$exists),
  all(figure2_file_check$size_bytes > 0),
  nrow(figure2_data) == 15,
  n_distinct(figure2_data$region_fe) == 5,
  n_distinct(figure2_data$domain) == 3
)

cat(
  "\nFigure 2 PDF and TIFF exported successfully.\n"
)

# ================================================================
# 13A. CONSTRUCT FINAL FIGURE 3
# Country-level forest-value perceptions
# ================================================================

# ------------------------------------------------
# Prepare country-level plotting data
# ------------------------------------------------

figure3_data <- country_value_stats %>%
  mutate(
    region_fe = factor(
      region_fe,
      levels = c(
        "North Europe",
        "Central-West Europe",
        "Central-East Europe",
        "South-West Europe",
        "South-East Europe"
      )
    ),
    
    # Manuscript display name
    country_display = dplyr::case_when(
      as.character(country) == "Czech" ~ "Czechia",
      TRUE ~ as.character(country)
    ),
    
    # Preserve intended ordering within regions
    country_display = factor(
      country_display,
      levels = c(
        "Denmark",
        "Sweden",
        "France",
        "Germany",
        "Netherlands",
        "Scotland",
        "Czechia",
        "Romania",
        "Italy",
        "Spain",
        "Croatia",
        "Serbia"
      )
    ),
    
    domain = factor(
      domain,
      levels = c(
        "Socio-cultural",
        "Provisioning",
        "Regulating/supporting"
      )
    )
  )

# ------------------------------------------------
# Validate Figure 3 data
# ------------------------------------------------

stopifnot(
  nrow(figure3_data) == 36,
  n_distinct(figure3_data$region_fe) == 5,
  n_distinct(figure3_data$country_display) == 12,
  n_distinct(figure3_data$domain) == 3,
  "Czechia" %in%
    as.character(figure3_data$country_display),
  !"Czech" %in%
    as.character(figure3_data$country_display),
  !anyNA(figure3_data$weighted_mean),
  !anyNA(figure3_data$ci_lower),
  !anyNA(figure3_data$ci_upper)
)

cat("\nFIGURE 3 COUNTRY ORDER CHECK\n\n")

print(
  figure3_data %>%
    distinct(
      region_fe,
      country_display
    ),
  n = Inf
)

# ------------------------------------------------
# Construct final Figure 3
# ------------------------------------------------

figure3 <- ggplot2::ggplot(
  figure3_data,
  ggplot2::aes(
    x = country_display,
    y = weighted_mean,
    shape = domain,
    group = domain
  )
) +
  
  ggplot2::geom_point(
    position =
      ggplot2::position_dodge(
        width = 0.55
      ),
    size = 3
  ) +
  
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    position =
      ggplot2::position_dodge(
        width = 0.55
      ),
    width = 0.15,
    linewidth = 0.65
  ) +
  
  ggplot2::facet_grid(
    . ~ region_fe,
    scales = "free_x",
    space = "free_x"
  ) +
  
  ggplot2::scale_y_continuous(
    limits = c(1, 5),
    breaks = 1:5
  ) +
  
  ggplot2::labs(
    x = NULL,
    y = "Mean forest-value score",
    shape = "Forest-value domain"
  ) +
  
  ggplot2::theme_classic(
    base_size = 14
  ) +
  
  ggplot2::theme(
    axis.title.y =
      ggplot2::element_text(
        size = 14,
        face = "bold"
      ),
    
    axis.text.y =
      ggplot2::element_text(
        size = 14
      ),
    
    axis.text.x =
      ggplot2::element_text(
        size = 14,
        angle = 45,
        hjust = 1
      ),
    
    strip.background =
      ggplot2::element_blank(),
    
    strip.text =
      ggplot2::element_text(
        size = 14,
        face = "bold"
      ),
    
    legend.position = "bottom",
    
    legend.title =
      ggplot2::element_text(
        size = 14,
        face = "bold"
      ),
    
    legend.text =
      ggplot2::element_text(
        size = 14
      ),
    
    panel.spacing.x =
      grid::unit(
        1,
        "lines"
      )
  )

# ------------------------------------------------
# Display only
# ------------------------------------------------

print(figure3)

cat(
  "\nFigure 3 object constructed successfully.\n"
)

# ================================================================
# 13B. EXPORT AND VERIFY FINAL FIGURE 3
# ================================================================

figure3_pdf <- file.path(
  figure_dir,
  "Figure_3_country_forest_values.pdf"
)

figure3_tiff <- file.path(
  figure_dir,
  "Figure_3_country_forest_values.tiff"
)

# ------------------------------------------------
# Export PDF
# ------------------------------------------------

ggplot2::ggsave(
  filename = figure3_pdf,
  plot = figure3,
  width = 12,
  height = 6,
  units = "in"
)

# ------------------------------------------------
# Export TIFF
# ------------------------------------------------

ggplot2::ggsave(
  filename = figure3_tiff,
  plot = figure3,
  device = "tiff",
  width = 12,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# ------------------------------------------------
# Verify files
# ------------------------------------------------

figure3_files <- c(
  figure3_pdf,
  figure3_tiff
)

figure3_file_check <- data.frame(
  file = figure3_files,
  exists = file.exists(figure3_files),
  size_bytes = file.info(figure3_files)$size,
  size_mb = round(
    file.info(figure3_files)$size /
      (1024^2),
    2
  ),
  modified = file.info(figure3_files)$mtime
)

cat(
  "\nFIGURE 3 FILE VERIFICATION\n\n"
)

print(
  figure3_file_check,
  row.names = FALSE
)

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

stopifnot(
  all(figure3_file_check$exists),
  all(figure3_file_check$size_bytes > 0),
  
  nrow(figure3_data) == 36,
  
  n_distinct(
    figure3_data$country_display
  ) == 12,
  
  n_distinct(
    figure3_data$region_fe
  ) == 5,
  
  n_distinct(
    figure3_data$domain
  ) == 3,
  
  "Czechia" %in%
    as.character(
      figure3_data$country_display
    ),
  
  !"Czech" %in%
    as.character(
      figure3_data$country_display
    )
)

cat(
  "\nFigure 3 PDF and TIFF exported successfully.\n"
)

# ================================================================
# 13C. FIX FIGURE 3 FACET LABELS
# ================================================================

figure3 <- figure3 +
  ggplot2::facet_grid(
    . ~ region_fe,
    scales = "free_x",
    space = "free_x",
    labeller = ggplot2::labeller(
      region_fe = c(
        "North Europe" = "North",
        "Central-West Europe" = "Central-West",
        "Central-East Europe" = "Central-East",
        "South-West Europe" = "South-West",
        "South-East Europe" = "South-East"
      )
    )
  )

print(figure3)

# ================================================================
# 13D. GIVE FIGURE 3 FACETS EQUAL WIDTH
# ================================================================

figure3 <- figure3 +
  ggplot2::facet_grid(
    . ~ region_fe,
    scales = "free_x",
    space = "fixed",
    labeller = ggplot2::labeller(
      region_fe = c(
        "North Europe" = "North",
        "Central-West Europe" = "Central-West",
        "Central-East Europe" = "Central-East",
        "South-West Europe" = "South-West",
        "South-East Europe" = "South-East"
      )
    )
  )

print(figure3)


# ================================================================
# 13E. EXPORT REVISED FINAL FIGURE 3
# Equal-width macro-region facets
# ================================================================

figure3_pdf <- file.path(
  figure_dir,
  "Figure_3_country_forest_values.pdf"
)

figure3_tiff <- file.path(
  figure_dir,
  "Figure_3_country_forest_values.tiff"
)

# ------------------------------------------------
# Export PDF
# ------------------------------------------------

ggplot2::ggsave(
  filename = figure3_pdf,
  plot = figure3,
  width = 12,
  height = 6,
  units = "in"
)

# ------------------------------------------------
# Export TIFF
# ------------------------------------------------

ggplot2::ggsave(
  filename = figure3_tiff,
  plot = figure3,
  device = "tiff",
  width = 12,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# ------------------------------------------------
# Verify exported files
# ------------------------------------------------

figure3_files <- c(
  figure3_pdf,
  figure3_tiff
)

figure3_file_check <- data.frame(
  file = figure3_files,
  exists = file.exists(figure3_files),
  size_bytes = file.info(figure3_files)$size,
  size_mb = round(
    file.info(figure3_files)$size / (1024^2),
    2
  ),
  modified = file.info(figure3_files)$mtime
)

cat("\nFIGURE 3 FILE VERIFICATION\n\n")

print(
  figure3_file_check,
  row.names = FALSE
)

stopifnot(
  all(figure3_file_check$exists),
  all(figure3_file_check$size_bytes > 0),
  nrow(figure3_data) == 36,
  n_distinct(figure3_data$country_display) == 12,
  n_distinct(figure3_data$region_fe) == 5,
  n_distinct(figure3_data$domain) == 3
)

cat(
  "\nRevised Figure 3 PDF and TIFF exported successfully.\n"
)


# ================================================================
# 14A. PRIMARY WEIGHTED MULTILEVEL MODELS
# ================================================================

# ------------------------------------------------
# Primary model-fitting function
#
# Macro-region: fixed effect
# Country: random intercept
# North Europe is the regional reference category
# ------------------------------------------------

fit_primary <- function(
    outcome,
    data = analysis_data
) {
  
  formula_primary <- as.formula(
    paste0(
      outcome,
      " ~ ",
      "region_fe + ",
      "age_model + ",
      "gender_model + ",
      "education_model + ",
      "residence_model + ",
      "ownership_model + ",
      "social_model + ",
      "(1 | country)"
    )
  )
  
  lme4::lmer(
    formula_primary,
    data = data,
    weights = weight_norm,
    REML = TRUE,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(
        maxfun = 200000
      )
    )
  )
}

# ------------------------------------------------
# Fit the three primary models
# ------------------------------------------------

model_sc_primary <-
  fit_primary(
    "socio_cultural"
  )

model_pr_primary <-
  fit_primary(
    "provisioning"
  )

model_rs_primary <-
  fit_primary(
    "regulating"
  )

# ------------------------------------------------
# Store models together
# ------------------------------------------------

primary_models <- list(
  "Socio-cultural" =
    model_sc_primary,
  
  "Provisioning" =
    model_pr_primary,
  
  "Regulating/supporting" =
    model_rs_primary
)

# ------------------------------------------------
# Basic model diagnostics
# ------------------------------------------------

primary_diagnostics <- dplyr::bind_rows(
  lapply(
    names(primary_models),
    function(domain) {
      
      m <- primary_models[[domain]]
      
      vc <-
        as.data.frame(
          lme4::VarCorr(m)
        )
      
      conv_message <-
        m@optinfo$conv$lme4$messages
      
      tibble::tibble(
        domain = domain,
        
        N =
          stats::nobs(m),
        
        singular =
          lme4::isSingular(
            m,
            tol = 1e-4
          ),
        
        convergence =
          if (
            is.null(conv_message)
          ) {
            "OK"
          } else {
            paste(
              conv_message,
              collapse = "; "
            )
          },
        
        country_variance =
          vc$vcov[
            vc$grp == "country"
          ],
        
        residual_variance =
          vc$vcov[
            vc$grp == "Residual"
          ]
      )
    }
  )
)

cat(
  "\nPRIMARY MODEL DIAGNOSTICS\n\n"
)

print(
  primary_diagnostics,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Confirm model formulas
# ------------------------------------------------

cat(
  "\nPRIMARY MODEL FORMULAS\n"
)

for (
  nm in names(primary_models)
) {
  
  cat(
    "\n",
    nm,
    ":\n",
    sep = ""
  )
  
  print(
    stats::formula(
      primary_models[[nm]]
    )
  )
}

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

stopifnot(
  all(
    primary_diagnostics$N ==
      12460
  ),
  
  all(
    primary_diagnostics$singular ==
      FALSE
  ),
  
  all(
    primary_diagnostics$convergence ==
      "OK"
  ),
  
  all(
    primary_diagnostics$country_variance >
      0
  ),
  
  all(
    primary_diagnostics$residual_variance >
      0
  )
)

cat(
  "\nPrimary weighted-model validation passed.\n"
)

# ================================================================
# 14B. EXTRACT AND VALIDATE PRIMARY FIXED EFFECTS
# Corrected: no assumption that tidy() returns p.value
# ================================================================

extract_primary_fixed <- function(
    model,
    domain_name
) {
  
  broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    mutate(
      domain = domain_name
    ) %>%
    select(
      domain,
      term,
      estimate,
      std.error,
      statistic,
      conf.low,
      conf.high
    )
}

# ------------------------------------------------
# Extract all three models
# ------------------------------------------------

primary_fixed <- dplyr::bind_rows(
  
  extract_primary_fixed(
    model_sc_primary,
    "Socio-cultural"
  ),
  
  extract_primary_fixed(
    model_pr_primary,
    "Provisioning"
  ),
  
  extract_primary_fixed(
    model_rs_primary,
    "Regulating/supporting"
  )
)

# ------------------------------------------------
# Print complete fixed-effect results
# ------------------------------------------------

cat(
  "\nPRIMARY FIXED-EFFECT RESULTS\n\n"
)

print(
  primary_fixed %>%
    mutate(
      across(
        c(
          estimate,
          std.error,
          statistic,
          conf.low,
          conf.high
        ),
        ~ round(.x, 5)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Check number of coefficients
# ------------------------------------------------

primary_fixed_structure <- primary_fixed %>%
  count(
    domain,
    name = "number_of_fixed_effects"
  )

cat(
  "\nFIXED-EFFECT STRUCTURE\n\n"
)

print(
  primary_fixed_structure,
  n = Inf
)

# ------------------------------------------------
# Structural validation
# ------------------------------------------------

stopifnot(
  nrow(primary_fixed) == 75,
  
  all(
    primary_fixed_structure$number_of_fixed_effects ==
      25
  ),
  
  !anyNA(primary_fixed$estimate),
  !anyNA(primary_fixed$std.error),
  !anyNA(primary_fixed$conf.low),
  !anyNA(primary_fixed$conf.high)
)

# ------------------------------------------------
# Helper for numerical checks
# ------------------------------------------------

get_estimate <- function(
    domain_name,
    term_name
) {
  
  primary_fixed %>%
    filter(
      domain == domain_name,
      term == term_name
    ) %>%
    pull(estimate)
}

# ------------------------------------------------
# Regional contrasts
# ------------------------------------------------

check_sc_cw <- get_estimate(
  "Socio-cultural",
  "region_feCentral-West Europe"
)

check_pr_ce <- get_estimate(
  "Provisioning",
  "region_feCentral-East Europe"
)

check_rs_sw <- get_estimate(
  "Regulating/supporting",
  "region_feSouth-West Europe"
)

# ------------------------------------------------
# Gender
# ------------------------------------------------

check_sc_female <- get_estimate(
  "Socio-cultural",
  "gender_modelFemale"
)

check_pr_female <- get_estimate(
  "Provisioning",
  "gender_modelFemale"
)

check_rs_female <- get_estimate(
  "Regulating/supporting",
  "gender_modelFemale"
)

# ------------------------------------------------
# Forest ownership
# ------------------------------------------------

check_sc_owner <- get_estimate(
  "Socio-cultural",
  "ownership_modelYes"
)

check_pr_owner <- get_estimate(
  "Provisioning",
  "ownership_modelYes"
)

check_rs_owner <- get_estimate(
  "Regulating/supporting",
  "ownership_modelYes"
)

# ------------------------------------------------
# Display validation coefficients
# ------------------------------------------------

validation_coefficients <- tibble::tibble(
  coefficient = c(
    "Socio: Central-West",
    "Provisioning: Central-East",
    "Regulating: South-West",
    "Socio: Female",
    "Provisioning: Female",
    "Regulating: Female",
    "Socio: Forest owner",
    "Provisioning: Forest owner",
    "Regulating: Forest owner"
  ),
  
  estimate = c(
    check_sc_cw,
    check_pr_ce,
    check_rs_sw,
    check_sc_female,
    check_pr_female,
    check_rs_female,
    check_sc_owner,
    check_pr_owner,
    check_rs_owner
  )
)

cat(
  "\nSELECTED COEFFICIENT VALIDATION\n\n"
)

print(
  validation_coefficients,
  n = Inf
)

# ------------------------------------------------
# Formal numerical validation
# ------------------------------------------------

stopifnot(
  length(check_sc_cw) == 1,
  length(check_pr_ce) == 1,
  length(check_rs_sw) == 1,
  
  length(check_sc_female) == 1,
  length(check_pr_female) == 1,
  length(check_rs_female) == 1,
  
  length(check_sc_owner) == 1,
  length(check_pr_owner) == 1,
  length(check_rs_owner) == 1,
  
  abs(check_sc_cw - 0.0179) < 0.001,
  abs(check_pr_ce - 0.746) < 0.001,
  abs(check_rs_sw - 0.120) < 0.001,
  
  abs(check_sc_female - 0.105) < 0.001,
  abs(check_pr_female - (-0.0300)) < 0.001,
  abs(check_rs_female - 0.0916) < 0.001,
  
  abs(check_sc_owner - (-0.114)) < 0.001,
  abs(check_pr_owner - 0.161) < 0.001,
  abs(check_rs_owner - (-0.219)) < 0.001
)

cat(
  "\nPrimary fixed-effect numerical validation passed.\n"
)

# ================================================================
# 14C. SAVE AND VERIFY VALIDATED PRIMARY MODELS
# ================================================================

primary_models_file <- file.path(
  model_dir,
  "primary_models_corrected.rds"
)

# ------------------------------------------------
# Save using stable internal names
# ------------------------------------------------

primary_models_saved <- list(
  socio_cultural =
    model_sc_primary,
  
  provisioning =
    model_pr_primary,
  
  regulating_supporting =
    model_rs_primary
)

saveRDS(
  primary_models_saved,
  primary_models_file
)

# ------------------------------------------------
# Read back from disk
# ------------------------------------------------

primary_models_verify <-
  readRDS(
    primary_models_file
  )

# ------------------------------------------------
# Verify model identities and sample sizes
# ------------------------------------------------

cat(
  "\nSAVED PRIMARY MODELS\n\n"
)

print(
  names(
    primary_models_verify
  )
)

saved_model_check <- tibble::tibble(
  model = names(
    primary_models_verify
  ),
  
  N = vapply(
    primary_models_verify,
    stats::nobs,
    numeric(1)
  ),
  
  singular = vapply(
    primary_models_verify,
    function(x) {
      lme4::isSingular(
        x,
        tol = 1e-4
      )
    },
    logical(1)
  )
)

print(
  saved_model_check,
  n = Inf
)

# ------------------------------------------------
# Verify selected coefficients survived serialization
# ------------------------------------------------

saved_sc_cw <-
  lme4::fixef(
    primary_models_verify$socio_cultural
  )[
    "region_feCentral-West Europe"
  ]

saved_pr_ce <-
  lme4::fixef(
    primary_models_verify$provisioning
  )[
    "region_feCentral-East Europe"
  ]

saved_rs_female <-
  lme4::fixef(
    primary_models_verify$regulating_supporting
  )[
    "gender_modelFemale"
  ]

# ------------------------------------------------
# File information
# ------------------------------------------------

primary_model_file_check <-
  data.frame(
    file =
      primary_models_file,
    
    exists =
      file.exists(
        primary_models_file
      ),
    
    size_bytes =
      file.info(
        primary_models_file
      )$size,
    
    modified =
      file.info(
        primary_models_file
      )$mtime
  )

cat(
  "\nPRIMARY MODEL FILE VERIFICATION\n\n"
)

print(
  primary_model_file_check,
  row.names = FALSE
)

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

stopifnot(
  identical(
    names(primary_models_verify),
    c(
      "socio_cultural",
      "provisioning",
      "regulating_supporting"
    )
  ),
  
  all(
    saved_model_check$N ==
      12460
  ),
  
  all(
    saved_model_check$singular ==
      FALSE
  ),
  
  abs(
    saved_sc_cw -
      check_sc_cw
  ) < 1e-12,
  
  abs(
    saved_pr_ce -
      check_pr_ce
  ) < 1e-12,
  
  abs(
    saved_rs_female -
      check_rs_female
  ) < 1e-12,
  
  file.exists(
    primary_models_file
  ),
  
  file.info(
    primary_models_file
  )$size > 0
)

cat(
  "\nValidated primary models saved and read-back verification passed.\n"
)

# ================================================================
# 15A. H2: FIT MATCHED REGION-ONLY MODELS
# ================================================================

# ------------------------------------------------
# Region-only models:
# macro-region fixed effect + country random intercept
#
# Same analytical sample, weights, and clustering structure
# as the fully adjusted primary models.
# ------------------------------------------------

fit_region_only <- function(
    outcome,
    data = analysis_data
) {
  
  formula_region_only <- as.formula(
    paste0(
      outcome,
      " ~ region_fe + (1 | country)"
    )
  )
  
  lme4::lmer(
    formula_region_only,
    data = data,
    weights = weight_norm,
    REML = TRUE,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(
        maxfun = 200000
      )
    )
  )
}

# ------------------------------------------------
# Fit the three region-only models
# ------------------------------------------------

h2_region_socio <-
  fit_region_only(
    "socio_cultural"
  )

h2_region_prov <-
  fit_region_only(
    "provisioning"
  )

h2_region_reg <-
  fit_region_only(
    "regulating"
  )

# ------------------------------------------------
# Store models
# ------------------------------------------------

h2_region_only_models <- list(
  socio_cultural =
    h2_region_socio,
  
  provisioning =
    h2_region_prov,
  
  regulating_supporting =
    h2_region_reg
)

# ------------------------------------------------
# Diagnostic summary
# ------------------------------------------------

h2_region_diagnostics <-
  dplyr::bind_rows(
    lapply(
      names(h2_region_only_models),
      function(nm) {
        
        m <-
          h2_region_only_models[[nm]]
        
        conv_message <-
          m@optinfo$conv$lme4$messages
        
        tibble::tibble(
          model = nm,
          
          N =
            stats::nobs(m),
          
          singular =
            lme4::isSingular(
              m,
              tol = 1e-4
            ),
          
          convergence =
            if (
              is.null(conv_message)
            ) {
              "OK"
            } else {
              paste(
                conv_message,
                collapse = "; "
              )
            }
        )
      }
    )
  )

cat(
  "\nH2 REGION-ONLY MODEL DIAGNOSTICS\n\n"
)

print(
  h2_region_diagnostics,
  n = Inf
)

# ------------------------------------------------
# Display regional coefficients
# ------------------------------------------------

h2_region_coefficients <-
  dplyr::bind_rows(
    
    tibble::tibble(
      domain = "Socio-cultural",
      term =
        names(
          lme4::fixef(
            h2_region_socio
          )
        ),
      estimate =
        unname(
          lme4::fixef(
            h2_region_socio
          )
        )
    ),
    
    tibble::tibble(
      domain = "Provisioning",
      term =
        names(
          lme4::fixef(
            h2_region_prov
          )
        ),
      estimate =
        unname(
          lme4::fixef(
            h2_region_prov
          )
        )
    ),
    
    tibble::tibble(
      domain =
        "Regulating/supporting",
      term =
        names(
          lme4::fixef(
            h2_region_reg
          )
        ),
      estimate =
        unname(
          lme4::fixef(
            h2_region_reg
          )
        )
    )
  ) %>%
  filter(
    grepl(
      "^region_fe",
      term
    )
  )

cat(
  "\nH2 REGION-ONLY REGIONAL COEFFICIENTS\n\n"
)

print(
  h2_region_coefficients %>%
    mutate(
      estimate =
        round(
          estimate,
          5
        )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Expected coefficients from validated H2 analysis
# ------------------------------------------------

expected_h2 <- c(
  0.1760,  0.0212,  0.0403, -0.0200,
  0.8077, -0.3410,  0.2576, -0.4151,
  0.1313,  0.1104,  0.0318, -0.0434
)

# Match by explicit domain/term rather than row position
h2_expected_check <- tibble::tribble(
  ~domain, ~term, ~expected,
  
  "Socio-cultural",
  "region_feCentral-East Europe",
  0.1760,
  
  "Socio-cultural",
  "region_feSouth-West Europe",
  0.0212,
  
  "Socio-cultural",
  "region_feSouth-East Europe",
  0.0403,
  
  "Socio-cultural",
  "region_feCentral-West Europe",
  -0.0200,
  
  "Provisioning",
  "region_feCentral-East Europe",
  0.8077,
  
  "Provisioning",
  "region_feSouth-West Europe",
  -0.3410,
  
  "Provisioning",
  "region_feSouth-East Europe",
  0.2576,
  
  "Provisioning",
  "region_feCentral-West Europe",
  -0.4151,
  
  "Regulating/supporting",
  "region_feCentral-East Europe",
  0.1313,
  
  "Regulating/supporting",
  "region_feSouth-West Europe",
  0.1104,
  
  "Regulating/supporting",
  "region_feSouth-East Europe",
  0.0318,
  
  "Regulating/supporting",
  "region_feCentral-West Europe",
  -0.0434
) %>%
  left_join(
    h2_region_coefficients,
    by = c(
      "domain",
      "term"
    )
  )

cat(
  "\nH2 NUMERICAL VALIDATION\n\n"
)

print(
  h2_expected_check %>%
    mutate(
      expected =
        round(expected, 5),
      
      estimate =
        round(estimate, 5),
      
      difference =
        round(
          estimate - expected,
          6
        )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

stopifnot(
  all(
    h2_region_diagnostics$N ==
      12460
  ),
  
  all(
    h2_region_diagnostics$singular ==
      FALSE
  ),
  
  all(
    h2_region_diagnostics$convergence ==
      "OK"
  ),
  
  nrow(
    h2_region_coefficients
  ) == 12,
  
  !anyNA(
    h2_expected_check$estimate
  ),
  
  all(
    abs(
      h2_expected_check$estimate -
        h2_expected_check$expected
    ) < 0.001
  )
)

cat(
  "\nH2 region-only model validation passed.\n"
)


# ================================================================
# 15B. H2: REGION-ONLY VS FULLY ADJUSTED REGIONAL EFFECTS
# ================================================================

# ------------------------------------------------
# Helper: extract macro-regional fixed effects
# with 95% confidence intervals
# ------------------------------------------------

extract_region_effects <- function(
    model,
    domain_name
) {
  
  broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    filter(
      grepl(
        "^region_fe",
        term
      )
    ) %>%
    transmute(
      domain = domain_name,
      term = term,
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high
    )
}

# ------------------------------------------------
# Region-only estimates
# ------------------------------------------------

region_only_effects <- dplyr::bind_rows(
  
  extract_region_effects(
    h2_region_socio,
    "Socio-cultural"
  ),
  
  extract_region_effects(
    h2_region_prov,
    "Provisioning"
  ),
  
  extract_region_effects(
    h2_region_reg,
    "Regulating/supporting"
  )
)

# ------------------------------------------------
# Fully adjusted estimates
# ------------------------------------------------

adjusted_region_effects <- dplyr::bind_rows(
  
  extract_region_effects(
    model_sc_primary,
    "Socio-cultural"
  ),
  
  extract_region_effects(
    model_pr_primary,
    "Provisioning"
  ),
  
  extract_region_effects(
    model_rs_primary,
    "Regulating/supporting"
  )
)

# ------------------------------------------------
# Join matched regional contrasts
# ------------------------------------------------

h2_comparison <- region_only_effects %>%
  
  rename(
    region_only_beta = estimate,
    region_only_low = conf.low,
    region_only_high = conf.high
  ) %>%
  
  left_join(
    adjusted_region_effects %>%
      rename(
        adjusted_beta = estimate,
        adjusted_low = conf.low,
        adjusted_high = conf.high
      ),
    by = c(
      "domain",
      "term"
    )
  ) %>%
  
  mutate(
    region =
      sub(
        "^region_fe",
        "",
        term
      ),
    
    region_only_abs =
      abs(region_only_beta),
    
    adjusted_abs =
      abs(adjusted_beta),
    
    # Positive = coefficient moved toward zero
    absolute_reduction =
      region_only_abs -
      adjusted_abs,
    
    attenuated =
      adjusted_abs <
      region_only_abs,
    
    percent_attenuation =
      100 *
      (
        region_only_abs -
          adjusted_abs
      ) /
      region_only_abs,
    
    region_only_ci_excludes_zero =
      region_only_low > 0 |
      region_only_high < 0,
    
    adjusted_ci_excludes_zero =
      adjusted_low > 0 |
      adjusted_high < 0
  )

# ------------------------------------------------
# Display complete comparison
# ------------------------------------------------

cat(
  "\nH2 REGION-ONLY VS ADJUSTED COMPARISON\n\n"
)

print(
  h2_comparison %>%
    select(
      domain,
      region,
      region_only_beta,
      region_only_low,
      region_only_high,
      adjusted_beta,
      adjusted_low,
      adjusted_high,
      absolute_reduction,
      percent_attenuation,
      attenuated,
      region_only_ci_excludes_zero,
      adjusted_ci_excludes_zero
    ) %>%
    mutate(
      across(
        c(
          region_only_beta,
          region_only_low,
          region_only_high,
          adjusted_beta,
          adjusted_low,
          adjusted_high,
          absolute_reduction,
          percent_attenuation
        ),
        ~ round(.x, 3)
      )
    ),
  n = Inf,
  width = Inf
)

# ================================================================
# 15B-2. H2 SUMMARY AND VALIDATION
# ================================================================

# ------------------------------------------------
# Overall H2 summary
#
# IMPORTANT:
# Calculate counts from the original logical vector
# before assigning summary-column names.
# ------------------------------------------------

h2_summary <- h2_comparison %>%
  summarise(
    contrasts = n(),
    
    n_attenuated =
      sum(
        attenuated,
        na.rm = TRUE
      ),
    
    n_increased_in_magnitude =
      sum(
        !attenuated,
        na.rm = TRUE
      ),
    
    region_only_ci_excluding_zero =
      sum(
        region_only_ci_excludes_zero,
        na.rm = TRUE
      ),
    
    adjusted_ci_excluding_zero =
      sum(
        adjusted_ci_excludes_zero,
        na.rm = TRUE
      )
  )

cat(
  "\nH2 OVERALL SUMMARY\n\n"
)

print(
  h2_summary,
  width = Inf
)

# ------------------------------------------------
# Domain-specific summary
# ------------------------------------------------

h2_domain_summary <- h2_comparison %>%
  group_by(domain) %>%
  summarise(
    contrasts = n(),
    
    n_attenuated =
      sum(
        attenuated,
        na.rm = TRUE
      ),
    
    n_increased_in_magnitude =
      sum(
        !attenuated,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

cat(
  "\nH2 DOMAIN-SPECIFIC SUMMARY\n\n"
)

print(
  h2_domain_summary,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Direct logical-vector check
# ------------------------------------------------

cat(
  "\nDIRECT ATTENUATION CHECK\n\n"
)

print(
  table(
    h2_comparison$attenuated
  )
)

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

stopifnot(
  nrow(h2_comparison) == 12,
  
  h2_summary$contrasts == 12,
  
  h2_summary$n_attenuated == 6,
  
  h2_summary$n_increased_in_magnitude == 6,
  
  h2_summary$region_only_ci_excluding_zero == 0,
  
  h2_summary$adjusted_ci_excluding_zero == 0,
  
  h2_domain_summary$n_attenuated[
    h2_domain_summary$domain ==
      "Provisioning"
  ] == 4,
  
  h2_domain_summary$n_attenuated[
    h2_domain_summary$domain ==
      "Socio-cultural"
  ] == 1,
  
  h2_domain_summary$n_attenuated[
    h2_domain_summary$domain ==
      "Regulating/supporting"
  ] == 1
)

cat(
  "\nCorrected H2 comparison validation passed.\n"
)


# ================================================================
# 15C. FINALIZE TABLE S3
# Region-only versus fully adjusted regional coefficients
# ================================================================

# ------------------------------------------------
# Create manuscript-ready Table S3
# ------------------------------------------------

table_s3 <- h2_comparison %>%
  
  mutate(
    domain = factor(
      domain,
      levels = c(
        "Socio-cultural",
        "Provisioning",
        "Regulating/supporting"
      )
    ),
    
    region = factor(
      region,
      levels = c(
        "Central-West Europe",
        "Central-East Europe",
        "South-West Europe",
        "South-East Europe"
      )
    )
  ) %>%
  
  arrange(
    domain,
    region
  ) %>%
  
  transmute(
    `Forest-value domain` =
      as.character(domain),
    
    `Macro-region` =
      as.character(region),
    
    `Region-only beta` =
      sprintf(
        "%.3f",
        region_only_beta
      ),
    
    `Region-only 95% CI` =
      sprintf(
        "%.3f to %.3f",
        region_only_low,
        region_only_high
      ),
    
    `Fully adjusted beta` =
      sprintf(
        "%.3f",
        adjusted_beta
      ),
    
    `Fully adjusted 95% CI` =
      sprintf(
        "%.3f to %.3f",
        adjusted_low,
        adjusted_high
      )
  )

cat(
  "\nFINAL TABLE S3\n\n"
)

print(
  as.data.frame(table_s3),
  row.names = FALSE
)

# ------------------------------------------------
# Output paths
# ------------------------------------------------

s3_csv <- file.path(
  supp_dir,
  "Table_S3_region_only_vs_adjusted_models.csv"
)

s3_xlsx <- file.path(
  supp_dir,
  "Table_S3_region_only_vs_adjusted_models.xlsx"
)

# ------------------------------------------------
# Save CSV
# ------------------------------------------------

write.csv(
  table_s3,
  s3_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------
# Save Excel
# ------------------------------------------------

wb_s3 <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb_s3,
  "Table S3",
  gridLines = FALSE
)

title_s3 <- paste0(
  "Table S3. Comparison of macro-regional coefficients ",
  "from region-only and fully adjusted mixed-effects models"
)

openxlsx::writeData(
  wb_s3,
  "Table S3",
  title_s3,
  startRow = 1,
  startCol = 1
)

openxlsx::writeData(
  wb_s3,
  "Table S3",
  table_s3,
  startRow = 3,
  startCol = 1
)

note_s3 <- paste0(
  "Note: North Europe is the reference macro-region. ",
  "Region-only models included macro-region as a fixed effect ",
  "and country as a random intercept. Fully adjusted models ",
  "additionally included age group, gender, educational attainment, ",
  "length of residence, forest ownership, and social proximity to ",
  "forestry. Both specifications used the same analytical sample ",
  "(N = 12,460), country-normalised respondent weights, and ",
  "country-level random-intercept structure. Estimates are regression ",
  "coefficients with 95% confidence intervals."
)

openxlsx::writeData(
  wb_s3,
  "Table S3",
  note_s3,
  startRow = nrow(table_s3) + 5,
  startCol = 1
)

# ------------------------------------------------
# Formatting
# ------------------------------------------------

title_style <- openxlsx::createStyle(
  textDecoration = "bold",
  wrapText = TRUE
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE
)

note_style <- openxlsx::createStyle(
  wrapText = TRUE,
  valign = "top"
)

openxlsx::addStyle(
  wb_s3,
  "Table S3",
  title_style,
  rows = 1,
  cols = 1,
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb_s3,
  "Table S3",
  header_style,
  rows = 3,
  cols = 1:ncol(table_s3),
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb_s3,
  "Table S3",
  note_style,
  rows = nrow(table_s3) + 5,
  cols = 1,
  gridExpand = TRUE
)

openxlsx::mergeCells(
  wb_s3,
  "Table S3",
  cols = 1:ncol(table_s3),
  rows = 1
)

openxlsx::mergeCells(
  wb_s3,
  "Table S3",
  cols = 1:ncol(table_s3),
  rows = nrow(table_s3) + 5
)

openxlsx::setColWidths(
  wb_s3,
  "Table S3",
  cols = 1:2,
  widths = 24
)

openxlsx::setColWidths(
  wb_s3,
  "Table S3",
  cols = 3:6,
  widths = 22
)

openxlsx::setRowHeights(
  wb_s3,
  "Table S3",
  rows = nrow(table_s3) + 5,
  heights = 55
)

openxlsx::freezePane(
  wb_s3,
  "Table S3",
  firstActiveRow = 4
)

openxlsx::saveWorkbook(
  wb_s3,
  s3_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------
# Read-back verification
# ------------------------------------------------

s3_csv_check <- read.csv(
  s3_csv,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

s3_xlsx_check <- openxlsx::read.xlsx(
  s3_xlsx,
  sheet = "Table S3",
  startRow = 3,
  rows = 3:(3 + nrow(table_s3)),
  cols = 1:ncol(table_s3),
  check.names = FALSE
)

# ------------------------------------------------
# Formal validation
# ------------------------------------------------

stopifnot(
  nrow(table_s3) == 12,
  
  ncol(table_s3) == 6,
  
  nrow(s3_csv_check) == 12,
  
  nrow(s3_xlsx_check) == 12,
  
  identical(
    names(s3_csv_check),
    names(table_s3)
  ),
  
  file.exists(s3_csv),
  
  file.exists(s3_xlsx),
  
  file.info(s3_csv)$size > 0,
  
  file.info(s3_xlsx)$size > 0
)

# ------------------------------------------------
# File verification
# ------------------------------------------------

cat(
  "\nTABLE S3 FILE VERIFICATION\n\n"
)

print(
  data.frame(
    file = c(
      s3_csv,
      s3_xlsx
    ),
    
    exists = file.exists(
      c(
        s3_csv,
        s3_xlsx
      )
    ),
    
    size_bytes = file.info(
      c(
        s3_csv,
        s3_xlsx
      )
    )$size
  ),
  row.names = FALSE
)

cat(
  "\nTable S3 finalized and read-back validation passed.\n"
)

# ================================================================
# 16A. THREE-LEVEL VARIANCE-PARTITION MODELS
# Section 4.5: Contextual variance and clustering
# ================================================================

# ------------------------------------------------
# Model structure:
#
# respondent level
#     nested within country
#         nested within macro-region
#
# Individual predictors are retained.
#
# Macro-region is NOT entered as a fixed effect here because
# the purpose is to partition residual variance between:
#   1. macro-region
#   2. country within macro-region
#   3. respondent/residual level
# ------------------------------------------------

fit_variance_partition <- function(
    outcome,
    data = analysis_data
) {
  
  formula_vp <- as.formula(
    paste0(
      outcome,
      " ~ age_model + gender_model + education_model + ",
      "residence_model + ownership_model + social_model + ",
      "(1 | region_fe/country)"
    )
  )
  
  lme4::lmer(
    formula_vp,
    data = data,
    weights = weight_norm,
    REML = TRUE,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(
        maxfun = 200000
      )
    )
  )
}

# ------------------------------------------------
# Fit the three models
# ------------------------------------------------

vp_socio <-
  fit_variance_partition(
    "socio_cultural"
  )

vp_provisioning <-
  fit_variance_partition(
    "provisioning"
  )

vp_regulating <-
  fit_variance_partition(
    "regulating"
  )

# ------------------------------------------------
# Store models
# ------------------------------------------------

variance_partition_models <- list(
  socio_cultural =
    vp_socio,
  
  provisioning =
    vp_provisioning,
  
  regulating_supporting =
    vp_regulating
)

# ------------------------------------------------
# Model diagnostics
# ------------------------------------------------

vp_diagnostics <- dplyr::bind_rows(
  lapply(
    names(variance_partition_models),
    function(nm) {
      
      m <-
        variance_partition_models[[nm]]
      
      conv_message <-
        m@optinfo$conv$lme4$messages
      
      tibble::tibble(
        model = nm,
        
        N =
          stats::nobs(m),
        
        singular =
          lme4::isSingular(
            m,
            tol = 1e-4
          ),
        
        convergence =
          if (
            is.null(conv_message)
          ) {
            "OK"
          } else {
            paste(
              conv_message,
              collapse = "; "
            )
          }
      )
    }
  )
)

cat(
  "\nVARIANCE-PARTITION MODEL DIAGNOSTICS\n\n"
)

print(
  vp_diagnostics,
  n = Inf
)

# ------------------------------------------------
# Extract variance components and ICCs
# ------------------------------------------------

extract_vp_variance <- function(
    model,
    domain_name
) {
  
  vc <-
    as.data.frame(
      lme4::VarCorr(model)
    )
  
  macro_var <-
    vc$vcov[
      vc$grp == "region_fe"
    ]
  
  country_var <-
    vc$vcov[
      vc$grp == "country:region_fe"
    ]
  
  residual_var <-
    vc$vcov[
      vc$grp == "Residual"
    ]
  
  total_var <-
    macro_var +
    country_var +
    residual_var
  
  tibble::tibble(
    domain =
      domain_name,
    
    macroregion_variance =
      macro_var,
    
    country_variance =
      country_var,
    
    residual_variance =
      residual_var,
    
    total_variance =
      total_var,
    
    macroregion_icc =
      100 *
      macro_var /
      total_var,
    
    country_icc =
      100 *
      country_var /
      total_var,
    
    combined_contextual_icc =
      100 *
      (
        macro_var +
          country_var
      ) /
      total_var,
    
    residual_percent =
      100 *
      residual_var /
      total_var
  )
}

vp_point_estimates <- dplyr::bind_rows(
  
  extract_vp_variance(
    vp_socio,
    "Socio-cultural"
  ),
  
  extract_vp_variance(
    vp_provisioning,
    "Provisioning"
  ),
  
  extract_vp_variance(
    vp_regulating,
    "Regulating/supporting"
  )
)

# ------------------------------------------------
# Display point estimates
# ------------------------------------------------

cat(
  "\nVARIANCE COMPONENTS AND ICC POINT ESTIMATES\n\n"
)

print(
  vp_point_estimates %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 4)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Structural validation
# ------------------------------------------------

stopifnot(
  all(
    vp_diagnostics$N ==
      12460
  ),
  
  vp_diagnostics$singular[
    vp_diagnostics$model ==
      "socio_cultural"
  ],

  !vp_diagnostics$singular[
    vp_diagnostics$model ==
      "provisioning"
  ],

  !vp_diagnostics$singular[
    vp_diagnostics$model ==
      "regulating_supporting"
  ],
  
  all(
    vp_diagnostics$convergence ==
      "OK"
  ),
  
  nrow(
    vp_point_estimates
  ) == 3,
  
  !anyNA(
    vp_point_estimates
  )
)

cat(
  "\nThree-level variance-partition models fitted successfully.\n"
)


# ================================================================
# 16A-2. THREE-LEVEL MODEL DIAGNOSTICS
# ================================================================

cat(
  "\nVARIANCE-PARTITION DIAGNOSTICS\n\n"
)

print(
  vp_diagnostics,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Detailed variance components
# ------------------------------------------------

for (
  nm in names(variance_partition_models)
) {
  
  cat(
    "\n========================================\n",
    "MODEL: ", nm, "\n",
    "========================================\n",
    sep = ""
  )
  
  m <-
    variance_partition_models[[nm]]
  
  cat(
    "\nN = ",
    stats::nobs(m),
    "\n",
    sep = ""
  )
  
  cat(
    "isSingular(tol = 1e-4): ",
    lme4::isSingular(
      m,
      tol = 1e-4
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "isSingular(tol = 1e-5): ",
    lme4::isSingular(
      m,
      tol = 1e-5
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "isSingular(tol = 1e-6): ",
    lme4::isSingular(
      m,
      tol = 1e-6
    ),
    "\n\n",
    sep = ""
  )
  
  print(
    as.data.frame(
      lme4::VarCorr(m)
    )[
      ,
      c(
        "grp",
        "var1",
        "vcov",
        "sdcor"
      )
    ],
    row.names = FALSE
  )
  
  cat(
    "\nConvergence messages:\n"
  )
  
  print(
    m@optinfo$conv$lme4$messages
  )
}

# ------------------------------------------------
# Show the point estimates that were calculated
# before stopifnot() halted execution
# ------------------------------------------------

cat(
  "\n========================================\n",
  "ICC POINT ESTIMATES\n",
  "========================================\n\n",
  sep = ""
)

print(
  vp_point_estimates %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 6)
      )
    ),
  n = Inf,
  width = Inf
)

# ================================================================
# 16B. VALIDATE VARIANCE-PARTITION POINT ESTIMATES
# ================================================================

# ------------------------------------------------
# The socio-cultural model is expected to be a
# boundary fit because its estimated macro-region
# variance is exactly zero.
#
# This is a substantive variance estimate, not a
# reason to alter the specified hierarchy.
# ------------------------------------------------

vp_validation <- vp_point_estimates %>%
  select(
    domain,
    macroregion_variance,
    country_variance,
    residual_variance,
    macroregion_icc,
    country_icc,
    combined_contextual_icc,
    residual_percent
  )

cat(
  "\nVALIDATED VARIANCE-PARTITION POINT ESTIMATES\n\n"
)

print(
  vp_validation %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 4)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Expected point estimates from validated analysis
# ------------------------------------------------

vp_expected <- tibble::tribble(
  ~domain,                  ~macro_icc, ~country_icc_expected,
  ~combined_icc, ~residual_expected,
  
  "Socio-cultural",
  0.00, 2.19, 2.19, 97.81,
  
  "Provisioning",
  7.48, 13.74, 21.22, 78.78,
  
  "Regulating/supporting",
  0.18, 2.43, 2.60, 97.40
)

vp_check <- vp_validation %>%
  left_join(
    vp_expected,
    by = "domain"
  ) %>%
  mutate(
    macro_difference =
      macroregion_icc -
      macro_icc,
    
    country_difference =
      country_icc -
      country_icc_expected,
    
    combined_difference =
      combined_contextual_icc -
      combined_icc,
    
    residual_difference =
      residual_percent -
      residual_expected
  )

cat(
  "\nPOINT-ESTIMATE VALIDATION DIFFERENCES\n\n"
)

print(
  vp_check %>%
    select(
      domain,
      macro_difference,
      country_difference,
      combined_difference,
      residual_difference
    ) %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 4)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Formal validation
#
# Allow small differences because expected values
# above are rounded to two decimal places.
# ------------------------------------------------

stopifnot(
  nrow(vp_validation) == 3,
  
  all(
    vp_diagnostics$N ==
      12460
  ),
  
  vp_diagnostics$singular[
    vp_diagnostics$model ==
      "socio_cultural"
  ],
  
  !vp_diagnostics$singular[
    vp_diagnostics$model ==
      "provisioning"
  ],
  
  !vp_diagnostics$singular[
    vp_diagnostics$model ==
      "regulating_supporting"
  ],
  
  vp_validation$macroregion_variance[
    vp_validation$domain ==
      "Socio-cultural"
  ] == 0,
  
  all(
    abs(
      vp_check$macro_difference
    ) < 0.01
  ),
  
  all(
    abs(
      vp_check$country_difference
    ) < 0.01
  ),
  
  all(
    abs(
      vp_check$combined_difference
    ) < 0.01
  ),
  
  all(
    abs(
      vp_check$residual_difference
    ) < 0.01
  )
)

# ------------------------------------------------
# Save models for reproducibility
# ------------------------------------------------

vp_models_file <- file.path(
  model_dir,
  "variance_partition_models.rds"
)

saveRDS(
  variance_partition_models,
  vp_models_file
)

vp_models_verify <-
  readRDS(
    vp_models_file
  )

stopifnot(
  file.exists(
    vp_models_file
  ),
  
  file.info(
    vp_models_file
  )$size > 0,
  
  identical(
    names(vp_models_verify),
    names(variance_partition_models)
  ),
  
  all(
    vapply(
      vp_models_verify,
      stats::nobs,
      numeric(1)
    ) == 12460
  )
)

cat(
  "\nVariance-partition point estimates validated and models saved successfully.\n"
)

# ================================================================
# 16C. PARAMETRIC BOOTSTRAP FOR ICC CONFIDENCE INTERVALS
# ================================================================

# ------------------------------------------------
# Bootstrap statistic
#
# Returns percentage of total residual variance at:
#   1. macro-region level
#   2. country-within-region level
#   3. combined contextual level
#   4. respondent/residual level
# ------------------------------------------------

icc_boot_stat <- function(model) {
  
  vc <-
    as.data.frame(
      lme4::VarCorr(model)
    )
  
  macro_var <-
    vc$vcov[
      vc$grp == "region_fe"
    ]
  
  country_var <-
    vc$vcov[
      vc$grp == "country:region_fe"
    ]
  
  residual_var <-
    vc$vcov[
      vc$grp == "Residual"
    ]
  
  total_var <-
    macro_var +
    country_var +
    residual_var
  
  c(
    macroregion_icc =
      100 *
      macro_var /
      total_var,
    
    country_icc =
      100 *
      country_var /
      total_var,
    
    combined_contextual_icc =
      100 *
      (
        macro_var +
          country_var
      ) /
      total_var,
    
    residual_percent =
      100 *
      residual_var /
      total_var
  )
}

# ------------------------------------------------
# Reproducibility
# ------------------------------------------------

set.seed(20260925)

# ------------------------------------------------
# Socio-cultural
# ------------------------------------------------

cat(
  "\nStarting socio-cultural bootstrap: 1,000 simulations...\n"
)

boot_vp_socio <- lme4::bootMer(
  vp_socio,
  FUN = icc_boot_stat,
  nsim = 1000,
  type = "parametric",
  use.u = FALSE,
  .progress = "txt"
)

cat(
  "\nSocio-cultural bootstrap completed.\n"
)

# ------------------------------------------------
# Provisioning
# ------------------------------------------------

cat(
  "\nStarting provisioning bootstrap: 1,000 simulations...\n"
)

boot_vp_provisioning <- lme4::bootMer(
  vp_provisioning,
  FUN = icc_boot_stat,
  nsim = 1000,
  type = "parametric",
  use.u = FALSE,
  .progress = "txt"
)

cat(
  "\nProvisioning bootstrap completed.\n"
)

# ------------------------------------------------
# Regulating/supporting
# ------------------------------------------------

cat(
  "\nStarting regulating/supporting bootstrap: 1,000 simulations...\n"
)

boot_vp_regulating <- lme4::bootMer(
  vp_regulating,
  FUN = icc_boot_stat,
  nsim = 1000,
  type = "parametric",
  use.u = FALSE,
  .progress = "txt"
)

cat(
  "\nRegulating/supporting bootstrap completed.\n"
)

# ------------------------------------------------
# Store bootstrap objects
# ------------------------------------------------

vp_bootstrap_objects <- list(
  socio_cultural =
    boot_vp_socio,
  
  provisioning =
    boot_vp_provisioning,
  
  regulating_supporting =
    boot_vp_regulating
)

# ------------------------------------------------
# Check bootstrap dimensions and failures
# ------------------------------------------------

bootstrap_diagnostics <- dplyr::bind_rows(
  lapply(
    names(vp_bootstrap_objects),
    function(nm) {
      
      b <-
        vp_bootstrap_objects[[nm]]
      
      tibble::tibble(
        domain = nm,
        
        requested_simulations =
          1000,
        
        returned_simulations =
          nrow(b$t),
        
        complete_simulations =
          sum(
            stats::complete.cases(
              b$t
            )
          ),
        
        failed_simulations =
          sum(
            !stats::complete.cases(
              b$t
            )
          )
      )
    }
  )
)

cat(
  "\nBOOTSTRAP DIAGNOSTICS\n\n"
)

print(
  bootstrap_diagnostics,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Percentile 95% confidence intervals
# ------------------------------------------------

extract_boot_ci <- function(
    boot_object,
    domain_name
) {
  
  boot_matrix <-
    as.data.frame(
      boot_object$t
    )
  
  names(
    boot_matrix
  ) <- c(
    "macroregion_icc",
    "country_icc",
    "combined_contextual_icc",
    "residual_percent"
  )
  
  tibble::tibble(
    domain =
      domain_name,
    
    macroregion_low =
      stats::quantile(
        boot_matrix$macroregion_icc,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    macroregion_high =
      stats::quantile(
        boot_matrix$macroregion_icc,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
    
    country_low =
      stats::quantile(
        boot_matrix$country_icc,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    country_high =
      stats::quantile(
        boot_matrix$country_icc,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
    
    combined_low =
      stats::quantile(
        boot_matrix$combined_contextual_icc,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    combined_high =
      stats::quantile(
        boot_matrix$combined_contextual_icc,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
    
    residual_low =
      stats::quantile(
        boot_matrix$residual_percent,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    residual_high =
      stats::quantile(
        boot_matrix$residual_percent,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      )
  )
}

vp_bootstrap_ci <- dplyr::bind_rows(
  
  extract_boot_ci(
    boot_vp_socio,
    "Socio-cultural"
  ),
  
  extract_boot_ci(
    boot_vp_provisioning,
    "Provisioning"
  ),
  
  extract_boot_ci(
    boot_vp_regulating,
    "Regulating/supporting"
  )
)

cat(
  "\nBOOTSTRAP 95% CONFIDENCE INTERVALS\n\n"
)

print(
  vp_bootstrap_ci %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 2)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Formal bootstrap validation
# ------------------------------------------------

stopifnot(
  all(
    bootstrap_diagnostics$requested_simulations ==
      1000
  ),
  
  all(
    bootstrap_diagnostics$returned_simulations ==
      1000
  ),
  
  all(
    bootstrap_diagnostics$complete_simulations ==
      1000
  ),
  
  all(
    bootstrap_diagnostics$failed_simulations ==
      0
  ),
  
  nrow(
    vp_bootstrap_ci
  ) == 3,
  
  !anyNA(
    vp_bootstrap_ci
  )
)

cat(
  "\nAll 3,000 bootstrap simulations completed successfully.\n"
)


# ================================================================
# 16D-1. DEFINE EXISTING OUTPUT PATHS EXPLICITLY
# ================================================================

vp_bootstrap_file <- file.path(
  "major_revision",
  "models_corrected",
  "variance_partition_bootstrap_1000.rds"
)

vp_results_file <- file.path(
  "major_revision",
  "final_results",
  "variance_partition_results.csv"
)

# Confirm the parent directories already exist
stopifnot(
  dir.exists(
    file.path(
      "major_revision",
      "models_corrected"
    )
  ),
  dir.exists(
    file.path(
      "major_revision",
      "final_results"
    )
  )
)

cat(
  "\nOutput directories confirmed.\n",
  "\nBootstrap destination: ",
  vp_bootstrap_file,
  "\nResults destination:   ",
  vp_results_file,
  "\n",
  sep = ""
)

# ================================================================
# 16D-2. SAVE AND VERIFY BOOTSTRAP AND ICC RESULTS
# ================================================================

# ------------------------------------------------
# Combine point estimates with bootstrap CIs
# ------------------------------------------------

vp_results_final <- vp_validation %>%
  select(
    domain,
    macroregion_icc,
    country_icc,
    combined_contextual_icc,
    residual_percent
  ) %>%
  left_join(
    vp_bootstrap_ci,
    by = "domain"
  )

cat(
  "\nFINAL VARIANCE-PARTITION RESULTS\n\n"
)

print(
  vp_results_final %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 2)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Save completed bootstrap objects
# ------------------------------------------------

saveRDS(
  vp_bootstrap_objects,
  vp_bootstrap_file
)

# ------------------------------------------------
# Save compact analytical results
# ------------------------------------------------

write.csv(
  vp_results_final,
  vp_results_file,
  row.names = FALSE
)

# ------------------------------------------------
# Read files back
# ------------------------------------------------

vp_bootstrap_verify <-
  readRDS(
    vp_bootstrap_file
  )

vp_results_verify <-
  read.csv(
    vp_results_file,
    check.names = FALSE
  )

# ------------------------------------------------
# Validate read-back
# ------------------------------------------------

stopifnot(
  file.exists(vp_bootstrap_file),
  file.exists(vp_results_file),
  
  file.info(vp_bootstrap_file)$size > 0,
  file.info(vp_results_file)$size > 0,
  
  identical(
    names(vp_bootstrap_verify),
    names(vp_bootstrap_objects)
  ),
  
  all(
    vapply(
      vp_bootstrap_verify,
      function(x) nrow(x$t),
      integer(1)
    ) == 1000
  ),
  
  nrow(vp_results_verify) == 3,
  
  ncol(vp_results_verify) ==
    ncol(vp_results_final),
  
  !anyNA(vp_results_verify)
)

# ------------------------------------------------
# Confirm bootstrap failures from saved objects
# ------------------------------------------------

saved_bootstrap_check <- dplyr::bind_rows(
  lapply(
    names(vp_bootstrap_verify),
    function(nm) {
      
      b <- vp_bootstrap_verify[[nm]]
      
      tibble::tibble(
        domain = nm,
        simulations = nrow(b$t),
        complete =
          sum(
            stats::complete.cases(b$t)
          ),
        failed =
          sum(
            !stats::complete.cases(b$t)
          )
      )
    }
  )
)

cat(
  "\nSAVED BOOTSTRAP VERIFICATION\n\n"
)

print(
  saved_bootstrap_check,
  n = Inf
)

stopifnot(
  all(saved_bootstrap_check$simulations == 1000),
  all(saved_bootstrap_check$complete == 1000),
  all(saved_bootstrap_check$failed == 0)
)

# ------------------------------------------------
# File confirmation
# ------------------------------------------------

cat(
  "\nFILES SAVED AND VERIFIED\n\n",
  "Bootstrap: ",
  vp_bootstrap_file,
  " (",
  file.info(vp_bootstrap_file)$size,
  " bytes)\n",
  "Results:   ",
  vp_results_file,
  " (",
  file.info(vp_results_file)$size,
  " bytes)\n",
  sep = ""
)

cat(
  "\nStep 16D completed successfully.\n"
)

# ================================================================
# 16E. MAIN TABLE 3
# VARIANCE PARTITIONING AND ICCs
# ================================================================

# ------------------------------------------------
# Helper for estimate (95% CI)
# ------------------------------------------------

format_icc_ci <- function(
    estimate,
    lower,
    upper
) {
  
  sprintf(
    "%.2f (%.2f–%.2f)",
    estimate,
    lower,
    upper
  )
}

# ------------------------------------------------
# Construct publication-ready Table 3
# ------------------------------------------------

table3 <- vp_results_final %>%
  
  transmute(
    
    `Forest-value domain` =
      domain,
    
    `Macro-region ICC, % (95% CI)` =
      format_icc_ci(
        macroregion_icc,
        macroregion_low,
        macroregion_high
      ),
    
    `Country ICC, % (95% CI)` =
      format_icc_ci(
        country_icc,
        country_low,
        country_high
      ),
    
    `Combined contextual ICC, % (95% CI)` =
      format_icc_ci(
        combined_contextual_icc,
        combined_low,
        combined_high
      ),
    
    `Residual proportion, % (95% CI)` =
      format_icc_ci(
        residual_percent,
        residual_low,
        residual_high
      )
  )

# ------------------------------------------------
# Display Table 3
# ------------------------------------------------

cat(
  "\nTABLE 3\n",
  "Variance partitioning of forest-value perceptions\n\n",
  sep = ""
)

print(
  table3,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Expected structure
# ------------------------------------------------

stopifnot(
  nrow(table3) == 3,
  ncol(table3) == 5,
  
  identical(
    table3$`Forest-value domain`,
    c(
      "Socio-cultural",
      "Provisioning",
      "Regulating/supporting"
    )
  )
)

# ------------------------------------------------
# Output paths
# ------------------------------------------------

table3_csv <- file.path(
  "major_revision",
  "final_results",
  "tables",
  "Table_3_variance_partitioning_ICCs.csv"
)

table3_xlsx <- file.path(
  "major_revision",
  "final_results",
  "tables",
  "Table_3_variance_partitioning_ICCs.xlsx"
)

# ------------------------------------------------
# Export CSV
# ------------------------------------------------

write.csv(
  table3,
  table3_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------
# Export Excel
# ------------------------------------------------

wb_table3 <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb_table3,
  "Table 3"
)

openxlsx::writeData(
  wb_table3,
  sheet = "Table 3",
  x = table3,
  startRow = 1,
  startCol = 1
)

# Bold header
header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  wrapText = TRUE,
  valign = "center"
)

openxlsx::addStyle(
  wb_table3,
  sheet = "Table 3",
  style = header_style,
  rows = 1,
  cols = 1:ncol(table3),
  gridExpand = TRUE
)

# Add note below table
table3_note <- paste0(
  "Note. ICC = intraclass correlation coefficient. ",
  "Models included age, gender, education, residence length, ",
  "forest ownership, and social proximity to forestry as individual-level predictors, ",
  "with countries nested within the five FOREST EUROPE macro-regions. ",
  "Models used country-normalised respondent-level weights. ",
  "Values are percentages of total residual variance after adjustment for the included individual-level predictors. ",
  "95% confidence intervals were obtained from 1,000 parametric bootstrap simulations. ",
  "The socio-cultural macro-region variance was estimated at the boundary (zero)."
)

openxlsx::writeData(
  wb_table3,
  sheet = "Table 3",
  x = table3_note,
  startRow = nrow(table3) + 3,
  startCol = 1
)

openxlsx::mergeCells(
  wb_table3,
  sheet = "Table 3",
  cols = 1:ncol(table3),
  rows = nrow(table3) + 3
)

note_style <- openxlsx::createStyle(
  wrapText = TRUE,
  valign = "top"
)

openxlsx::addStyle(
  wb_table3,
  sheet = "Table 3",
  style = note_style,
  rows = nrow(table3) + 3,
  cols = 1,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  wb_table3,
  sheet = "Table 3",
  cols = 1,
  widths = 24
)

openxlsx::setColWidths(
  wb_table3,
  sheet = "Table 3",
  cols = 2:5,
  widths = 28
)

openxlsx::setRowHeights(
  wb_table3,
  sheet = "Table 3",
  rows = nrow(table3) + 3,
  heights = 55
)

openxlsx::saveWorkbook(
  wb_table3,
  table3_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------
# Read-back validation
# ------------------------------------------------

table3_csv_check <- read.csv(
  table3_csv,
  check.names = FALSE
)

table3_xlsx_check <- openxlsx::read.xlsx(
  table3_xlsx,
  sheet = "Table 3",
  rows = 1:4,
  check.names = FALSE
)

stopifnot(
  file.exists(table3_csv),
  file.exists(table3_xlsx),
  
  file.info(table3_csv)$size > 0,
  file.info(table3_xlsx)$size > 0,
  
  nrow(table3_csv_check) == 3,
  nrow(table3_xlsx_check) == 3,
  
  identical(
    as.character(
      table3_csv_check$`Forest-value domain`
    ),
    table3$`Forest-value domain`
  )
)

# ------------------------------------------------
# Final confirmation
# ------------------------------------------------

cat(
  "\nTABLE 3 SAVED AND VERIFIED\n\n",
  "CSV:  ",
  table3_csv,
  " (",
  file.info(table3_csv)$size,
  " bytes)\n",
  "XLSX: ",
  table3_xlsx,
  " (",
  file.info(table3_xlsx)$size,
  " bytes)\n",
  sep = ""
)

# ================================================================
# 17A. HIERARCHY SENSITIVITY ANALYSIS
# ================================================================

# ------------------------------------------------
# Purpose:
# Test whether individual-level coefficient estimates
# are sensitive to explicitly representing the
# macro-region/country hierarchy.
#
# Alternative specification:
#   individual predictors
#   + country nested within macro-region random intercepts
#
# The comparison focuses on the individual-level
# predictors rather than the regional coefficients.
# ------------------------------------------------

fit_hierarchy_sensitivity <- function(
    outcome,
    data = analysis_data
) {
  
  formula_hierarchy <- as.formula(
    paste0(
      outcome,
      " ~ age_model + gender_model + education_model + ",
      "residence_model + ownership_model + social_model + ",
      "(1 | region_fe/country)"
    )
  )
  
  lme4::lmer(
    formula_hierarchy,
    data = data,
    weights = weight_norm,
    REML = TRUE,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(
        maxfun = 200000
      )
    )
  )
}

# ------------------------------------------------
# Fit alternative hierarchy models
# ------------------------------------------------

hierarchy_socio <-
  fit_hierarchy_sensitivity(
    "socio_cultural"
  )

hierarchy_provisioning <-
  fit_hierarchy_sensitivity(
    "provisioning"
  )

hierarchy_regulating <-
  fit_hierarchy_sensitivity(
    "regulating"
  )

hierarchy_models <- list(
  socio_cultural =
    hierarchy_socio,
  
  provisioning =
    hierarchy_provisioning,
  
  regulating_supporting =
    hierarchy_regulating
)

# ------------------------------------------------
# Diagnostics
# ------------------------------------------------

hierarchy_diagnostics <- dplyr::bind_rows(
  lapply(
    names(hierarchy_models),
    function(nm) {
      
      m <- hierarchy_models[[nm]]
      
      conv_message <-
        m@optinfo$conv$lme4$messages
      
      tibble::tibble(
        model = nm,
        
        N =
          stats::nobs(m),
        
        singular =
          lme4::isSingular(
            m,
            tol = 1e-4
          ),
        
        convergence =
          if (
            is.null(conv_message)
          ) {
            "OK"
          } else {
            paste(
              conv_message,
              collapse = "; "
            )
          }
      )
    }
  )
)

cat(
  "\nHIERARCHY SENSITIVITY MODEL DIAGNOSTICS\n\n"
)

print(
  hierarchy_diagnostics,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Variance components
# ------------------------------------------------

hierarchy_variance <- dplyr::bind_rows(
  lapply(
    names(hierarchy_models),
    function(nm) {
      
      m <- hierarchy_models[[nm]]
      
      vc <-
        as.data.frame(
          lme4::VarCorr(m)
        )
      
      tibble::tibble(
        model = nm,
        
        macroregion_variance =
          vc$vcov[
            vc$grp == "region_fe"
          ],
        
        country_variance =
          vc$vcov[
            vc$grp == "country:region_fe"
          ],
        
        residual_variance =
          vc$vcov[
            vc$grp == "Residual"
          ]
      )
    }
  )
)

cat(
  "\nHIERARCHY SENSITIVITY VARIANCE COMPONENTS\n\n"
)

print(
  hierarchy_variance %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 6)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Structural validation
#
# Socio-cultural is expected to be singular because
# the macro-region variance is estimated at zero.
# ------------------------------------------------

stopifnot(
  all(
    hierarchy_diagnostics$N ==
      12460
  ),
  
  hierarchy_diagnostics$singular[
    hierarchy_diagnostics$model ==
      "socio_cultural"
  ],
  
  !hierarchy_diagnostics$singular[
    hierarchy_diagnostics$model ==
      "provisioning"
  ],
  
  !hierarchy_diagnostics$singular[
    hierarchy_diagnostics$model ==
      "regulating_supporting"
  ],
  
  hierarchy_variance$macroregion_variance[
    hierarchy_variance$model ==
      "socio_cultural"
  ] == 0
)

cat(
  "\nHierarchy sensitivity models fitted successfully.\n"
)


# ================================================================
# 17B. COMPARE INDIVIDUAL-LEVEL COEFFICIENTS
# PRIMARY VS ALTERNATIVE HIERARCHY MODELS
# ================================================================

# ------------------------------------------------
# Extract fixed effects with 95% CIs
# ------------------------------------------------

extract_fixed_hierarchy <- function(
    model,
    domain_name
) {
  
  broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    mutate(
      domain = domain_name
    ) %>%
    select(
      domain,
      term,
      estimate,
      conf.low,
      conf.high
    )
}

hierarchy_fixed <- bind_rows(
  
  extract_fixed_hierarchy(
    hierarchy_socio,
    "Socio-cultural"
  ),
  
  extract_fixed_hierarchy(
    hierarchy_provisioning,
    "Provisioning"
  ),
  
  extract_fixed_hierarchy(
    hierarchy_regulating,
    "Regulating/supporting"
  )
)

# ------------------------------------------------
# Prepare primary-model coefficients
#
# primary_fixed was created and validated in Step 14B.
# Keep only the 20 individual-level coefficients/domain.
# ------------------------------------------------

individual_term_pattern <-
  paste(
    c(
      "^age_model",
      "^gender_model",
      "^education_model",
      "^residence_model",
      "^ownership_model",
      "^social_model"
    ),
    collapse = "|"
  )

primary_individual <- primary_fixed %>%
  filter(
    grepl(
      individual_term_pattern,
      term
    )
  ) %>%
  select(
    domain,
    term,
    primary_estimate = estimate,
    primary_low = conf.low,
    primary_high = conf.high
  )

hierarchy_individual <- hierarchy_fixed %>%
  filter(
    grepl(
      individual_term_pattern,
      term
    )
  ) %>%
  select(
    domain,
    term,
    hierarchy_estimate = estimate,
    hierarchy_low = conf.low,
    hierarchy_high = conf.high
  )

# ------------------------------------------------
# Check coefficient counts before joining
# ------------------------------------------------

cat(
  "\nINDIVIDUAL COEFFICIENT COUNTS\n\n"
)

print(
  primary_individual %>%
    count(
      domain,
      name = "primary_n"
    ),
  n = Inf
)

print(
  hierarchy_individual %>%
    count(
      domain,
      name = "hierarchy_n"
    ),
  n = Inf
)

stopifnot(
  nrow(primary_individual) == 60,
  nrow(hierarchy_individual) == 60
)

# ------------------------------------------------
# Join primary and hierarchy estimates
# ------------------------------------------------

hierarchy_comparison <- primary_individual %>%
  inner_join(
    hierarchy_individual,
    by = c(
      "domain",
      "term"
    )
  ) %>%
  mutate(
    
    coefficient_difference =
      hierarchy_estimate -
      primary_estimate,
    
    absolute_difference =
      abs(
        coefficient_difference
      ),
    
    same_direction =
      sign(primary_estimate) ==
      sign(hierarchy_estimate),
    
    primary_ci_excludes_zero =
      primary_low > 0 |
      primary_high < 0,
    
    hierarchy_ci_excludes_zero =
      hierarchy_low > 0 |
      hierarchy_high < 0,
    
    same_ci_conclusion =
      primary_ci_excludes_zero ==
      hierarchy_ci_excludes_zero
  )

# ------------------------------------------------
# Validate complete matching
# ------------------------------------------------

stopifnot(
  nrow(hierarchy_comparison) == 60,
  !anyNA(hierarchy_comparison)
)

# ------------------------------------------------
# Overall comparison summary
# ------------------------------------------------

hierarchy_summary_overall <-
  hierarchy_comparison %>%
  summarise(
    coefficients_compared =
      n(),
    
    same_direction =
      sum(same_direction),
    
    same_ci_conclusion =
      sum(same_ci_conclusion),
    
    direction_changes =
      sum(!same_direction),
    
    ci_conclusion_changes =
      sum(!same_ci_conclusion),
    
    maximum_absolute_difference =
      max(absolute_difference)
  )

cat(
  "\nHIERARCHY SENSITIVITY: OVERALL SUMMARY\n\n"
)

print(
  hierarchy_summary_overall,
  width = Inf
)

# ------------------------------------------------
# Domain-specific summary
# ------------------------------------------------

hierarchy_summary_domain <-
  hierarchy_comparison %>%
  group_by(
    domain
  ) %>%
  summarise(
    coefficients_compared =
      n(),
    
    same_direction =
      sum(same_direction),
    
    same_ci_conclusion =
      sum(same_ci_conclusion),
    
    direction_changes =
      sum(!same_direction),
    
    ci_conclusion_changes =
      sum(!same_ci_conclusion),
    
    maximum_absolute_difference =
      max(absolute_difference),
    
    .groups = "drop"
  )

cat(
  "\nHIERARCHY SENSITIVITY: DOMAIN SUMMARY\n\n"
)

print(
  hierarchy_summary_domain %>%
    mutate(
      maximum_absolute_difference =
        round(
          maximum_absolute_difference,
          6
        )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Show any disagreements
# ------------------------------------------------

hierarchy_disagreements <-
  hierarchy_comparison %>%
  filter(
    !same_direction |
      !same_ci_conclusion
  )

cat(
  "\nCOEFFICIENTS WITH ANY DIRECTION OR CI-CONCLUSION CHANGE\n\n"
)

print(
  hierarchy_disagreements,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Formal validation against the previously
# validated sensitivity result
# ------------------------------------------------

stopifnot(
  hierarchy_summary_overall$coefficients_compared == 60,
  
  hierarchy_summary_overall$same_direction == 60,
  
  hierarchy_summary_overall$same_ci_conclusion == 60,
  
  hierarchy_summary_overall$direction_changes == 0,
  
  hierarchy_summary_overall$ci_conclusion_changes == 0,
  
  hierarchy_summary_overall$maximum_absolute_difference < 0.002
)

cat(
  "\nHierarchy coefficient comparison validated successfully.\n"
)

# ================================================================
# 17B-2. HIERARCHY COMPARISON DIAGNOSTICS
# ================================================================

cat(
  "\nOVERALL SUMMARY\n\n"
)

print(
  hierarchy_summary_overall,
  width = Inf
)

cat(
  "\nDOMAIN SUMMARY\n\n"
)

print(
  hierarchy_summary_domain %>%
    mutate(
      maximum_absolute_difference =
        round(
          maximum_absolute_difference,
          6
        )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Largest coefficient differences
# ------------------------------------------------

hierarchy_largest_differences <-
  hierarchy_comparison %>%
  arrange(
    desc(absolute_difference)
  ) %>%
  select(
    domain,
    term,
    primary_estimate,
    hierarchy_estimate,
    coefficient_difference,
    absolute_difference,
    same_direction,
    primary_ci_excludes_zero,
    hierarchy_ci_excludes_zero,
    same_ci_conclusion
  ) %>%
  slice_head(
    n = 15
  )

cat(
  "\n15 LARGEST COEFFICIENT DIFFERENCES\n\n"
)

print(
  hierarchy_largest_differences %>%
    mutate(
      across(
        c(
          primary_estimate,
          hierarchy_estimate,
          coefficient_difference,
          absolute_difference
        ),
        ~ round(.x, 6)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Confirm number of direction and CI changes
# ------------------------------------------------

cat(
  "\nDIRECTION AND CI AGREEMENT\n\n"
)

hierarchy_agreement_check <-
  hierarchy_comparison %>%
  summarise(
    total = n(),
    
    same_direction =
      sum(same_direction),
    
    changed_direction =
      sum(!same_direction),
    
    same_ci_conclusion =
      sum(same_ci_conclusion),
    
    changed_ci_conclusion =
      sum(!same_ci_conclusion)
  )

print(
  hierarchy_agreement_check,
  width = Inf
)

# ------------------------------------------------
# Inspect model formulas being compared
# ------------------------------------------------

cat(
  "\nPRIMARY MODEL FORMULAS\n\n"
)

print(
  formula(model_sc_primary)
)

print(
  formula(model_pr_primary)
)

print(
  formula(model_rs_primary)
)

cat(
  "\nHIERARCHY MODEL FORMULAS\n\n"
)

print(
  formula(hierarchy_socio)
)

print(
  formula(hierarchy_provisioning)
)

print(
  formula(hierarchy_regulating)
)

# ------------------------------------------------
# Confirm primary model random-effect variance
# ------------------------------------------------

cat(
  "\nPRIMARY MODEL RANDOM-EFFECT VARIANCES\n\n"
)

for (
  nm in c(
    "Socio-cultural",
    "Provisioning",
    "Regulating/supporting"
  )
) {
  
  m <-
    switch(
      nm,
      "Socio-cultural" =
        model_sc_primary,
      "Provisioning" =
        model_pr_primary,
      "Regulating/supporting" =
        model_rs_primary
    )
  
  cat(
    "\n",
    nm,
    "\n",
    sep = ""
  )
  
  print(
    as.data.frame(
      lme4::VarCorr(m)
    )[
      ,
      c(
        "grp",
        "vcov"
      )
    ],
    row.names = FALSE
  )
}


# ================================================================
# 17C. FINAL TABLE S4
# HIERARCHY SENSITIVITY ANALYSIS
# ================================================================

# ------------------------------------------------
# Calculate contextual ICCs for hierarchy models
# ------------------------------------------------

hierarchy_icc <- hierarchy_variance %>%
  mutate(
    total_variance =
      macroregion_variance +
      country_variance +
      residual_variance,
    
    macroregion_icc =
      100 *
      macroregion_variance /
      total_variance,
    
    country_icc =
      100 *
      country_variance /
      total_variance,
    
    combined_contextual_icc =
      100 *
      (
        macroregion_variance +
          country_variance
      ) /
      total_variance
  ) %>%
  select(
    model,
    macroregion_icc,
    country_icc,
    combined_contextual_icc
  )

# ------------------------------------------------
# Assemble domain-level sensitivity summary
# ------------------------------------------------

table_s4 <- hierarchy_summary_domain %>%
  mutate(
    model =
      case_when(
        domain == "Socio-cultural" ~
          "socio_cultural",
        
        domain == "Provisioning" ~
          "provisioning",
        
        domain == "Regulating/supporting" ~
          "regulating_supporting"
      )
  ) %>%
  left_join(
    hierarchy_icc,
    by = "model"
  ) %>%
  transmute(
    `Forest-value domain` =
      domain,
    
    `Individual coefficients compared` =
      coefficients_compared,
    
    `Same direction, n` =
      same_direction,
    
    `Same CI conclusion, n` =
      same_ci_conclusion,
    
    `Direction changes, n` =
      direction_changes,
    
    `CI-conclusion changes, n` =
      ci_conclusion_changes,
    
    `Maximum absolute coefficient difference` =
      round(
        maximum_absolute_difference,
        4
      ),
    
    `Macro-region ICC, %` =
      round(
        macroregion_icc,
        2
      ),
    
    `Country ICC, %` =
      round(
        country_icc,
        2
      ),
    
    `Combined contextual ICC, %` =
      round(
        combined_contextual_icc,
        2
      )
  )

# ------------------------------------------------
# Put domains in manuscript order
# ------------------------------------------------

table_s4 <-
  table_s4 %>%
  mutate(
    `Forest-value domain` =
      factor(
        `Forest-value domain`,
        levels = c(
          "Socio-cultural",
          "Provisioning",
          "Regulating/supporting"
        )
      )
  ) %>%
  arrange(
    `Forest-value domain`
  ) %>%
  mutate(
    `Forest-value domain` =
      as.character(
        `Forest-value domain`
      )
  )

cat(
  "\nFINAL TABLE S4\n\n"
)

print(
  table_s4,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Final validation
# ------------------------------------------------

stopifnot(
  nrow(table_s4) == 3,
  
  all(
    table_s4$`Individual coefficients compared` ==
      20
  ),
  
  all(
    table_s4$`Same direction, n` ==
      20
  ),
  
  all(
    table_s4$`Same CI conclusion, n` ==
      20
  ),
  
  all(
    table_s4$`Direction changes, n` ==
      0
  ),
  
  all(
    table_s4$`CI-conclusion changes, n` ==
      0
  ),
  
  max(
    table_s4$
      `Maximum absolute coefficient difference`
  ) < 0.004
)

# ------------------------------------------------
# Export
# ------------------------------------------------

s4_csv <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S4_hierarchy_sensitivity.csv"
  )

s4_xlsx <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S4_hierarchy_sensitivity.xlsx"
  )

write.csv(
  table_s4,
  s4_csv,
  row.names = FALSE,
  na = ""
)

openxlsx::write.xlsx(
  table_s4,
  s4_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------
# Read-back verification
# ------------------------------------------------

s4_csv_check <-
  read.csv(
    s4_csv,
    check.names = FALSE
  )

s4_xlsx_check <-
  openxlsx::read.xlsx(
    s4_xlsx,
    check.names = FALSE
  )

stopifnot(
  nrow(s4_csv_check) == 3,
  nrow(s4_xlsx_check) == 3,
  ncol(s4_csv_check) == ncol(table_s4),
  ncol(s4_xlsx_check) == ncol(table_s4)
)

cat(
  "\nTable S4 exported and read back successfully.\n"
)

cat(
  "\nCSV:",
  s4_csv,
  "\nXLSX:",
  s4_xlsx,
  "\n"
)


# ================================================================
# 18A. WEIGHTED VS UNWEIGHTED SENSITIVITY ANALYSIS
# ================================================================

# ------------------------------------------------
# Purpose:
# Refit the primary models without respondent weights
# while keeping all other model components identical.
#
# Weighted models:
#   already fitted and validated in Step 14A
#
# Unweighted models:
#   same fixed effects
#   same country random intercept
#   same analytical sample
#   same REML estimation
#   no weights argument
# ------------------------------------------------

fit_unweighted_primary <- function(
    outcome,
    data = analysis_data
) {
  
  formula_unweighted <- as.formula(
    paste0(
      outcome,
      " ~ region_fe + age_model + gender_model + ",
      "education_model + residence_model + ownership_model + ",
      "social_model + (1 | country)"
    )
  )
  
  lme4::lmer(
    formula_unweighted,
    data = data,
    REML = TRUE,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(
        maxfun = 200000
      )
    )
  )
}

# ------------------------------------------------
# Fit models
# ------------------------------------------------

model_sc_unweighted <-
  fit_unweighted_primary(
    "socio_cultural"
  )

model_pr_unweighted <-
  fit_unweighted_primary(
    "provisioning"
  )

model_rs_unweighted <-
  fit_unweighted_primary(
    "regulating"
  )

unweighted_models <- list(
  socio_cultural =
    model_sc_unweighted,
  
  provisioning =
    model_pr_unweighted,
  
  regulating_supporting =
    model_rs_unweighted
)

# ------------------------------------------------
# Diagnostics
# ------------------------------------------------

unweighted_diagnostics <-
  dplyr::bind_rows(
    lapply(
      names(unweighted_models),
      function(nm) {
        
        m <-
          unweighted_models[[nm]]
        
        conv_message <-
          m@optinfo$conv$lme4$messages
        
        tibble::tibble(
          model = nm,
          
          N =
            stats::nobs(m),
          
          singular =
            lme4::isSingular(
              m,
              tol = 1e-4
            ),
          
          convergence =
            if (
              is.null(conv_message)
            ) {
              "OK"
            } else {
              paste(
                conv_message,
                collapse = "; "
              )
            }
        )
      }
    )
  )

cat(
  "\nUNWEIGHTED MODEL DIAGNOSTICS\n\n"
)

print(
  unweighted_diagnostics,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Variance components
# ------------------------------------------------

unweighted_variance <-
  dplyr::bind_rows(
    lapply(
      names(unweighted_models),
      function(nm) {
        
        m <-
          unweighted_models[[nm]]
        
        vc <-
          as.data.frame(
            lme4::VarCorr(m)
          )
        
        tibble::tibble(
          model = nm,
          
          country_variance =
            vc$vcov[
              vc$grp == "country"
            ],
          
          residual_variance =
            vc$vcov[
              vc$grp == "Residual"
            ]
        )
      }
    )
  )

cat(
  "\nUNWEIGHTED MODEL VARIANCE COMPONENTS\n\n"
)

print(
  unweighted_variance %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 6)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Confirm formulas
# ------------------------------------------------

cat(
  "\nUNWEIGHTED MODEL FORMULAS\n\n"
)

print(
  formula(
    model_sc_unweighted
  )
)

print(
  formula(
    model_pr_unweighted
  )
)

print(
  formula(
    model_rs_unweighted
  )
)

# ------------------------------------------------
# Structural validation
# ------------------------------------------------

stopifnot(
  
  all(
    unweighted_diagnostics$N ==
      12460
  ),
  
  all(
    unweighted_diagnostics$singular ==
      FALSE
  ),
  
  all(
    unweighted_diagnostics$convergence ==
      "OK"
  )
)

cat(
  "\nUnweighted primary models fitted successfully.\n"
)


# ================================================================
# 18B. COMPARE WEIGHTED AND UNWEIGHTED PRIMARY MODELS
# ================================================================

# ------------------------------------------------
# Extract unweighted fixed effects with 95% CIs
# ------------------------------------------------

extract_unweighted_fixed <- function(
    model,
    domain_name
) {
  
  broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    mutate(
      domain = domain_name
    ) %>%
    select(
      domain,
      term,
      estimate,
      conf.low,
      conf.high
    )
}

unweighted_fixed <- bind_rows(
  
  extract_unweighted_fixed(
    model_sc_unweighted,
    "Socio-cultural"
  ),
  
  extract_unweighted_fixed(
    model_pr_unweighted,
    "Provisioning"
  ),
  
  extract_unweighted_fixed(
    model_rs_unweighted,
    "Regulating/supporting"
  )
)

# ------------------------------------------------
# Remove intercept from both specifications
#
# Expected:
# 24 non-intercept coefficients per domain
# 72 coefficients overall
# ------------------------------------------------

weighted_nonintercept <- primary_fixed %>%
  filter(
    term != "(Intercept)"
  ) %>%
  select(
    domain,
    term,
    weighted_estimate = estimate,
    weighted_low = conf.low,
    weighted_high = conf.high
  )

unweighted_nonintercept <- unweighted_fixed %>%
  filter(
    term != "(Intercept)"
  ) %>%
  select(
    domain,
    term,
    unweighted_estimate = estimate,
    unweighted_low = conf.low,
    unweighted_high = conf.high
  )

# ------------------------------------------------
# Check coefficient counts before joining
# ------------------------------------------------

cat(
  "\nNON-INTERCEPT COEFFICIENT COUNTS\n\n"
)

print(
  weighted_nonintercept %>%
    count(
      domain,
      name = "weighted_n"
    ),
  n = Inf
)

print(
  unweighted_nonintercept %>%
    count(
      domain,
      name = "unweighted_n"
    ),
  n = Inf
)

stopifnot(
  nrow(weighted_nonintercept) == 72,
  nrow(unweighted_nonintercept) == 72
)

# ------------------------------------------------
# Join weighted and unweighted estimates
# ------------------------------------------------

weighting_comparison <- weighted_nonintercept %>%
  inner_join(
    unweighted_nonintercept,
    by = c(
      "domain",
      "term"
    )
  ) %>%
  mutate(
    
    coefficient_difference =
      unweighted_estimate -
      weighted_estimate,
    
    absolute_difference =
      abs(
        coefficient_difference
      ),
    
    same_direction =
      sign(weighted_estimate) ==
      sign(unweighted_estimate),
    
    weighted_ci_excludes_zero =
      weighted_low > 0 |
      weighted_high < 0,
    
    unweighted_ci_excludes_zero =
      unweighted_low > 0 |
      unweighted_high < 0,
    
    same_ci_conclusion =
      weighted_ci_excludes_zero ==
      unweighted_ci_excludes_zero
  )

stopifnot(
  nrow(weighting_comparison) == 72,
  !anyNA(weighting_comparison)
)

# ------------------------------------------------
# Overall summary
# ------------------------------------------------

weighting_summary_overall <-
  weighting_comparison %>%
  summarise(
    coefficients_compared =
      n(),
    
    same_direction =
      sum(same_direction),
    
    direction_changes =
      sum(!same_direction),
    
    same_ci_conclusion =
      sum(same_ci_conclusion),
    
    ci_conclusion_changes =
      sum(!same_ci_conclusion),
    
    maximum_absolute_difference =
      max(absolute_difference)
  )

cat(
  "\nWEIGHTED VS UNWEIGHTED: OVERALL SUMMARY\n\n"
)

print(
  weighting_summary_overall,
  width = Inf
)

# ------------------------------------------------
# Domain-specific summary
# ------------------------------------------------

weighting_summary_domain <-
  weighting_comparison %>%
  group_by(
    domain
  ) %>%
  summarise(
    coefficients_compared =
      n(),
    
    same_direction =
      sum(same_direction),
    
    direction_changes =
      sum(!same_direction),
    
    same_ci_conclusion =
      sum(same_ci_conclusion),
    
    ci_conclusion_changes =
      sum(!same_ci_conclusion),
    
    maximum_absolute_difference =
      max(absolute_difference),
    
    .groups = "drop"
  )

cat(
  "\nWEIGHTED VS UNWEIGHTED: DOMAIN SUMMARY\n\n"
)

print(
  weighting_summary_domain %>%
    mutate(
      maximum_absolute_difference =
        round(
          maximum_absolute_difference,
          6
        )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Identify all direction or CI-status changes
# ------------------------------------------------

weighting_disagreements <-
  weighting_comparison %>%
  filter(
    !same_direction |
      !same_ci_conclusion
  ) %>%
  arrange(
    domain,
    term
  )

cat(
  "\nDIRECTION OR CI-CONCLUSION CHANGES\n\n"
)

print(
  weighting_disagreements,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Show the 10 largest absolute coefficient differences
# ------------------------------------------------

cat(
  "\n10 LARGEST ABSOLUTE COEFFICIENT DIFFERENCES\n\n"
)

print(
  weighting_comparison %>%
    arrange(
      desc(absolute_difference)
    ) %>%
    select(
      domain,
      term,
      weighted_estimate,
      unweighted_estimate,
      coefficient_difference,
      absolute_difference,
      same_direction,
      same_ci_conclusion
    ) %>%
    slice_head(
      n = 10
    ) %>%
    mutate(
      across(
        c(
          weighted_estimate,
          unweighted_estimate,
          coefficient_difference,
          absolute_difference
        ),
        ~ round(.x, 6)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Validation of the broad previously established result
#
# We deliberately do NOT hard-code the old maximum difference
# or the identities of disagreements before seeing the clean run.
# ------------------------------------------------

stopifnot(
  weighting_summary_overall$coefficients_compared == 72
)

cat(
  "\nWeighted versus unweighted comparison completed successfully.\n"
)

# ================================================================
# 18C. WEIGHTING-SENSITIVITY SUMMARY
# ================================================================

# ------------------------------------------------
# Overall summary
# Use distinct output names to avoid dplyr name masking
# ------------------------------------------------

weighting_summary_overall <-
  weighting_comparison %>%
  summarise(
    coefficients_compared =
      n(),
    
    n_same_direction =
      sum(same_direction),
    
    n_direction_changes =
      sum(!same_direction),
    
    n_same_ci_conclusion =
      sum(same_ci_conclusion),
    
    n_ci_conclusion_changes =
      sum(!same_ci_conclusion),
    
    maximum_absolute_difference =
      max(absolute_difference)
  )

# ------------------------------------------------
# Domain-specific summary
# ------------------------------------------------

weighting_summary_domain <-
  weighting_comparison %>%
  group_by(
    domain
  ) %>%
  summarise(
    coefficients_compared =
      n(),
    
    n_same_direction =
      sum(same_direction),
    
    n_direction_changes =
      sum(!same_direction),
    
    n_same_ci_conclusion =
      sum(same_ci_conclusion),
    
    n_ci_conclusion_changes =
      sum(!same_ci_conclusion),
    
    maximum_absolute_difference =
      max(absolute_difference),
    
    .groups = "drop"
  )

cat(
  "\nCORRECTED WEIGHTED VS UNWEIGHTED OVERALL SUMMARY\n\n"
)

print(
  weighting_summary_overall,
  width = Inf
)

cat(
  "\nCORRECTED DOMAIN SUMMARY\n\n"
)

print(
  weighting_summary_domain %>%
    mutate(
      maximum_absolute_difference =
        round(
          maximum_absolute_difference,
          6
        )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Formal validation against detailed comparison
# ------------------------------------------------

stopifnot(
  weighting_summary_overall$coefficients_compared == 72,
  
  weighting_summary_overall$n_same_direction == 70,
  
  weighting_summary_overall$n_direction_changes == 2,
  
  weighting_summary_overall$n_same_ci_conclusion == 71,
  
  weighting_summary_overall$n_ci_conclusion_changes == 1,
  
  abs(
    weighting_summary_overall$maximum_absolute_difference -
      0.038508
  ) < 0.00001,
  
  nrow(
    weighting_comparison %>%
      filter(!same_direction)
  ) == 2,
  
  nrow(
    weighting_comparison %>%
      filter(!same_ci_conclusion)
  ) == 1
)

cat(
  "\nCorrected weighting-sensitivity summary validated successfully.\n"
)

# ================================================================
# 18D. FINAL TABLE S5
# WEIGHTED VS UNWEIGHTED SENSITIVITY ANALYSIS
# ================================================================

# ------------------------------------------------
# Assemble domain-level summary
# ------------------------------------------------

table_s5 <-
  weighting_summary_domain %>%
  transmute(
    `Forest-value domain` =
      domain,
    
    `Coefficients compared` =
      coefficients_compared,
    
    `Same direction, n` =
      n_same_direction,
    
    `Direction changes, n` =
      n_direction_changes,
    
    `Same CI conclusion, n` =
      n_same_ci_conclusion,
    
    `CI-conclusion changes, n` =
      n_ci_conclusion_changes,
    
    `Maximum absolute coefficient difference` =
      round(
        maximum_absolute_difference,
        4
      )
  )

# ------------------------------------------------
# Put domains in manuscript order
# ------------------------------------------------

table_s5 <-
  table_s5 %>%
  mutate(
    `Forest-value domain` =
      factor(
        `Forest-value domain`,
        levels = c(
          "Socio-cultural",
          "Provisioning",
          "Regulating/supporting"
        )
      )
  ) %>%
  arrange(
    `Forest-value domain`
  ) %>%
  mutate(
    `Forest-value domain` =
      as.character(
        `Forest-value domain`
      )
  )

cat(
  "\nFINAL TABLE S5\n\n"
)

print(
  table_s5,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Overall row for validation/reporting
# ------------------------------------------------

table_s5_overall <-
  tibble::tibble(
    `Forest-value domain` =
      "Overall",
    
    `Coefficients compared` =
      weighting_summary_overall$
      coefficients_compared,
    
    `Same direction, n` =
      weighting_summary_overall$
      n_same_direction,
    
    `Direction changes, n` =
      weighting_summary_overall$
      n_direction_changes,
    
    `Same CI conclusion, n` =
      weighting_summary_overall$
      n_same_ci_conclusion,
    
    `CI-conclusion changes, n` =
      weighting_summary_overall$
      n_ci_conclusion_changes,
    
    `Maximum absolute coefficient difference` =
      round(
        weighting_summary_overall$
          maximum_absolute_difference,
        4
      )
  )

table_s5_export <-
  bind_rows(
    table_s5,
    table_s5_overall
  )

cat(
  "\nFINAL TABLE S5 WITH OVERALL ROW\n\n"
)

print(
  table_s5_export,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Validation
# ------------------------------------------------

stopifnot(
  
  nrow(table_s5) == 3,
  
  all(
    table_s5$
      `Coefficients compared` ==
      24
  ),
  
  sum(
    table_s5$
      `Same direction, n`
  ) == 70,
  
  sum(
    table_s5$
      `Direction changes, n`
  ) == 2,
  
  sum(
    table_s5$
      `Same CI conclusion, n`
  ) == 71,
  
  sum(
    table_s5$
      `CI-conclusion changes, n`
  ) == 1,
  
  table_s5_overall$
    `Coefficients compared` == 72,
  
  table_s5_overall$
    `Same direction, n` == 70,
  
  table_s5_overall$
    `Direction changes, n` == 2,
  
  table_s5_overall$
    `Same CI conclusion, n` == 71,
  
  table_s5_overall$
    `CI-conclusion changes, n` == 1
)

# ------------------------------------------------
# Export
# ------------------------------------------------

s5_csv <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S5_weighted_vs_unweighted.csv"
  )

s5_xlsx <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S5_weighted_vs_unweighted.xlsx"
  )

write.csv(
  table_s5_export,
  s5_csv,
  row.names = FALSE,
  na = ""
)

openxlsx::write.xlsx(
  table_s5_export,
  s5_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------
# Read-back verification
# ------------------------------------------------

s5_csv_check <-
  read.csv(
    s5_csv,
    check.names = FALSE
  )

s5_xlsx_check <-
  openxlsx::read.xlsx(
    s5_xlsx,
    check.names = FALSE
  )

stopifnot(
  nrow(s5_csv_check) == 4,
  nrow(s5_xlsx_check) == 4,
  ncol(s5_csv_check) ==
    ncol(table_s5_export),
  ncol(s5_xlsx_check) ==
    ncol(table_s5_export)
)

cat(
  "\nTable S5 exported and read back successfully.\n"
)

cat(
  "\nCSV:",
  s5_csv,
  "\nXLSX:",
  s5_xlsx,
  "\n"
)


# ================================================================
# 19A. MISSING-CATEGORY SENSITIVITY:
# CONSTRUCT RESTRICTED ANALYTICAL SAMPLE
# ================================================================

# ------------------------------------------------
# Inspect the relevant factor levels first
# ------------------------------------------------

cat("\nRESIDENCE MODEL LEVELS\n\n")
print(
  levels(analysis_data$residence_model)
)

cat("\nSOCIAL PROXIMITY MODEL LEVELS\n\n")
print(
  levels(analysis_data$social_model)
)

# ------------------------------------------------
# Frequency distributions in the full analytical sample
# ------------------------------------------------

cat("\nRESIDENCE MODEL DISTRIBUTION\n\n")

print(
  analysis_data %>%
    count(
      residence_model,
      .drop = FALSE
    ),
  n = Inf
)

cat("\nSOCIAL PROXIMITY MODEL DISTRIBUTION\n\n")

print(
  analysis_data %>%
    count(
      social_model,
      .drop = FALSE
    ),
  n = Inf
)

# ------------------------------------------------
# Identify exclusion conditions
#
# residence_model == "Missing"
# social_model == "Don't know"
# ------------------------------------------------

missing_sensitivity_flags <-
  analysis_data %>%
  transmute(
    country,
    residence_model,
    social_model,
    
    missing_residence =
      residence_model == "Missing",
    
    social_dont_know =
      social_model == "Don't know",
    
    exclude_restricted =
      missing_residence |
      social_dont_know
  )

# ------------------------------------------------
# Overall exclusion accounting
# ------------------------------------------------

exclusion_summary <-
  missing_sensitivity_flags %>%
  summarise(
    full_sample_n =
      n(),
    
    missing_residence_n =
      sum(missing_residence),
    
    social_dont_know_n =
      sum(social_dont_know),
    
    both_conditions_n =
      sum(
        missing_residence &
          social_dont_know
      ),
    
    excluded_unique_n =
      sum(exclude_restricted),
    
    retained_n =
      sum(!exclude_restricted),
    
    retained_percent =
      100 *
      mean(!exclude_restricted)
  )

cat("\nMISSING-CATEGORY EXCLUSION SUMMARY\n\n")

print(
  exclusion_summary,
  width = Inf
)

# ------------------------------------------------
# Cross-tabulation to verify overlap
# ------------------------------------------------

cat("\nOVERLAP OF EXCLUSION CONDITIONS\n\n")

print(
  with(
    missing_sensitivity_flags,
    table(
      missing_residence,
      social_dont_know,
      useNA = "ifany"
    )
  )
)

# ------------------------------------------------
# Construct restricted sample
# ------------------------------------------------

analysis_data_restricted <-
  analysis_data %>%
  filter(
    residence_model != "Missing",
    social_model != "Don't know"
  ) %>%
  droplevels()

# ------------------------------------------------
# Verify restricted sample
# ------------------------------------------------

cat("\nRESTRICTED SAMPLE SIZE\n\n")

cat(
  "Full analytical N:",
  nrow(analysis_data),
  "\n"
)

cat(
  "Restricted analytical N:",
  nrow(analysis_data_restricted),
  "\n"
)

cat(
  "Excluded:",
  nrow(analysis_data) -
    nrow(analysis_data_restricted),
  "\n"
)

cat(
  "Retained (%):",
  round(
    100 *
      nrow(analysis_data_restricted) /
      nrow(analysis_data),
    2
  ),
  "\n"
)

# ------------------------------------------------
# Country counts after restriction
# ------------------------------------------------

cat("\nRESTRICTED SAMPLE BY COUNTRY\n\n")

restricted_country_counts <-
  analysis_data_restricted %>%
  count(
    country,
    name = "N"
  )

print(
  restricted_country_counts,
  n = Inf
)

# ------------------------------------------------
# Confirm excluded categories are gone
# ------------------------------------------------

cat("\nRESTRICTED RESIDENCE LEVELS AND COUNTS\n\n")

print(
  analysis_data_restricted %>%
    count(
      residence_model,
      .drop = FALSE
    ),
  n = Inf
)

cat("\nRESTRICTED SOCIAL PROXIMITY LEVELS AND COUNTS\n\n")

print(
  analysis_data_restricted %>%
    count(
      social_model,
      .drop = FALSE
    ),
  n = Inf
)

# ------------------------------------------------
# Basic structural checks only
# ------------------------------------------------

stopifnot(
  nrow(analysis_data) == 12460,
  
  !any(
    analysis_data_restricted$
      residence_model == "Missing"
  ),
  
  !any(
    analysis_data_restricted$
      social_model == "Don't know"
  )
)

cat(
  "\nRestricted sample constructed successfully.\n"
)


# ================================================================
# 19B. FIT MISSING-CATEGORY SENSITIVITY MODELS
# ================================================================

# ------------------------------------------------
# Refit primary specification using restricted sample
#
# Excluded:
#   residence_model == "Missing"
#   social_model == "Don't know"
#
# All remaining modelling choices match primary models:
#   macro-region fixed effect
#   individual-level predictors
#   country random intercept
#   country-normalised respondent weights
#   REML estimation
# ------------------------------------------------

fit_restricted_primary <- function(
    outcome,
    data = analysis_data_restricted
) {
  
  formula_restricted <- as.formula(
    paste0(
      outcome,
      " ~ region_fe + age_model + gender_model + ",
      "education_model + residence_model + ownership_model + ",
      "social_model + (1 | country)"
    )
  )
  
  lme4::lmer(
    formula_restricted,
    data = data,
    weights = weight_norm,
    REML = TRUE,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(
        maxfun = 200000
      )
    )
  )
}

# ------------------------------------------------
# Fit three restricted models
# ------------------------------------------------

model_sc_restricted <-
  fit_restricted_primary(
    "socio_cultural"
  )

model_pr_restricted <-
  fit_restricted_primary(
    "provisioning"
  )

model_rs_restricted <-
  fit_restricted_primary(
    "regulating"
  )

restricted_models <- list(
  socio_cultural =
    model_sc_restricted,
  
  provisioning =
    model_pr_restricted,
  
  regulating_supporting =
    model_rs_restricted
)

# ------------------------------------------------
# Model diagnostics
# ------------------------------------------------

restricted_diagnostics <-
  dplyr::bind_rows(
    lapply(
      names(restricted_models),
      function(nm) {
        
        m <-
          restricted_models[[nm]]
        
        conv_message <-
          m@optinfo$conv$lme4$messages
        
        tibble::tibble(
          model = nm,
          
          N =
            stats::nobs(m),
          
          singular =
            lme4::isSingular(
              m,
              tol = 1e-4
            ),
          
          convergence =
            if (
              is.null(conv_message)
            ) {
              "OK"
            } else {
              paste(
                conv_message,
                collapse = "; "
              )
            }
        )
      }
    )
  )

cat(
  "\nRESTRICTED MODEL DIAGNOSTICS\n\n"
)

print(
  restricted_diagnostics,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Variance components
# ------------------------------------------------

restricted_variance <-
  dplyr::bind_rows(
    lapply(
      names(restricted_models),
      function(nm) {
        
        m <-
          restricted_models[[nm]]
        
        vc <-
          as.data.frame(
            lme4::VarCorr(m)
          )
        
        tibble::tibble(
          model = nm,
          
          country_variance =
            vc$vcov[
              vc$grp == "country"
            ],
          
          residual_variance =
            vc$vcov[
              vc$grp == "Residual"
            ]
        )
      }
    )
  )

cat(
  "\nRESTRICTED MODEL VARIANCE COMPONENTS\n\n"
)

print(
  restricted_variance %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 6)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Number of fixed effects
# ------------------------------------------------

restricted_fixed_counts <-
  tibble::tibble(
    model =
      names(restricted_models),
    
    fixed_effects_including_intercept =
      vapply(
        restricted_models,
        function(m) {
          length(
            lme4::fixef(m)
          )
        },
        integer(1)
      ),
    
    fixed_effects_excluding_intercept =
      vapply(
        restricted_models,
        function(m) {
          length(
            lme4::fixef(m)
          ) - 1L
        },
        integer(1)
      )
  )

cat(
  "\nRESTRICTED MODEL FIXED-EFFECT COUNTS\n\n"
)

print(
  restricted_fixed_counts,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Confirm formulas
# ------------------------------------------------

cat(
  "\nRESTRICTED MODEL FORMULAS\n\n"
)

print(
  formula(
    model_sc_restricted
  )
)

print(
  formula(
    model_pr_restricted
  )
)

print(
  formula(
    model_rs_restricted
  )
)

# ------------------------------------------------
# Structural validation
# ------------------------------------------------

stopifnot(
  
  all(
    restricted_diagnostics$N ==
      10382
  ),
  
  all(
    restricted_diagnostics$singular ==
      FALSE
  ),
  
  all(
    restricted_diagnostics$convergence ==
      "OK"
  )
)

cat(
  "\nRestricted-sample sensitivity models fitted successfully.\n"
)


# ================================================================
# 19C. COMPARE FULL-SAMPLE AND RESTRICTED-SAMPLE MODELS
# ================================================================

# ------------------------------------------------
# Extract restricted-model fixed effects and 95% CIs
# ------------------------------------------------

extract_restricted_fixed <- function(
    model,
    domain_name
) {
  
  broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    mutate(
      domain = domain_name
    ) %>%
    select(
      domain,
      term,
      estimate,
      conf.low,
      conf.high
    )
}

restricted_fixed <- bind_rows(
  
  extract_restricted_fixed(
    model_sc_restricted,
    "Socio-cultural"
  ),
  
  extract_restricted_fixed(
    model_pr_restricted,
    "Provisioning"
  ),
  
  extract_restricted_fixed(
    model_rs_restricted,
    "Regulating/supporting"
  )
)

# ------------------------------------------------
# Remove intercepts
# ------------------------------------------------

full_nonintercept_s6 <-
  primary_fixed %>%
  filter(
    term != "(Intercept)"
  ) %>%
  select(
    domain,
    term,
    full_estimate = estimate,
    full_low = conf.low,
    full_high = conf.high
  )

restricted_nonintercept_s6 <-
  restricted_fixed %>%
  filter(
    term != "(Intercept)"
  ) %>%
  select(
    domain,
    term,
    restricted_estimate = estimate,
    restricted_low = conf.low,
    restricted_high = conf.high
  )

# ------------------------------------------------
# Join only coefficients estimable in both models
# ------------------------------------------------

missing_category_comparison <-
  inner_join(
    full_nonintercept_s6,
    restricted_nonintercept_s6,
    by = c(
      "domain",
      "term"
    )
  ) %>%
  mutate(
    
    coefficient_difference =
      restricted_estimate -
      full_estimate,
    
    absolute_difference =
      abs(
        coefficient_difference
      ),
    
    same_direction =
      sign(full_estimate) ==
      sign(restricted_estimate),
    
    full_ci_excludes_zero =
      full_low > 0 |
      full_high < 0,
    
    restricted_ci_excludes_zero =
      restricted_low > 0 |
      restricted_high < 0,
    
    same_ci_conclusion =
      full_ci_excludes_zero ==
      restricted_ci_excludes_zero
  )

# ------------------------------------------------
# Verify expected number of common coefficients
# ------------------------------------------------

cat(
  "\nCOMMON COEFFICIENT COUNTS BY DOMAIN\n\n"
)

print(
  missing_category_comparison %>%
    count(
      domain,
      name = "coefficients_compared"
    ),
  n = Inf
)

cat(
  "\nTOTAL COMMON COEFFICIENTS:",
  nrow(missing_category_comparison),
  "\n"
)

stopifnot(
  nrow(missing_category_comparison) == 66,
  !anyNA(missing_category_comparison)
)

# ------------------------------------------------
# Overall summary
#
# Distinct names avoid dplyr summary-name masking.
# ------------------------------------------------

missing_summary_overall <-
  missing_category_comparison %>%
  summarise(
    
    coefficients_compared =
      n(),
    
    n_same_direction =
      sum(same_direction),
    
    n_direction_changes =
      sum(!same_direction),
    
    n_same_ci_conclusion =
      sum(same_ci_conclusion),
    
    n_ci_conclusion_changes =
      sum(!same_ci_conclusion),
    
    maximum_absolute_difference =
      max(absolute_difference)
  )

cat(
  "\nFULL VS RESTRICTED: OVERALL SUMMARY\n\n"
)

print(
  missing_summary_overall,
  width = Inf
)

# ------------------------------------------------
# Domain-specific summary
# ------------------------------------------------

missing_summary_domain <-
  missing_category_comparison %>%
  group_by(
    domain
  ) %>%
  summarise(
    
    coefficients_compared =
      n(),
    
    n_same_direction =
      sum(same_direction),
    
    n_direction_changes =
      sum(!same_direction),
    
    n_same_ci_conclusion =
      sum(same_ci_conclusion),
    
    n_ci_conclusion_changes =
      sum(!same_ci_conclusion),
    
    maximum_absolute_difference =
      max(absolute_difference),
    
    .groups = "drop"
  )

cat(
  "\nFULL VS RESTRICTED: DOMAIN SUMMARY\n\n"
)

print(
  missing_summary_domain %>%
    mutate(
      maximum_absolute_difference =
        round(
          maximum_absolute_difference,
          6
        )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# All direction or CI-conclusion changes
# ------------------------------------------------

missing_category_disagreements <-
  missing_category_comparison %>%
  filter(
    !same_direction |
      !same_ci_conclusion
  ) %>%
  arrange(
    domain,
    term
  )

cat(
  "\nDIRECTION OR CI-CONCLUSION CHANGES\n\n"
)

print(
  missing_category_disagreements,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Ten largest absolute coefficient differences
# ------------------------------------------------

cat(
  "\n10 LARGEST ABSOLUTE COEFFICIENT DIFFERENCES\n\n"
)

print(
  missing_category_comparison %>%
    arrange(
      desc(absolute_difference)
    ) %>%
    select(
      domain,
      term,
      full_estimate,
      restricted_estimate,
      coefficient_difference,
      absolute_difference,
      same_direction,
      same_ci_conclusion
    ) %>%
    slice_head(
      n = 10
    ) %>%
    mutate(
      across(
        c(
          full_estimate,
          restricted_estimate,
          coefficient_difference,
          absolute_difference
        ),
        ~ round(.x, 6)
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Basic validation only
#
# Do not hard-code the previous substantive results
# until the clean run confirms them.
# ------------------------------------------------

stopifnot(
  missing_summary_overall$
    coefficients_compared == 66
)

cat(
  "\nMissing-category sensitivity comparison completed successfully.\n"
)


# ================================================================
# 19D. FINAL TABLE S6
# MISSING-CATEGORY SENSITIVITY ANALYSIS
# ================================================================

# ------------------------------------------------
# Domain-level summary
# ------------------------------------------------

table_s6 <-
  missing_summary_domain %>%
  transmute(
    `Forest-value domain` =
      domain,
    
    `Coefficients compared` =
      coefficients_compared,
    
    `Same direction, n` =
      n_same_direction,
    
    `Direction changes, n` =
      n_direction_changes,
    
    `Same CI conclusion, n` =
      n_same_ci_conclusion,
    
    `CI-conclusion changes, n` =
      n_ci_conclusion_changes,
    
    `Maximum absolute coefficient difference` =
      round(
        maximum_absolute_difference,
        4
      )
  )

# ------------------------------------------------
# Manuscript domain order
# ------------------------------------------------

table_s6 <-
  table_s6 %>%
  mutate(
    `Forest-value domain` =
      factor(
        `Forest-value domain`,
        levels = c(
          "Socio-cultural",
          "Provisioning",
          "Regulating/supporting"
        )
      )
  ) %>%
  arrange(
    `Forest-value domain`
  ) %>%
  mutate(
    `Forest-value domain` =
      as.character(
        `Forest-value domain`
      )
  )

# ------------------------------------------------
# Overall row
# ------------------------------------------------

table_s6_overall <-
  tibble::tibble(
    `Forest-value domain` =
      "Overall",
    
    `Coefficients compared` =
      missing_summary_overall$
      coefficients_compared,
    
    `Same direction, n` =
      missing_summary_overall$
      n_same_direction,
    
    `Direction changes, n` =
      missing_summary_overall$
      n_direction_changes,
    
    `Same CI conclusion, n` =
      missing_summary_overall$
      n_same_ci_conclusion,
    
    `CI-conclusion changes, n` =
      missing_summary_overall$
      n_ci_conclusion_changes,
    
    `Maximum absolute coefficient difference` =
      round(
        missing_summary_overall$
          maximum_absolute_difference,
        4
      )
  )

table_s6_export <-
  bind_rows(
    table_s6,
    table_s6_overall
  )

cat(
  "\nFINAL TABLE S6\n\n"
)

print(
  table_s6_export,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Validation
# ------------------------------------------------

stopifnot(
  
  nrow(table_s6) == 3,
  
  all(
    table_s6$
      `Coefficients compared` == 22
  ),
  
  sum(
    table_s6$
      `Same direction, n`
  ) == 61,
  
  sum(
    table_s6$
      `Direction changes, n`
  ) == 5,
  
  sum(
    table_s6$
      `Same CI conclusion, n`
  ) == 58,
  
  sum(
    table_s6$
      `CI-conclusion changes, n`
  ) == 8,
  
  table_s6_overall$
    `Coefficients compared` == 66,
  
  table_s6_overall$
    `Same direction, n` == 61,
  
  table_s6_overall$
    `Direction changes, n` == 5,
  
  table_s6_overall$
    `Same CI conclusion, n` == 58,
  
  table_s6_overall$
    `CI-conclusion changes, n` == 8,
  
  abs(
    table_s6_overall$
      `Maximum absolute coefficient difference` -
      0.0907
  ) < 0.0001
)

# ------------------------------------------------
# Export
# ------------------------------------------------

s6_csv <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S6_missing_category_sensitivity.csv"
  )

s6_xlsx <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S6_missing_category_sensitivity.xlsx"
  )

write.csv(
  table_s6_export,
  s6_csv,
  row.names = FALSE,
  na = ""
)

openxlsx::write.xlsx(
  table_s6_export,
  s6_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------
# Read-back verification
# ------------------------------------------------

s6_csv_check <-
  read.csv(
    s6_csv,
    check.names = FALSE
  )

s6_xlsx_check <-
  openxlsx::read.xlsx(
    s6_xlsx,
    check.names = FALSE
  )

stopifnot(
  nrow(s6_csv_check) == 4,
  nrow(s6_xlsx_check) == 4,
  ncol(s6_csv_check) ==
    ncol(table_s6_export),
  ncol(s6_xlsx_check) ==
    ncol(table_s6_export)
)

cat(
  "\nTable S6 exported and read back successfully.\n"
)

cat(
  "\nCSV:",
  s6_csv,
  "\nXLSX:",
  s6_xlsx,
  "\n"
)

# ================================================================
# 20A. TABLE S7:
# UNDERLYING VARIANCE COMPONENTS AND ICCs
# ================================================================

# ------------------------------------------------
# IMPORTANT
#
# Do NOT refit models.
# Do NOT rerun bootMer().
#
# Use the validated three-level models from Step 16
# and the fixed-seed bootstrap results already stored in:
#
# major_revision/final_results/
# variance_partition_results.csv
# ------------------------------------------------


# ------------------------------------------------
# 1. Read the validated compact bootstrap/ICC results
# ------------------------------------------------

variance_results_path <-
  file.path(
    "major_revision",
    "final_results",
    "variance_partition_results.csv"
  )

stopifnot(
  file.exists(
    variance_results_path
  )
)

variance_partition_saved <-
  read.csv(
    variance_results_path,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

cat(
  "\nSAVED VARIANCE-PARTITION RESULTS\n\n"
)

print(
  variance_partition_saved,
  n = Inf,
  width = Inf
)

cat(
  "\nCOLUMN NAMES\n\n"
)

print(
  names(
    variance_partition_saved
  )
)


# ------------------------------------------------
# 2. Extract raw variance components directly
#    from the validated three-level models
# ------------------------------------------------

extract_variance_components_s7 <-
  function(
    model,
    domain_name
  ) {
    
    vc <-
      as.data.frame(
        lme4::VarCorr(model)
      )
    
    macro_variance <-
      vc$vcov[
        vc$grp == "region_fe"
      ]
    
    country_variance <-
      vc$vcov[
        vc$grp == "country:region_fe"
      ]
    
    residual_variance <-
      vc$vcov[
        vc$grp == "Residual"
      ]
    
    total_variance <-
      macro_variance +
      country_variance +
      residual_variance
    
    tibble::tibble(
      domain =
        domain_name,
      
      macro_region_variance =
        macro_variance,
      
      country_variance =
        country_variance,
      
      residual_variance =
        residual_variance,
      
      total_residual_variance =
        total_variance,
      
      macro_region_icc_percent =
        100 *
        macro_variance /
        total_variance,
      
      country_icc_percent =
        100 *
        country_variance /
        total_variance,
      
      combined_contextual_icc_percent =
        100 *
        (
          macro_variance +
            country_variance
        ) /
        total_variance,
      
      respondent_residual_percent =
        100 *
        residual_variance /
        total_variance
    )
  }


# ------------------------------------------------
# 3. Use the validated Step-16 model objects
#
# These should be the same objects used for Table 3.
# ------------------------------------------------

variance_components_s7 <-
  dplyr::bind_rows(
    
    extract_variance_components_s7(
      model_sc_variance,
      "Socio-cultural"
    ),
    
    extract_variance_components_s7(
      model_pr_variance,
      "Provisioning"
    ),
    
    extract_variance_components_s7(
      model_rs_variance,
      "Regulating/supporting"
    )
  )


# ------------------------------------------------
# 4. Display raw components and calculated ICCs
# ------------------------------------------------

cat(
  "\nRAW VARIANCE COMPONENTS AND ICCs FOR S7\n\n"
)

print(
  variance_components_s7 %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(
          .x,
          6
        )
      )
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------
# 5. Check that variance proportions sum to 100%
# ------------------------------------------------

variance_components_s7 <-
  variance_components_s7 %>%
  mutate(
    percentage_sum =
      macro_region_icc_percent +
      country_icc_percent +
      respondent_residual_percent
  )

cat(
  "\nPERCENTAGE-SUM CHECK\n\n"
)

print(
  variance_components_s7 %>%
    select(
      domain,
      percentage_sum
    ) %>%
    mutate(
      percentage_sum =
        round(
          percentage_sum,
          10
        )
    ),
  n = Inf
)


# ------------------------------------------------
# 6. Formal validation against the already locked
#    Table 3 point estimates
# ------------------------------------------------

expected_s7_points <-
  tibble::tribble(
    ~domain,
    ~macro_expected,
    ~country_expected,
    ~combined_expected,
    ~residual_expected,
    
    "Socio-cultural",
    0.00,
    2.19,
    2.19,
    97.81,
    
    "Provisioning",
    7.48,
    13.74,
    21.22,
    78.78,
    
    "Regulating/supporting",
    0.18,
    2.43,
    2.60,
    97.40
  )

s7_point_validation <-
  variance_components_s7 %>%
  left_join(
    expected_s7_points,
    by = "domain"
  ) %>%
  transmute(
    domain,
    
    macro_difference =
      abs(
        macro_region_icc_percent -
          macro_expected
      ),
    
    country_difference =
      abs(
        country_icc_percent -
          country_expected
      ),
    
    combined_difference =
      abs(
        combined_contextual_icc_percent -
          combined_expected
      ),
    
    residual_difference =
      abs(
        respondent_residual_percent -
          residual_expected
      )
  )

cat(
  "\nTABLE 3 POINT-ESTIMATE VALIDATION\n\n"
)

print(
  s7_point_validation,
  n = Inf,
  width = Inf
)

stopifnot(
  
  all(
    abs(
      variance_components_s7$
        percentage_sum -
        100
    ) < 1e-8
  ),
  
  all(
    s7_point_validation$
      macro_difference <
      0.01
  ),
  
  all(
    s7_point_validation$
      country_difference <
      0.01
  ),
  
  all(
    s7_point_validation$
      combined_difference <
      0.01
  ),
  
  all(
    s7_point_validation$
      residual_difference <
      0.01
  )
)

cat(
  "\nUnderlying S7 variance components validated successfully.\n"
)

# ================================================================
# 20A-2. VERIFY SAVED VARIANCE-PARTITION RESULTS
# ================================================================

# Convert the object read by read.csv() to a tibble
variance_partition_saved <-
  tibble::as_tibble(
    variance_partition_saved
  )

cat(
  "\nSAVED VARIANCE-PARTITION RESULTS\n\n"
)

print(
  variance_partition_saved,
  n = Inf,
  width = Inf
)

cat(
  "\nCOLUMN NAMES\n\n"
)

print(
  names(
    variance_partition_saved
  )
)

cat(
  "\nOBJECTS CONTAINING 'variance'\n\n"
)

print(
  ls(
    pattern = "variance"
  )
)

cat(
  "\nOBJECTS CONTAINING 'model_sc'\n\n"
)

print(
  ls(
    pattern = "model_sc"
  )
)

cat(
  "\nOBJECTS CONTAINING 'model_pr'\n\n"
)

print(
  ls(
    pattern = "model_pr"
  )
)

cat(
  "\nOBJECTS CONTAINING 'model_rs'\n\n"
)

print(
  ls(
    pattern = "model_rs"
  )
)


# ================================================================
# 20B. EXTRACT VALIDATED RAW VARIANCE COMPONENTS FOR TABLE S7
# ================================================================

# ------------------------------------------------
# Inspect the validated model-list structure
# ------------------------------------------------

cat(
  "\nVARIANCE-PARTITION MODEL LIST NAMES\n\n"
)

print(
  names(
    variance_partition_models
  )
)

# ------------------------------------------------
# Extract variance components from each stored model
# ------------------------------------------------

extract_s7_variance <- function(
    model,
    domain_name
) {
  
  vc <-
    as.data.frame(
      lme4::VarCorr(model)
    )
  
  tibble::tibble(
    domain =
      domain_name,
    
    macro_region_variance =
      vc$vcov[
        vc$grp == "region_fe"
      ],
    
    country_variance =
      vc$vcov[
        vc$grp == "country:region_fe"
      ],
    
    residual_variance =
      vc$vcov[
        vc$grp == "Residual"
      ]
  ) %>%
    mutate(
      total_variance =
        macro_region_variance +
        country_variance +
        residual_variance,
      
      macroregion_icc_check =
        100 *
        macro_region_variance /
        total_variance,
      
      country_icc_check =
        100 *
        country_variance /
        total_variance,
      
      combined_icc_check =
        100 *
        (
          macro_region_variance +
            country_variance
        ) /
        total_variance,
      
      residual_percent_check =
        100 *
        residual_variance /
        total_variance
    )
}

# ------------------------------------------------
# Extract using the actual list order/names
#
# We first require exactly three models.
# ------------------------------------------------

stopifnot(
  length(
    variance_partition_models
  ) == 3
)

variance_components_s7 <-
  dplyr::bind_rows(
    lapply(
      seq_along(
        variance_partition_models
      ),
      function(i) {
        
        domain_labels <-
          c(
            "Socio-cultural",
            "Provisioning",
            "Regulating/supporting"
          )
        
        extract_s7_variance(
          variance_partition_models[[i]],
          domain_labels[i]
        )
      }
    )
  )

# ------------------------------------------------
# Display extracted components
# ------------------------------------------------

cat(
  "\nRAW VARIANCE COMPONENTS AND RECALCULATED ICCs\n\n"
)

print(
  variance_components_s7 %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(
          .x,
          6
        )
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Compare recalculated ICCs against the saved
# fixed-seed Table 3 point estimates
# ------------------------------------------------

s7_validation <-
  variance_components_s7 %>%
  select(
    domain,
    macroregion_icc_check,
    country_icc_check,
    combined_icc_check,
    residual_percent_check
  ) %>%
  left_join(
    variance_partition_saved %>%
      select(
        domain,
        macroregion_icc,
        country_icc,
        combined_contextual_icc,
        residual_percent
      ),
    by = "domain"
  ) %>%
  mutate(
    macro_difference =
      abs(
        macroregion_icc_check -
          macroregion_icc
      ),
    
    country_difference =
      abs(
        country_icc_check -
          country_icc
      ),
    
    combined_difference =
      abs(
        combined_icc_check -
          combined_contextual_icc
      ),
    
    residual_difference =
      abs(
        residual_percent_check -
          residual_percent
      )
  )

cat(
  "\nVALIDATION AGAINST SAVED TABLE 3 POINT ESTIMATES\n\n"
)

print(
  s7_validation %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(
          .x,
          6
        )
      )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Formal validation
#
# Saved values retain greater precision internally,
# so differences should be essentially zero.
# ------------------------------------------------

stopifnot(
  
  all(
    s7_validation$
      macro_difference <
      0.001
  ),
  
  all(
    s7_validation$
      country_difference <
      0.001
  ),
  
  all(
    s7_validation$
      combined_difference <
      0.001
  ),
  
  all(
    s7_validation$
      residual_difference <
      0.001
  )
)

cat(
  "\nValidated raw variance components recovered successfully.\n"
)

# ================================================================
# 20C. FINAL TABLE S7
# VARIANCE COMPONENTS AND ICCs
# ================================================================

# ------------------------------------------------
# Join validated raw variance components to the
# saved fixed-seed ICC and bootstrap-CI results
# ------------------------------------------------

table_s7_data <-
  variance_components_s7 %>%
  select(
    domain,
    macro_region_variance,
    country_variance,
    residual_variance
  ) %>%
  left_join(
    variance_partition_saved,
    by = "domain"
  )

# ------------------------------------------------
# Format point estimates and bootstrap CIs
# ------------------------------------------------

table_s7 <-
  table_s7_data %>%
  transmute(
    
    `Forest-value domain` =
      domain,
    
    `Macro-region variance` =
      sprintf(
        "%.4f",
        macro_region_variance
      ),
    
    `Country variance` =
      sprintf(
        "%.4f",
        country_variance
      ),
    
    `Residual variance` =
      sprintf(
        "%.4f",
        residual_variance
      ),
    
    `Macro-region ICC, % (95% CI)` =
      sprintf(
        "%.2f (%.2f–%.2f)",
        macroregion_icc,
        macroregion_low,
        macroregion_high
      ),
    
    `Country ICC, % (95% CI)` =
      sprintf(
        "%.2f (%.2f–%.2f)",
        country_icc,
        country_low,
        country_high
      ),
    
    `Combined contextual ICC, % (95% CI)` =
      sprintf(
        "%.2f (%.2f–%.2f)",
        combined_contextual_icc,
        combined_low,
        combined_high
      ),
    
    `Residual proportion, % (95% CI)` =
      sprintf(
        "%.2f (%.2f–%.2f)",
        residual_percent,
        residual_low,
        residual_high
      )
  )

# ------------------------------------------------
# Ensure manuscript domain order
# ------------------------------------------------

table_s7 <-
  table_s7 %>%
  mutate(
    `Forest-value domain` =
      factor(
        `Forest-value domain`,
        levels = c(
          "Socio-cultural",
          "Provisioning",
          "Regulating/supporting"
        )
      )
  ) %>%
  arrange(
    `Forest-value domain`
  ) %>%
  mutate(
    `Forest-value domain` =
      as.character(
        `Forest-value domain`
      )
  )

cat(
  "\nFINAL TABLE S7\n\n"
)

print(
  table_s7,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Formal validation of final displayed values
# ------------------------------------------------

stopifnot(
  
  nrow(table_s7) == 3,
  
  table_s7$
    `Macro-region ICC, % (95% CI)`[1] ==
    "0.00 (0.00–1.79)",
  
  table_s7$
    `Country ICC, % (95% CI)`[1] ==
    "2.19 (0.42–4.30)",
  
  table_s7$
    `Combined contextual ICC, % (95% CI)`[1] ==
    "2.19 (0.73–4.50)",
  
  table_s7$
    `Residual proportion, % (95% CI)`[1] ==
    "97.81 (95.50–99.27)",
  
  table_s7$
    `Macro-region ICC, % (95% CI)`[2] ==
    "7.48 (0.00–25.99)",
  
  table_s7$
    `Country ICC, % (95% CI)`[2] ==
    "13.74 (3.46–25.76)",
  
  table_s7$
    `Combined contextual ICC, % (95% CI)`[2] ==
    "21.22 (8.13–37.17)",
  
  table_s7$
    `Residual proportion, % (95% CI)`[2] ==
    "78.78 (62.83–91.87)",
  
  table_s7$
    `Macro-region ICC, % (95% CI)`[3] ==
    "0.18 (0.00–2.73)",
  
  table_s7$
    `Country ICC, % (95% CI)`[3] ==
    "2.43 (0.53–4.64)",
  
  table_s7$
    `Combined contextual ICC, % (95% CI)`[3] ==
    "2.60 (0.84–5.34)",
  
  table_s7$
    `Residual proportion, % (95% CI)`[3] ==
    "97.40 (94.66–99.16)"
)

# ------------------------------------------------
# Export
# ------------------------------------------------

s7_csv <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S7_variance_components_ICCs.csv"
  )

s7_xlsx <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S7_variance_components_ICCs.xlsx"
  )

write.csv(
  table_s7,
  s7_csv,
  row.names = FALSE,
  na = ""
)

openxlsx::write.xlsx(
  table_s7,
  s7_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------
# Read-back verification
# ------------------------------------------------

s7_csv_check <-
  read.csv(
    s7_csv,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

s7_xlsx_check <-
  openxlsx::read.xlsx(
    s7_xlsx,
    check.names = FALSE
  )

stopifnot(
  nrow(s7_csv_check) == 3,
  nrow(s7_xlsx_check) == 3,
  ncol(s7_csv_check) ==
    ncol(table_s7),
  ncol(s7_xlsx_check) ==
    ncol(table_s7)
)

cat(
  "\nTable S7 exported and read back successfully.\n"
)

cat(
  "\nCSV:",
  s7_csv,
  "\nXLSX:",
  s7_xlsx,
  "\n"
)


# ================================================================
# 21A. PRIMARY-MODEL DIAGNOSTICS FOR TABLE S8
# ================================================================

# ------------------------------------------------
# Use the already validated primary models
# ------------------------------------------------

primary_models_s8 <- list(
  `Socio-cultural` =
    model_sc_primary,
  
  `Provisioning` =
    model_pr_primary,
  
  `Regulating/supporting` =
    model_rs_primary
)

# ------------------------------------------------
# Extract convergence, singularity, and
# standardized residual diagnostics
# ------------------------------------------------

diagnostics_s8 <-
  dplyr::bind_rows(
    lapply(
      names(primary_models_s8),
      function(domain_name) {
        
        m <-
          primary_models_s8[[domain_name]]
        
        conv_message <-
          m@optinfo$conv$lme4$messages
        
        # Pearson residuals standardized by
        # estimated residual standard deviation
        std_resid <-
          residuals(
            m,
            type = "pearson"
          )
        
        n_model <-
          stats::nobs(m)
        
        n_gt3 <-
          sum(
            abs(std_resid) > 3,
            na.rm = TRUE
          )
        
        tibble::tibble(
          domain =
            domain_name,
          
          N =
            n_model,
          
          converged =
            if (
              is.null(conv_message)
            ) {
              "Yes"
            } else {
              "No"
            },
          
          singular =
            lme4::isSingular(
              m,
              tol = 1e-4
            ),
          
          residual_abs_gt3_n =
            n_gt3,
          
          residual_abs_gt3_percent =
            100 *
            n_gt3 /
            n_model,
          
          maximum_absolute_standardized_residual =
            max(
              abs(std_resid),
              na.rm = TRUE
            )
        )
      }
    )
  )

cat(
  "\nPRIMARY-MODEL DIAGNOSTICS FOR TABLE S8\n\n"
)

print(
  diagnostics_s8 %>%
    mutate(
      residual_abs_gt3_percent =
        round(
          residual_abs_gt3_percent,
          2
        ),
      
      maximum_absolute_standardized_residual =
        round(
          maximum_absolute_standardized_residual,
          3
        )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Structural validation
# ------------------------------------------------

stopifnot(
  
  all(
    diagnostics_s8$N ==
      12460
  ),
  
  all(
    diagnostics_s8$converged ==
      "Yes"
  ),
  
  all(
    diagnostics_s8$singular ==
      FALSE
  )
)

cat(
  "\nPrimary-model diagnostic extraction completed successfully.\n"
)


# ================================================================
# 21B CORRECTED
# IDENTIFY EXISTING DIAGNOSTIC / CR2 OBJECTS
# ================================================================

all_objects <-
  ls()

find_objects <- function(pattern) {
  
  all_objects[
    grepl(
      pattern,
      tolower(all_objects)
    )
  ]
}

cat(
  "\nOBJECTS CONTAINING 'diagn'\n\n"
)

print(
  find_objects("diagn")
)

cat(
  "\nOBJECTS CONTAINING 'resid'\n\n"
)

print(
  find_objects("resid")
)

cat(
  "\nOBJECTS CONTAINING 'cr2'\n\n"
)

print(
  find_objects("cr2")
)

cat(
  "\nOBJECTS CONTAINING 'robust'\n\n"
)

print(
  find_objects("robust")
)

cat(
  "\nOBJECTS CONTAINING 's8'\n\n"
)

print(
  find_objects("s8")
)

# ================================================================
# 21C. INSPECT EXISTING PRIMARY DIAGNOSTICS
# ================================================================

cat(
  "\nCLASS OF primary_diagnostics\n\n"
)

print(
  class(
    primary_diagnostics
  )
)

cat(
  "\nSTRUCTURE OF primary_diagnostics\n\n"
)

str(
  primary_diagnostics,
  max.level = 2
)

cat(
  "\nCONTENTS OF primary_diagnostics\n\n"
)

print(
  primary_diagnostics
)

cat(
  "\nNAMES / COLUMN NAMES\n\n"
)

if (
  is.data.frame(primary_diagnostics)
) {
  
  print(
    names(
      primary_diagnostics
    )
  )
  
} else if (
  is.list(primary_diagnostics)
) {
  
  print(
    names(
      primary_diagnostics
    )
  )
}


# ================================================================
# 21D. REPRODUCE FINAL PRIMARY-MODEL RESIDUAL DIAGNOSTICS
# Using the diagnostic definition in the corrected source:
# standardized residual = raw residual / sigma(model)
# ================================================================

primary_models_diagnostics <- list(
  `Socio-cultural` =
    model_sc_primary,
  
  `Provisioning` =
    model_pr_primary,
  
  `Regulating/supporting` =
    model_rs_primary
)

diagnose_primary_model_s8 <- function(
    model,
    domain_name
) {
  
  vc <-
    as.data.frame(
      lme4::VarCorr(model)
    )
  
  conv_message <-
    model@optinfo$conv$lme4$messages
  
  residuals_raw <-
    residuals(model)
  
  standardized_residuals <-
    residuals_raw /
    sigma(model)
  
  n_gt3 <-
    sum(
      abs(standardized_residuals) > 3,
      na.rm = TRUE
    )
  
  tibble::tibble(
    domain =
      domain_name,
    
    N =
      nobs(model),
    
    singular =
      lme4::isSingular(
        model,
        tol = 1e-4
      ),
    
    convergence =
      if (
        is.null(conv_message)
      ) {
        "OK"
      } else {
        paste(
          conv_message,
          collapse = "; "
        )
      },
    
    country_variance =
      vc$vcov[
        vc$grp == "country"
      ],
    
    residual_variance =
      vc$vcov[
        vc$grp == "Residual"
      ],
    
    standardized_residual_gt_3 =
      n_gt3,
    
    standardized_residual_gt_3_pct =
      100 *
      mean(
        abs(standardized_residuals) > 3,
        na.rm = TRUE
      ),
    
    maximum_absolute_standardized_residual =
      max(
        abs(standardized_residuals),
        na.rm = TRUE
      )
  )
}

# ================================================================
# 21D-2. FINAL PRIMARY-MODEL DIAGNOSTICS
# Construct residual diagnostics from the three primary models
# ================================================================

primary_diagnostics_s8 <-
  dplyr::bind_rows(
    lapply(
      names(primary_models_diagnostics),
      function(domain_name) {
        
        diagnose_primary_model_s8(
          primary_models_diagnostics[[domain_name]],
          domain_name
        )
      }
    )
  )

cat(
  "\nFINAL PRIMARY-MODEL DIAGNOSTICS\n\n"
)

print(
  primary_diagnostics_s8 %>%
    mutate(
      across(
        c(
          country_variance,
          residual_variance,
          maximum_absolute_standardized_residual
        ),
        ~ round(.x, 4)
      ),
      
      standardized_residual_gt_3_pct =
        round(
          standardized_residual_gt_3_pct,
          2
        )
    ),
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Validate model status only
# ------------------------------------------------

stopifnot(
  all(
    primary_diagnostics_s8$N == 12460
  ),
  
  all(
    primary_diagnostics_s8$singular == FALSE
  ),
  
  all(
    primary_diagnostics_s8$convergence == "OK"
  )
)

cat(
  "\nFinal residual diagnostics reproduced successfully.\n"
)


# ================================================================
# 21E. COUNTRY-CLUSTERED CR2 ROBUST INFERENCE
# ================================================================

# ------------------------------------------------
# IMPORTANT
#
# CR2 is applied to the already validated
# UNWEIGHTED sensitivity models.
#
# Clustering: country
# Covariance adjustment: CR2
# Small-sample test: Satterthwaite
# ------------------------------------------------

stopifnot(
  requireNamespace(
    "clubSandwich",
    quietly = TRUE
  )
)

# ------------------------------------------------
# Fit CR2 inference
# ------------------------------------------------

robust_test_sc <-
  clubSandwich::coef_test(
    model_sc_unweighted,
    vcov = "CR2",
    cluster = analysis_data$country,
    test = "Satterthwaite"
  )

robust_test_pr <-
  clubSandwich::coef_test(
    model_pr_unweighted,
    vcov = "CR2",
    cluster = analysis_data$country,
    test = "Satterthwaite"
  )

robust_test_rs <-
  clubSandwich::coef_test(
    model_rs_unweighted,
    vcov = "CR2",
    cluster = analysis_data$country,
    test = "Satterthwaite"
  )

# ------------------------------------------------
# Store together
# ------------------------------------------------

cr2_results_s8 <-
  list(
    `Socio-cultural` =
      robust_test_sc,
    
    `Provisioning` =
      robust_test_pr,
    
    `Regulating/supporting` =
      robust_test_rs
  )

# ------------------------------------------------
# Print each result
# ------------------------------------------------

cat(
  "\n================ SOCIO-CULTURAL ================\n\n"
)

print(
  robust_test_sc
)

cat(
  "\n================ PROVISIONING ==================\n\n"
)

print(
  robust_test_pr
)

cat(
  "\n=========== REGULATING/SUPPORTING ==============\n\n"
)

print(
  robust_test_rs
)

# ------------------------------------------------
# Basic structural checks
# ------------------------------------------------

cat(
  "\nCR2 RESULT DIMENSIONS\n\n"
)

print(
  sapply(
    cr2_results_s8,
    dim
  )
)

cat(
  "\nCR2 COLUMN NAMES\n\n"
)

print(
  names(
    as.data.frame(
      robust_test_sc
    )
  )
)

cat(
  "\nCR2 robust inference completed successfully.\n"
)

# ================================================================
# 21F. COMPARE PRIMARY MODEL INFERENCE WITH CR2 INFERENCE
# ================================================================

# ------------------------------------------------
# Helper: extract CR2 results
# ------------------------------------------------

extract_cr2_s8 <- function(
    cr2_object,
    domain_name
) {
  
  x <-
    as.data.frame(
      cr2_object
    )
  
  tibble::tibble(
    domain =
      domain_name,
    
    term =
      rownames(x),
    
    cr2_estimate =
      x$beta,
    
    cr2_se =
      x$SE,
    
    cr2_df =
      x$df_Satt,
    
    cr2_p =
      x$p_Satt,
    
    cr2_excludes_zero =
      x$p_Satt < 0.05
  )
}

# ------------------------------------------------
# Extract all CR2 results
# ------------------------------------------------

cr2_fixed_s8 <-
  dplyr::bind_rows(
    
    extract_cr2_s8(
      robust_test_sc,
      "Socio-cultural"
    ),
    
    extract_cr2_s8(
      robust_test_pr,
      "Provisioning"
    ),
    
    extract_cr2_s8(
      robust_test_rs,
      "Regulating/supporting"
    )
  )

# ------------------------------------------------
# Primary-model inference
#
# primary_fixed was created earlier from the
# validated weighted primary models.
# ------------------------------------------------

primary_fixed_s8 <-
  primary_fixed %>%
  transmute(
    domain,
    term,
    
    primary_estimate =
      estimate,
    
    primary_conf_low =
      conf.low,
    
    primary_conf_high =
      conf.high,
    
    primary_excludes_zero =
      (
        primary_conf_low > 0 |
          primary_conf_high < 0
      )
  )

# ------------------------------------------------
# Join primary and CR2 results
# ------------------------------------------------

cr2_comparison_s8 <-
  primary_fixed_s8 %>%
  inner_join(
    cr2_fixed_s8,
    by = c(
      "domain",
      "term"
    )
  ) %>%
  mutate(
    
    same_direction =
      sign(primary_estimate) ==
      sign(cr2_estimate),
    
    same_inference =
      primary_excludes_zero ==
      cr2_excludes_zero,
    
    coefficient_type =
      case_when(
        
        grepl(
          "^region_fe",
          term
        ) ~ "Macro-region",
        
        term ==
          "(Intercept)" ~
          "Intercept",
        
        TRUE ~
          "Individual-level"
      )
  )

# ------------------------------------------------
# Check matching
#
# 25 coefficients/domain:
# 1 intercept + 4 region + 20 individual
# ------------------------------------------------

cat(
  "\nMATCHED COEFFICIENT COUNTS\n\n"
)

print(
  cr2_comparison_s8 %>%
    count(
      domain,
      coefficient_type
    ),
  n = Inf
)

stopifnot(
  nrow(cr2_comparison_s8) == 75
)

# ------------------------------------------------
# Macro-region agreement
# ------------------------------------------------

cr2_region_summary_s8 <-
  cr2_comparison_s8 %>%
  filter(
    coefficient_type ==
      "Macro-region"
  ) %>%
  summarise(
    coefficients_compared =
      n(),
    
    same_direction =
      sum(
        same_direction
      ),
    
    same_inference =
      sum(
        same_inference
      ),
    
    inference_changes =
      sum(
        !same_inference
      )
  )

cat(
  "\nMACRO-REGIONAL CR2 COMPARISON\n\n"
)

print(
  cr2_region_summary_s8,
  width = Inf
)

# ------------------------------------------------
# Individual-level agreement by domain
# ------------------------------------------------

cr2_individual_summary_s8 <-
  cr2_comparison_s8 %>%
  filter(
    coefficient_type ==
      "Individual-level"
  ) %>%
  group_by(
    domain
  ) %>%
  summarise(
    coefficients_compared =
      n(),
    
    same_direction =
      sum(
        same_direction
      ),
    
    direction_changes =
      sum(
        !same_direction
      ),
    
    same_inference =
      sum(
        same_inference
      ),
    
    inference_changes =
      sum(
        !same_inference
      ),
    
    .groups = "drop"
  )

cat(
  "\nINDIVIDUAL-LEVEL CR2 COMPARISON\n\n"
)

print(
  cr2_individual_summary_s8,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Display every inferential disagreement
# ------------------------------------------------

cr2_inference_disagreements_s8 <-
  cr2_comparison_s8 %>%
  filter(
    coefficient_type !=
      "Intercept",
    !same_inference
  ) %>%
  select(
    domain,
    coefficient_type,
    term,
    primary_estimate,
    primary_conf_low,
    primary_conf_high,
    cr2_estimate,
    cr2_se,
    cr2_df,
    cr2_p,
    primary_excludes_zero,
    cr2_excludes_zero
  )

cat(
  "\nALL PRIMARY-vs-CR2 INFERENTIAL DISAGREEMENTS\n\n"
)

print(
  cr2_inference_disagreements_s8,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Overall individual-level summary
# ------------------------------------------------

cr2_individual_overall_s8 <-
  cr2_comparison_s8 %>%
  filter(
    coefficient_type ==
      "Individual-level"
  ) %>%
  summarise(
    coefficients_compared =
      n(),
    
    same_direction =
      sum(
        same_direction
      ),
    
    direction_changes =
      sum(
        !same_direction
      ),
    
    same_inference =
      sum(
        same_inference
      ),
    
    inference_changes =
      sum(
        !same_inference
      )
  )

cat(
  "\nOVERALL INDIVIDUAL-LEVEL CR2 COMPARISON\n\n"
)

print(
  cr2_individual_overall_s8,
  width = Inf
)

# ================================================================
# 21G. CR2 SUMMARY COUNTS
# Avoid dplyr name-masking
# ================================================================

# ------------------------------------------------
# Macro-region summary
# ------------------------------------------------

cr2_region_summary_final <-
  cr2_comparison_s8 %>%
  filter(
    coefficient_type == "Macro-region"
  ) %>%
  summarise(
    coefficients_compared =
      n(),
    
    n_same_direction =
      sum(same_direction),
    
    n_direction_changes =
      sum(!same_direction),
    
    n_same_inference =
      sum(same_inference),
    
    n_inference_changes =
      sum(!same_inference)
  )

# ------------------------------------------------
# Individual-level summary by domain
# ------------------------------------------------

cr2_individual_summary_final <-
  cr2_comparison_s8 %>%
  filter(
    coefficient_type == "Individual-level"
  ) %>%
  group_by(
    domain
  ) %>%
  summarise(
    coefficients_compared =
      n(),
    
    n_same_direction =
      sum(same_direction),
    
    n_direction_changes =
      sum(!same_direction),
    
    n_same_inference =
      sum(same_inference),
    
    n_inference_changes =
      sum(!same_inference),
    
    .groups = "drop"
  )

# ------------------------------------------------
# Overall individual-level summary
# ------------------------------------------------

cr2_individual_overall_final <-
  cr2_comparison_s8 %>%
  filter(
    coefficient_type == "Individual-level"
  ) %>%
  summarise(
    coefficients_compared =
      n(),
    
    n_same_direction =
      sum(same_direction),
    
    n_direction_changes =
      sum(!same_direction),
    
    n_same_inference =
      sum(same_inference),
    
    n_inference_changes =
      sum(!same_inference)
  )

# ------------------------------------------------
# Print corrected results
# ------------------------------------------------

cat(
  "\nCORRECTED MACRO-REGIONAL SUMMARY\n\n"
)

print(
  cr2_region_summary_final,
  width = Inf
)

cat(
  "\nCORRECTED INDIVIDUAL-LEVEL SUMMARY\n\n"
)

print(
  cr2_individual_summary_final,
  n = Inf,
  width = Inf
)

cat(
  "\nCORRECTED OVERALL INDIVIDUAL-LEVEL SUMMARY\n\n"
)

print(
  cr2_individual_overall_final,
  width = Inf
)

# ------------------------------------------------
# Formal checks
# ------------------------------------------------

stopifnot(
  
  cr2_region_summary_final$
    coefficients_compared == 12,
  
  cr2_region_summary_final$
    n_same_direction == 12,
  
  cr2_region_summary_final$
    n_direction_changes == 0,
  
  cr2_region_summary_final$
    n_same_inference == 12,
  
  cr2_region_summary_final$
    n_inference_changes == 0,
  
  sum(
    cr2_individual_summary_final$
      coefficients_compared
  ) == 60,
  
  sum(
    cr2_individual_summary_final$
      n_same_direction
  ) == 58,
  
  sum(
    cr2_individual_summary_final$
      n_direction_changes
  ) == 2,
  
  sum(
    cr2_individual_summary_final$
      n_same_inference
  ) == 50,
  
  sum(
    cr2_individual_summary_final$
      n_inference_changes
  ) == 10
)

cat(
  "\nCorrected CR2 summaries validated successfully.\n"
)


# ================================================================
# 21H. FINAL TABLE S8
# MODEL DIAGNOSTICS AND CR2 ROBUST-INFERENCE CHECKS
# ================================================================

# ------------------------------------------------
# 1. Prepare residual diagnostics
# ------------------------------------------------

s8_diagnostics <-
  primary_diagnostics_s8 %>%
  transmute(
    domain,
    
    N,
    
    model_status =
      ifelse(
        convergence == "OK" &
          singular == FALSE,
        "Converged; nonsingular",
        "Check model"
      ),
    
    residual_gt3 =
      sprintf(
        "%d (%.2f%%)",
        standardized_residual_gt_3,
        standardized_residual_gt_3_pct
      ),
    
    max_abs_residual =
      sprintf(
        "%.2f",
        maximum_absolute_standardized_residual
      )
  )

# ------------------------------------------------
# 2. Prepare individual-level CR2 comparison
# ------------------------------------------------

s8_individual <-
  cr2_individual_summary_final %>%
  transmute(
    domain,
    
    individual_compared =
      coefficients_compared,
    
    individual_same_direction =
      n_same_direction,
    
    individual_direction_changes =
      n_direction_changes,
    
    individual_same_inference =
      n_same_inference,
    
    individual_inference_changes =
      n_inference_changes
  )

# ------------------------------------------------
# 3. Macro-regional CR2 results are identical
#    across the three domains:
#    4 contrasts/domain, all same inference.
# ------------------------------------------------

s8_region <-
  tibble::tibble(
    domain =
      c(
        "Socio-cultural",
        "Provisioning",
        "Regulating/supporting"
      ),
    
    regional_compared =
      4L,
    
    regional_same_inference =
      4L,
    
    regional_inference_changes =
      0L
  )

# ------------------------------------------------
# 4. Combine
# ------------------------------------------------

table_s8 <-
  s8_diagnostics %>%
  left_join(
    s8_region,
    by = "domain"
  ) %>%
  left_join(
    s8_individual,
    by = "domain"
  ) %>%
  transmute(
    
    `Forest-value domain` =
      domain,
    
    `N` =
      N,
    
    `Model status` =
      model_status,
    
    `|Standardized residual| > 3, n (%)` =
      residual_gt3,
    
    `Maximum |standardized residual|` =
      max_abs_residual,
    
    `Regional contrasts compared` =
      regional_compared,
    
    `Regional contrasts with same inference, n` =
      regional_same_inference,
    
    `Individual coefficients compared` =
      individual_compared,
    
    `Individual coefficients with same direction, n` =
      individual_same_direction,
    
    `Individual direction changes, n` =
      individual_direction_changes,
    
    `Individual coefficients with same inference, n` =
      individual_same_inference,
    
    `Individual inference changes, n` =
      individual_inference_changes
  )

# ------------------------------------------------
# 5. Manuscript domain order
# ------------------------------------------------

table_s8 <-
  table_s8 %>%
  mutate(
    `Forest-value domain` =
      factor(
        `Forest-value domain`,
        levels = c(
          "Socio-cultural",
          "Provisioning",
          "Regulating/supporting"
        )
      )
  ) %>%
  arrange(
    `Forest-value domain`
  ) %>%
  mutate(
    `Forest-value domain` =
      as.character(
        `Forest-value domain`
      )
  )

cat(
  "\nFINAL TABLE S8\n\n"
)

print(
  table_s8,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# 6. Formal validation
# ------------------------------------------------

stopifnot(
  
  nrow(table_s8) == 3,
  
  all(
    table_s8$N == 12460
  ),
  
  all(
    table_s8$`Model status` ==
      "Converged; nonsingular"
  ),
  
  identical(
    table_s8$
      `|Standardized residual| > 3, n (%)`,
    c(
      "168 (1.35%)",
      "1 (0.01%)",
      "228 (1.83%)"
    )
  ),
  
  all(
    table_s8$
      `Regional contrasts compared` == 4
  ),
  
  all(
    table_s8$
      `Regional contrasts with same inference, n` == 4
  ),
  
  identical(
    table_s8$
      `Individual coefficients with same direction, n`,
    c(
      20L,
      18L,
      20L
    )
  ),
  
  identical(
    table_s8$
      `Individual coefficients with same inference, n`,
    c(
      16L,
      16L,
      18L
    )
  ),
  
  identical(
    table_s8$
      `Individual inference changes, n`,
    c(
      4L,
      4L,
      2L
    )
  )
)

# ------------------------------------------------
# 7. Export
# ------------------------------------------------

s8_csv <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S8_diagnostics_CR2.csv"
  )

s8_xlsx <-
  file.path(
    "major_revision",
    "final_results",
    "supplementary",
    "Table_S8_diagnostics_CR2.xlsx"
  )

write.csv(
  table_s8,
  s8_csv,
  row.names = FALSE,
  na = ""
)

openxlsx::write.xlsx(
  table_s8,
  s8_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------
# 8. Read-back verification
# ------------------------------------------------

s8_csv_check <-
  read.csv(
    s8_csv,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

s8_xlsx_check <-
  openxlsx::read.xlsx(
    s8_xlsx,
    check.names = FALSE
  )

stopifnot(
  nrow(s8_csv_check) == 3,
  nrow(s8_xlsx_check) == 3,
  ncol(s8_csv_check) ==
    ncol(table_s8),
  ncol(s8_xlsx_check) ==
    ncol(table_s8)
)

cat(
  "\nTable S8 exported and read back successfully.\n"
)

cat(
  "\nCSV:",
  s8_csv,
  "\nXLSX:",
  s8_xlsx,
  "\n"
)


# ================================================================
# 22A. VERIFY LMERTEST COEFFICIENT STRUCTURE
# ================================================================

coef_lmerTest_22 <-
  as.data.frame(
    coef(
      summary(
        model_pr_primary
      )
    )
  )

cat(
  "\nCLASS\n\n"
)

print(
  class(
    coef_lmerTest_22
  )
)

cat(
  "\nCOLUMN NAMES\n\n"
)

print(
  names(
    coef_lmerTest_22
  )
)

cat(
  "\nFIRST 10 ROWS\n\n"
)

print(
  head(
    coef_lmerTest_22,
    10
  )
)

cat(
  "\nROW NAMES\n\n"
)

print(
  head(
    rownames(
      coef_lmerTest_22
    ),
    10
  )
)


# ================================================================
# 22B. VERIFY COMPLETE PRIMARY RESULTS BEFORE TABLE 2
# ================================================================

primary_results_22 <-
  primary_fixed %>%
  select(
    domain,
    term,
    estimate,
    std.error,
    conf.low,
    conf.high
  ) %>%
  arrange(
    factor(
      domain,
      levels = c(
        "Socio-cultural",
        "Provisioning",
        "Regulating/supporting"
      )
    )
  )

cat(
  "\nCOMPLETE PRIMARY FIXED-EFFECT RESULTS\n\n"
)

print(
  primary_results_22,
  n = Inf,
  width = Inf
)

cat(
  "\nROWS PER DOMAIN\n\n"
)

print(
  primary_results_22 %>%
    count(domain)
)

stopifnot(
  nrow(primary_results_22) == 75,
  all(
    primary_results_22 %>%
      count(domain) %>%
      pull(n) == 25
  ),
  !anyNA(
    primary_results_22[
      c(
        "estimate",
        "std.error",
        "conf.low",
        "conf.high"
      )
    ]
  )
)

cat(
  "\nPrimary results validated: 75 coefficients, 25 per domain.\n"
)

# ================================================================
# 22C. CONSTRUCT PUBLICATION-READY TABLE 2
# Inspect before export
# ================================================================

# ------------------------------------------------
# 1. Labels and ordering
# ------------------------------------------------

table2_labels <- c(
  
  # Macro-region
  "region_feCentral-West Europe" =
    "Central-West Europe",
  "region_feCentral-East Europe" =
    "Central-East Europe",
  "region_feSouth-West Europe" =
    "South-West Europe",
  "region_feSouth-East Europe" =
    "South-East Europe",
  
  # Age
  "age_model27-37" =
    "27-37 years",
  "age_model38-48" =
    "38-48 years",
  "age_model49-59" =
    "49-59 years",
  "age_model60+" =
    "60+ years",
  
  # Gender
  "gender_modelFemale" =
    "Female",
  
  # Education
  "education_modelLevel 2" =
    "Primary",
  "education_modelLevel 3" =
    "Lower secondary",
  "education_modelLevel 4" =
    "Upper secondary",
  "education_modelLevel 5" =
    "Post-secondary non-tertiary",
  "education_modelLevel 6" =
    "Tertiary",
  
  # Residence
  "residence_model2" =
    "6-10 years",
  "residence_model3" =
    "11-15 years",
  "residence_model4" =
    "16-20 years",
  "residence_model5" =
    "21-25 years",
  "residence_model6" =
    "26-30 years",
  "residence_model7" =
    ">30 years",
  "residence_modelMissing" =
    "Missing",
  
  # Ownership
  "ownership_modelYes" =
    "Yes",
  
  # Social proximity
  "social_modelYes" =
    "Yes",
  "social_modelDon't know" =
    "Don't know"
)


table2_order <- c(
  
  # Macro-region
  "region_feCentral-West Europe",
  "region_feCentral-East Europe",
  "region_feSouth-West Europe",
  "region_feSouth-East Europe",
  
  # Age
  "age_model27-37",
  "age_model38-48",
  "age_model49-59",
  "age_model60+",
  
  # Gender
  "gender_modelFemale",
  
  # Education
  "education_modelLevel 2",
  "education_modelLevel 3",
  "education_modelLevel 4",
  "education_modelLevel 5",
  "education_modelLevel 6",
  
  # Residence
  "residence_model2",
  "residence_model3",
  "residence_model4",
  "residence_model5",
  "residence_model6",
  "residence_model7",
  "residence_modelMissing",
  
  # Ownership
  "ownership_modelYes",
  
  # Social proximity
  "social_modelYes",
  "social_modelDon't know"
)


# ------------------------------------------------
# 2. Helper for formatting beta and 95% CI
# ------------------------------------------------

format_beta_ci <- function(beta, low, high) {
  
  sprintf(
    "%.3f (%.3f to %.3f)",
    beta,
    low,
    high
  )
}


# ------------------------------------------------
# 3. Format model coefficients
# ------------------------------------------------

table2_model_rows <-
  primary_results_22 %>%
  filter(
    term != "(Intercept)"
  ) %>%
  mutate(
    
    predictor =
      unname(
        table2_labels[term]
      ),
    
    result =
      format_beta_ci(
        estimate,
        conf.low,
        conf.high
      ),
    
    order =
      match(
        term,
        table2_order
      )
  ) %>%
  select(
    domain,
    term,
    predictor,
    order,
    result
  )


# ------------------------------------------------
# 4. Verify every term received a label/order
# ------------------------------------------------

stopifnot(
  nrow(table2_model_rows) == 72,
  !anyNA(table2_model_rows$predictor),
  !anyNA(table2_model_rows$order)
)


# ------------------------------------------------
# 5. Convert to three-domain wide format
# ------------------------------------------------

table2_wide <-
  table2_model_rows %>%
  select(
    predictor,
    order,
    domain,
    result
  ) %>%
  tidyr::pivot_wider(
    names_from = domain,
    values_from = result
  ) %>%
  arrange(
    order
  ) %>%
  select(
    predictor,
    `Socio-cultural`,
    Provisioning,
    `Regulating/supporting`
  )


# ------------------------------------------------
# 6. Add predictor-group headings and references
# ------------------------------------------------

table2_display <-
  tibble::tribble(
    
    ~Predictor,
    ~`Socio-cultural`,
    ~Provisioning,
    ~`Regulating/supporting`,
    
    "Macro-region", "", "", "",
    "Northern Europe (reference)", "Reference", "Reference", "Reference",
    
    "Central-West Europe",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Central-West Europe"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Central-West Europe"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Central-West Europe"
    ],
    
    "Central-East Europe",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Central-East Europe"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Central-East Europe"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Central-East Europe"
    ],
    
    "South-West Europe",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "South-West Europe"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "South-West Europe"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "South-West Europe"
    ],
    
    "South-East Europe",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "South-East Europe"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "South-East Europe"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "South-East Europe"
    ],
    
    "Age", "", "", "",
    "18-26 years (reference)", "Reference", "Reference", "Reference",
    
    "27-37 years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "27-37 years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "27-37 years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "27-37 years"
    ],
    
    "38-48 years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "38-48 years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "38-48 years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "38-48 years"
    ],
    
    "49-59 years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "49-59 years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "49-59 years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "49-59 years"
    ],
    
    "60+ years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "60+ years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "60+ years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "60+ years"
    ],
    
    "Gender", "", "", "",
    "Male (reference)", "Reference", "Reference", "Reference",
    
    "Female",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Female"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Female"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Female"
    ],
    
    "Education", "", "", "",
    "No formal education (reference)", "Reference", "Reference", "Reference",
    
    "Primary",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Primary"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Primary"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Primary"
    ],
    
    "Lower secondary",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Lower secondary"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Lower secondary"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Lower secondary"
    ],
    
    "Upper secondary",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Upper secondary"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Upper secondary"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Upper secondary"
    ],
    
    "Post-secondary non-tertiary",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Post-secondary non-tertiary"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Post-secondary non-tertiary"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Post-secondary non-tertiary"
    ],
    
    "Tertiary",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Tertiary"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Tertiary"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Tertiary"
    ],
    
    "Residence length", "", "", "",
    "0-5 years (reference)", "Reference", "Reference", "Reference",
    
    "6-10 years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "6-10 years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "6-10 years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "6-10 years"
    ],
    
    "11-15 years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "11-15 years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "11-15 years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "11-15 years"
    ],
    
    "16-20 years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "16-20 years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "16-20 years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "16-20 years"
    ],
    
    "21-25 years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "21-25 years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "21-25 years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "21-25 years"
    ],
    
    "26-30 years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "26-30 years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "26-30 years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "26-30 years"
    ],
    
    ">30 years",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == ">30 years"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == ">30 years"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == ">30 years"
    ],
    
    "Missing",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Missing"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Missing"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Missing"
    ],
    
    "Forest ownership", "", "", "",
    "No (reference)", "Reference", "Reference", "Reference",
    
    "Yes",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Yes"
    ][1],
    table2_wide$Provisioning[
      table2_wide$predictor == "Yes"
    ][1],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Yes"
    ][1],
    
    "Social proximity to forest sector", "", "", "",
    "No (reference)", "Reference", "Reference", "Reference",
    
    "Yes",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Yes"
    ][2],
    table2_wide$Provisioning[
      table2_wide$predictor == "Yes"
    ][2],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Yes"
    ][2],
    
    "Don't know",
    table2_wide$`Socio-cultural`[
      table2_wide$predictor == "Don't know"
    ],
    table2_wide$Provisioning[
      table2_wide$predictor == "Don't know"
    ],
    table2_wide$`Regulating/supporting`[
      table2_wide$predictor == "Don't know"
    ]
  )


# ------------------------------------------------
# 7. Print for inspection
# ------------------------------------------------

cat(
  "\nPUBLICATION-READY TABLE 2\n\n"
)

print(
  table2_display,
  n = Inf,
  width = Inf
)

cat(
  "\nTable 2 constructed successfully. Inspect before export.\n"
)

# ================================================================
# 22D. FINALIZE AND EXPORT TABLE 2
# Robust term-based construction
# ================================================================

# ------------------------------------------------
# 1. Create direct lookup from MODEL TERM
# ------------------------------------------------

table2_lookup <-
  table2_model_rows %>%
  select(
    domain,
    term,
    result
  ) %>%
  tidyr::pivot_wider(
    names_from = domain,
    values_from = result
  )

get_t2 <- function(term_name, domain_name) {
  
  value <-
    table2_lookup[
      table2_lookup$term == term_name,
      domain_name,
      drop = TRUE
    ]
  
  stopifnot(
    length(value) == 1,
    !is.na(value)
  )
  
  value
}


# ------------------------------------------------
# 2. Helper to create coefficient row
# ------------------------------------------------

t2_coef_row <- function(label, term_name) {
  
  tibble::tibble(
    
    Predictor = label,
    
    `Socio-cultural β (95% CI)` =
      get_t2(
        term_name,
        "Socio-cultural"
      ),
    
    `Provisioning β (95% CI)` =
      get_t2(
        term_name,
        "Provisioning"
      ),
    
    `Regulating/supporting β (95% CI)` =
      get_t2(
        term_name,
        "Regulating/supporting"
      )
  )
}


# ------------------------------------------------
# 3. Helper rows
# ------------------------------------------------

t2_heading <- function(label) {
  
  tibble::tibble(
    Predictor = label,
    `Socio-cultural β (95% CI)` = "",
    `Provisioning β (95% CI)` = "",
    `Regulating/supporting β (95% CI)` = ""
  )
}


t2_reference <- function(label) {
  
  tibble::tibble(
    Predictor = label,
    `Socio-cultural β (95% CI)` = "Reference",
    `Provisioning β (95% CI)` = "Reference",
    `Regulating/supporting β (95% CI)` = "Reference"
  )
}


# ------------------------------------------------
# 4. Build final Table 2 explicitly from model terms
# ------------------------------------------------

table2_final <-
  dplyr::bind_rows(
    
    t2_heading(
      "Macro-region"
    ),
    
    t2_reference(
      "Northern Europe (reference)"
    ),
    
    t2_coef_row(
      "Central-West Europe",
      "region_feCentral-West Europe"
    ),
    
    t2_coef_row(
      "Central-East Europe",
      "region_feCentral-East Europe"
    ),
    
    t2_coef_row(
      "South-West Europe",
      "region_feSouth-West Europe"
    ),
    
    t2_coef_row(
      "South-East Europe",
      "region_feSouth-East Europe"
    ),
    
    
    t2_heading(
      "Age"
    ),
    
    t2_reference(
      "18-26 years (reference)"
    ),
    
    t2_coef_row(
      "27-37 years",
      "age_model27-37"
    ),
    
    t2_coef_row(
      "38-48 years",
      "age_model38-48"
    ),
    
    t2_coef_row(
      "49-59 years",
      "age_model49-59"
    ),
    
    t2_coef_row(
      "60+ years",
      "age_model60+"
    ),
    
    
    t2_heading(
      "Gender"
    ),
    
    t2_reference(
      "Male (reference)"
    ),
    
    t2_coef_row(
      "Female",
      "gender_modelFemale"
    ),
    
    
    t2_heading(
      "Education"
    ),
    
    t2_reference(
      "No formal education (reference)"
    ),
    
    t2_coef_row(
      "Primary",
      "education_modelLevel 2"
    ),
    
    t2_coef_row(
      "Lower secondary",
      "education_modelLevel 3"
    ),
    
    t2_coef_row(
      "Upper secondary",
      "education_modelLevel 4"
    ),
    
    t2_coef_row(
      "Post-secondary non-tertiary",
      "education_modelLevel 5"
    ),
    
    t2_coef_row(
      "Tertiary",
      "education_modelLevel 6"
    ),
    
    
    t2_heading(
      "Residence length"
    ),
    
    t2_reference(
      "0-5 years (reference)"
    ),
    
    t2_coef_row(
      "6-10 years",
      "residence_model2"
    ),
    
    t2_coef_row(
      "11-15 years",
      "residence_model3"
    ),
    
    t2_coef_row(
      "16-20 years",
      "residence_model4"
    ),
    
    t2_coef_row(
      "21-25 years",
      "residence_model5"
    ),
    
    t2_coef_row(
      "26-30 years",
      "residence_model6"
    ),
    
    t2_coef_row(
      ">30 years",
      "residence_model7"
    ),
    
    t2_coef_row(
      "Missing",
      "residence_modelMissing"
    ),
    
    
    t2_heading(
      "Forest ownership"
    ),
    
    t2_reference(
      "No (reference)"
    ),
    
    t2_coef_row(
      "Yes",
      "ownership_modelYes"
    ),
    
    
    t2_heading(
      "Social proximity to forest sector"
    ),
    
    t2_reference(
      "No (reference)"
    ),
    
    t2_coef_row(
      "Yes",
      "social_modelYes"
    ),
    
    t2_coef_row(
      "Don't know",
      "social_modelDon't know"
    )
  )


# ------------------------------------------------
# 5. Formal checks
# ------------------------------------------------

stopifnot(
  
  nrow(table2_final) == 38,
  
  # Ownership row
  table2_final$
    `Socio-cultural β (95% CI)`[
      table2_final$Predictor == "Yes"
    ][1] ==
    "-0.114 (-0.168 to -0.061)",
  
  table2_final$
    `Provisioning β (95% CI)`[
      table2_final$Predictor == "Yes"
    ][1] ==
    "0.161 (0.086 to 0.236)",
  
  # Social-proximity row
  table2_final$
    `Socio-cultural β (95% CI)`[
      table2_final$Predictor == "Yes"
    ][2] ==
    "0.123 (0.083 to 0.162)",
  
  table2_final$
    `Provisioning β (95% CI)`[
      table2_final$Predictor == "Yes"
    ][2] ==
    "0.342 (0.286 to 0.398)"
)


# ------------------------------------------------
# 6. Print final table
# ------------------------------------------------

cat(
  "\nFINAL TABLE 2\n\n"
)

print(
  table2_final,
  n = Inf,
  width = Inf
)


# ------------------------------------------------
# 7. Export
# ------------------------------------------------

table2_csv <-
  file.path(
    "major_revision",
    "final_results",
    "tables",
    "Table_2_primary_multilevel_models.csv"
  )

table2_xlsx <-
  file.path(
    "major_revision",
    "final_results",
    "tables",
    "Table_2_primary_multilevel_models.xlsx"
  )

write.csv(
  table2_final,
  table2_csv,
  row.names = FALSE,
  na = ""
)

openxlsx::write.xlsx(
  table2_final,
  table2_xlsx,
  overwrite = TRUE
)


# ------------------------------------------------
# 8. Read-back verification
# ------------------------------------------------

table2_csv_check <-
  read.csv(
    table2_csv,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

table2_xlsx_check <-
  openxlsx::read.xlsx(
    table2_xlsx,
    check.names = FALSE
  )

stopifnot(
  nrow(table2_csv_check) == 38,
  nrow(table2_xlsx_check) == 38,
  ncol(table2_csv_check) == 4,
  ncol(table2_xlsx_check) == 4
)

cat(
  "\nTable 2 exported and read back successfully.\n"
)

cat(
  "\nCSV:",
  table2_csv,
  "\nXLSX:",
  table2_xlsx,
  "\n"
)

# ================================================================
# 22E. VERIFY RELIABILITY OF FOREST-VALUE INDICES
# ================================================================

library(psych)
library(dplyr)
library(tidyr)

domains_map <- list(
  socio_cultural = c("q6", "q7a", "q8", "q9"),
  provisioning   = c("q10a", "q11a"),
  regulating     = c("q12", "q13a", "q14")
)

reliability_one_domain <- function(df, items, domain_name) {
  
  dat <- df %>%
    select(all_of(items)) %>%
    drop_na()
  
  k <- ncol(dat)
  
  # Cronbach's alpha
  alpha_val <- tryCatch(
    unname(
      psych::alpha(
        dat,
        warnings = FALSE,
        check.keys = FALSE
      )$total$raw_alpha
    ),
    error = function(e) NA_real_
  )
  
  # Spearman-Brown for two-item scale
  spearman_brown <- NA_real_
  
  if (k == 2) {
    r <- cor(
      dat[[1]],
      dat[[2]],
      use = "complete.obs"
    )
    
    spearman_brown <- (2 * r) / (1 + r)
  }
  
  # McDonald's omega for scales with >= 3 items
  omega_total <- NA_real_
  
  if (k >= 3) {
    omega_total <- tryCatch(
      {
        om <- psych::omega(
          dat,
          nfactors = 1,
          plot = FALSE
        )
        
        unname(om$omega.tot)
      },
      error = function(e) NA_real_
    )
  }
  
  tibble(
    domain = domain_name,
    n_items = k,
    n_complete = nrow(dat),
    cronbach_alpha = alpha_val,
    mcdonald_omega = omega_total,
    spearman_brown = spearman_brown
  )
}

reliability_results <- bind_rows(
  
  reliability_one_domain(
    analysis_data,
    domains_map$socio_cultural,
    "Socio-cultural"
  ),
  
  reliability_one_domain(
    analysis_data,
    domains_map$provisioning,
    "Provisioning"
  ),
  
  reliability_one_domain(
    analysis_data,
    domains_map$regulating,
    "Regulating/supporting"
  )
)

cat(
  "\n============================================================\n",
  "RELIABILITY OF FOREST-VALUE INDICES\n",
  "============================================================\n\n",
  sep = ""
)

print(
  reliability_results,
  n = Inf,
  width = Inf
)

# ================================================================
# 22F. TABLE S2b: INTERNAL CONSISTENCY OF FOREST-VALUE INDICES
# ================================================================

table_s2b <- reliability_results %>%
  transmute(
    `Forest-value domain` = domain,
    `Number of items` = n_items,
    `N` = n_complete,
    `Cronbach's alpha` = round(cronbach_alpha, 2),
    `McDonald's omega` = round(mcdonald_omega, 2),
    `Spearman-Brown coefficient` = round(spearman_brown, 2)
  )

cat(
  "\nTABLE S2b. INTERNAL CONSISTENCY OF FOREST-VALUE INDICES\n\n"
)

print(
  table_s2b,
  n = Inf,
  width = Inf
)

# ------------------------------------------------
# Export
# ------------------------------------------------

s2b_csv <- file.path(
  "major_revision",
  "final_results",
  "supplementary",
  "Table_S2b_internal_consistency_forest_value_indices.csv"
)

s2b_xlsx <- file.path(
  "major_revision",
  "final_results",
  "supplementary",
  "Table_S2b_internal_consistency_forest_value_indices.xlsx"
)

write.csv(
  table_s2b,
  s2b_csv,
  row.names = FALSE,
  na = ""
)

openxlsx::write.xlsx(
  table_s2b,
  s2b_xlsx,
  overwrite = TRUE,
  na.string = ""
)

# ------------------------------------------------
# Read-back verification
# ------------------------------------------------

s2b_csv_check <- read.csv(
  s2b_csv,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

s2b_xlsx_check <- openxlsx::read.xlsx(
  s2b_xlsx,
  check.names = FALSE
)

stopifnot(
  nrow(table_s2b) == 3,
  nrow(s2b_csv_check) == 3,
  nrow(s2b_xlsx_check) == 3,
  ncol(table_s2b) == 6,
  all(table_s2b$N == 12460),
  table_s2b$`Cronbach's alpha`[
    table_s2b$`Forest-value domain` == "Socio-cultural"
  ] == 0.79,
  table_s2b$`Cronbach's alpha`[
    table_s2b$`Forest-value domain` == "Provisioning"
  ] == 0.74,
  table_s2b$`Cronbach's alpha`[
    table_s2b$`Forest-value domain` == "Regulating/supporting"
  ] == 0.82,
  table_s2b$`Spearman-Brown coefficient`[
    table_s2b$`Forest-value domain` == "Provisioning"
  ] == 0.74
)

cat(
  "\nTable S2b exported and read back successfully.\n"
)

cat(
  "\nCSV: ", s2b_csv,
  "\nXLSX: ", s2b_xlsx,
  "\n",
  sep = ""
)


# ================================================================
# 22G-A. INSPECT TABLE S2a COUNTRY ORDER AND N
# ================================================================

table_s2a %>%
  select(
    `Macro-region`,
    Country,
    N
  ) %>%
  print(n = Inf)

# ================================================================
# 22G-B. STANDARDISE COUNTRY LABELS AND BUILD TABLE S2a
# ================================================================

table_s2a_long <- country_value_stats %>%
  mutate(
    # IMPORTANT: convert factor to character first
    country = as.character(country),
    country = ifelse(country == "Czech", "Czechia", country),
    
    statistic = sprintf(
      "%.3f (%.3f) [%.3f, %.3f]",
      weighted_mean,
      weighted_sd,
      ci_lower,
      ci_upper
    ),
    
    domain = factor(
      domain,
      levels = c(
        "Socio-cultural",
        "Provisioning",
        "Regulating/supporting"
      )
    ),
    
    region_fe = factor(
      region_fe,
      levels = c(
        "North Europe",
        "Central-West Europe",
        "Central-East Europe",
        "South-West Europe",
        "South-East Europe"
      )
    )
  )

table_s2a <- table_s2a_long %>%
  select(
    region_fe,
    country,
    n,
    domain,
    statistic
  ) %>%
  distinct() %>%
  pivot_wider(
    names_from = domain,
    values_from = statistic
  ) %>%
  arrange(region_fe, country) %>%
  rename(
    `Macro-region` = region_fe,
    Country = country,
    N = n,
    `Socio-cultural values` = `Socio-cultural`,
    `Provisioning values` = `Provisioning`,
    `Regulating/supporting values` = `Regulating/supporting`
  )

cat("\nCORRECTED TABLE S2a\n\n")

print(
  as.data.frame(table_s2a),
  row.names = FALSE
)


# ================================================================
# 22G-C. SAVE AND VERIFY FINAL TABLE S2a
# ================================================================

supp_dir <- file.path(
  "major_revision",
  "final_results",
  "supplementary"
)

dir.create(
  supp_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------
# File paths
# ------------------------------------------------

s2a_csv <- file.path(
  supp_dir,
  "Table_S2a_country_level_forest_value_statistics.csv"
)

s2a_xlsx <- file.path(
  supp_dir,
  "Table_S2a_country_level_forest_value_statistics.xlsx"
)

# ------------------------------------------------
# Save CSV
# ------------------------------------------------

write.csv(
  table_s2a,
  s2a_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------
# Create publication-ready Excel file
# ------------------------------------------------

wb_s2a <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb_s2a,
  "Table S2a",
  gridLines = FALSE
)

title_s2a <- paste0(
  "Table S2a. Country-level descriptive statistics for ",
  "socio-cultural, provisioning, and regulating/supporting ",
  "forest-value domains"
)

openxlsx::writeData(
  wb_s2a,
  "Table S2a",
  title_s2a,
  startRow = 1
)

openxlsx::writeData(
  wb_s2a,
  "Table S2a",
  table_s2a,
  startRow = 3
)

note_s2a <- paste0(
  "Note. Values are country-normalised weighted means, with weighted ",
  "standard deviations in parentheses and 95% confidence intervals ",
  "in square brackets. N denotes the number of respondents included ",
  "in the analytical sample for each country. Countries are grouped ",
  "according to the five FOREST EUROPE macro-regions."
)

openxlsx::writeData(
  wb_s2a,
  "Table S2a",
  note_s2a,
  startRow = nrow(table_s2a) + 5
)

# Header formatting
header_style_s2a <- openxlsx::createStyle(
  textDecoration = "bold",
  wrapText = TRUE,
  valign = "center"
)

openxlsx::addStyle(
  wb_s2a,
  "Table S2a",
  header_style_s2a,
  rows = 3,
  cols = 1:ncol(table_s2a),
  gridExpand = TRUE
)

openxlsx::setColWidths(
  wb_s2a,
  "Table S2a",
  cols = 1:ncol(table_s2a),
  widths = "auto"
)

openxlsx::saveWorkbook(
  wb_s2a,
  s2a_xlsx,
  overwrite = TRUE
)

# ------------------------------------------------
# Country-name-based validation
# ------------------------------------------------

expected_country_n <- c(
  Croatia     = 1024,
  Czechia     = 1033,
  Denmark     = 1032,
  France      = 1028,
  Germany     = 1011,
  Italy       = 1042,
  Netherlands = 1145,
  Romania     = 1034,
  Scotland    = 1008,
  Serbia      = 1065,
  Spain       = 1006,
  Sweden      = 1032
)

observed_country_n <- setNames(
  table_s2a$N,
  table_s2a$Country
)

stopifnot(
  nrow(table_s2a) == 12,
  setequal(
    names(observed_country_n),
    names(expected_country_n)
  ),
  all(
    observed_country_n[names(expected_country_n)] ==
      expected_country_n
  ),
  sum(table_s2a$N) == 12460,
  file.exists(s2a_csv),
  file.exists(s2a_xlsx)
)

# ------------------------------------------------
# Read CSV back and verify
# ------------------------------------------------

s2a_check <- read.csv(
  s2a_csv,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(s2a_check) == 12,
  sum(s2a_check$N) == 12460
)

cat(
  "\n============================================================\n",
  "TABLE S2a SAVED AND VERIFIED SUCCESSFULLY\n",
  "============================================================\n",
  "\nCSV: ", s2a_csv,
  "\nXLSX: ", s2a_xlsx,
  "\nCountries: ", nrow(table_s2a),
  "\nTotal N: ", sum(table_s2a$N),
  "\n",
  sep = ""
)

# ================================================================
# 23. FINAL REPRODUCIBILITY CHECKS AND SESSION INFORMATION
# ================================================================

cat(
  "\n============================================================\n",
  "FINAL REPRODUCIBILITY CHECK\n",
  "============================================================\n",
  "Analytical N: ", nrow(analysis_data), "\n",
  "Countries: ", dplyr::n_distinct(analysis_data$country), "\n",
  "Forest Europe macro-regions: ",
  dplyr::n_distinct(analysis_data$region_fe), "\n",
  sep = ""
)

stopifnot(
  nrow(analysis_data) == 12460,
  dplyr::n_distinct(analysis_data$country) == 12,
  dplyr::n_distinct(analysis_data$region_fe) == 5,
  file.exists(table1_csv),
  file.exists(figure1_pdf),
  file.exists(figure1_tiff),
  file.exists(figure2_pdf),
  file.exists(figure2_tiff)
)

session_info_file <- file.path(
  final_dir,
  "sessionInfo.txt"
)

capture.output(
  sessionInfo(),
  file = session_info_file
)

stopifnot(
  file.exists(session_info_file),
  file.info(session_info_file)$size > 0
)

cat(
  "\nFinal reproducibility checks passed.\n",
  "Session information saved to: ",
  session_info_file,
  "\n",
  sep = ""
)

# End of final reproducible analysis workflow.

