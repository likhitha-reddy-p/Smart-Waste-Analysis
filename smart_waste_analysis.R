# ============================================================
# SMART WASTE GENERATION PREDICTION
# AND URBAN WASTE MANAGEMENT ANALYSIS
#
# Name    : Likhitha Pothyreddy
# Roll No : 160124771019
# Class   : I1 AI&DS
# Subject : Introduction to Data Science
# ============================================================


# ============================================================
# ============================================================
project_dir <- "C:/Users/likhi/OneDrive/Documents/Smart_Waste_Analysis"
if (!dir.exists(project_dir)) {
  stop(paste("Project folder not found:", project_dir,
             "Please update project_dir to your actual folder."))
}
setwd(project_dir)
cat("Working directory:", getwd(), "\n")


# ============================================================
# 1. INSTALL AND LOAD REQUIRED PACKAGES
# ============================================================

packages <- c("ggplot2", "dplyr", "randomForest")

for (p in packages) {
  
  if (!require(p, character.only = TRUE)) {
    
    install.packages(p, dependencies = TRUE)
    
    library(p, character.only = TRUE)
    
  }
}


# ============================================================
# 2. CREATE OUTPUT FOLDER
# ============================================================

output_dir <- file.path(project_dir, "outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(output_dir) || file.access(output_dir, 2) != 0) {
  stop(paste("Cannot write to output folder:", output_dir,
             "\nChoose a folder where your Windows account has write permission."))
}

# Save plots to an absolute path. Try the standard PNG device first,
# then use the Cairo PNG device if available.
save_plot <- function(filename, plot, width = 9, height = 6, dpi = 300) {
  target <- file.path(output_dir, filename)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  
  tryCatch(
    ggplot2::ggsave(
      filename = target, plot = plot,
      width = width, height = height, units = "in",
      dpi = dpi, device = "png", bg = "white"
    ),
    error = function(e1) {
      if (capabilities("cairo")) {
        ggplot2::ggsave(
          filename = target, plot = plot,
          width = width, height = height, units = "in",
          dpi = dpi, device = grDevices::cairo_png, bg = "white"
        )
      } else {
        stop(paste0(
          "Could not save graph: ", target,
          "\nCheck that the outputs folder is writable and the image is not open elsewhere.",
          "\nOriginal error: ", conditionMessage(e1)
        ))
      }
    }
  )
  if (!file.exists(target)) stop(paste("Plot was not created:", target))
  message("Saved graph: ", target)
}

cat("\nOutput folder ready: ", output_dir, "\n", sep = "")


# ============================================================
# 3. LOAD DATASET
# ============================================================

file_name <- "Waste_Management_and_Recycling_India.csv"

# Check whether file exists
if (!file.exists(file_name)) {
  stop(
    paste(
      "Dataset not found.",
      "Make sure",
      file_name,
      "is in the same folder as the R script."
    )
  )
}

waste <- read.csv(
  file_name,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

cat("\n========================================\n")
cat("DATASET LOADED SUCCESSFULLY\n")
cat("========================================\n")

cat("Rows:", nrow(waste), "\n")
cat("Columns:", ncol(waste), "\n")


# ============================================================
# 4. DISPLAY ORIGINAL DATASET INFORMATION
# ============================================================

cat("\nORIGINAL COLUMN NAMES\n")
print(names(waste))

cat("\nFIRST SIX RECORDS\n")
print(head(waste))

cat("\nDATASET STRUCTURE\n")
str(waste)

cat("\nDATASET SUMMARY\n")
print(summary(waste))


# ============================================================
# 5. STANDARDIZE DATASET COLUMN NAMES
# ============================================================

# These are the actual column names reported by your CSV.
# Map them to shorter, consistent names used by the analysis below.
name_map <- c(
  city_district = "city",
  waste_type = "waste_type",
  waste_generated_tons_day = "waste_generated_tpd",
  recycling_rate_pct = "recycling_rate_pct",
  population_density_people_km = "population_density",
  municipal_efficiency_score_1_10 = "municipal_efficiency",
  disposal_method = "disposal_method",
  cost_of_waste_management_ton = "cost_per_ton",
  awareness_campaigns_count = "awareness_campaigns",
  landfill_name = "landfill_name",
  landfill_location_lat_long = "landfill_location",
  landfill_capacity = "landfill_capacity",
  year = "year"
)

# Normalize spaces/case/punctuation first, without changing meaning.
normalize_names <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}
names(waste) <- normalize_names(names(waste))

# Handle the two alternate names confirmed in your latest Console screenshot.
# Some versions of the CSV use "recycling_rate" and
# "landfill_capacity_tons" instead of the names listed in name_map.
if ("recycling_rate" %in% names(waste) &&
    !"recycling_rate_pct" %in% names(waste)) {
  names(waste)[names(waste) == "recycling_rate"] <- "recycling_rate_pct"
}

if ("landfill_capacity_tons" %in% names(waste) &&
    !"landfill_capacity" %in% names(waste)) {
  names(waste)[names(waste) == "landfill_capacity_tons"] <- "landfill_capacity"
}

missing_source <- setdiff(names(name_map), names(waste))
if (length(missing_source) > 0) {
  stop(paste0(
    "The selected CSV is missing expected source columns: ",
    paste(missing_source, collapse = ", "),
    "\nColumns found: ", paste(names(waste), collapse = ", "),
    "\nPlease check the selected CSV file."
  ))
}

# Rename actual CSV columns to the standardized names used below.
rename_to <- unname(name_map[match(names(waste), names(name_map))])
# Keep the dataset's original column order while applying the mapping.
names(waste) <- rename_to

required_columns <- unname(name_map)
missing_required <- setdiff(required_columns, names(waste))
if (length(missing_required) > 0) {
  stop(paste("Standardized columns missing:",
             paste(missing_required, collapse = ", ")))
}

cat("\nSTANDARDIZED COLUMNS\n")
print(names(waste))


# ============================================================
# 6. DATA PREPROCESSING
# ============================================================

cat("\n========================================\n")
cat("DATA PREPROCESSING\n")
cat("========================================\n")


# ------------------------------------------------------------
# 6.1 Check missing values
# ------------------------------------------------------------

cat("\nMissing values before cleaning:\n")

missing_values <- colSums(is.na(waste))

print(missing_values)


# ------------------------------------------------------------
# 6.2 Check duplicate records
# ------------------------------------------------------------

duplicate_count <- sum(duplicated(waste))

cat("\nDuplicate records:", duplicate_count, "\n")


# Remove duplicates if present

if (duplicate_count > 0) {
  
  waste <- waste[!duplicated(waste), ]
  
  cat(
    duplicate_count,
    "duplicate records removed.\n"
  )
  
} else {
  
  cat("No duplicate records found.\n")
}


# ------------------------------------------------------------
# 6.3 Convert categorical columns
# ------------------------------------------------------------

waste$city <-
  as.factor(waste$city)

waste$waste_type <-
  as.factor(waste$waste_type)

waste$disposal_method <-
  as.factor(waste$disposal_method)

waste$landfill_name <-
  as.factor(waste$landfill_name)

waste$landfill_location <-
  as.factor(waste$landfill_location)


# ------------------------------------------------------------
# 6.4 Convert numeric columns
# ------------------------------------------------------------

waste$waste_generated_tpd <-
  as.numeric(waste$waste_generated_tpd)

waste$recycling_rate_pct <-
  as.numeric(waste$recycling_rate_pct)

waste$population_density <-
  as.numeric(waste$population_density)

waste$municipal_efficiency <-
  as.numeric(waste$municipal_efficiency)

waste$cost_per_ton <-
  as.numeric(waste$cost_per_ton)

waste$awareness_campaigns <-
  as.numeric(waste$awareness_campaigns)

waste$landfill_capacity <-
  as.numeric(waste$landfill_capacity)

waste$year <-
  as.integer(waste$year)


# ------------------------------------------------------------
# 6.5 Remove rows missing important values
# ------------------------------------------------------------

important_columns <- c(
  "waste_generated_tpd",
  "recycling_rate_pct",
  "population_density",
  "municipal_efficiency",
  "cost_per_ton",
  "awareness_campaigns",
  "landfill_capacity",
  "year"
)

before_cleaning <- nrow(waste)

waste <- waste[
  complete.cases(waste[, important_columns]),
]

after_cleaning <- nrow(waste)

cat(
  "\nRows removed because of missing important values:",
  before_cleaning - after_cleaning,
  "\n"
)

cat(
  "Final number of records:",
  nrow(waste),
  "\n"
)


# ------------------------------------------------------------
# 6.6 Check final structure
# ------------------------------------------------------------

cat("\nFINAL DATA STRUCTURE\n")

str(waste)

cat("\nData preprocessing completed successfully.\n")


# ============================================================
# 7. BASIC SUMMARY STATISTICS
# ============================================================

cat("\n========================================\n")
cat("SUMMARY STATISTICS\n")
cat("========================================\n")

cat("\nWaste Generation Summary:\n")

print(
  summary(
    waste$waste_generated_tpd
  )
)


average_waste <-
  mean(
    waste$waste_generated_tpd,
    na.rm = TRUE
  )


minimum_waste <-
  min(
    waste$waste_generated_tpd,
    na.rm = TRUE
  )


maximum_waste <-
  max(
    waste$waste_generated_tpd,
    na.rm = TRUE
  )


median_waste <-
  median(
    waste$waste_generated_tpd,
    na.rm = TRUE
  )


cat(
  "\nAverage Waste Generated:",
  round(average_waste, 2),
  "Tons/Day\n"
)

cat(
  "Median Waste Generated:",
  round(median_waste, 2),
  "Tons/Day\n"
)

cat(
  "Minimum Waste Generated:",
  round(minimum_waste, 2),
  "Tons/Day\n"
)

cat(
  "Maximum Waste Generated:",
  round(maximum_waste, 2),
  "Tons/Day\n"
)


# ============================================================
# 8. DATASET OVERVIEW
# ============================================================

cat("\n========================================\n")
cat("DATASET OVERVIEW\n")
cat("========================================\n")

cat(
  "Number of records:",
  nrow(waste),
  "\n"
)

cat(
  "Number of cities/districts:",
  length(unique(waste$city)),
  "\n"
)

cat(
  "Number of waste types:",
  length(unique(waste$waste_type)),
  "\n"
)

cat(
  "Years available:",
  min(waste$year),
  "-",
  max(waste$year),
  "\n"
)


# ============================================================
# 9. CITY-WISE ANALYSIS
# ============================================================

city_summary <- waste %>%
  
  group_by(city) %>%
  
  summarise(
    
    average_waste =
      mean(
        waste_generated_tpd,
        na.rm = TRUE
      ),
    
    total_waste =
      sum(
        waste_generated_tpd,
        na.rm = TRUE
      ),
    
    average_recycling =
      mean(
        recycling_rate_pct,
        na.rm = TRUE
      ),
    
    average_population_density =
      mean(
        population_density,
        na.rm = TRUE
      ),
    
    records = n()
    
  ) %>%
  
  arrange(
    desc(average_waste)
  )


cat("\nCITY-WISE SUMMARY\n")

print(city_summary)


write.csv(
  city_summary,
  "outputs/city_summary.csv",
  row.names = FALSE
)


# ============================================================
# 10. YEAR-WISE ANALYSIS
# ============================================================

year_summary <- waste %>%
  
  group_by(year) %>%
  
  summarise(
    
    average_waste =
      mean(
        waste_generated_tpd,
        na.rm = TRUE
      ),
    
    total_waste =
      sum(
        waste_generated_tpd,
        na.rm = TRUE
      ),
    
    average_recycling =
      mean(
        recycling_rate_pct,
        na.rm = TRUE
      ),
    
    records = n()
    
  ) %>%
  
  arrange(year)


cat("\nYEAR-WISE SUMMARY\n")

print(year_summary)


write.csv(
  year_summary,
  "outputs/year_summary.csv",
  row.names = FALSE
)


# ============================================================
# 11. WASTE-TYPE ANALYSIS
# ============================================================

waste_type_summary <- waste %>%
  
  group_by(waste_type) %>%
  
  summarise(
    
    average_waste =
      mean(
        waste_generated_tpd,
        na.rm = TRUE
      ),
    
    total_waste =
      sum(
        waste_generated_tpd,
        na.rm = TRUE
      ),
    
    average_recycling =
      mean(
        recycling_rate_pct,
        na.rm = TRUE
      ),
    
    records = n()
    
  ) %>%
  
  arrange(
    desc(average_waste)
  )


cat("\nWASTE TYPE SUMMARY\n")

print(waste_type_summary)


write.csv(
  waste_type_summary,
  "outputs/waste_type_summary.csv",
  row.names = FALSE
)


# ============================================================
# 12. DISPOSAL METHOD ANALYSIS
# ============================================================

disposal_summary <- waste %>%
  
  group_by(disposal_method) %>%
  
  summarise(
    
    average_waste =
      mean(
        waste_generated_tpd,
        na.rm = TRUE
      ),
    
    average_recycling =
      mean(
        recycling_rate_pct,
        na.rm = TRUE
      ),
    
    records = n()
    
  ) %>%
  
  arrange(
    desc(average_waste)
  )


cat("\nDISPOSAL METHOD SUMMARY\n")

print(disposal_summary)


write.csv(
  disposal_summary,
  "outputs/disposal_summary.csv",
  row.names = FALSE
)


# ============================================================
# 13. GRAPH 1 - TOP 10 CITIES
# ============================================================

top10 <- city_summary %>%
  slice_head(n = 10)


p1 <- ggplot(
  
  top10,
  
  aes(
    x = reorder(
      city,
      average_waste
    ),
    y = average_waste
  )
  
) +
  
  geom_col(
    fill = "steelblue"
  ) +
  
  coord_flip() +
  
  labs(
    title =
      "Top 10 Cities by Average Waste Generation",
    
    x =
      "City / District",
    
    y =
      "Average Waste Generated (Tons/Day)"
  ) +
  
  theme_minimal()


print(p1)


save_plot("01_top_10_cities.png", p1, width = 9, height = 6)


# ============================================================
# 14. GRAPH 2 - YEAR-WISE WASTE TREND
# ============================================================

p2 <- ggplot(
  
  year_summary,
  
  aes(
    x = year,
    y = average_waste
  )
  
) +
  
  geom_line(
    linewidth = 1.2,
    color = "darkgreen"
  ) +
  
  geom_point(
    size = 3,
    color = "darkgreen"
  ) +
  
  labs(
    title =
      "Year-wise Average Waste Generation",
    
    x =
      "Year",
    
    y =
      "Average Waste Generated (Tons/Day)"
  ) +
  
  theme_minimal()


print(p2)


save_plot("02_year_wise_waste.png", p2, width = 9, height = 6)


# ============================================================
# 15. GRAPH 3 - POPULATION DENSITY VS WASTE
# ============================================================

p3 <- ggplot(
  
  waste,
  
  aes(
    x = population_density,
    y = waste_generated_tpd
  )
  
) +
  
  geom_point(
    alpha = 0.6,
    color = "purple"
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  ) +
  
  labs(
    title =
      "Population Density vs Waste Generation",
    
    x =
      "Population Density (People/km²)",
    
    y =
      "Waste Generated (Tons/Day)"
  ) +
  
  theme_minimal()


print(p3)


save_plot("03_population_vs_waste.png", p3, width = 9, height = 6)


# ============================================================
# 16. GRAPH 4 - RECYCLING RATE VS WASTE
# ============================================================

p4 <- ggplot(
  
  waste,
  
  aes(
    x = recycling_rate_pct,
    y = waste_generated_tpd
  )
  
) +
  
  geom_point(
    alpha = 0.6,
    color = "orange"
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  
  labs(
    title =
      "Recycling Rate vs Waste Generation",
    
    x =
      "Recycling Rate (%)",
    
    y =
      "Waste Generated (Tons/Day)"
  ) +
  
  theme_minimal()


print(p4)


save_plot("04_recycling_vs_waste.png", p4, width = 9, height = 6)


# ============================================================
# 17. GRAPH 5 - WASTE TYPE ANALYSIS
# ============================================================

p5 <- ggplot(
  
  waste_type_summary,
  
  aes(
    x = reorder(
      waste_type,
      average_waste
    ),
    y = average_waste
  )
  
) +
  
  geom_col(
    fill = "darkcyan"
  ) +
  
  coord_flip() +
  
  labs(
    title =
      "Average Waste Generation by Waste Type",
    
    x =
      "Waste Type",
    
    y =
      "Average Waste Generated (Tons/Day)"
  ) +
  
  theme_minimal()


print(p5)


save_plot("05_waste_type.png", p5, width = 9, height = 6)


# ============================================================
# 18. GRAPH 6 - MUNICIPAL EFFICIENCY VS WASTE
# ============================================================

p6 <- ggplot(
  
  waste,
  
  aes(
    x = municipal_efficiency,
    y = waste_generated_tpd
  )
  
) +
  
  geom_point(
    alpha = 0.6,
    color = "darkblue"
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  ) +
  
  labs(
    title =
      "Municipal Efficiency vs Waste Generation",
    
    x =
      "Municipal Efficiency Score",
    
    y =
      "Waste Generated (Tons/Day)"
  ) +
  
  theme_minimal()


print(p6)


save_plot("06_efficiency_vs_waste.png", p6, width = 9, height = 6)


# ============================================================
# 19. CORRELATION ANALYSIS
# ============================================================

numeric_data <- waste %>%
  
  select(
    waste_generated_tpd,
    recycling_rate_pct,
    population_density,
    municipal_efficiency,
    cost_per_ton,
    awareness_campaigns,
    landfill_capacity,
    year
  )


# Verify that every selected column is numeric

cat("\nNUMERIC COLUMN CHECK\n")

print(
  sapply(
    numeric_data,
    is.numeric
  )
)


correlation_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)


cat("\nCORRELATION MATRIX\n")

print(
  round(
    correlation_matrix,
    3
  )
)


write.csv(
  correlation_matrix,
  "outputs/correlation_matrix.csv",
  row.names = TRUE
)


# ============================================================
# 20. TRAIN / TEST SPLIT
# ============================================================

set.seed(123)


train_index <- sample(
  
  seq_len(
    nrow(waste)
  ),
  
  size =
    floor(
      0.80 *
        nrow(waste)
    )
)


train_data <-
  waste[
    train_index,
  ]


test_data <-
  waste[
    -train_index,
  ]


cat("\n========================================\n")
cat("TRAIN / TEST SPLIT\n")
cat("========================================\n")

cat(
  "Total records:",
  nrow(waste),
  "\n"
)

cat(
  "Training records:",
  nrow(train_data),
  "\n"
)

cat(
  "Testing records:",
  nrow(test_data),
  "\n"
)


# ============================================================
# 21. LINEAR REGRESSION MODEL
# ============================================================

linear_model <- lm(
  
  waste_generated_tpd ~
    
    recycling_rate_pct +
    population_density +
    municipal_efficiency +
    cost_per_ton +
    awareness_campaigns +
    landfill_capacity +
    year,
  
  data = train_data
)


cat("\n========================================\n")
cat("LINEAR REGRESSION MODEL\n")
cat("========================================\n")


print(
  summary(
    linear_model
  )
)


# Linear Regression Predictions

linear_predictions <- predict(
  linear_model,
  newdata = test_data
)


# ============================================================
# 22. RANDOM FOREST MODEL
# ============================================================

set.seed(123)


rf_model <- randomForest(
  
  waste_generated_tpd ~
    
    recycling_rate_pct +
    population_density +
    municipal_efficiency +
    cost_per_ton +
    awareness_campaigns +
    landfill_capacity +
    year +
    waste_type +
    disposal_method,
  
  data = train_data,
  
  ntree = 500,
  
  importance = TRUE
)


cat("\n========================================\n")
cat("RANDOM FOREST MODEL\n")
cat("========================================\n")


print(rf_model)


# Random Forest Predictions

rf_predictions <- predict(
  rf_model,
  newdata = test_data
)


# ============================================================
# 23. MODEL EVALUATION FUNCTIONS
# ============================================================

MAE <- function(
    actual,
    predicted
) {
  
  mean(
    abs(
      actual -
        predicted
    ),
    na.rm = TRUE
  )
}


RMSE <- function(
    actual,
    predicted
) {
  
  sqrt(
    mean(
      (
        actual -
          predicted
      )^2,
      na.rm = TRUE
    )
  )
}


R2 <- function(
    actual,
    predicted
) {
  
  1 -
    
    (
      sum(
        (
          actual -
            predicted
        )^2,
        na.rm = TRUE
      )
      /
        sum(
          (
            actual -
              mean(
                actual,
                na.rm = TRUE
              )
          )^2,
          na.rm = TRUE
        )
    )
}


# ============================================================
# 24. LINEAR REGRESSION METRICS
# ============================================================

linear_mae <- MAE(
  test_data$waste_generated_tpd,
  linear_predictions
)


linear_rmse <- RMSE(
  test_data$waste_generated_tpd,
  linear_predictions
)


linear_r2 <- R2(
  test_data$waste_generated_tpd,
  linear_predictions
)


# ============================================================
# 25. RANDOM FOREST METRICS
# ============================================================

rf_mae <- MAE(
  test_data$waste_generated_tpd,
  rf_predictions
)


rf_rmse <- RMSE(
  test_data$waste_generated_tpd,
  rf_predictions
)


rf_r2 <- R2(
  test_data$waste_generated_tpd,
  rf_predictions
)


# ============================================================
# 26. MODEL COMPARISON
# ============================================================

model_comparison <- data.frame(
  
  Model = c(
    "Linear Regression",
    "Random Forest"
  ),
  
  MAE = c(
    linear_mae,
    rf_mae
  ),
  
  RMSE = c(
    linear_rmse,
    rf_rmse
  ),
  
  R2 = c(
    linear_r2,
    rf_r2
  )
)


cat("\n========================================\n")
cat("MODEL COMPARISON\n")
cat("========================================\n")


print(
  model_comparison
)


write.csv(
  model_comparison,
  "outputs/model_comparison.csv",
  row.names = FALSE
)


# ============================================================
# 27. FIND BETTER MODEL
# ============================================================

best_model_index <-
  which.min(
    model_comparison$RMSE
  )


best_model <-
  model_comparison$Model[
    best_model_index
  ]


cat(
  "\nBest model based on RMSE:",
  best_model,
  "\n"
)


# ============================================================
# 28. SAVE TEST PREDICTIONS
# ============================================================

predictions <- data.frame(
  
  Actual =
    test_data$waste_generated_tpd,
  
  Linear_Regression =
    linear_predictions,
  
  Random_Forest =
    rf_predictions
)


write.csv(
  predictions,
  "outputs/predictions.csv",
  row.names = FALSE
)


# ============================================================
# 29. ACTUAL VS PREDICTED GRAPH
# ============================================================

prediction_plot <- data.frame(
  
  Actual =
    test_data$waste_generated_tpd,
  
  Predicted =
    rf_predictions
)


p7 <- ggplot(
  
  prediction_plot,
  
  aes(
    x = Actual,
    y = Predicted
  )
  
) +
  
  geom_point(
    alpha = 0.7,
    color = "blue"
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  
  labs(
    title =
      "Actual vs Predicted Waste Generation",
    
    subtitle =
      "Random Forest Model",
    
    x =
      "Actual Waste (Tons/Day)",
    
    y =
      "Predicted Waste (Tons/Day)"
  ) +
  
  theme_minimal()


print(p7)


save_plot("07_actual_vs_predicted.png", p7, width = 8, height = 6)


# ============================================================
# 30. RANDOM FOREST FEATURE IMPORTANCE
# ============================================================

importance_values <-
  importance(
    rf_model
  )


cat("\nRANDOM FOREST FEATURE IMPORTANCE\n")

print(
  importance_values
)


# Random Forest regression normally provides IncNodePurity
# and may also provide %IncMSE when importance = TRUE.

if ("%IncMSE" %in% colnames(importance_values)) {
  
  importance_score <-
    importance_values[
      ,
      "%IncMSE"
    ]
  
} else {
  
  importance_score <-
    importance_values[
      ,
      "IncNodePurity"
    ]
}


importance_df <- data.frame(
  
  Feature =
    rownames(
      importance_values
    ),
  
  Importance =
    importance_score
)


importance_df <- importance_df %>%
  
  arrange(
    desc(Importance)
  )


print(
  importance_df
)


write.csv(
  importance_df,
  "outputs/feature_importance.csv",
  row.names = FALSE
)


# ============================================================
# 31. FEATURE IMPORTANCE GRAPH
# ============================================================

p8 <- ggplot(
  
  importance_df,
  
  aes(
    x = reorder(
      Feature,
      Importance
    ),
    y = Importance
  )
  
) +
  
  geom_col(
    fill = "darkorange"
  ) +
  
  coord_flip() +
  
  labs(
    title =
      "Random Forest Feature Importance",
    
    x =
      "Feature",
    
    y =
      "Importance"
  ) +
  
  theme_minimal()


print(p8)


save_plot("08_feature_importance.png", p8, width = 9, height = 6)


# ============================================================
# 32. SAVE CLEAN DATASET
# ============================================================

write.csv(
  waste,
  "outputs/clean_waste_dataset.csv",
  row.names = FALSE
)


# ============================================================
# 33. SAVE MODELS
# ============================================================

saveRDS(
  linear_model,
  "outputs/linear_regression_model.rds"
)


saveRDS(
  rf_model,
  "outputs/random_forest_model.rds"
)


# ============================================================
# 34. SAVE PROJECT SUMMARY
# ============================================================

project_summary <- data.frame(
  
  Measure = c(
    "Total Clean Records",
    "Number of Cities",
    "Number of Waste Types",
    "Minimum Year",
    "Maximum Year",
    "Average Waste Generated",
    "Best Model"
  ),
  
  Value = c(
    nrow(waste),
    length(unique(waste$city)),
    length(unique(waste$waste_type)),
    min(waste$year),
    max(waste$year),
    round(
      mean(
        waste$waste_generated_tpd
      ),
      2
    ),
    best_model
  )
)


write.csv(
  project_summary,
  "outputs/project_summary.csv",
  row.names = FALSE
)


# ============================================================
# 35. FINAL OUTPUT
# ============================================================

cat("\n\n")
cat("=============================================\n")
cat("SMART WASTE PROJECT COMPLETED SUCCESSFULLY\n")
cat("=============================================\n")


cat(
  "\nClean records:",
  nrow(waste)
)


cat(
  "\nCities/Districts:",
  length(
    unique(
      waste$city
    )
  )
)


cat(
  "\nWaste Types:",
  length(
    unique(
      waste$waste_type
    )
  )
)


cat(
  "\nYear Range:",
  min(waste$year),
  "-",
  max(waste$year)
)


cat(
  "\n\nLinear Regression R2:",
  round(
    linear_r2,
    4
  )
)


cat(
  "\nRandom Forest R2:",
  round(
    rf_r2,
    4
  )
)


cat(
  "\n\nBest Model:",
  best_model
)


cat("\n\nCheck the 'outputs' folder.\n")


cat("\nGenerated CSV files:\n")

cat("- clean_waste_dataset.csv\n")
cat("- city_summary.csv\n")
cat("- year_summary.csv\n")
cat("- waste_type_summary.csv\n")
cat("- disposal_summary.csv\n")
cat("- correlation_matrix.csv\n")
cat("- model_comparison.csv\n")
cat("- predictions.csv\n")
cat("- feature_importance.csv\n")
cat("- project_summary.csv\n")


cat("\nGenerated Graphs:\n")

cat("- 01_top_10_cities.png\n")
cat("- 02_year_wise_waste.png\n")
cat("- 03_population_vs_waste.png\n")
cat("- 04_recycling_vs_waste.png\n")
cat("- 05_waste_type.png\n")
cat("- 06_efficiency_vs_waste.png\n")
cat("- 07_actual_vs_predicted.png\n")
cat("- 08_feature_importance.png\n")


cat("\nSaved Models:\n")

cat("- linear_regression_model.rds\n")
cat("- random_forest_model.rds\n")


cat("\n=============================================\n")
cat("END OF PROGRAM\n")
