# Filter the CO2 hydrological dataset to one basin

out_dir <- "zenodo_20479866"
target_basin <- NULL
if (is.null(target_basin)) {
  filtered_files <- list.files(
    out_dir,
    pattern = "^co2_one_basin_.*\\.rds$",
    full.names = TRUE
  )
  if (length(filtered_files) == 0) {
    stop("No filtered basin file found. Run 02_Filter_CO2_data.R first.")
  }
  filtered_file <- filtered_files[1]
} else {
  filtered_file <- file.path(out_dir, paste0("co2_one_basin_", target_basin, ".rds"))
}

one_basin_data <- readRDS(filtered_file)
message("Using file: ", filtered_file)
message("Rows: ", nrow(one_basin_data))

print(names(one_basin_data))
numeric_variables <- names(one_basin_data)[vapply(one_basin_data, is.numeric, logical(1))]
message("Numeric variables: ", paste(numeric_variables, collapse = ", "))

variable_column <- "variable"
value_column <- "values"
target_variable <- NULL

if (!all(c(variable_column, value_column) %in% names(one_basin_data))) {
  stop("Expected long-format columns were not found. Available columns: ",
       paste(names(one_basin_data), collapse = ", "))
}
available_variables <- unique(one_basin_data[[variable_column]])
message("Available variables: ", paste(available_variables, collapse = ", "))
if (is.null(target_variable)) target_variable <- available_variables[1]
if (!target_variable %in% available_variables) {
  stop("Variable '", target_variable, "' was not found. Choose one of: ",
       paste(available_variables, collapse = ", "))
}
if (!is.numeric(one_basin_data[[value_column]])) {
  stop("Column '", value_column, "' is not numeric.")
}

x <- one_basin_data[one_basin_data[[variable_column]] == target_variable, value_column]

statistics <- data.frame(
  basin_file = basename(filtered_file),
  variable = target_variable,
  n = length(x),
  missing_values = sum(is.na(x)),
  mean = mean(x, na.rm = TRUE),
  median = median(x, na.rm = TRUE),
  min = min(x, na.rm = TRUE),
  max = max(x, na.rm = TRUE),
  range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
  q25 = unname(quantile(x, 0.25, na.rm = TRUE)),
  q50 = unname(quantile(x, 0.50, na.rm = TRUE)),
  q75 = unname(quantile(x, 0.75, na.rm = TRUE))
)

print(statistics)

output_csv <- file.path(out_dir, paste0("statistics_", target_variable, "_one_basin.csv"))
write.csv(statistics, output_csv, row.names = FALSE)
message("Statistics saved to: ", output_csv)


if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Install it with: install.packages('ggplot2')")
}

out_dir <- "zenodo_20479866"
plot_dir <- file.path(out_dir, "co2_plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

filtered_files <- list.files(
  out_dir,
  pattern = "^co2_one_basin_.*\\.rds$",
  full.names = TRUE
)
if (length(filtered_files) == 0) {
  stop("No one-basin CO2 file found. Run 02_Filter_CO2_data.R first.")
}
one_basin_data <- readRDS(filtered_files[1])

variable_column <- "variable"
value_column <- "values"

co2_variable <- NULL
scatter_x_variable <- NULL
date_column <- "date"

if (!all(c(variable_column, value_column) %in% names(one_basin_data))) {
  stop("Expected columns were not found. Available columns: ",
       paste(names(one_basin_data), collapse = ", "))
}
if (!is.numeric(one_basin_data[[value_column]])) {
  stop("Column '", value_column, "' must be numeric.")
}

available_variables <- unique(one_basin_data[[variable_column]])
message("Available variables: ", paste(available_variables, collapse = ", "))
if (is.null(co2_variable)) co2_variable <- available_variables[1]
if (is.null(scatter_x_variable)) {
  alternatives <- setdiff(available_variables, co2_variable)
  if (length(alternatives) == 0) stop("A second variable is needed for the scatterplot.")
  scatter_x_variable <- alternatives[1]
}
if (!co2_variable %in% available_variables) {
  stop("Selected variable '", co2_variable, "' was not found in '", variable_column, "'.")
}
if (!scatter_x_variable %in% available_variables) {
  stop("Scatterplot variable '", scatter_x_variable, "' was not found in '", variable_column, "'.")
}

co2_data <- one_basin_data[one_basin_data[[variable_column]] == co2_variable, , drop = FALSE]

basin_label <- if ("basin" %in% names(one_basin_data)) {
  as.character(one_basin_data$basin[1])
} else {
  basename(filtered_files[1])
}

# 1. Time series
if (date_column %in% names(one_basin_data)) {
  plot_data <- co2_data
  plot_data$plot_date <- if (inherits(plot_data[[date_column]], "Date")) {
    plot_data[[date_column]]
  } else {
    as.Date(as.character(plot_data[[date_column]]))
  }
  
  if (all(is.na(plot_data$plot_date))) {
    message("Time-series plot skipped: '", date_column, "' could not be read as a date.")
  } else {
    p_time <- ggplot2::ggplot(plot_data, ggplot2::aes(x = plot_date, y = .data[[value_column]])) +
      ggplot2::geom_line(colour = "steelblue", linewidth = 0.35, na.rm = TRUE) +
      ggplot2::labs(title = paste(co2_variable, "time series — basin", basin_label),
                    x = "Date", y = co2_variable) +
      ggplot2::theme_minimal()
    print(p_time)
    ggplot2::ggsave(file.path(plot_dir, "co2_timeseries.png"), p_time,
                    width = 10, height = 5, dpi = 300)
  }
} else {
  message("Time-series plot skipped: no '", date_column, "' column.")
}

# 2. Scatterplot

predictor_data <- one_basin_data[one_basin_data[[variable_column]] == scatter_x_variable, , drop = FALSE]
join_columns <- intersect(c(date_column, "pet_method"), names(one_basin_data))
if (length(join_columns) == 0) stop("No shared date column was found for the scatterplot.")
scatter_data <- merge(
  co2_data[, c(join_columns, value_column), drop = FALSE],
  predictor_data[, c(join_columns, value_column), drop = FALSE],
  by = join_columns,
  suffixes = c("_co2", "_x")
)
names(scatter_data)[names(scatter_data) == paste0(value_column, "_co2")] <- "co2_value"
names(scatter_data)[names(scatter_data) == paste0(value_column, "_x")] <- "x_value"

p_scatter <- ggplot2::ggplot(
  scatter_data,
  ggplot2::aes(x = x_value, y = co2_value)
) +
  ggplot2::geom_point(alpha = 0.35, colour = "darkorange", na.rm = TRUE) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "black", na.rm = TRUE) +
  ggplot2::labs(title = paste(scatter_x_variable, "versus", co2_variable, "— basin", basin_label),
                x = scatter_x_variable, y = co2_variable) +
  ggplot2::theme_minimal()
print(p_scatter)
ggplot2::ggsave(file.path(plot_dir, "co2_scatterplot.png"), p_scatter,
                width = 7, height = 5, dpi = 300)

# 3. Fractional plot 
co2_state <- ifelse(co2_data[[value_column]] > 0, "Positive",
                    ifelse(co2_data[[value_column]] < 0, "Negative", "Zero"))
co2_state[is.na(co2_data[[value_column]])] <- NA
fraction_data <- as.data.frame(prop.table(table(co2_state, useNA = "no")))
names(fraction_data) <- c("CO2 state", "Fraction")

p_fraction <- ggplot2::ggplot(fraction_data,
                              ggplot2::aes(x = "", y = Fraction, fill = `CO2 state`)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
  ggplot2::labs(title = paste("Fraction of", co2_variable, "states — basin", basin_label),
                x = NULL, y = "Fraction of days", fill = NULL) +
  ggplot2::theme_minimal()
print(p_fraction)
ggplot2::ggsave(file.path(plot_dir, "co2_fractional_stacked_bar.png"), p_fraction,
                width = 7, height = 5, dpi = 300)

# 4. Map
longitude_candidates <- c("longitude", "lon", "long", "x")
latitude_candidates <- c("latitude", "lat", "y")
longitude_column <- longitude_candidates[longitude_candidates %in% names(one_basin_data)][1]
latitude_column <- latitude_candidates[latitude_candidates %in% names(one_basin_data)][1]

if (!is.na(longitude_column) && !is.na(latitude_column) &&
    is.numeric(one_basin_data[[longitude_column]]) && is.numeric(one_basin_data[[latitude_column]])) {
  map_data <- co2_data[!is.na(co2_data[[longitude_column]]) &
                         !is.na(co2_data[[latitude_column]]), ]
  p_map <- ggplot2::ggplot(map_data,
                           ggplot2::aes(x = .data[[longitude_column]], y = .data[[latitude_column]])) +
    ggplot2::geom_point(size = 3, colour = "red") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = paste("Location of basin", basin_label),
                  x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal()
  print(p_map)
  ggplot2::ggsave(file.path(plot_dir, "co2_basin_map.png"), p_map,
                  width = 7, height = 5, dpi = 300)
} else {
  message("Map skipped: no numeric longitude/latitude columns were found.")
}

message("Plots saved in: ", plot_dir)
