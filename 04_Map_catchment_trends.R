# https://zenodo.org/records/21223242

if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("sf", quietly = TRUE) ||
    !requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Install required packages: install.packages(c('jsonlite', 'sf', 'ggplot2'))")
}

record_id <- "21223242"
out_dir <- paste0("zenodo_", record_id)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
options(timeout = 3600)
record <- jsonlite::fromJSON(paste0("https://zenodo.org/api/records/", record_id),
                             simplifyVector = FALSE)

for (f in record$files) {
  out_file <- file.path(out_dir, f$key)
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  complete_file <- file.exists(out_file) && !is.null(f$size) &&
    file.info(out_file)$size == f$size
  if (!complete_file) {
    if (file.exists(out_file)) file.remove(out_file)
    message("Downloading: ", f$key)
    download.file(f$links$self, out_file, mode = "wb", method = "libcurl")
  }
}

info_file <- file.path(out_dir, "catchment_info_table.rds")
trend_file <- file.path(out_dir, "trend_annual_pre_aet_q_q_surf_tws.rds")
if (!file.exists(info_file) || !file.exists(trend_file)) {
  stop("Expected RDS files were not found in: ", out_dir)
}
catchment_info <- as.data.frame(readRDS(info_file))
trend_data <- as.data.frame(readRDS(trend_file))

message("Catchment information columns: ", paste(names(catchment_info), collapse = ", "))
message("Trend table columns: ", paste(names(trend_data), collapse = ", "))

common_columns <- intersect(names(catchment_info), names(trend_data))
basin_candidates <- common_columns[grepl("basin|catchment|gauge|station|id",
                                         common_columns, ignore.case = TRUE)]
if (length(basin_candidates) == 0) basin_candidates <- common_columns
if (length(basin_candidates) == 0) stop("No shared basin identifier was found.")
basin_column <- basin_candidates[1]

geometry_column <- "geometry"
if (!geometry_column %in% names(catchment_info)) {
  stop("No 'geometry' column was found in catchment_info.")
}

variable_column <- names(trend_data)[grepl("^variable$|parameter|metric", names(trend_data), ignore.case = TRUE)][1]
target_variable <- NULL
if (!is.na(variable_column)) {
  available_variables <- unique(trend_data[[variable_column]])
  message("Available trend variables: ", paste(available_variables, collapse = ", "))
  if (is.null(target_variable)) target_variable <- available_variables[1]
  trend_data <- trend_data[trend_data[[variable_column]] == target_variable, , drop = FALSE]
}

trend_candidates <- names(trend_data)[grepl("trend|slope|value", names(trend_data), ignore.case = TRUE)]
trend_candidates <- trend_candidates[vapply(trend_data[trend_candidates], is.numeric, logical(1))]
if (length(trend_candidates) == 0) {
  numeric_columns <- names(trend_data)[vapply(trend_data, is.numeric, logical(1))]
  trend_candidates <- setdiff(numeric_columns, basin_column)
}
if (length(trend_candidates) == 0) stop("No numeric trend value column was found.")
trend_column <- trend_candidates[1]
message("Mapping trend column: ", trend_column)

trend_summary <- stats::aggregate(
  trend_data[[trend_column]], by = list(trend_data[[basin_column]]),
  FUN = function(x) mean(x, na.rm = TRUE)
)
names(trend_summary) <- c(basin_column, "trend_value")
map_data <- merge(
  catchment_info[, c(basin_column, geometry_column), drop = FALSE],
  trend_summary, by = basin_column
)
map_data <- map_data[is.finite(map_data$trend_value), , drop = FALSE]
if (nrow(map_data) == 0) stop("No catchment geometries could be joined to trend values.")

if (inherits(map_data[[geometry_column]], "sfc")) {
  map_sf <- sf::st_as_sf(map_data, sf_column_name = geometry_column)
} else if (is.character(map_data[[geometry_column]])) {
  map_sf <- sf::st_as_sf(map_data, wkt = geometry_column, crs = 4326)
} else {
  stop("The geometry column is not an sf geometry or WKT text column.")
}

title_variable <- if (is.null(target_variable)) trend_column else target_variable
p_map <- ggplot2::ggplot(
  map_sf,
  ggplot2::aes(fill = trend_value)
) +
  ggplot2::geom_sf(colour = NA) +
  ggplot2::scale_fill_gradient2(low = "firebrick", mid = "white", high = "navy",
                                midpoint = 0, name = "Trend") +
  ggplot2::labs(title = paste("Annual catchment trend:", title_variable),
                x = NULL, y = NULL) +
  ggplot2::theme_minimal()
print(p_map)
