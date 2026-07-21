for (file in rds_files) {
  object <- readRDS(file)
  
  cat("\n-----------------------------\n")
  cat("File name:", basename(file), "\n")
  cat("Class:", class(object), "\n")
  cat("Type:", typeof(object), "\n")
  
  if (is.data.frame(object)) {
    cat("Rows:", nrow(object), "\n")
    cat("Columns:", ncol(object), "\n")
    cat("Column names:\n")
    print(names(object))
  } else if (is.list(object)) {
    cat("This file contains a list.\nList names:\n")
    print(names(object))
  } else {
    cat("Structure:\n")
    str(object)
  }
}

install.packages(c("data.table", "ggplot2"))

hydro_data <- as.data.table(readRDS(rds_files[1]))

numeric_cols <- names(hydro_data)[sapply(hydro_data, is.numeric)]
stats_table <- rbindlist(lapply(numeric_cols, function(col) {
  x <- hydro_data[[col]]
  data.table(
    variable = col,
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q50 = quantile(x, 0.50, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE)
  )
}))
print(stats_table)
