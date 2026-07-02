library(jsonlite)

# Zenodo data
doi <- "10.5281/zenodo.20479866"

record_id <- sub(".*zenodo\\.", "", doi)

out_dir <- "zenodo_20479866"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

api_url <- paste0("https://zenodo.org/api/records/", record_id)

record <- fromJSON(api_url, simplifyVector = FALSE)

files <- record$files

for (f in files) {
  file_name <- f$key
  download_url <- f$links$self
  out_path <- file.path(out_dir, file_name)
  
  message("Downloading: ", file_name)
  
  download.file(
    url = download_url,
    destfile = out_path,
    mode = "wb",
    quiet = FALSE
  )
}

message("Done")

getwd()

rds_files <- list.files(
  "zenodo_20479866",
  pattern = "\\.rds$",
  full.names = TRUE,
  recursive = TRUE
)

hydro_data <- readRDS(rds_files[1])

str(hydro_data)

head(hydro_data)
tail(hydro_data)
summary(hydro_data)
dim(hydro_data)
nrow(hydro_data)
ncol(hydro_data)
names(hydro_data)
colnames(hydro_data)
rownames(hydro_data)
class(hydro_data)
typeof(hydro_data)
attributes(hydro_data)

# Check data provided

print(rds_files)

for (file in rds_files) {
  hydro_data <- readRDS(file)
  
  cat("\n-----------------------------\n")
  cat("File name:", basename(file), "\n")
  cat("Class:", class(hydro_data), "\n")
  cat("Type:", typeof(hydro_data), "\n")
  
  if (is.data.frame(hydro_data)) {
    cat("Rows:", nrow(hydro_data), "\n")
    cat("Columns:", ncol(hydro_data), "\n")
    cat("Column names:\n")
    print(names(hydro_data))
  } else if (is.list(hydro_data)) {
    cat("This file contains a list.\n")
    cat("List names:\n")
    print(names(hydro_data))
  } else {
    cat("Structure:\n")
    str(hydro_data)
  }
}


# Check shapefiles

shape_files <- list.files(
  out_dir,
  pattern = "\\.(shp|shx|dbf|prj)$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

print(shape_files)

if (length(shape_files) == 0) {
  message("No")
} else {
  message("Yes")
}

# install.packages("data.table")

library(data.table)

out_dir <- "zenodo_20479866"

rds_files <- list.files(
  out_dir,
  pattern = "\\.rds$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

hydro_data <- lapply(rds_files, function(file) {
  x <- readRDS(file)
  
  if (is.data.frame(x)) {
    x <- as.data.table(x)
  } else {
    x <- as.data.table(x)
  }
  
  return(x)
})

names(hydro_data) <- tools::file_path_sans_ext(basename(rds_files))

sapply(hydro_data, is.data.table)


# Calculate statistics

library(data.table)

hydro_data <- readRDS(rds_files[1])
hydro_data <- as.data.table(hydro_data)

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

### Check for trend

names(hydro_data)

### Plots

# install.packages(c("ggplot2", "data.table"))

library(ggplot2)
library(data.table)

# Make sure the data is data.table
hydro_data <- as.data.table(hydro_data)

# plot using basin and biome

ggplot(hydro_data, aes(x = biome_name, y = basin_type, fill = elevation_m)) +
  geom_tile(color = "white") +
  labs(
    title = "Map-Style Plot of Basins by Biome and Basin Type",
    x = "Biome",
    y = "Basin Type",
    fill = "Elevation (m)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatterplot of basin area and elevation

ggplot(hydro_data, aes(x = `area_km^2`, y = elevation_m, color = basin_type)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Basin Area and Elevation",
    x = "Area (km^2)",
    y = "Elevation (m)",
    color = "Basin Type"
  ) +
  theme_minimal()

# Count basins by biome and basin type

biome_counts <- hydro_data[, .N, by = .(biome_name, basin_type)]

ggplot(biome_counts, aes(x = biome_name, y = N, fill = basin_type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Fraction of Basin Types by Biome",
    x = "Biome",
    y = "Fraction",
    fill = "Basin Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pie plot of basin types

basin_counts <- hydro_data[, .N, by = basin_type]

ggplot(basin_counts, aes(x = "", y = N, fill = basin_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(
    title = "Fraction of Basin Types",
    fill = "Basin Type"
  ) +
  theme_void()

# Boxplot of elevation by basin type

ggplot(hydro_data, aes(x = basin_type, y = elevation_m, fill = basin_type)) +
  geom_boxplot() +
  labs(
    title = "Elevation by Basin Type",
    x = "Basin Type",
    y = "Elevation (m)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
  
