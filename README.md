# Load the package
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

if (length(shape_files) == 0) {
  message("No")
} else {
  message("Yes")
}

