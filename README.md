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
