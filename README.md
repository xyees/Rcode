# Rcode
Linking daily hydroclimatic extremes to long-term terrestrial watercycle trends

# install.packages(c("jsonlite", "digest"))

library(jsonlite)
library(digest)

doi <- "10.5281/zenodo.20479866"
record_id <- sub(".*zenodo\\.", "", doi)

out_dir <- "zenodo_20479866"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

api_url <- paste0("https://zenodo.org/api/records/", record_id)

record <- fromJSON(
  api_url,
  simplifyVector = FALSE
)

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
    quiet = FALSE,
    headers = c("User-Agent" = "R Zenodo downloader")
  )
  
  # Optional checksum verification when Zenodo provides MD5
  if (!is.null(f$checksum) && startsWith(f$checksum, "md5:")) {
    expected_md5 <- sub("^md5:", "", f$checksum)
    actual_md5 <- digest(out_path, algo = "md5", file = TRUE)
    
    if (!identical(expected_md5, actual_md5)) {
      warning("Checksum mismatch for ", file_name)
    } else {
      message("Checksum OK: ", file_name)
    }
  }
}

message("Done. Files saved in: ", normalizePath(out_dir))
