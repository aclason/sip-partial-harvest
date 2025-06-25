


# Install required packages
if (!requireNamespace("googledrive", quietly = TRUE)) install.packages("googledrive")
if (!requireNamespace("magick", quietly = TRUE)) install.packages("magick")
if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")

library(googledrive)
library(magick)
library(fs)

# Authenticate with Google Drive
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# Define the Google Drive folder ID and output folder
parent_folder_id <- "1ybzTAGCAzT8oDzXASa7oqjzokfshPbf8" # Replace with your Google Drive folder ID
output_folder <- "output_images"
#dir_create(output_folder)

# Function to process and save images
process_and_save_images <- function(folder_id, folder_name, output_folder) {
  # List files in the folder
  files <- drive_ls(as_id(folder_id))
  
  # Filter for HEIC files
  heic_files <- files[grepl("\\.HEIC$", files$name, ignore.case = TRUE), ]
  
  # Loop through HEIC files
  for (i in seq_len(nrow(heic_files))) {
    file <- heic_files[i, ]
    local_file <- file.path(tempdir(), file$name)
    
    # Download the file
    drive_download(as_id(file$id), path = local_file, overwrite = TRUE)
    
    # Convert the image
    img <- image_read(local_file)
    new_name <- sprintf("%s_%03d.jpg", folder_name, i) # e.g., FolderName_001.jpg
    output_path <- file.path(output_folder, new_name)
    image_write(img, path = output_path, format = "jpeg")
  }
}

# List subfolders in the parent folder
subfolders <- drive_ls(as_id(parent_folder_id), type = "folder")

# Loop through each subfolder
for (i in seq_len(nrow(subfolders))) {
  subfolder <- subfolders[i, ]
  folder_name <- gsub("\\s+", "_", subfolder$name) # Replace spaces with underscores
  process_and_save_images(subfolder$id, folder_name, output_folder)
}

cat("Processing complete. Images saved to:", output_folder, "\n")







