convert_pdf_to_png <- function(pdf_file_path, output_dir) {
  
  png_filename <- paste0(output_dir, "/", 
                         tools::file_path_sans_ext(basename(pdf_file_path)), ".png")
  
  
  # Higher quality
  bitmap <- pdftools::pdf_render_page(pdf_file_path, page = 1, dpi = 600)
  png::writePNG(bitmap, png_filename)
  
}

# List all PDF files in the directory
pdf_files <- list.files(path = "output/liss_panelmatch/", 
                        pattern = "*.pdf", full.names = TRUE)

# Apply the function to all PDF files
lapply(pdf_files, convert_pdf_to_png, 
       output_dir = "report/images/")
