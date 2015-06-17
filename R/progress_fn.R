# Writes a message to the predefined progress file, creates the file
# if it doesn't exist
progress_fn <- function(message,progress_rep_file) {

    if(!file.exists(progress_rep_file)) {
        file.create(progress_rep_file)
    }

    file_obj <- file(progress_rep_file)
    print(message)
    writeLines(message, file_obj)
    ##cat(message, file=progress_rep_file, sep='\n', append=TRUE)
    close(file_obj)
}
