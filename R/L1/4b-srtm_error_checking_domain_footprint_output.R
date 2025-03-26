# Set working directory
dir <- "/mnt/ufs18/home-109/kapsarke/Documents/neonEnvData/R/L1/"

# List all slurm output files
files <- list.files(pattern = "slurm-.*\\.out$", path = dir, full.names = T)

# Patterns to look for
error_patterns <- c("error", "core dumped", "segmentation fault", "traceback", "aborted", "fatal")

# Function to check file for error messages and return matched pattern(s)
check_file_errors <- function(file) {
  lines <- readLines(file, warn = FALSE)
  
  # Find which patterns match
  matched <- sapply(error_patterns, function(p) {
    any(grepl(p, lines, ignore.case = TRUE))
  })
  
  # Return names (patterns) that matched, or NULL if none
  if (any(matched)) {
    return(names(matched[matched]))
  } else {
    return(NULL)
  }
}

# Apply function and name results by file
problem_patterns <- setNames(lapply(files, check_file_errors), files)

# Filter only files with problems
problem_patterns <- problem_patterns[!sapply(problem_patterns, is.null)]

# Extract the names (filenames) from the named list
error_files <- basename(names(problem_patterns))

# Extract the number using regex
job_numbers <- as.integer(sub("slurm-\\d+_(\\d+)\\.out", "\\1", error_files))


# Collapse each error vector into a single string
error_messages <- sapply(problem_patterns, function(errs) paste(errs, collapse = "; "))

# Build the dataframe
error_df <- data.frame(
  job_number = job_numbers,
  error = error_messages,
  row.names = NULL,
  stringsAsFactors = FALSE
)

# View it
print(error_df)

bad_jobs <- combos[job_numbers,]

error_df <- cbind(error_df, bad_jobs)
