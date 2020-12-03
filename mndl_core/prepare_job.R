# prepare_job.R
# This script sorts the data of the job_TICKET.CSV file

# Import job from file
job <- rio::import(paste("job_", ticket, ".csv", sep = ""), encoding = "UTF-8")
# Import data
data <- rio::import(dplyr::filter(job, key == "data_file")$value[1])
# Import table type
table_type <- dplyr::filter(job, key == "table_type")$value[1]
# Import metadata and the twoway table plan
meta <- rio::import(dplyr::filter(job, key == "meta_file")$value[1], encoding = "UTF-8")
# Import dependend and independend variables
dependend_vars <- unlist(strsplit(dplyr::filter(job, key == "dependend_vars")$value, ",", fixed = TRUE))
independend_vars <- unlist(strsplit(dplyr::filter(job, key == "independend_vars")$value, ",", fixed = TRUE))

# Create (empty) data frame for options
options <- data.frame(option = integer(0), value = integer(0))

available_keys <- c("caption",
                    "ci_level",
                    "decimal_places",
                    "decimal_places_perc",
                    "footer",
                    "statistical_values")

# Fill options data frame with values
for (.row in seq_len(nrow(job))) {
  if (job[.row, "key"] %in% available_keys && !is.na(job[.row, "value"]))
    options <- rbind.data.frame(options, c(job[.row, "key"], job[.row, "value"]))
}

# Rename columns of options data frame
colnames(options) <- c("option", "value")

# Free memory
rm(available_keys, job)
