library(openxlsx)
library(data.table)

# Read in service data ----------------------------------------------------
data_wide <- data.table(read.xlsx("data-raw/src data/stand down data/stand down data - editted - 2017.02.24.xlsx", sheet = "Sheet1", rows = 1:7, colNames = FALSE, skipEmptyRows = TRUE))
suppressWarnings(setnames(data_wide, c("amb_service", "measure_code", "measure_descr", make.unique(paste0("date_", as.integer(data_wide[1, 4:ncol(data_wide), with = FALSE]))))))

# Keep only measure code columns and data
data_wide[, c("measure_descr", colnames(data_wide)[substr(colnames(data_wide), 1, 7) == "date_NA"]) := NULL]

# Keep only rows of data (remove blank rows)
suppressWarnings(data_wide_selected <- data_wide[!is.na(as.integer(substr(measure_code, 1, 1)))])

# Ensure we uniquely identify each row of data
if(any(duplicated(data_wide_selected[, .(measure_code, amb_service)]), na.rm = TRUE)) warning(paste0("Duplicated measure codes in data (", paste0(unique(data_wide_selected$measure_code[duplicated(data_wide_selected[, .(measure_code, amb_service)])]), collapse = "; "), ")."))

# make long form
suppressWarnings(arp_stand_down_data <- melt(data_wide_selected, id.vars = c("measure_code", "amb_service"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE))

# save data
save(arp_stand_down_data, file = "data/arp_stand_down_data_raw.Rda")
