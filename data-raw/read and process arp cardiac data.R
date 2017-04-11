library(openxlsx)
library(data.table)
library(lubridate)



# Read in service data ----------------------------------------------------

cardiac_data_wide <- data.table(read.xlsx("data-raw/src data/cardiac data/Cardiac by week perfbestresp - edited.xlsx", rows = 1:8, colNames = FALSE, skipEmptyRows = TRUE))
suppressWarnings(setnames(cardiac_data_wide, c("measure_code", "measure_descr", make.unique(paste0("date_", as.integer(cardiac_data_wide[1, 3:ncol(cardiac_data_wide), with = FALSE]))))))
cardiac_data_wide[, row_num := as.integer(row.names(cardiac_data_wide))]

# corrections to sheet formatting
cardiac_data_wide[, measure_code := as.character(measure_code)]

# Keep only measure code columns and data
cardiac_data_wide[, measure_descr := NULL]

# Keep only rows of data (remove blank rows)
suppressWarnings(cardiac_data_wide_selected <- cardiac_data_wide[!is.na(as.integer(substr(measure_code, 1, 1)))])

# melt the data
suppressWarnings(data_cardiac <- melt(cardiac_data_wide_selected, id.vars = c("measure_code", "row_num"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE))
data_cardiac[, ':=' (amb_service = "YAS",
                     value = as.character(value))]

old_labels <- c("Red1", "Red", "Cat1")
new_labels <- c("1", "2.1", "2.2")
data_cardiac[value %in% old_labels, value := new_labels[match(value, old_labels)]]

# save data
save(data_cardiac, file = "data/arp_cardiac_raw.Rda")


arp_data <- merge(data_cardiac, measures_data_cardiac, by = "measure_code")
# Deal with conversion from Excel
## We can turn warnings off and on again after to avoid expected "NAs introduced by coercion" warnings, but be careful!
old_warn_val <- getOption("warn")
# options(warn = -1)
arp_data[value_format_raw == "time", value := cleanTimes(value)]

# Convert values to type double
arp_data[, value := as.double(value)]
options(warn = old_warn_val)

# Convert week beginnings to type date
arp_data[, week_beginning := as.Date(as.integer(substr(week_beginning, 6, nchar(week_beginning))), origin = "1899-12-30")]

# Read in additional data
hours.and.incidents <- readRDS("data/arp_data_combined - 2017-02-22 14.05.29.Rds")[std_measure_code %in% c("24", "36") &
                                                                                     amb_service == "YAS" &
                                                                                     week_beginning >= as.Date("2014-10-06")]

arp_data <- rbind(arp_data, hours.and.incidents, fill = TRUE)
arp_data[is.na(std_measure_name), std_measure_name := make.names(measure)]


fname <- saveDataForRJ(arp_data, "std_measure_name", "YAS cardiac data")
bla <- readRDS(fname)
