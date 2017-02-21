library(openxlsx)
library(data.table)

# Read in service data ----------------------------------------------------
data_wide <- data.table(read.xlsx("data-raw/src data/phase 2 red1 response data/Additional ARP Info requests Responses Collated editted.xlsx", sheet = "Red R", rows = 31:52, colNames = FALSE, skipEmptyRows = TRUE))
suppressWarnings(setnames(data_wide, c("measure_code", "amb_service", "measure_descr", make.unique(paste0("date_", as.integer(data_wide[1, 4:ncol(data_wide), with = FALSE]))))))

# Keep only measure code columns and data
data_wide[, c("measure_descr", colnames(data_wide)[substr(colnames(data_wide), 1, 7) == "date_NA"]) := NULL]

# Keep only rows of data (remove blank rows)
suppressWarnings(data_wide_selected <- data_wide[!is.na(as.integer(substr(measure_code, 1, 1)))])

# Ensure we uniquely identify each row of data
if(any(duplicated(data_wide_selected[, .(measure_code, amb_service)]), na.rm = TRUE)) warning(paste0("Duplicated measure codes in data (", paste0(unique(data_wide_selected$measure_code[duplicated(data_wide_selected[, .(measure_code, amb_service)])]), collapse = "; "), ")."))

# make long form
suppressWarnings(data_long <- melt(data_wide_selected, id.vars = c("measure_code", "amb_service"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE))

# Fix WMAS duplicate dates
data_long <- data_long[week_beginning != "date_42527.1"]
overlap_dates <- paste0("date_", as.integer(seq.Date(as.Date("2016-04-18"), as.Date("2016-05-30"), "week")) + 25569)
data_long_corrected <- data_long[(amb_service %in% c("YAS", "SWAS") & nchar(week_beginning) == 10) |
                                                       (amb_service == "WMAS" & ((nchar(week_beginning) == 10 & !(week_beginning %in% overlap_dates)) | (nchar(week_beginning) == 12 & week_beginning %in% paste0(overlap_dates, ".1"))))]
data_long_corrected[, week_beginning := substr(week_beginning, 1, 10)]

# Convert % to absolute value
measure_5a_absolute <- data_long_corrected[measure_code == "3.a" | measure_code == "5.a", .(measure_code = paste0("m", measure_code), amb_service, week_beginning, value = as.double(value))]
measure_5a_absolute <- dcast(measure_5a_absolute, amb_service + week_beginning ~ measure_code, value.var = "value")
measure_5a_absolute[, ':=' (measure_code = "5.a",
                 value = as.character(round(m3.a * m5.a)),
                 m3.a = NULL,
                 m5.a = NULL)]

arp_data2_red1_responses <- rbind(data_long_corrected[measure_code != "5.a"], measure_5a_absolute)

# save data
save(arp_data2_red1_responses, file = "data/arp_data2_red1_responses_raw.Rda")
