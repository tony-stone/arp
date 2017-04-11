library(openxlsx)
library(data.table)
library(lubridate)

load("data/auxillary_data_wholeservice.Rda")

# Read in service data ----------------------------------------------------
arp_service_list <- lapply(service_data_wholeservice$sheet_name, function(service) {
  data_wide <- data.table(read.xlsx("data-raw/src data/wholeservice data/ARP Final Req Collation revised Tracy 20-02-2017 edited 23-02.xlsx", sheet = service, rows = 4:50, colNames = FALSE, skipEmptyRows = TRUE))
  suppressWarnings(setnames(data_wide, c("measure_code", "measure_descr", make.unique(paste0("date_", as.integer(data_wide[1, 3:ncol(data_wide), with = FALSE]))))))
  data_wide[, row_num := as.integer(row.names(data_wide))]

  # corrections to sheet formatting
  data_wide[, measure_code := as.character(measure_code)]

  # Keep only measure code columns and data
  data_wide[, c("measure_descr", colnames(data_wide)[substr(colnames(data_wide), 1, 7) == "date_NA"]) := NULL]

  # Keep only rows of data (remove blank rows)
  suppressWarnings(data_wide_selected <- data_wide[!is.na(as.integer(substr(measure_code, 1, 1)))])

  # Ensure we uniquely identify each row of data
  if(any(duplicated(data_wide_selected$measure_code))) warning(paste0("Duplicated measure codes in ", service, " data (", paste0(unique(data_wide_selected$measure_code[duplicated(data_wide_selected$measure_code)]), collapse = "; "), ")."))

  suppressWarnings(data_long <- melt(data_wide_selected, id.vars = c("measure_code", "row_num"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE))
  data_long[, ':=' (service_sheet_name = service,
                    value = as.character(value))]
  return(data_long)
})

# Bind data into one dt
arp_data_wholeservice <- rbindlist(arp_service_list)


# save data
save(arp_data_wholeservice, file = "data/arp_data_wholeservice_raw.Rda")
