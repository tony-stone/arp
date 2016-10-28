library(openxlsx)
library(data.table)
library(lubridate)

load("data/auxillary_data1.Rda")


# Read in service data ----------------------------------------------------
arp_service_list <- lapply(service_data1$sheet_name, function(service) {
  data_wide <- data.table(read.xlsx("data-raw/src data/Ambulance Response Programme -17102016 National Collation.xlsx", sheet = service, rows = 2:219, colNames = FALSE, skipEmptyRows = TRUE))
  suppressWarnings(setnames(data_wide, c("measure_code", "measure_descr", "sub_measure", "data_format", make.unique(paste0("date_", as.integer(data_wide[1, 5:ncol(data_wide), with = FALSE]))))))
  data_wide[, row_num := as.integer(row.names(data_wide))]

  # corrections to sheet formatting
  data_wide[measure_descr == "Hours Lost at hospital (Turnaround) ", measure_code := paste0(17, ".", rank(row_num))]
  data_wide[measure_code == "Hours", measure_code := "22"]

  # Keep only measure code columns and data
  data_wide[, c("data_format", "measure_descr", "sub_measure", colnames(data_wide)[substr(colnames(data_wide), 1, 7) == "date_NA"]) := NULL]

  # Keep only rows of data (remove blank rows)
  suppressWarnings(data_wide_selected <- data_wide[!is.na(as.integer(substr(measure_code, 1, 1)))])

  # Ensure we uniquely identify each row of data
  try(if(any(duplicated(data_wide_selected$measure_code))) stop(paste0("Duplicated measure codes in ", service, " data (", paste0(unique(data_wide_selected$measure_code[duplicated(data_wide_selected$measure_code)]), collapse = "; "), ").")))

  suppressWarnings(data_long <- melt(data_wide_selected, id.vars = c("measure_code", "row_num"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE))
  data_long[, ':=' (amb_service = service,
                    value = as.character(value))]
  return(data_long)
})

# Bind data into one dt
arp_data1 <- rbindlist(arp_service_list)

# more corrections to sheet formatting
## (multiple "11.c(i)")
arp_data1[measure_code == "11.c(i)", measure_code := paste0("11.c(", tolower(as.roman(rank(row_num))), ")"), by = .(amb_service, week_beginning)]
## Transposed month and day in date
arp_data1[week_beginning == "date_42554", week_beginning := "date_42436"]

# save data
save(arp_data1, file = "data/arp_data1_raw.Rda")
