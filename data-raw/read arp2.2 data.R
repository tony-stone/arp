library(openxlsx)
library(data.table)
library(lubridate)

load("data/auxillary_data2.2.Rda")

# Read in service data ----------------------------------------------------
arp_service_list <- lapply(service_data2.2$sheet_name, function(service) {
  data_wide <- data.table(read.xlsx("data-raw/src data/phase 2.2 data/Monthly Data Submissions P2.2 Collation to 2017-04-24 - 2017-05-19.xlsx", sheet = service, rows = 3:256, colNames = FALSE, skipEmptyRows = TRUE))
  suppressWarnings(setnames(data_wide, c("measure_code", "measure_descr", "sub_measure", "data_format", make.unique(paste0("date_", as.integer(data_wide[1, 5:ncol(data_wide), with = FALSE]))))))
  data_wide[, row_num := as.integer(row.names(data_wide))]

  # corrections to sheet formatting
  ## missing
  data_wide[measure_code == "Staffing Hours", measure_code := "22"]
  data_wide[measure_descr == "Number of Category 1 calls with resource on scene within 8 minutes", measure_code := "5/6.a"]
  data_wide[measure_descr == "Total Category 1 transported  Incidents", measure_code := "7.a"]
  data_wide[measure_descr == "Number of Category 1  transported incidents with resource on scene within 19 minutes", measure_code := "7.b"]
  data_wide[measure_descr == "Average time from Call Connect to Clock Start - Category 4 T", measure_code := "11.j"]
  data_wide[measure_descr == "Average time from Call Connect to Clock Start - Category 4 H ", measure_code := "11.k"]
  data_wide[sub_measure == "Unknown ", measure_code := "2.o"]


  data_wide[measure_descr == "Hours Lost at hospital (Turnaround) ", measure_code := paste0(17, ".", rank(row_num))]

  ## duplicates
  data_wide[substr(measure_code, 1, 1) == "4" | (substr(measure_code, 1, 1) == "3" & substr(measure_code, 3, 3) %in% letters[1:7]), pos := rank(row_num), by = measure_code][pos != 1, measure_code := ""][, pos := NULL]

  # Keep only measure code columns and data
  data_wide[, c("data_format", "measure_descr", "sub_measure", colnames(data_wide)[substr(colnames(data_wide), 1, 7) == "date_NA"]) := NULL]

  # Keep only rows of data (remove blank rows)
  suppressWarnings(data_wide_selected <- data_wide[!is.na(as.integer(substr(measure_code, 1, 1)))])

  # Ensure we uniquely identify each row of data
  try(if(any(duplicated(data_wide_selected$measure_code))) stop(paste0("Duplicated measure codes in ", service, " data (", paste0(unique(data_wide_selected$measure_code[duplicated(data_wide_selected$measure_code)]), collapse = "; "), ").")))

  suppressWarnings(data_long <- melt(data_wide_selected, id.vars = c("measure_code", "row_num"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE))
  data_long[, ':=' (service_sheet_name = service,
                    value = as.character(value))]
  return(data_long)
})

# Bind data into one dt
arp_data2.2 <- rbindlist(arp_service_list)

# Keep legitimate dates only (at least 10,000 days since 1900)
arp_data2.2 <- arp_data2.2[nchar(week_beginning) > 9]

# save data
save(arp_data2.2, file = "data/arp_data2.2_raw.Rda")
