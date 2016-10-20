library(openxlsx)
library(data.table)
library(lubridate)


# Raw data ----------------------------------------------------------------

measures_data <- fread("data-raw/src data/measure_descriptions.csv", sep = ",", header = TRUE, colClasses = "character")

service_data <- data.table(amb_service = c("EoE",
                                           "EMAS",
                                           "LAS",
                                           "NEAS",
                                           "NWAS",
                                           "SCAS",
                                           "SECAMB",
                                           "SWAST",
                                           "WMAS",
                                           "YAS"),
                           triage_system = c("AMPDS",
                                             "AMPDS",
                                             "AMPDS",
                                             "Pathways",
                                             "AMPDS",
                                             "Pathways",
                                             "Pathways",
                                             "AMPDS/Pathways",
                                             "Pathways",
                                             "AMPDS")
                           )

save(measures_data, service_data, file = "data/auxillary data.Rda")


# Read in service data ----------------------------------------------------
arp_service_list <- lapply(service_data$amb_service, function(service) {
  data_wide <- data.table(read.xlsx("data-raw/src data/Ambulance Response Programme -05092016 National Collation.xlsx", sheet = service, rows = 2:219, cols = 1:59, colNames = FALSE, skipEmptyRows = TRUE))
  suppressWarnings(setnames(data_wide, c("measure_code", "measure_descr", "sub_measure", "data_format", make.unique(paste0("date_", as.integer(data_wide[1, 5:ncol(data_wide), with = FALSE]))))))
  data_wide[, row_num := as.integer(row.names(data_wide))]

  # corrections to sheet formatting
  data_wide[measure_descr == "Hours Lost at hospital (Turnaround) ", measure_code := paste0(17, ".", rank(row_num))]
  data_wide[measure_code == "Hours", measure_code := "22"]

  data_wide[, c("data_format", "measure_descr", "sub_measure", colnames(data_wide)[substr(colnames(data_wide), 1, 7) == "date_NA"]) := NULL]

  suppressWarnings(data_wide_selected <- data_wide[!is.na(as.integer(substr(measure_code, 1, 1)))])
  suppressWarnings(data_long <- melt(data_wide_selected, id.vars = c("measure_code", "row_num"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE))
  data_long[, ':=' (amb_service = service,
                    value = as.character(value))]
  return(data_long)
})

# Bind data into one dt
arp_data <- rbindlist(arp_service_list)

# more corrections to sheet formatting (multiple "11.c(i)")
arp_data[measure_code == "11.c(i)", measure_code := paste0("11.c(", tolower(as.roman(rank(row_num))), ")"), by = .(amb_service, week_beginning)]

# save data
save(arp_data, file = "data/arp_data_raw.Rda")
