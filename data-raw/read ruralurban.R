library(openxlsx)
library(data.table)

# Read in service data ----------------------------------------------------
load("data/auxillary_data_ruralurban.Rda")

ruralurban_data_list <- apply(service_data_ruralurban, 1, function(service_data) {
  service_level_ru_data_list <- lapply(c("urban", "rural", "urban-rural"), function(sheetname, fn) {
    data_wide <- data.table(read.xlsx(paste0("data-raw/src data/rural urban/", fn, ".xlsx"), sheet = sheetname, colNames = FALSE, skipEmptyRows = TRUE))
    suppressWarnings(setnames(data_wide, c("measure_descr", make.unique(paste0("date_", as.integer(data_wide[1, 2:ncol(data_wide), with = FALSE]))))))

    data_wide <- data_wide[!(measure_descr %in% c("Emergency Only ", "All 999 S&T/S&C Incidents", "QA: Call Connect to arrival of first core resource. ",
                             "All S&C Incidents ", "QC: Call Connect to arrival at hospital ",
                             "All S&T Incidents", "QE: Call Connect to leaving scene/Clear"))]

    # attach measure codes
    data_wide[, measure_code := row.names(data_wide)]

    # ensure we only have expected measures
    stopifnot(all(data_wide$measure_descr %in% c("50th centile", "95th centile", "99th centile", paste0("Q", c("B", "D", "F"), ": RPI - Core resources "), "Incident Total")))
    data_wide[, measure_descr := NULL]

    # Keep only measure code columns and data
    data_wide[, colnames(data_wide)[substr(colnames(data_wide), 1, 7) == "date_NA"] := NULL]

    data_wide[, sub_measure := sheetname]

    return(data_wide)

  }, fn = unlist(service_data)["filename"])

  service_level_ru_data <- rbindlist(service_level_ru_data_list)
  service_level_ru_data[, amb_service := unlist(service_data)["amb_service"]]

  return(service_level_ru_data)
})

ruralurban_data_wide <- rbindlist(ruralurban_data_list)

# make long form
suppressWarnings(arp_ruralurban_data <- melt(ruralurban_data_wide, id.vars = c("measure_code", "sub_measure", "amb_service"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE))

# save data
save(arp_ruralurban_data, file = "data/arp_ruralurban_data_raw.Rda")
