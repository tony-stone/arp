processARPData <- function(phase = "0") {
  if(missing(phase) | !(phase %in% c("1", "2.1", "2.2", "_wholeservice", "_red1"))) stop("Invalid phase")

  if(phase == "1") {
    arp_data <- processARP1Data()
  } else if(phase == "2.1") {
    arp_data <- processARP2.1Data()
  } else if(phase == "2.2") {
    arp_data <- processARP2.2Data()
  } else if(phase == "_wholeservice") {
    arp_data <- processARPwholeserviceData()
  } else if(phase == "_red1") {
    arp_data <- processARPRed1Data()
  }

  # remove trimmed mean data, not using
  arp_data_final <- arp_data[measure_type != "trimmed mean"]

  # save
  fname <- paste0("data/arp_data", phase, "_final.Rds")
  saveRDS(arp_data_final, file = fname)
  print(paste0("File saved: ", fname))
  return(fname)
}

processARP1Data <- function() {

  load("data/auxillary_data1.Rda")
  load("data/arp_data1_raw.Rda")

  # make wide then long again so we can assess missingness
  arp_data_wide <- dcast(arp_data1,
                         service_sheet_name + measure_code ~ week_beginning,
                         fill = NA,
                         drop = FALSE,
                         value.var = "value")

  arp_data <- melt(arp_data_wide, id.vars = c("service_sheet_name", "measure_code"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE)

  # Merge in info about measures/services -----------------------------------

  arp_data <- merge(arp_data, measures_data1, by = "measure_code")
  arp_data <- merge(arp_data, service_data1, by.x = "service_sheet_name", by.y = "sheet_name")
  arp_data[, service_sheet_name := NULL]

  # Deal with conversion from Excel
  ## We can turn warnings off and on again after to avoid expected "NAs introduced by coercion" warnings, but be careful!
  old_warn_val <- getOption("warn")
  # options(warn = -1)
  arp_data[value_format_raw == "time", value := cleanTimes(value)]
  arp_data[value_format_raw == "date" & measure_code == "22", value := cleanDatetimes(value, 10000)]
  arp_data[value_format_raw == "date" & measure_code == "17.1" & amb_service == "NWAS", value := cleanDatetimes(value, 250)]
  arp_data[value_format_raw == "date" & measure_code == "17.1" & amb_service != "NWAS", value := cleanDatetimes(value, 150)]
  arp_data[value_format_raw == "date" & !(measure_code %in% c("17.1", "22")) & amb_service == "NEAS", value := cleanDatetimes(value, 20)]
  arp_data[value_format_raw == "date" & !(measure_code %in% c("17.1", "22")) & amb_service != "NEAS", value := cleanDatetimes(value, 150)]

  # Convert values to type double
  arp_data[, value := as.double(value)]
  options(warn = old_warn_val)

  # Convert week beginnings to type date
  arp_data[, week_beginning := as.Date(as.integer(substr(week_beginning, 6, nchar(week_beginning))), origin = "1899-12-30")]

  # Remove specific measures
  ## Remove 8a for AMPDS sites
  arp_data <- arp_data[triage_system  != "AMPDS" | (triage_system  == "AMPDS" & measure_code != "8a")]

  ## Remove G1 + G3 for SECAmb (but keep G3 for call totals)
  arp_data <- arp_data[!(amb_service == "SECAMB" & (call_level == "green1" | (call_level == "green3" & measure != "Total number of calls answered")))]

  ## Remove G1 + G3 for WMAS
  arp_data <- arp_data[!(amb_service == "WMAS" & (call_level == "green1" | call_level == "green3"))]

  ## Remove Hours lost at turnaround (whole hours)
  arp_data <- arp_data[!(measure_code == "17.2")]

  # Remove time periods
  ## Remove all data before 2014-10-06
  arp_data <- arp_data[!(week_beginning < as.Date("2014-10-06"))]

  ## YAS/SWAS moved to phase 2.1 in w/b 2016-04-18
  arp_data <- arp_data[!((amb_service == "SWAS" | amb_service == "YAS") & week_beginning >= as.Date("2016-04-18"))]

  ## WMAS moved to phase 2.1 in w/b 2016-06-06
  arp_data <- arp_data[!(amb_service == "WMAS" & week_beginning >= as.Date("2016-06-06"))]



  # Fix percentages
  arp_data <- merge(arp_data,
                    arp_data[measure == "Clock start triggers", .(sum = sum(value, na.rm = TRUE)), by = .(amb_service, week_beginning, measure, call_level)][sum > 95 & sum < 105, .(amb_service, week_beginning, measure, call_level, fix_percentage = TRUE)],
                    by = c("amb_service", "week_beginning", "measure", "call_level"),
                    all = TRUE)

  arp_data[fix_percentage == TRUE, value := value / 100]
  arp_data[, fix_percentage := NULL]

  # return
  return(arp_data)
}


processARP2.1Data <- function() {

  load("data/auxillary_data2.1.Rda")
  load("data/arp_data2.1_raw.Rda")

  # make wide then long again so we can assess missingness
  arp_data_wide <- dcast(arp_data2.1,
                         service_sheet_name + measure_code ~ week_beginning,
                         fill = NA,
                         drop = FALSE,
                         value.var = "value")

  arp_data <- melt(arp_data_wide, id.vars = c("service_sheet_name", "measure_code"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE)

  # Merge in info about measures/services -----------------------------------

  arp_data <- merge(arp_data, measures_data2.1, by = "measure_code")
  arp_data <- merge(arp_data, service_data2.1, by.x = "service_sheet_name", by.y = "sheet_name")
  arp_data[, service_sheet_name := NULL]

  # Deal with conversion from Excel - turn warnings off (and on again after)
  ## We can turn warnings off and on again after to avoid expected "NAs introduced by coercion" warnings, but be careful!
  old_warn_val <- getOption("warn")
  # options(warn = -1)
  arp_data[value_format_raw == "time", value := cleanTimes(value)]
  arp_data[value_format_raw == "date" & measure_code == "22", value := cleanDatetimes(value, 10000)]
  arp_data[value_format_raw == "date" & measure_code != "22" & amb_service == "NEAS", value := cleanDatetimes(value, 20)]
  arp_data[value_format_raw == "date" & measure_code != "22" & amb_service != "NEAS", value := cleanDatetimes(value, 150)]

  # Convert values to type double
  arp_data[, value := as.double(value)]
  options(warn = old_warn_val)

  # Convert week beginnings to type date
  arp_data[, week_beginning := as.Date(as.integer(substr(week_beginning, 6, nchar(week_beginning))), origin = "1899-12-30")]

  # Remove specific measures
  ## Remove 8a for AMPDS sites
  arp_data2.1_final <- arp_data[!(triage_system  == "AMPDS" & measure_code == "8a")]

  # Remove specific time periods
  ## YAS/SWAS moved to phase 2.1 in w/b 2016-04-18; and to phase 2.2 in w/b 2016-10-17
  arp_data2.1_final <- arp_data2.1_final[!((amb_service == "YAS" | amb_service == "SWAS") & (week_beginning <= as.Date("2016-04-18") | week_beginning >= as.Date("2016-10-17")))]

  ## WMAS moved to phase 2.1 in w/b 2016-06-06; and to phase 2.2 in w/b 2016-10-10
  arp_data2.1_final <- arp_data2.1_final[!(amb_service == "WMAS" & (week_beginning <= as.Date("2016-06-06") | week_beginning >= as.Date("2016-10-10")))]

  return(arp_data2.1_final)
}


processARP2.2Data <- function() {

  load("data/auxillary_data2.2.Rda")
  load("data/arp_data2.2_raw.Rda")

  # make wide then long again so we can assess missingness
  arp_data_wide <- dcast(arp_data2.2,
                         service_sheet_name + measure_code ~ week_beginning,
                         fill = NA,
                         drop = FALSE,
                         value.var = "value")

  arp_data <- melt(arp_data_wide, id.vars = c("service_sheet_name", "measure_code"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE)

  # Merge in info about measures/services -----------------------------------

  arp_data <- merge(arp_data, measures_data2.2, by = "measure_code")
  arp_data <- merge(arp_data, service_data2.2, by.x = "service_sheet_name", by.y = "sheet_name")
  arp_data[, service_sheet_name := NULL]

  # Deal with conversion from Excel - turn warnings off (and on again after)
  ## We can turn warnings off and on again after to avoid expected "NAs introduced by coercion" warnings, but be careful!
  old_warn_val <- getOption("warn")
  # options(warn = -1)
  arp_data[value_format_raw == "time", value := cleanTimes(value)]
  arp_data[value_format_raw == "date" & measure_code == "22", value := cleanDatetimes(value, 10000)]
  arp_data[value_format_raw == "date" & measure_code != "22" & amb_service == "NEAS", value := cleanDatetimes(value, 20)]
  arp_data[value_format_raw == "date" & measure_code != "22" & amb_service != "NEAS", value := cleanDatetimes(value, 150)]

  # Convert values to type double
  arp_data[, value := as.double(value)]
  options(warn = old_warn_val)

  # Convert week beginnings to type date
  arp_data[, week_beginning := as.Date(as.integer(substr(week_beginning, 6, nchar(week_beginning))), origin = "1899-12-30")]

  # Remove specific measures
  ## Remove 8a for AMPDS sites
  arp_data2.2_final <- arp_data[triage_system  != "AMPDS" | (triage_system  == "AMPDS" & measure_code != "8a")]

  # Remove specific time periods
  ## YAS/SWAS moved to phase 2.2 in w/b 2016-10-17
  arp_data2.2_final <- arp_data2.2_final[!((amb_service == "YAS" | amb_service == "SWAS") & week_beginning <= as.Date("2016-10-17"))]

  ## WMAS moved to phase 2.2 in w/b 2016-10-10
  arp_data2.2_final <- arp_data2.2_final[!(amb_service == "WMAS" & week_beginning <= as.Date("2016-10-10"))]


  return(arp_data2.2_final)
}

processARPwholeserviceData <- function() {

  load("data/auxillary_data_wholeservice.Rda")
  load("data/arp_data_wholeservice_raw.Rda")

  # make wide then long again so we can assess missingness
  arp_data_wide <- dcast(arp_data_wholeservice,
                         service_sheet_name + measure_code ~ week_beginning,
                         fill = NA,
                         drop = FALSE,
                         value.var = "value")

  arp_data <- melt(arp_data_wide, id.vars = c("service_sheet_name", "measure_code"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE)

  # Merge in info about measures/services -----------------------------------

  arp_data <- merge(arp_data, measures_data_wholeservice, by = "measure_code")
  arp_data <- merge(arp_data, service_data_wholeservice, by.x = "service_sheet_name", by.y = "sheet_name")
  arp_data[, service_sheet_name := NULL]

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

  # return
  return(arp_data)
}


processARPRed1Data <- function() {

  load("data/arp_data2_red1_responses_raw.Rda")
  load("data/auxillary_data1.Rda")

    # Merge in info about measures/services -----------------------------------

  arp_data <- merge(arp_data2_red1_responses, measures_data1, by = "measure_code")
  arp_data <- merge(arp_data, service_data1, by = "amb_service")
  arp_data[, sheet_name := NULL]

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

  # return
  return(arp_data)
}



combineComparablePhaseData <- function() {
  # read in individual phase data
  arp_data1 <- readRDS("data/arp_data1_final.Rds")
  arp_data2.1 <- readRDS("data/arp_data2.1_final.Rds")
  arp_data2.2 <- readRDS("data/arp_data2.2_final.Rds")
  arp_data_wholeservice <- readRDS("data/arp_data_wholeservice_final.Rds")
  arp_data_red1 <- readRDS("data/arp_data_red1_final.Rds")

  # add phase field
  arp_data_wholeservice[, phase := "w"]
  arp_data1[, phase := "1"]
  arp_data2.1[, phase := "2.1"]
  arp_data2.2[, phase := "2.2"]
  arp_data_red1[, phase := "red1"]

  # bind (standard) data together
  arp_data_all <- rbind(arp_data_wholeservice, arp_data1, arp_data2.1, arp_data2.2)[!is.na(std_measure_code)]

  # Ensure no overlaps between phases - we use equalities on both sides of intervals as we wish to exclude changeover weeks
  ## Phase 1
  arp_data_all <- arp_data_all[!(phase == "1" & (((amb_service == "SWAS" | amb_service == "YAS") & week_beginning >= as.Date("2016-04-18")) |
                                                   (amb_service == "WMAS" & week_beginning >= as.Date("2016-06-06"))))]

  ## Phase 2.1
  arp_data_all <- arp_data_all[!(phase == "2.1" & (((amb_service == "SWAS" | amb_service == "YAS") & (week_beginning <= as.Date("2016-04-18") | week_beginning >= as.Date("2016-10-17"))) |
                                                     (amb_service == "WMAS" & (week_beginning <= as.Date("2016-06-06") | week_beginning >= as.Date("2016-10-10")))))]

  ## Phase 2.2
  arp_data_all <- arp_data_all[!(phase == "2.2" & (((amb_service == "SWAS" | amb_service == "YAS") & week_beginning <= as.Date("2016-10-17")) |
                                                     (amb_service == "WMAS" & week_beginning <= as.Date("2016-10-10"))))]

  # create 2char measure codes and sum values where necessary
  arp_data_all[, std_measure_code.2char := substr(std_measure_code, 1, 2)]
  summed_measures <- arp_data_all[nchar(std_measure_code) == 3, .(value = sum(value, na.rm = TRUE)), by = .(amb_service, triage_system, week_beginning, section, std_measure_code.2char, measure, sub_measure, measure_type, value_format_raw, phase)]
  summed_measures[value == 0, value := NA]

  # create necessary columns to allow binding, and bind data
  summed_measures[, ':=' (call_level = "all",
                          measure_code = ".sum.",
                          measure_order = NA,
                          std_measure_code = std_measure_code.2char,
                          std_measure_code.2char = NULL)]


  arp_data_all[, std_measure_code.2char := NULL]

  # Red1 calls
  arp_all_red1_data <- rbind(arp_data_all[phase == "1" & std_measure_code %in% c("24a", "26a", as.character(31:34)) &
                                            (week_beginning < as.Date("2016-01-04") | !(amb_service %in% c("SWAS", "WMAS", "YAS")))],
                             arp_data_red1[week_beginning >= as.Date("2016-01-04")])

  arp_all_red1_data[std_measure_code == "24a", std_measure_code := "50"]
  arp_all_red1_data[std_measure_code == "26a", std_measure_code := "51"]
  arp_all_red1_data[std_measure_code %in% as.character(31:34), std_measure_code := as.character(as.integer(std_measure_code) + 21)]

  # ARP combined dataset
  ## Remove components of summed data
  arp_data_all <- arp_data_all[nchar(std_measure_code) < 3]
  arp_combined_data <- rbind(arp_data_all, summed_measures, arp_all_red1_data)

  # Rename standard measures - so they're standardised
  setorder(arp_combined_data, phase)
  standardised_names <- arp_combined_data[!duplicated(arp_combined_data[, std_measure_code]), .(std_measure_code, measure_code, measure, sub_measure, measure_type, measure_order)]
  arp_combined_data[, c("measure_code", "measure", "sub_measure", "measure_type", "measure_order") := NULL]
  arp_combined_data <- merge(arp_combined_data, standardised_names, by = "std_measure_code")

  arp_combined_data[is.na(sub_measure), sub_measure := ""]
  arp_combined_data[, std_measure_code_str := paste0("o", std_measure_code)]
  arp_combined_data[nchar(std_measure_code) == 1, std_measure_code_str := paste0("o0", std_measure_code)]
  arp_combined_data[, std_measure_name := make.names(paste(std_measure_code_str, measure_code, measure, sub_measure, measure_type, call_level, sep = "_"))]

  fname <- paste0("data/arp_data_combined - ", getTimestamp(), ".Rds")
  saveRDS(arp_combined_data, file = fname)
  print(paste0("File saved: ", fname))
  return(fname)
}



combineWSandPhaseData <- function(phase = "0") {
  if(missing(phase) | !(phase %in% c("1", "2.1", "2.2", "_red1"))) stop("Invalid phase")
  arp_data <- readRDS(paste0("data/arp_data", phase, "_final.Rds"))
  arp_WS_data <- readRDS("data/arp_data_wholeservice_final.Rds")

  arp_WS_data <- arp_WS_data[amb_service %in% unique(arp_data$amb_service) & week_beginning >= min(arp_data$week_beginning) & week_beginning <= max(arp_data$week_beginning)]

  combined_data <- rbind(arp_data, arp_WS_data)

  combined_data[is.na(sub_measure), sub_measure := ""]
  combined_data[, measure_name := make.names(paste0("m", paste(measure_code, measure, sub_measure, measure_type, call_level, sep = "_")))]

  fname <- paste0("data/arp_data", phase, "_wholeservice_combined - ", getTimestamp(), ".Rds")
  saveRDS(combined_data, file = fname)
  print(paste0("File saved: ", fname))
  return(fname)
}


saveDataForRJ <- function(data, colname_var, filename) {
  if(length(colname_var) != 1 | any(!(c("amb_service", "week_beginning", "value", colname_var) %in% colnames(data)))) stop("Data in invalid form or colname_var not correctly specified")

  setnames(data, colname_var, "measure_str")
  data_wide <- dcast(data, amb_service + week_beginning ~ measure_str, value.var = "value")
  setnames(data_wide, make.names(names(data_wide), unique = TRUE))

  fname <- paste0("output_data/", filename, " - ", getTimestamp(), ".Rds")
  saveRDS(data_wide, file = fname)
  print(paste0("File saved: ", fname))
  return(fname)
}


cleanTimes <- function(t1) {
  t1_numeric <- as.double(t1) * 1440

  # If not valid double assume time string
  pattern_match <- regexec("^(\\d+):(\\d+):(\\d+)$", t1[is.na(t1_numeric)])

  t1_numeric[is.na(t1_numeric)] <- sapply(regmatches(t1[is.na(t1_numeric)], pattern_match), function(match_data) {
    if(length(match_data) != 4) return(NA)

    match_data <- as.double(match_data[2:4])
    return(match_data[1] * 60 + match_data[2] + match_data[3] / 60)
  })

  # units = minutes
  return(as.character(t1_numeric))
}

cleanDatetimes <- function(t1, hours_cut_off = 0) {

  t1_numeric <- as.double(t1)

  # Asssume units is days (Excel date type) if it does not meet cut off; otherwise assume hours
  t1_numeric[!is.na(t1_numeric) & t1_numeric < hours_cut_off] <- t1_numeric[!is.na(t1_numeric) & t1_numeric < hours_cut_off] * 24

  # If not valid double assume date string
  t1_numeric[is.na(t1_numeric)] <- (as.double(lubridate::fast_strptime(t1[is.na(t1_numeric)], "%d/%m/%Y  %H:%M:%S", lt = FALSE)) + 2209161600) / 3600

  # If standard date string doesn't work, check for Hrs:Min:Sec format
  pattern_match <- regexec("^(\\d+):(\\d+):(\\d+)$", t1[is.na(t1_numeric)])

  t1_numeric[is.na(t1_numeric)] <- sapply(regmatches(t1[is.na(t1_numeric)], pattern_match), function(match_data) {
    if(length(match_data) != 4) return(NA)

    match_data <- as.double(match_data[2:4])
    return(match_data[1] + match_data[2] / 60 + match_data[3] / 3600)
  })

  # units = hours
  return(as.character(t1_numeric))
}


getTimestamp <- function() {
  return(format(Sys.time(), "%Y-%m-%d %H.%M.%S"))
}
