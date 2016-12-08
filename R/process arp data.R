library(data.table)

# Once the raw data has been read in, see "data-raw/read arp1 data.R" and "data-raw/read arp2.1 data.R",
# process it:
processARP1Data()
processARP2.1Data()
processARP2.2Data()

# How does it look?
summ_data1 <- getARPSUmmaryData(phase = 1)
View(summ_data1)
summ_data2.1 <- getARPSUmmaryData(phase = 2.1)
View(summ_data2.1)
summ_data2.2 <- getARPSUmmaryData(phase = 2.2)
View(summ_data2.2)

# With all plots, below, I recommend viewing them full size on a separate minitor

# Well that was horrible, how does the missingness (by service/time) look graphically?
examineProblematicData(phase = 1, measuresCombined = TRUE)
examineProblematicData(phase = 2.1, measuresCombined = TRUE)
missing_and_outlying <- examineProblematicData(phase = 2.2, measuresCombined = TRUE)

# We can do stuff with this is we want
View(missing_and_outlying)

# What's missing and what's "outlying"? (by measure/service/time)
examineProblematicData(phase = 1, measuresCombined = FALSE, outliersAtSD = 3)
examineProblematicData(phase = 2.1, measuresCombined = FALSE, outliersAtSD = 3)
examineProblematicData(phase = 2.2, measuresCombined = FALSE, outliersAtSD = 3)

# Now let's look measure by measure
examineData(phase = 1, outliers_SD = 3)
examineData(phase = 2.1, outliers_SD = 3)
examineData(phase = 2.2, outliers_SD = 3)

m17# Can (somewhat naively) also just look at the Call Start Triggers
examineCSTriggers(phase = 1, showSDLimits = TRUE, SDLimits = 3)
examineCSTriggers(phase = 2.1, showSDLimits = TRUE, SDLimits = 3)
examineCSTriggers(phase = 2.2, showSDLimits = TRUE, SDLimits = 3)

# That's enough to be getting on with, surely!


# Process ARP data --------------------------------------------------------

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
  arp_data[value_format_raw == "date" & measure_code != "22" & amb_service == "NEAS", value := cleanDatetimes(value, 20)]
  arp_data[value_format_raw == "date" & measure_code != "22" & amb_service != "NEAS", value := cleanDatetimes(value, 150)]

  # Convert values to type double
  arp_data[, value := as.double(value)]
  options(warn = old_warn_val)

  # Convert week beginnings to type date
  arp_data[, week_beginning := as.Date(as.integer(substr(week_beginning, 6, nchar(week_beginning))), origin = "1899-12-30")]

  # Remove specific measures
  ## Remove 8a for AMPDS sites
  arp_data <- arp_data[triage_system  != "AMPDS" | (triage_system  == "AMPDS" & measure_code != "8a")]

  ## Remove G1; G3 (for measures other than call totals) for SECAmb
  arp_data <- arp_data[!(amb_service == "SECAMB" & (call_level == "green1" | (call_level == "green3" & measure != "Total number of calls answered")))]

  ## Remove G1 + G3 for WMAS
  arp_data <- arp_data[!(amb_service == "WMAS" & (call_level == "green1" | call_level == "green3"))]

  # Fix percentages
    arp_data <- merge(arp_data,
                    arp_data[measure == "Clock start triggers", .(sum = sum(value, na.rm = TRUE)), by = .(amb_service, week_beginning, measure, call_level)][sum > 95 & sum < 105, .(amb_service, week_beginning, measure, call_level, fix_percentage = TRUE)],
                    by = c("amb_service", "week_beginning", "measure", "call_level"),
                    all = TRUE)

  arp_data[fix_percentage == TRUE, value := value / 100]
  arp_data[, fix_percentage := NULL]

  # save
  arp_data1_final <- arp_data
  saveRDS(arp_data1_final, file = "data/arp_data1_final.Rds")
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
  old_warn_val <- getOption("warn")
  options(warn = -1)
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
  arp_data2.1_final <- arp_data[triage_system  != "AMPDS" | (triage_system  == "AMPDS" & measure_code != "8a")]

  ## Clearly YAS did not supply a 7day period of data for the first week (w/b: 2016-04-18). Remove
  arp_data2.1_final <- arp_data2.1_final[amb_service != "YAS" | week_beginning != as.Date("2016-04-18")]
  # save
  saveRDS(arp_data2.1_final, file = "data/arp_data2.1_final.Rds")
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
  old_warn_val <- getOption("warn")
  options(warn = -1)
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

  ## Clearly YAS did not supply a 7day period of data for the first week (w/b: 2016-04-18). Remove
  #arp_data2.2_final <- arp_data2.2_final[amb_service != "YAS" | week_beginning != as.Date("2016-04-18")]
  # save
  saveRDS(arp_data2.2_final, file = "data/arp_data2.2_final.Rds")
}

