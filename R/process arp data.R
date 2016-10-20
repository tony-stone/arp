main <- function() {

  processARPData()

  # Info about data quality
  # dates for which we have data
  arp_data_dates <-
    arp_data[!is.na(value), .(
      earliest_data = min(week_beginning), latest_data = max(week_beginning)
    ), by = amb_service]

  # missingness by week and service (across all measures)
  arp_data_weekly_missingness <-
    arp_data[, .(N = .N, missing = sum(is.na(value))), by = .(amb_service, week_beginning)]
  arp_data_weekly_missingness[, missing_pc := round(missing / N * 100, 1)]

  # summary by measure and service (across all time)
  arp_data_info <-
    arp_data[, .(
      N = .N,
      min = min(value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      missing = sum(is.na(value))
    ), by = .(amb_service, measure_code)]
  arp_data_info[N == missing, ':=' (min = NA, max = NA)]
}


processARPData <- function() {

  load("data/auxillary data.Rda")
  load("data/arp_data_raw.Rda")

  # make wide then long again so we can assess missingness
  arp_data_wide <- dcast(arp_data,
                         amb_service + measure_code ~ week_beginning,
                         fill = NA,
                         drop = FALSE,
                         value.var = "value")

  arp_data <- melt(arp_data_wide, id.vars = c("amb_service", "measure_code"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE)

  # Merge in info about measures/services -----------------------------------

  arp_data <- merge(arp_data, measures_data, by = "measure_code")
  arp_data <- merge(arp_data, service_data, by = "amb_service")

  # Deal with conversion from Excel
  arp_data[value_format_raw == "time", value := cleanTimes(value)]
  arp_data[value_format_raw == "date" & measure_code == "22", value := cleanDatetimes(value, 10000)]
  arp_data[value_format_raw == "date" & measure_code != "22" & amb_service == "NEAS", value := cleanDatetimes(value, 20)]
  arp_data[value_format_raw == "date" & measure_code != "22" & amb_service != "NEAS", value := cleanDatetimes(value, 150)]

  # Convert week beginnings to type date; values to type double
  arp_data[, ':=' (week_beginning = as.Date(as.integer(substr(week_beginning, 6, nchar(week_beginning))), origin = "1899-12-30"),
                   value = as.double(value))]

  # Remove specific measures
  ## Remove 8a for AMPDS sites
  arp_data <- arp_data[triage_system  != "AMPDS" | (triage_system  == "AMPDS" & measure_code != "8a")]

  ## Remove G1; G3 (for measures other than call totals) for SECAmb
  arp_data <- arp_data[!(amb_service == "SECAMB" & (call_level == "green1" | (call_level == "green3" & measure != "Total number of calls answered")))]

  ## Remove G1 + G3 for WMAS
  arp_data_final <- arp_data[!(amb_service == "WMAS" & (call_level == "green1" | call_level == "green3"))]

  # save
  save(arp_data_final, file = "data/arp_data_final.Rda")
}

