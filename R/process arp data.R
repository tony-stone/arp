main <- function() {

  processARP1Data()
  processARP2.1Data()

  load(file = "data/arp_data1_final.Rda")
  load(file = "data/arp_data2.1_final.Rda")

  # Info about data quality
  # dates for which we have data
  arp_data_dates <-
    arp_data2.1_final[!is.na(value), .(
      earliest_data = min(week_beginning), latest_data = max(week_beginning)
    ), by = amb_service]

  measures <- unique(arp_data2.1_final[, .(measure, sub_measure, measure_type, call_level, measure_order)])
  setorder(measures, measure_order)

  print(plotWeeklyVals(arp_data2.1_final, "Total number of calls answered", "any origin", show_call_level = TRUE, call_level_version = "2.1"))

  i <- 1
  while(i <= nrow(measures)) {
    if(nrow(arp_data2.1_final[measure == measures[i, measure] & sub_measure == measures[i, sub_measure] & measure_type == measures[i, measure_type] & call_level == measures[i, call_level]]) == 0) {
      cat(paste("No data for:", paste(measures[i, .(measure, sub_measure, measure_type, call_level)], collapse = " - ")))
    } else {
    print(plotWeeklyVals(arp_data2.1_final, measures[i, measure], measures[i, sub_measure], measures[i, measure_type], measures[i, call_level], show_call_level = TRUE, call_level_version = "2.1"))
    }
    cmd <- readline(prompt="Press [enter] to continue; [b] to go back; or [q] to quit: ")
    if(tolower(cmd) == "q") {
      break
    } else if(tolower(cmd) == "b") {
      i <- max(c(i - 1, 1))
    } else {
      i <- i + 1
    }
  }

  outlying_or_missing <- identifyOutlyingAndMissingData(arp_data2.1_final, 3)

  outlying_or_missing[problem == "missing", .N]


  # missingness by week and service (across all measures)
  arp_data_weekly_missingness <-
    arp_data_final[, .(N = .N, missing = sum(is.na(value))), by = .(amb_service, week_beginning)]
  arp_data_weekly_missingness[, missing_pc := round(missing / N, 3)]

  plot <- ggplot2::ggplot(arp_data_weekly_missingness, ggplot2::aes(x = week_beginning, y = missing_pc)) +
    ggplot2::facet_wrap(~amb_service, ncol = 2, scales = "free_y") +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0))

  print(plot)

  meas_plots <- lapply(unique(arp_data_final$measure), function(meas) {
    plot <- ggplot2::ggplot(arp_data_final[measure == meas], ggplot2::aes(measure_code, value)) +
      ggplot2::ggtitle(meas) +
      ggplot2::facet_wrap(~amb_service, ncol = 1, scales = "free_y") +
      ggplot2::geom_jitter(width = 0.4)
  })

  print(meas_plots[[1]])

  # summary by measure and service (across all time)
  arp_data_info <-
    arp_data_final[, .(
      N = .N,
      min = round(min(value, na.rm = TRUE), 1),
      mean = round(mean(value, na.rm = TRUE), 1),
      median = round(median(value, na.rm = TRUE), 1),
      max = round(max(value, na.rm = TRUE), 1),
      missing = sum(is.na(value))
    ), by = .(amb_service, measure_code)]
  arp_data_info[N == missing, ':=' (min = NA, max = NA)]
}


processARP1Data <- function() {

  load("data/auxillary_data1.Rda")
  load("data/arp_data1_raw.Rda")

  # make wide then long again so we can assess missingness
  arp_data_wide <- dcast(arp_data1,
                         amb_service + measure_code ~ week_beginning,
                         fill = NA,
                         drop = FALSE,
                         value.var = "value")

  arp_data <- melt(arp_data_wide, id.vars = c("amb_service", "measure_code"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE)

  # Merge in info about measures/services -----------------------------------

  arp_data <- merge(arp_data, measures_data1, by = "measure_code")
  arp_data <- merge(arp_data, service_data1, by = "amb_service")

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
  arp_data <- arp_data[triage_system  != "AMPDS" | (triage_system  == "AMPDS" & measure_code != "8a")]

  ## Remove G1; G3 (for measures other than call totals) for SECAmb
  arp_data <- arp_data[!(amb_service == "SECAMB" & (call_level == "green1" | (call_level == "green3" & measure != "Total number of calls answered")))]

  ## Remove G1 + G3 for WMAS
  arp_data1_final <- arp_data[!(amb_service == "WMAS" & (call_level == "green1" | call_level == "green3"))]

  # save
  save(arp_data1_final, file = "data/arp_data1_final.Rda")
}


processARP2.1Data <- function() {

  load("data/auxillary_data2.1.Rda")
  load("data/arp_data2.1_raw.Rda")

  # make wide then long again so we can assess missingness
  arp_data_wide <- dcast(arp_data2.1,
                         amb_service + measure_code ~ week_beginning,
                         fill = NA,
                         drop = FALSE,
                         value.var = "value")

  arp_data <- melt(arp_data_wide, id.vars = c("amb_service", "measure_code"), variable.name = "week_beginning", value.name = "value", variable.factor = FALSE)

  # Merge in info about measures/services -----------------------------------

  arp_data <- merge(arp_data, measures_data2.1, by = "measure_code")
  arp_data <- merge(arp_data, service_data2.1, by = "amb_service")

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

  # save
  save(arp_data2.1_final, file = "data/arp_data2.1_final.Rda")
}

