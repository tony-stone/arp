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
  arp_data_weekly_missingness[, missing_pc := round(missing / N, 3)]

  plot <- ggplot2::ggplot(arp_data_weekly_missingness, ggplot2::aes(x = week_beginning, y = missing_pc)) +
    ggplot2::facet_wrap(~amb_service, ncol = 2, scales = "free_y") +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0))

  print(plot)

  meas_plots <- lapply(unique(arp_data$measure), function(meas) {
    plot <- ggplot2::ggplot(arp_data[measure == meas], ggplot2::aes(measure_code, value)) +
      ggplot2::ggtitle(meas) +
      ggplot2::facet_wrap(~amb_service, ncol = 1, scales = "free_y") +
      ggplot2::geom_jitter(width = 0.4)
  })

  print(meas_plots[[1]])

  # summary by measure and service (across all time)
  arp_data_info <-
    arp_data[, .(
      N = .N,
      min = round(min(value, na.rm = TRUE), 1),
      mean = round(mean(value, na.rm = TRUE), 1),
      median = round(median(value, na.rm = TRUE), 1),
      max = round(max(value, na.rm = TRUE), 1),
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
  arp_data_final <- arp_data[!(amb_service == "WMAS" & (call_level == "green1" | call_level == "green3"))]

  # save
  save(arp_data_final, file = "data/arp_data_final.Rda")
}

