processARPData <- function(phase = 1) {
  if(!(as.character(phase) %in% as.character(c(1, 2.1, 2.2)))) stop("Invalid phase")

  if(phase == 1) {
    arp_data <- processARP1Data()
  } else if(phase == 2.1) {
    arp_data <- processARP2.1Data()
  } else if(phase == 2.2) {
    arp_data <- processARP2.2Data()
  }

  # remove trimmed mean data, not using
  arp_data_final <- arp_data[measure_type != "trimmed mean"]

  # save
  saveRDS(arp_data_final, file = paste0("data/arp_data", phase, "_final.Rds"))
  print(paste0("File saved: ", "data/arp_data", phase, "_final.Rds"))
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

  ## Remove G1 + G3 for SECAmb (but keep G3 for call totals)
  arp_data <- arp_data[!(amb_service == "SECAMB" & (call_level == "green1" | (call_level == "green3" & measure != "Total number of calls answered")))]

  ## Remove G1 + G3 for WMAS
  arp_data <- arp_data[!(amb_service == "WMAS" & (call_level == "green1" | call_level == "green3"))]

  ## Remove Hours lost at turnaround (whole hours)
  arp_data <- arp_data[!(measure_code == "17.2")]

  # Remove time periods
  ## Remove all data before 2014-10-06
  arp_data <- arp_data[!(week_beginning < as.Date("2014-10-06"))]

  ## SWAS / YAS Phase 2 began 2016-04-18
  arp_data <- arp_data[!((amb_service == "SWAS" | amb_service == "YAS") & week_beginning >= as.Date("2016-04-18"))]

  ## WMAS Phase 2 began 2016-06-06
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
  arp_data2.1_final <- arp_data[triage_system  != "AMPDS" | (triage_system  == "AMPDS" & measure_code != "8a")]

  ## Clearly YAS did not supply a 7day period of data for the first week (w/b: 2016-04-18). Remove
  arp_data2.1_final <- arp_data2.1_final[amb_service != "YAS" | week_beginning != as.Date("2016-04-18")]


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

  return(arp_data2.2_final)
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

getCallColPallete1 <- function() {
  rgb_colours <- list(all = c(0, 0, 0),
                      red1 = c(103,0,31),
                      red2 = c(178,24,43),
                      green1 = c(0,68,27),
                      green2 = c(27,120,55),
                      green3 = c(90,174,97),
                      green4 = c(166,219,160),
                      uncoded = rep(100, 3))

  pallete_cols <- substr(sapply(rgb_colours, function(col) {
    return(rgb(col[1], col[2], col[3], 255, maxColorValue = 255))
  }), 1, 7)

  return(ggplot2::scale_colour_manual(name = "call types", limits = names(pallete_cols), breaks = names(pallete_cols), drop = TRUE, values = pallete_cols))
}

getCallColPallete2.1 <- function() {
  rgb_colours <- list(all = c(0, 0, 0),
                      red = c(103,0,31),
                      amberResponse = c(230,85,13),
                      amberTransport = c(253,141,60),
                      amberFaceToFace = c(253,190,133),
                      greenFaceToFace = c(0,68,27),
                      greenTransport = c(27,120,55),
                      greenHearAndTreat = c(90,174,97),
                      uncoded = rep(100, 3))

  pallete_cols <- substr(sapply(rgb_colours, function(col) {
    return(rgb(col[1], col[2], col[3], 255, maxColorValue = 255))
  }), 1, 7)

  return(ggplot2::scale_colour_manual(name = "call types", limits = names(pallete_cols), breaks = names(pallete_cols), drop = TRUE, values = pallete_cols))
}


getCallColPallete2.2 <- function() {
  rgb_colours <- list(all = c(0, 0, 0),
                      category1 = c(112,48,160),
                      category2Response = c(103,0,31),
                      category2Transport = c(178,24,43),
                      category3Response = c(230,85,13),
                      category3Transport = c(253,190,133),
                      category4Transport = c(0,68,27),
                      category4HearAndTreat = c(90,174,97),
                      uncoded = rep(100, 3))

  pallete_cols <- substr(sapply(rgb_colours, function(col) {
    return(rgb(col[1], col[2], col[3], 255, maxColorValue = 255))
  }), 1, 7)

  return(ggplot2::scale_colour_manual(name = "call types", limits = names(pallete_cols), breaks = names(pallete_cols), drop = TRUE, values = pallete_cols))
}


getProblematicColourPallete <- function(nSD) {
  pallete_cols <- c("#F8766D", "#00BFC4")
  names(pallete_cols) <- c("missing", paste0("outlier: >", nSD, "SDs"))

  return(ggplot2::scale_fill_manual(values = pallete_cols))
}


plotWeeklyVals <- function(data, measure_val, sub_measure_val = NA, measure_type_val = NA, call_level_vals = NA, show_call_level = FALSE, call_level_version = "1", nSD = 3, showSDLines = TRUE, highlightOutliers = TRUE) {

  plot_data <- copy(data[measure %in% measure_val & !is.na(value)])

  if(!any(is.na(sub_measure_val))) {
    plot_data <- copy(plot_data[sub_measure %in% sub_measure_val])
    sub_measure_val <- paste0(" - ", paste0(sub_measure_val, collapse = "; "))
  } else {
    sub_measure_val <- ""
  }

  if(!any(is.na(measure_type_val))) {
    plot_data <- copy(plot_data[measure_type %in% measure_type_val])
    measure_type_val <- paste0(" - ", paste0(measure_type_val, collapse = "; "))
  } else {
    measure_type_val <- ""
  }

  if(!any(is.na(call_level_vals))) {
    plot_data <- copy(plot_data[call_level %in% call_level_vals])
    call_level_vals <- paste0(" - ", paste0(call_level_vals, collapse = "; "))

    if(missing(show_call_level)) show_call_level <- TRUE
  } else {
    call_level_vals <- ""
  }

  plot_measures <- paste0(sort(unique(plot_data$measure_code)), collapse = "; ")


  if(show_call_level) {
    col_level_col <- copy(plot_data[, .(call_level)])
  } else if(length(unique(plot_data$sub_measure)) > 1) {
    col_level_col <- copy(plot_data[, .(sub_measure)])
  } else if(length(unique(plot_data$measure_type)) > 1) {
    col_level_col <- copy(plot_data[, .(measure_type)])
  }

  if(exists("col_level_col")) {
    setnames(col_level_col, "col_level")
    plot_data <- cbind(plot_data, col_level_col)

    sdn_lines <- plot_data[, .(mn = mean(value, na.rm = TRUE),
                               sdn = nSD * sd(value, na.rm = TRUE)),
                           by = .(call_level, amb_service, sub_measure, measure_type, col_level)]
  } else {
    sdn_lines <- plot_data[, .(mn = mean(value, na.rm = TRUE),
                               sdn = nSD * sd(value, na.rm = TRUE)),
                           by = .(call_level, amb_service, sub_measure, measure_type)]
  }

  sdn_lines[, ':=' (sdn_low = mn - sdn,
                    sdn_high = mn + sdn)]

  plot_data <- merge(plot_data, sdn_lines[, .(call_level, amb_service, sub_measure, measure_type, mn, sdn)], by = c("call_level", "amb_service", "sub_measure", "measure_type"))
  plot_data[, outlier := abs(value - mn) > sdn]


  if(show_call_level & length(unique(plot_data$sub_measure)) > 1) {
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week_beginning, y = value, colour = col_level, linetype = sub_measure))
  } else if("col_level" %in% colnames(plot_data)) {
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week_beginning, y = value, colour = col_level))
  } else {
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week_beginning, y = value))
  }

  if(show_call_level) {
    if(call_level_version == "1") {
      plot <- plot + getCallColPallete1()
    } else if(call_level_version == "2.1") {
      plot <- plot + getCallColPallete2.1()
    } else if(call_level_version == "2.2") {
      plot <- plot + getCallColPallete2.2()
    }
  }

  if(showSDLines) {
  if("col_level" %in% colnames(plot_data)) {
      plot <- plot +
    ggplot2::geom_hline(data = sdn_lines, ggplot2::aes(yintercept = sdn_high, colour = col_level), linetype = "dotdash") +
    ggplot2::geom_hline(data = sdn_lines, ggplot2::aes(yintercept = sdn_low, colour = col_level), linetype = "dotdash")
    } else {
      plot <- plot +
        ggplot2::geom_hline(data = sdn_lines, ggplot2::aes(yintercept = sdn_high), colour = "#000000", linetype = "dotdash") +
        ggplot2::geom_hline(data = sdn_lines, ggplot2::aes(yintercept = sdn_low), colour = "#000000", linetype = "dotdash")
    }
  }

  if(highlightOutliers) {
    if("col_level" %in% colnames(plot_data)) {
      plot <- plot +
        ggplot2::geom_point(data = plot_data[outlier == TRUE], ggplot2::aes(x = week_beginning, y = value, colour = col_level), size = 4.5) +
        ggplot2::geom_point(data = plot_data[outlier == TRUE], ggplot2::aes(x = week_beginning, y = value), colour = "#ffffff", size = 4, show.legend = TRUE) +
        ggplot2::geom_point(data = plot_data[outlier == TRUE], ggplot2::aes(x = week_beginning, y = value, colour = col_level), size = 2, show.legend = TRUE)
    } else {
      plot <- plot +
        ggplot2::geom_point(data = plot_data[outlier == TRUE], ggplot2::aes(x = week_beginning, y = value), colour = "#000000", size = 4.5) +
        ggplot2::geom_point(data = plot_data[outlier == TRUE], ggplot2::aes(x = week_beginning, y = value), colour = "#ffffff", size = 4, show.legend = TRUE) +
        ggplot2::geom_point(data = plot_data[outlier == TRUE], ggplot2::aes(x = week_beginning, y = value), colour = "#000000", size = 2, show.legend = TRUE)
    }
  }

  title_text <- paste0(measure_val, sub_measure_val, measure_type_val, " [", plot_measures, "]")
  if(showSDLines == TRUE | highlightOutliers == TRUE) title_text <- paste0(title_text, "\n(outliers/limits shown at ", nSD, "SDs)")

  plot <- plot +
    ggplot2::ggtitle(title_text)  +
    ggplot2::facet_wrap(~amb_service, ncol = 2, scales = "free_y") +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0))


  return(plot)
}


plotWeeklyCSTriggers <- function(data, call_level_val, nSD = 3, showSDLines = TRUE) {

  plot_data <- copy(data[measure == "Clock start triggers" & call_level == call_level_val & !is.na(value)])

  plot_measures <- paste0(sort(unique(plot_data$measure_code)), collapse = "; ")

  sdn_lines <- plot_data[, .(mn = mean(value, na.rm = TRUE),
                              sdn = nSD * sd(value, na.rm = TRUE)),
                          by = .(amb_service, sub_measure)]

  sdn_lines[, ':=' (sdn_low = mn - sdn,
                    sdn_high = mn + sdn,
                    mn = NULL,
                    sdn = NULL)]

  plot_data <- merge(plot_data, sdn_lines, by = c("amb_service", "sub_measure"))

  setorder(plot_data, sub_measure)
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week_beginning, y = value, fill = sub_measure)) +
    ggplot2::geom_density(stat = "identity", position = "stack")

  title_text <- paste0("Clock start triggers - ", call_level_val, " [", plot_measures, "]")

  if(showSDLines) {
      plot <- plot +
        ggplot2::geom_line(data = plot_data, ggplot2::aes(x = week_beginning, y = sdn_high, linetype = sub_measure), position = "stack") +
        ggplot2::geom_line(data = plot_data, ggplot2::aes(x = week_beginning, y = sdn_low, linetype = sub_measure), position = "stack")

      title_text <- paste0(title_text, "\n([naive!] limits shown at ", nSD, "SDs)")
  }

  plot <- plot +
    ggplot2::ggtitle(title_text)  +
    ggplot2::facet_wrap(~amb_service, ncol = 2) +
    ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "1 week", expand = c(0, 2)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0))


  return(plot)
}



identifyOutlyingAndMissingData <- function(data, nSD = 3) {
  outlier_data <- copy(data)

  outlier_data[, ':=' (mn = mean(value, na.rm = TRUE),
                       sd = sd(value, na.rm = TRUE)),
               by = .(amb_service, measure_order)]

  outliers <- outlier_data[(abs(value - mn) / sd > nSD | is.na(value)), .(amb_service, week_beginning, measure_code, measure, sub_measure, measure_type, call_level, problem = paste0("outlier: >", nSD, "SDs"), value, measure_order)]
  outliers[is.na(value), problem := "missing"]

  start_dates <- outlier_data[!is.na(value), .(strt_date = min(week_beginning)), by = amb_service]

  outliers <- merge(outliers, start_dates, by = "amb_service")
  outliers <- outliers[week_beginning >= strt_date]

  setorder(outliers, amb_service, week_beginning, measure_order)

  outliers[, c("value", "strt_date")  := NULL]

  if(nrow(outliers) == 0) outliers <- NA

  return(outliers)
}


examineData <- function(phase = 1, outliers_SD = 3, startDate = -Inf, endDate = Inf) {

  if(!(phase %in% c(1, 2.1, 2.2))) stop("Invalid phase")

  if(phase == 1) {
    arp_data <- readRDS("data/arp_data1_final.Rds")
  } else if(phase == 2.1) {
    arp_data <- readRDS("data/arp_data2.1_final.Rds")
  }  else if(phase == 2.2) {
    arp_data <- readRDS("data/arp_data2.2_final.Rds")
  }

  arp_data <- arp_data[week_beginning >= startDate & week_beginning <= endDate]

  setorder(arp_data, measure_order)
  measures <- unique(arp_data[, .(measure, sub_measure, measure_type, call_level, measure_order)])

  measure_code_lookup <- arp_data[, .(position = min(measure_order)), by = measure_code]

  i <- 1
  while(i <= nrow(measures)) {
    if(nrow(arp_data[!is.na(value) & measure == measures[i, measure] & sub_measure == measures[i, sub_measure] & measure_type == measures[i, measure_type] & call_level == measures[i, call_level]]) == 0) {
      cat(paste("No data for:", paste(measures[i, .(measure, sub_measure, measure_type, call_level)], collapse = " - ")))
    } else {
      print(plotWeeklyVals(arp_data, measures[i, measure], measures[i, sub_measure], measures[i, measure_type], measures[i, call_level], show_call_level = TRUE, nSD = outliers_SD, call_level_version = as.character(phase)))
    }
    cmd <- readline(prompt="Press [enter] to continue; [b] to go back; [m(measure code)] to jump to measure; or [q] to quit. Alternatively, enter a number to proceed (+ve) or move back (-ve) by: ")
    if(!is.na(as.integer(cmd))) {
      i <- i + as.integer(cmd)
    } else if(tolower(cmd) == "q") {
      break
    } else if(tolower(cmd) == "b") {
      i <- i - 1
    } else if(tolower(substr(cmd, 1, 1)) == "m") {
      if(substr(cmd, 2, nchar(cmd)) %in% measure_code_lookup[, measure_code]) {
        i <- measure_code_lookup[measure_code == substr(cmd, 2, nchar(cmd)), position]
      } else {
        warning("Measure code not found.", immediate. = TRUE)
      }
    } else {
      i <- i + 1
    }
  }
}


examineProblematicData <- function(phase = 1, measuresCombined = TRUE, outliersAtSD = 3, startDate = -Inf, endDate = Inf) {
  if(!(phase %in% c(1, 2.1, 2.2))) stop("Invalid phase")

  if(phase == 1) {
    arp_data <- readRDS("data/arp_data1_final.Rds")
  } else if(phase == 2.1) {
    arp_data <- readRDS("data/arp_data2.1_final.Rds")
  } else if(phase == 2.2) {
    arp_data <- readRDS("data/arp_data2.2_final.Rds")
  }

  arp_data <- arp_data[week_beginning >= startDate & week_beginning <= endDate]

  outlying_or_missing <- identifyOutlyingAndMissingData(arp_data, outliersAtSD)

  if(measuresCombined) {
    # missingness by week and service (across all measures)
    arp_data_weekly_missingness <-
      arp_data[, .(N = .N, missing = sum(is.na(value))), by = .(amb_service, week_beginning)]
    arp_data_weekly_missingness[, missing_pc := round(missing / N, 3) * 100]

    plot <- ggplot2::ggplot(arp_data_weekly_missingness[missing_pc != 100], ggplot2::aes(x = week_beginning, y = amb_service, fill = missing_pc)) +
      ggplot2::labs(title = "Missing data by service and week", x = "week beginning", y = "ambulance service")  +
      ggplot2::geom_tile(colour = "white", size = 0.1) +
      ggplot2::coord_fixed(ratio = 7) +
      ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "1 week", expand = c(0, 0)) +
      ggplot2::scale_y_discrete(limits = sort(unique(arp_data_weekly_missingness[, amb_service]), decreasing = TRUE)) +
      ggplot2::scale_fill_continuous(low = "#ffffcc", high = "#800026", guide = ggplot2::guide_colourbar("Missing values (%)")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0), legend.position = "bottom", panel.grid = ggplot2::element_blank())

    print(plot)
  } else {

    services <- sort(unique(arp_data$amb_service))

    setorder(outlying_or_missing, -measure_order)
    measure_codes <- unique(outlying_or_missing[, measure_code])
    outlying_or_missing[, measure_order_factor := factor(measure_code, levels = measure_codes, ordered = TRUE)]

    outlying_or_missing[, measure_set := as.integer(measure_order > median(unique(measure_order))) + 1L, by = amb_service]

    i <- 1
    while(i <= length(services)) {
      if (outlying_or_missing[amb_service == services[i], .N] == 0) {
        warning(paste0(services[i], " has no problematic data."))
        i <- i + 1
        next
      }
      plot <- ggplot2::ggplot(outlying_or_missing[amb_service == services[i]], ggplot2::aes(x = week_beginning, y = measure_order_factor, fill = problem)) +
        ggplot2::labs(title = paste0(services[i], " problematic data by week and problem"), x = "week beginning", y = "measure code") +
        ggplot2::geom_tile() +
        ggplot2::facet_wrap(~measure_set, ncol = 2, scales = "free_y") +
        ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 0)) +
        ggplot2::scale_y_discrete() +
        getProblematicColourPallete(outliersAtSD) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0),
                       strip.background = ggplot2::element_blank(),
                       strip.text.x = ggplot2::element_blank())

      print(plot)

      cmd <- readline(prompt="Press [enter] to continue; [b] to go back; or [q] to quit. Alternatively, enter a number to proceed (+ve) or move back (-ve) by: ")

      if(!is.na(as.integer(cmd))) {
        i <- i + as.integer(cmd)
      } else if(tolower(cmd) == "q") {
        break
      } else if(tolower(cmd) == "b") {
        i <- i - 1
      } else {
        i <- i + 1
      }
    }

  }

  return(outlying_or_missing)
}


getARPSummaryData <- function(phase = 1, startDate = -Inf, endDate = Inf) {
  if(!(phase %in% c(1, 2.1, 2.2))) stop("Invalid phase")

  if(phase == 1) {
    arp_data <- readRDS("data/arp_data1_final.Rds")
  } else if(phase == 2.1) {
    arp_data <- readRDS("data/arp_data2.1_final.Rds")
  } else if(phase == 2.2) {
    arp_data <- readRDS("data/arp_data2.2_final.Rds")
  }

  arp_data <- arp_data[week_beginning >= startDate & week_beginning <= endDate]

  suppressWarnings(
    arp_data_info <-
      arp_data[, .(
        total_observations = .N,
        min = round(min(value, na.rm = TRUE), 1),
        mean = round(mean(value, na.rm = TRUE), 1),
        median = round(median(value, na.rm = TRUE), 1),
        max = round(max(value, na.rm = TRUE), 1),
        missing = sum(is.na(value))
      ), by = .(amb_service, measure_code)])
  arp_data_info[total_observations == missing, ':=' (min = NA, max = NA)]

  return(arp_data_info)
}


examineCSTriggers <- function(phase = 1, showSDLimits = TRUE, SDLimits = 3, startDate = -Inf, endDate = Inf) {
  if(!(phase %in% c(1, 2.1, 2.2))) stop("Invalid phase")

  if(phase == 1) {
    arp_data <- readRDS("data/arp_data1_final.Rds")
    relevant_measure_codes <- paste0("15.", letters[2:6])
  } else if(phase == 2.1) {
    arp_data <- readRDS("data/arp_data2.1_final.Rds")
    relevant_measure_codes <- paste0("15.", letters[2:8])
  } else if(phase == 2.2) {
    arp_data <- readRDS("data/arp_data2.2_final.Rds")
    relevant_measure_codes <- paste0("15.", letters[2:8])
  }

  arp_data <- arp_data[week_beginning >= startDate & week_beginning <= endDate]

  setorder(arp_data, measure_order)
  call_levels <- unique(arp_data[substr(measure_code, 1, 4) %in% relevant_measure_codes, call_level])

  i <- 1
  while(i <= length(call_levels)) {
    print(plotWeeklyCSTriggers(arp_data, call_levels[i], nSD = SDLimits, showSDLines = showSDLimits))

    cmd <- readline(prompt="Press [enter] to continue; [b] to go back; or [q] to quit. Alternatively, enter a number to proceed (+ve) or move back (-ve) by: ")

    if(!is.na(as.integer(cmd))) {
      i <- i + as.integer(cmd)
    } else if(tolower(cmd) == "q") {
      break
    } else if(tolower(cmd) == "b") {
      i <- i - 1
    } else {
      i <- i + 1
    }
  }
}



saveProblematicDataImages <- function(phase = 1, measuresCombined = TRUE, outliersAtSD = 3, startDate = -Inf, endDate = Inf) {
  if(!(phase %in% c(1, 2.1, 2.2))) stop("Invalid phase")

  if(phase == 1) {
    arp_data <- readRDS("data/arp_data1_final.Rds")
  } else if(phase == 2.1) {
    arp_data <- readRDS("data/arp_data2.1_final.Rds")
  } else if(phase == 2.2) {
    arp_data <- readRDS("data/arp_data2.2_final.Rds")
  }

  arp_data <- arp_data[week_beginning >= startDate & week_beginning <= endDate]

  outlying_or_missing <- identifyOutlyingAndMissingData(arp_data, outliersAtSD)

  if(measuresCombined) {

    # missingness by week and service (across all measures)
    arp_data_weekly_missingness <-
      arp_data[, .(N = .N, missing = sum(is.na(value))), by = .(amb_service, week_beginning)]
    arp_data_weekly_missingness[, missing_pc := round(missing / N, 3) * 100]

    plot <- ggplot2::ggplot(arp_data_weekly_missingness[missing_pc != 100], ggplot2::aes(x = week_beginning, y = amb_service, fill = missing_pc)) +
      ggplot2::labs(title = "Missing data by service and week", x = "week beginning", y = "ambulance service")  +
      ggplot2::geom_tile(colour = "white", size = 0.1) +
      ggplot2::coord_fixed(ratio = 7) +
      ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "1 week", expand = c(0, 0)) +
      ggplot2::scale_y_discrete(limits = sort(unique(arp_data_weekly_missingness[, amb_service]), decreasing = TRUE)) +
      ggplot2::scale_fill_continuous(low = "#ffffcc", high = "#800026", guide = ggplot2::guide_colourbar("Missing values (%)")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0), legend.position = "bottom", panel.grid = ggplot2::element_blank())

    ggplot2::ggsave(paste0("ARP Phase", phase, " - problematic data - all measures, services combined - ", format(Sys.time(), "%Y-%m-%d %H.%M.%S"), ".png"),
                    plot,
                    path = paste0("output_images/phase", phase, "/"),
                    width = 30, height = 20, units = "cm")

  } else {

    services <- sort(unique(arp_data$amb_service))

    setorder(outlying_or_missing, -measure_order)
    measure_codes <- unique(outlying_or_missing[, measure_code])
    outlying_or_missing[, measure_order_factor := factor(measure_code, levels = measure_codes, ordered = TRUE)]

    outlying_or_missing[, measure_set := as.integer(measure_order > median(unique(measure_order))) + 1L, by = amb_service]

    invisible(lapply(services, function(service, data, nSD) {
      if (data[amb_service == service, .N] == 0) {
        print(paste0(service, " has no problematic data."))
        i <- i + 1
      } else {
      plot <- ggplot2::ggplot(data[amb_service == service], ggplot2::aes(x = week_beginning, y = measure_order_factor, fill = problem)) +
        ggplot2::labs(title = paste0(service, " problematic data by week and problem"), x = "week beginning", y = "measure code") +
        ggplot2::geom_tile() +
        ggplot2::facet_wrap(~measure_set, ncol = 2, scales = "free_y") +
        ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 0)) +
        ggplot2::scale_y_discrete() +
        getProblematicColourPallete(nSD) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0),
                       strip.background = ggplot2::element_blank(),
                       strip.text.x = ggplot2::element_blank())

      ggplot2::ggsave(paste0("ARP Phase", phase, " - problematic data - all measures, ", service," - ", format(Sys.time(), "%Y-%m-%d %H.%M.%S"), ".png"),
                      plot,
                      path = paste0("output_images/phase", phase, "/"),
                      width = 30, height = 20, units = "cm")

      print(paste0("Saved image: ", service))
      }
    }, data = outlying_or_missing, nSD = outliersAtSD))

  }
}
