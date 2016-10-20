cleanTimes <- function(t1) {
  t1_numeric <- as.double(t1) * 14400

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

getCallColPallete <- function() {
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


plotWeeklyVal <- function(data, measure_val, sub_measure_val = NA, measure_type_val = NA, call_level_vals = NA, show_call_level = FALSE, nSD = 3, showSDLines = TRUE, highlightOutliers = TRUE) {

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
  }

  sdn_lines <- plot_data[, .(mn = mean(value, na.rm = TRUE),
                             sdn = nSD * sd(value, na.rm = TRUE)),
                         by = .(call_level, amb_service, sub_measure, measure_type, col_level)]
  sdn_lines[, ':=' (sdn_low = mn - sdn,
                    sdn_high = mn + sdn)]

  plot_data <- merge(plot_data, sdn_lines[, .(call_level, amb_service, sub_measure, measure_type, col_level, mn, sdn)], by = c("call_level", "amb_service", "sub_measure", "measure_type", "col_level"))
  plot_data[, outlier := abs(value - mn) > sdn]


  if(show_call_level & length(unique(plot_data$sub_measure)) > 1) {
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week_beginning, y = value, colour = col_level, linetype = sub_measure))
  } else {
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week_beginning, y = value, colour = col_level))
  }

  if(show_call_level) plot <- plot + getCallColPallete()

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

  plot <- plot +
    ggplot2::ggtitle(paste0(measure_val, sub_measure_val, measure_type_val, " [", plot_measures, "]"))  +
    ggplot2::facet_wrap(~amb_service, ncol = 2, scales = "free_y") +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0))


  print(plot)

  return(plot)
}

plotWeeklyVal(arp_data_final, "Average allocations per incident", call_level_vals = c("red2", "green2"))


plotWeeklyVal(arp_data_final, "Time from call connect to", "resource allocation", "median", show_call_level = TRUE)

plotWeeklyVal(arp_data_final, "Hours lost at hospital")
plotWeeklyVal(arp_data_final, "Time to treat - Red incidents")

plotWeeklyVal(arp_data_final, "Staff hours (planned)")
