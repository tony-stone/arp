## RED1 stuff
red1 <- readRDS("data/arp_data_combined - 2017-02-17 16.49.57.Rds")[std_measure_code %in% as.character(c(22, 24, 27:28, 36, 42:47))]
red1[, measure_str := paste(measure_code, measure, measure_type, sub_measure, call_level, sep = "_")]

ggplot(red1[std_measure_code %in% c("22", "24", "36", "43", "42")], aes(x = week_beginning, y = value, colour = measure_str)) +
  geom_line() +
  ggplot2::scale_x_date(date_labels = "%e %b %Y", date_breaks = "4 weeks", expand = c(0, 2)) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0)) +
  facet_wrap(~ amb_service, ncol = 5)

# Make wide format
red1_wide <- dcast(red1, amb_service + week_beginning ~ measure_str, value.var = "value")
setnames(red1_wide, make.names(names(red1_wide), unique = TRUE))

saveRDS(red1_wide, file = paste0("output_data/arp_red1_calls ", getTimestamp(), ".Rds"))
