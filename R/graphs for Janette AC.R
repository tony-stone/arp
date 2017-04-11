library(data.table)
library(openxlsx)
library(ggplot2)

### Hours lost graph

hours.lost <- readRDS("data/arp_data_combined - 2017-02-22 14.05.29.Rds")[std_measure_code %in% as.character(36) &
                                         week_beginning >= as.Date("2014-10-06") &
                                         week_beginning <= as.Date("2017-01-02")]

template <- data.table(expand.grid(amb_service = unique(hours.lost$amb_service), week_beginning = unique(hours.lost$week_beginning)))
hours.lost <- merge(template, hours.lost[, .(amb_service, week_beginning, value)], by = c("amb_service", "week_beginning"), all.x = TRUE)

hours.lost[, incomplete_week := any(is.na(value)), by = week_beginning]
amb_lost_hours <- hours.lost[incomplete_week == FALSE, .(value = sum(value)), by = .(amb_service, week_beginning)]
amb_lost_hours[, value := value / sum(value), by = week_beginning]

sum_totals <- hours.lost[incomplete_week == FALSE, .(totals = sum(value)), by = amb_service]
sum_totals[, normed := totals / sum(totals)]

hours.lost <- merge(hours.lost, sum_totals[, .(amb_service, normed)], by = "amb_service")
hours.lost[is.na(value), normed := 0]

sum.hours.lost <- hours.lost[, .(raw = sum(value, na.rm = TRUE), norms = sum(normed), missing_services = sum(is.na(value))), by = week_beginning]
sum.hours.lost[, adjusted := raw / norms]

# # (sum total 2016 hours lost for Janette)
# sum.hours.lost.2016 <- sum.hours.lost[week_beginning >= as.Date("2016-01-04") &  week_beginning <= as.Date("2016-12-26")]
# total.2016 <- sum(sum.hours.lost.2016[, raw])
# print(total.2016)

sum.hours.lost.long <- melt(sum.hours.lost, id.vars = c("week_beginning", "missing_services"), measure.vars = c("raw", "adjusted"), variable.name = "adjusted", variable.factor = FALSE)

hours.lost.plot <- ggplot(sum.hours.lost.long, aes(x = week_beginning, y = value, colour = adjusted)) +
  labs(title = "Hours lost at hospital - all English ambulance services", x = "week beginning", y = "hours") +
  geom_line(size = 1) +
  geom_point(data = sum.hours.lost.long[missing_services > 0 & adjusted == "raw"], colour = "black", aes(x = week_beginning, y = value, size = as.factor(missing_services))) +
  scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA), breaks = seq(0, 24000, 1200)) +
  scale_colour_manual(values = c(raw = "#33a02c", adjusted = "#1f78b4"), guide = guide_legend(title = "Adjustment for missing data", direction = "vertical", reverse = TRUE)) +
  scale_size_discrete(guide = guide_legend(title = "Number of services with missing data", direction = "vertical"), range = c(2, 3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0), legend.position  = "bottom")

print(hours.lost.plot)

ggsave("ARP hours lost at hospital plot.jpg", hours.lost.plot, path = "output_images", width = 30, height = 20, units = "cm")


# Adjusted hours lost data only, no blobs
sum.hours.lost.long.adjusted <- sum.hours.lost.long[adjusted == "adjusted"]

hours.lost.plot.adjusted <- ggplot(sum.hours.lost.long.adjusted, aes(x = week_beginning, y = value)) +
  labs(title = "Hours lost at hospital - all English ambulance services", x = "week beginning", y = "hours") +
  geom_line(size = 1) +
  scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA), breaks = seq(0, 24000, 1200)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0))

print(hours.lost.plot.adjusted)
ggsave("ARP hours lost at hospital adjusted plot.jpg", hours.lost.plot.adjusted, path = "output_images", width = 30, height = 20, units = "cm")


# Assess stability of services' contributions to total
ggplot(amb_lost_hours, aes(x = week_beginning, y = value, colour = amb_service)) +
  labs(title = "Hours lost at hospital", x = "week beginning", y = "service's contribution to total") +
  geom_line(size = 1) +
  scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
  scale_colour_discrete(guide = guide_legend(title = "Ambulance service", title.position = "top")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0), legend.position  = "bottom")



### Demand graphs

# Calls graph

calls <- readRDS("data/arp_data_combined - 2017-02-24 16.36.47.Rds")[std_measure_code %in% as.character(22) &
                                                                        week_beginning >= as.Date("2014-10-06") &
                                                                        week_beginning <= as.Date("2017-01-02")]



template <- data.table(expand.grid(amb_service = unique(calls$amb_service), week_beginning = unique(calls$week_beginning)))
calls <- merge(template, calls[, .(amb_service, week_beginning, value)], by = c("amb_service", "week_beginning"), all.x = TRUE)

calls[, incomplete_week := any(is.na(value)), by = week_beginning]
amb_calls <- calls[incomplete_week == FALSE, .(value = sum(value)), by = .(amb_service, week_beginning)]
amb_calls[, value := value / sum(value), by = week_beginning]

sum_totals <- calls[incomplete_week == FALSE, .(totals = sum(value)), by = amb_service]
sum_totals[, normed := totals / sum(totals)]

calls <- merge(calls, sum_totals[, .(amb_service, normed)], by = "amb_service")
calls[is.na(value), normed := 0]

sum.calls <- calls[, .(raw = sum(value, na.rm = TRUE), norms = sum(normed), missing_services = sum(is.na(value))), by = week_beginning]
sum.calls[, adjusted := raw / norms]

sum.calls.long <- melt(sum.calls, id.vars = c("week_beginning", "missing_services"), measure.vars = c("raw", "adjusted"), variable.name = "adjusted", variable.factor = FALSE)

calls.plot <- ggplot(sum.calls.long, aes(x = week_beginning, y = value, colour = adjusted)) +
  labs(title = "Total Calls Answered - all English ambulance services", x = "week beginning", y = "calls") +
  geom_line(size = 1) +
  geom_point(data = sum.calls.long[missing_services > 0 & adjusted == "raw"], colour = "black", aes(x = week_beginning, y = value, size = as.factor(missing_services))) +
  scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
  scale_y_continuous(labels = scales::comma, limits = c(100000, NA), breaks = seq(0, 230000, 10000)) +
  scale_colour_manual(values = c(raw = "#33a02c", adjusted = "#1f78b4"), guide = guide_legend(title = "Adjustment for missing data", direction = "vertical", reverse = TRUE)) +
  scale_size_discrete(guide = guide_legend(title = "Number of services with missing data", direction = "vertical"), range = c(2, 3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0), legend.position  = "bottom")

print(calls.plot)

ggsave("ARP calls plot.jpg", calls.plot, path = "output_images", width = 30, height = 20, units = "cm")


# Adjusted calls data only, no blobs
sum.calls.long.adjusted <- sum.calls.long[adjusted == "adjusted"]

calls.plot.adjusted <- ggplot(sum.calls.long.adjusted, aes(x = week_beginning, y = value)) +
  labs(title = "Total Calls Answered - all English ambulance services", x = "week beginning", y = "calls") +
  geom_line(size = 1) +
  scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
  scale_y_continuous(labels = scales::comma, limits = c(100000, NA), breaks = seq(0, 230000, 10000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0))

print(calls.plot.adjusted)
ggsave("ARP calls adjusted plot.jpg", calls.plot.adjusted, path = "output_images", width = 30, height = 20, units = "cm")



## Incidents graph

incidents <- readRDS("data/arp_data_combined - 2017-02-24 16.36.47.Rds")[std_measure_code %in% as.character(24) &
                                                                        week_beginning >= as.Date("2014-10-06") &
                                                                        week_beginning <= as.Date("2017-01-02")]


template <- data.table(expand.grid(amb_service = unique(incidents$amb_service), week_beginning = unique(incidents$week_beginning)))
incidents <- merge(template, incidents[, .(amb_service, week_beginning, value)], by = c("amb_service", "week_beginning"), all.x = TRUE)

incidents[, incomplete_week := any(is.na(value)), by = week_beginning]
amb_incidents <- incidents[incomplete_week == FALSE, .(value = sum(value)), by = .(amb_service, week_beginning)]
amb_incidents[, value := value / sum(value), by = week_beginning]

sum_totals <- incidents[incomplete_week == FALSE, .(totals = sum(value)), by = amb_service]
sum_totals[, normed := totals / sum(totals)]

incidents <- merge(incidents, sum_totals[, .(amb_service, normed)], by = "amb_service")
incidents[is.na(value), normed := 0]

sum.incidents <- incidents[, .(raw = sum(value, na.rm = TRUE), norms = sum(normed), missing_services = sum(is.na(value))), by = week_beginning]
sum.incidents[, adjusted := raw / norms]

sum.incidents.long <- melt(sum.incidents, id.vars = c("week_beginning", "missing_services"), measure.vars = c("raw", "adjusted"), variable.name = "adjusted", variable.factor = FALSE)
sum.incidents.long.adjusted <- sum.incidents.long[adjusted == "adjusted"]

incidents.plot <- ggplot(sum.incidents.long, aes(x = week_beginning, y = value, colour = adjusted)) +
  labs(title = "Total Incidents - all English ambulance services", x = "week beginning", y = "calls") +
  geom_line(size = 1) +
  geom_point(data = sum.incidents.long[missing_services > 0 & adjusted == "raw"], colour = "black", aes(x = week_beginning, y = value, size = as.factor(missing_services))) +
  scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
  scale_y_continuous(labels = scales::comma, limits = c(100000, NA), breaks = seq(0, 230000, 10000)) +
  scale_colour_manual(values = c(raw = "#33a02c", adjusted = "#1f78b4"), guide = guide_legend(title = "Adjustment for missing data", direction = "vertical", reverse = TRUE)) +
  scale_size_discrete(guide = guide_legend(title = "Number of services with missing data", direction = "vertical"), range = c(2, 3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0), legend.position  = "bottom")

print(incidents.plot)

ggsave("ARP incidents plot.jpg", incidents.plot, path = "output_images", width = 30, height = 20, units = "cm")


# Adjusted incidents data only, no blobs
sum.incidents.long.adjusted <- sum.incidents.long[adjusted == "adjusted"]

incidents.plot.adjusted <- ggplot(sum.incidents.long.adjusted, aes(x = week_beginning, y = value)) +
  labs(title = "Total Incidents - all English ambulance services", x = "week beginning", y = "calls") +
  geom_line(size = 1) +
  scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
  scale_y_continuous(labels = scales::comma, limits = c(100000, NA), breaks = seq(0, 230000, 10000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0))

print(incidents.plot.adjusted)
ggsave("ARP incidents adjusted plot.jpg", incidents.plot.adjusted, path = "output_images", width = 30, height = 20, units = "cm")



# Combined demand graph

demand <- readRDS("data/arp_data_combined - 2017-02-24 16.36.47.Rds")[(std_measure_code %in% as.character (22) |
                                                                         std_measure_code %in% as.character (24)) &
                                                                        week_beginning >= as.Date("2014-10-06") &
                                                                        week_beginning <= as.Date("2017-01-02")]

template <- data.table(expand.grid(amb_service = unique(demand$amb_service), week_beginning = unique(demand$week_beginning), measure = unique(demand$measure)))
demand <- merge(template, demand[, .(amb_service, week_beginning, measure, value)], by = c("amb_service", "week_beginning", "measure"), all.x = TRUE)

demand[, incomplete_week := any(is.na(value)), by = .(week_beginning, measure)]
amb_demand <- demand[incomplete_week == FALSE, .(value = sum(value)), by = .(amb_service, week_beginning, measure)]
amb_demand[, value := value / sum(value), by = .(week_beginning, measure)]

sum_totals <- demand[incomplete_week == FALSE, .(totals = sum(value)), by = .(amb_service, measure)]
sum_totals[, normed := totals / sum(totals), by = measure]

demand <- merge(demand, sum_totals[, .(amb_service, measure, normed)], by = c("amb_service", "measure"))
demand[is.na(value), normed := 0]

sum.demand <- demand[, .(raw = sum(value, na.rm = TRUE), norms = sum(normed), missing_services = sum(is.na(value))), by = .(week_beginning, measure)]
sum.demand[, adjusted := raw / norms, by = measure]

sum.demand.long <- melt(sum.demand, id.vars = c("week_beginning", "measure", "missing_services"), measure.vars = c("raw", "adjusted"), variable.name = "adjusted", variable.factor = FALSE)
sum.demand.long[, measure.adjusted := paste(measure, adjusted, sep = " - ")]


demand.plot <- ggplot(sum.demand.long, aes(group = as.integer(as.factor(measure.adjusted)) * -1, x = week_beginning, y = value, colour = measure.adjusted)) +
  labs(title = "Total Demand - all English ambulance services", x = "week beginning", y = "calls / incidents") +
  geom_line(size = 1) +
  geom_point(data = sum.demand.long[missing_services > 0 & adjusted == "raw"], colour = "black", aes(x = week_beginning, y = value, size = as.factor(missing_services))) +
  scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
  scale_y_continuous(labels = scales::comma, limits = c(100000, NA), breaks = seq(0, 230000, 10000)) +
  scale_colour_brewer(palette = "Paired", direction = -1, guide = guide_legend(title = "Adjustment for missing data", direction = "vertical", reverse = FALSE)) + #  scale_colour_manual(values = c(raw = "#33a02c", adjusted = "#1f78b4"), guide = guide_legend(title = "Adjustment for missing data", direction = "vertical", reverse = TRUE)) +
  scale_size_discrete(guide = guide_legend(title = "Number of services with missing data", direction = "vertical"), range = c(2, 3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0), legend.position  = "bottom")

print(demand.plot)

ggsave("ARP demand plot.jpg", demand.plot, path = "output_images", width = 30, height = 20, units = "cm")


# Adjusted combined demand data only, no blobs

demand.plot.adjusted <- ggplot(sum.demand.long[adjusted == "adjusted"], aes(group = as.integer(as.factor(measure.adjusted)) * -1, x = week_beginning, y = value, colour = measure)) +
  labs(title = "Total Demand - all English ambulance services", x = "week beginning", y = "calls / incidents") +
  geom_line(size = 1) +
  scale_x_date(date_labels = "%e %b %Y", date_breaks = "2 weeks", expand = c(0, 2)) +
  scale_y_continuous(labels = scales::comma, limits = c(100000, NA), breaks = seq(0, 230000, 10000)) +
  scale_colour_brewer(palette = "Dark2", direction = -1, guide = guide_legend(title = "Measure (adjusted for missing data)", direction = "vertical", reverse = FALSE)) + #  scale_colour_manual(values = c(raw = "#33a02c", adjusted = "#1f78b4"), guide = guide_legend(title = "Adjustment for missing data", direction = "vertical", reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0), legend.position  = "bottom")

print(demand.plot.adjusted)

ggsave("ARP demand adjusted plot.jpg", demand.plot.adjusted, path = "output_images", width = 30, height = 20, units = "cm")

