library(data.table)
library(ggplot2)


combinePhasesData <- function() {
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
  #************************ FIX THIS CRAP **********************************************
  sort(unique(arp_all_red1_data[amb_service == "SWAS" & week_beginning == as.Date("2017-01-02"), std_measure_code]))

  arp_data_all <- arp_data_all[nchar(std_measure_code) < 3]

  arp_combined_data <- rbind(arp_data_all, summed_measures, arp_all_red1_data)

  # Rename standard measures - so they're standard
  setorder(arp_combined_data, phase)
  standardised_names <- arp_combined_data[!duplicated(arp_combined_data[, std_measure_code]), .(std_measure_code, measure_code, measure, sub_measure, measure_type)]
  arp_combined_data[, c("measure_code", "measure", "sub_measure", "measure_type") := NULL]
  arp_combined_data <- merge(arp_combined_data, standardised_names, by = "std_measure_code")

  arp_combined_data[is.na(sub_measure), sub_measure := ""]
  arp_combined_data[, std_measure_name := make.names(paste0("m", paste(measure_code, measure, sub_measure, measure_type, call_level, sep = "_")))]

  saveRDS(arp_combined_data, file = paste0("data/arp_data_combined - ", getTimestamp(), ".Rds"))
}


arp_data_all <- readRDS("data/arp_data_combined - 2017-02-21 17.58.36.Rds")
ggplot(arp_data_all[amb_service %in% c("SWAS", "YAS", "WMAS") & std_measure_code == "24"], aes(x = week_beginning, y = value, colour = amb_service)) +
  facet_wrap(~ std_measure_name, scale = "free", ncol = 3) +
  scale_x_date(date_labels = "%d %b %Y", date_breaks = "8 weeks") +
  scale_y_continuous(labels = scales::comma) +
  geom_line() +
  theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.4, hjust = 0), legend.position = "bottom")


ggplot(red1_wide, aes(x = week_beginning, y = value, colour = amb_service)) +
  geom_line()

# pull out wholeservice measures needed
wholeservice_measures <- arp_all_plus_summed[week_beginning >= as.Date("2016-01-01") &
                                               week_beginning <= as.Date("2016-12-31") &
                                               std_measure_code %in% c(1:21, 24, 36)]

# reorder/recode the measures
wholeservice_measures[, measure_order := measure_order + 2]
wholeservice_measures[std_measure_code == 24, measure_order := 1]
wholeservice_measures[std_measure_code == 36, measure_order := 2]
wholeservice_measures[measure_code == 17.1, measure_code := "hours_lost"]
wholeservice_measures[measure_code == "sum", measure_code := "incidents"]

# save COMBINED wholeservice data - don't overwrite original
saveRDS(wholeservice_measures, file = "data/arp_data_combined.Rds")

# examine wholeservice data
examineProblematicData(phase = "_wholeservice", measuresCombined = TRUE)
examineProblematicData(phase = "_wholeservice", measuresCombined = FALSE, endDate = as.Date("2017-01-01"))
#examineData(phase = "_wholeservice")


saveProblematicDataImages(phase = "_wholeservice", measuresCombined = TRUE)
saveProblematicDataImages(phase = "_wholeservice", measuresCombined = FALSE, endDate = as.Date("2017-01-01"))
q
