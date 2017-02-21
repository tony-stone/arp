
library(data.table)

arp_data1 <- readRDS("data/arp_data1_final.Rds")

# create measure_fullname column
arp_data1[, measure_fullname := make.names(paste(measure_code, measure, sub_measure, call_level, sep = "_"))]

# save vector of correctly-ordered measure_fullnames
order <- unique(arp_data1[, .(measure_order, measure_fullname)][order(measure_order), measure_fullname])

# dcast data to wide format using measure_fullname column
arp_data1.wide <- dcast(arp_data1, amb_service + triage_system + week_beginning ~ measure_fullname, value.var = "value")


# re-order columns using order vector
setcolorder(arp_data1.wide, c("amb_service", "triage_system", "week_beginning", order))

# save data
saveRDS(arp_data1.wide, file = paste0("output_data/arp_1 ", format(Sys.time(), "%Y-%m-%d %H.%M.%S"), ".Rds"))
