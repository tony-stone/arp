# Process raw ARP data

processARPData(phase = "1")
processARPData(phase = "2.1")
processARPData(phase = "2.2")
processARPData(phase = "_wholeservice")
processARPData(phase = "_red1")

filepath <- combineComparablePhaseData()
all.phase.data <- readRDS(filepath)
all_data_filepath <- saveDataForRJ(all.phase.data[std_measure_code %in% as.character(c(1:22, 24, 36)) &
                                     week_beginning >= as.Date("2016-01-04") &
                                     week_beginning <= as.Date("2016-12-26")],
                                   "std_measure_name", "ARP wholeservice data")
bla <- readRDS(all_data_filepath)


fname <- combineWSandPhaseData("2.2")
data2.2WS <- readRDS(fname)
saveDataForRJ(data2.2WS, "measure_name", "ARP data Phase 2.2 with wholeservice")
