# Process raw ARP data
processARPData(phase = "1")
processARPData(phase = "2.1")
processARPData(phase = "2.2")
processARPData(phase = "_wholeservice")
processARPData(phase = "_red1")
processARPData(phase = "_standdown")
processARPData(phase = "_ruralurban")


# Look at stuff
look.at.what <- "_wholeservice"
examineProblematicData(phase = look.at.what)
examineProblematicData(phase = look.at.what, measuresCombined = FALSE)
saveProblematicDataImages(phase = look.at.what, startDate = as.Date("2016-07-01"))
saveProblematicDataImages(phase = look.at.what, measuresCombined = FALSE, startDate = as.Date("2016-07-01"))
examineData(phase = look.at.what, startDate = as.Date("2016-07-01"))



## Save data for RJ
# All whole service data combined
filepath <- combineComparablePhaseData()
wholeservice.data <- readRDS(filepath)[std_measure_code %in% as.character(c(1:22, 24, 36, 41)) &
                                         week_beginning >= as.Date("2016-01-04")]
saveDataForRJ(wholeservice.data, "std_measure_name", "ARP wholeservice data")

# Phase 1 data
fname <- combineWSandPhaseData("1") # combineWSandPhaseData formats the data
data1 <- readRDS(fname)[phase == "1"]
examineData(data = data1)
saveDataForRJ(data1, "measure_name", "ARP Phase 1 data")

# Phase 2.1 data
fname <- combineWSandPhaseData("2.2")
data2.1 <- readRDS(fname)[phase == "2.1"]
examineData(data = data2.1)
saveDataForRJ(data2.1, "measure_name", "ARP Phase 2.1 data")

# Phase 2.2 data
fname <- combineWSandPhaseData("2.2")
data2.2 <- readRDS(fname)[phase == "2.2"]
examineData(data = data2.2)
saveDataForRJ(data2.2, "measure_name", "ARP Phase 2.2 data")

# Combined Phase 2.1 & 2.2 data
fname <- combinePhase2Data()
data2 <- readRDS(fname)
# examineData(data = data2)
saveDataForRJ(data2, "measure_name", "ARP Phase 2 combined data")


# Combine Phase 1, 2.1, 2.2 data from YAS, SWAS, WMAS with stand down data
fname <- combineStandDownWithStdMeasures()
data_standdown <- readRDS(fname)
# examineData(data = data_standdown)
saveDataForRJ(data_standdown, "std_measure_name", "ARP stand-down data")


# Rural-Urban data
fname <- combineUrbanRuralWithStdMeasures()
data_urbanrural <- readRDS(fname)
examineData(data = data_urbanrural)
saveDataForRJ(data_urbanrural, "std_measure_name", "ARP urban-rural data")


## lookup for Richard
bla <- unique(data_urbanrural[, .(std_measure_name, measure_code, measure, sub_measure, measure_type, call_level)])
setorder(bla, std_measure_name)
write.csv(bla[, .(std_measure_name, measure_code, measure, sub_measure, measure_type, call_level)], file = "output_data/rural urban data measure lookup.csv", row.names = FALSE)


