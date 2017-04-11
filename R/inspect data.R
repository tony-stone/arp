library(data.table)

# How does it look?
summ_data1 <- getARPSummaryData(phase = 1)
View(summ_data1)
summ_data2.1 <- getARPSummaryData(phase = 2.1)
View(summ_data2.1)
summ_data2.2 <- getARPSummaryData(phase = 2.2)
View(summ_data2.2)
summ_data_wholeservice <- getARPSummaryData(phase = "_wholeservice")
View(summ_data_wholeservice)


# With all plots, below, I recommend viewing them full size on a separate monitor

# Historic Phase 1 data
examineProblematicData(phase = 1, measuresCombined = TRUE, startDate = as.Date("2014-10-06"), endDate = as.Date("2016-09-30"))
examineProblematicData(phase = 1, measuresCombined = FALSE, startDate = as.Date("2014-10-06"), endDate = as.Date("2016-09-30"))
examineData(phase = 1, startDate = as.Date("2014-10-06"), endDate = as.Date("2016-09-30"))
examineCSTriggers(phase = 1, showSDLimits = TRUE, startDate = as.Date("2015-08-31"), endDate = as.Date("2016-09-30"))

saveProblematicDataImages(phase = 1, measuresCombined = TRUE, startDate = as.Date("2014-10-06"), endDate = as.Date("2016-09-30"))
saveProblematicDataImages(phase = 1, measuresCombined = FALSE, startDate = as.Date("2014-10-06"), endDate = as.Date("2016-09-30"))



# Contemporary Phase 1 data
examineProblematicData(phase = 1, measuresCombined = TRUE, startDate = as.Date("2016-09-30"))
examineProblematicData(phase = 1, measuresCombined = FALSE, startDate = as.Date("2016-09-30"))
examineData(phase = 1, startDate = as.Date("2016-09-30"))
examineCSTriggers(phase = 1, showSDLimits = TRUE, startDate = as.Date("2016-09-30"))

saveProblematicDataImages(phase = 1, measuresCombined = TRUE, startDate = as.Date("2016-09-30"))
saveProblematicDataImages(phase = 1, measuresCombined = FALSE, startDate = as.Date("2016-09-30"))


# All Phase 1 data
examineProblematicData(phase = 1, measuresCombined = TRUE)
examineProblematicData(phase = 1, measuresCombined = FALSE)
examineData(phase = 1)
examineCSTriggers(phase = 1, showSDLimits = TRUE)

saveProblematicDataImages(phase = 1, measuresCombined = TRUE)
saveProblematicDataImages(phase = 1, measuresCombined = FALSE)


# Phase 2.1 data
examineProblematicData(phase = 2.1, measuresCombined = TRUE)
examineProblematicData(phase = 2.1, measuresCombined = FALSE)
examineData(phase = 2.1)
examineCSTriggers(phase = 2.1, showSDLimits = TRUE)


saveProblematicDataImages(phase = 2.1, measuresCombined = TRUE)
saveProblematicDataImages(phase = 2.1, measuresCombined = FALSE)


# Phase 2.2 data
examineProblematicData(phase = 2.2, measuresCombined = TRUE)
examineProblematicData(phase = 2.2, measuresCombined = FALSE, endDate = as.Date("2017-03-27"))
examineData(phase = 2.2)
examineCSTriggers(phase = 2.2, showSDLimits = TRUE)

saveProblematicDataImages(phase = 2.2, measuresCombined = TRUE)
saveProblematicDataImages(phase = 2.2, measuresCombined = FALSE, endDate = as.Date("2017-03-27"))
