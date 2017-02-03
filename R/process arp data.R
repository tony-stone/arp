library(data.table)

# Once the raw data has been read in, see "data-raw/read arp1 data.R" and "data-raw/read arp2.1 data.R",
# process it:
processARPData(phase = 1)
processARPData(phase = 2.1)
processARPData(phase = 2.2)


# How does it look?
summ_data1 <- getARPSummaryData(phase = 1)
View(summ_data1)
summ_data2.1 <- getARPSummaryData(phase = 2.1)
View(summ_data2.1)
summ_data2.2 <- getARPSummaryData(phase = 2.2)
View(summ_data2.2)

# With all plots, below, I recommend viewing them full size on a separate monitor

# Well that was horrible, how does the missingness (by service/time) look graphically?
examineProblematicData(phase = 1, measuresCombined = TRUE)
saveProblematicDataImages(phase = 1, measuresCombined = TRUE)

examineProblematicData(phase = 1, measuresCombined = TRUE, startDate = as.Date("2014-10-06"), endDate = as.Date("2016-09-30"))
examineProblematicData(phase = 2.1, measuresCombined = TRUE)
missing_and_outlying <- examineProblematicData(phase = 2.2, measuresCombined = TRUE)

# We can do stuff with this is we want
View(missing_and_outlying)

# What's missing and what's "outlying"? (by measure/service/time)
examineProblematicData(phase = 1, measuresCombined = FALSE)
examineProblematicData(phase = 1, measuresCombined = FALSE, outliersAtSD = 3, startDate = as.Date("2014-10-06"), endDate = as.Date("2016-09-30"))
saveProblematicDataImages(phase = 1, measuresCombined = FALSE, outliersAtSD = 3, startDate = as.Date("2014-10-06"), endDate = as.Date("2016-09-30"))

examineProblematicData(phase = 2.1, measuresCombined = FALSE, outliersAtSD = 3)
examineProblematicData(phase = 2.2, measuresCombined = FALSE, outliersAtSD = 3)

# Now let's look measure by measure
examineData(phase = 1, outliers_SD = 3, startDate = as.Date("2014-10-06"), endDate = as.Date("2016-09-30"))
examineData(phase = 2.1, outliers_SD = 3)
examineData(phase = 2.2, outliers_SD = 3)

# Can (somewhat naively) also just look at the Call Start Triggers
examineCSTriggers(phase = 1, showSDLimits = TRUE, SDLimits = 3, startDate = as.Date("2015-08-31"), endDate = as.Date("2016-09-30"))
examineCSTriggers(phase = 2.1, showSDLimits = TRUE, SDLimits = 3)
examineCSTriggers(phase = 2.2, showSDLimits = TRUE, SDLimits = 3)

# That's enough to be getting on with, surely!
