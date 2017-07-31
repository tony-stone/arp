library(data.table)
library(ggplot2)

data <- readRDS("data/arp_data_ruralurban_final.Rds")
urban_rural_totals <- data[measure == "All 999 S&T/S&C Incidents: Total incidents", .(value = sum(value, na.rm = TRUE)), by = .(amb_service, sub_measure)]

write.table(urban_rural_totals, file = "clipboard", sep = "\t", row.names =FALSE)

ruc_plot <- ggplot(urban_rural_totals, aes(x = amb_service, y = value, fill = sub_measure)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "ambulance service", y = "incidents", title = "Number of S&T and S&C incidents by service and rural-urban classification") +
  scale_fill_grey(name = "rural-urban classification") +
  theme_bw()

ggsave("Number of SandT and SandC incidents by service and rural-urban classification.png", ruc_plot, path = "output_images", width = 30, height = 20, units = "cm")
