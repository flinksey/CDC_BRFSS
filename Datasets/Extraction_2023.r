# will add the libraries used once I confirm -- code used is still messy

# the data from the CDC BRFSS database is not readily available in CSV format so I downloaded the SAS ZIP file and then converted it into CSV
LLCP2023_data <- read_xpt("LLCP2023.XPT")
write.csv(LLCP2023_data,"2023-brfss-subset-xpt.csv")
