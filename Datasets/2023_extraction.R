library(readr)
library(haven)

# the raw data can be downloaded in either .ASC or .XPT (after unzipped) format
LLCP2023_data <- read_xpt("LLCP2023.XPT")
write.csv(LLCP2023_data,"2023-brfss-raw.csv") # saving it as a CSV file for convenience