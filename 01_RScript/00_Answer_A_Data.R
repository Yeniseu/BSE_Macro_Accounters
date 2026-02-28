# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting

rm(list = ls())
library(data.table)  # sorry for the inconvenience:)
library(readxl)
library(writexl)
library(dplyr)  # sorry, I am a dplyr girl;)
options(print.max = 300, scipen = 50, digits = 2)

#### Question A: Loading the data set
### Load & prepare EUKLEM Data
# Select Japan
inp_path    <- "02_Input/JP_national_accounts.xlsx"
sheet_names <- c("VA_Q", "VA_CP", "EMP", "COMP", "H_EMP", "H_EMPE")
dt <- data.table(NULL)
for (i in sheet_names) { # Read every sheet, rowbind them
  tmp <- read_xlsx(inp_path, sheet = i) |> as.data.table()
  dt  <- rbind(dt, tmp)
}

# Adding Human Capital Variables
inp_path<- "02_Input/JP_intangible_analytical.xlsx"
sheet_names <- c("K_Train", "Kq_Train")
for (i in sheet_names) {
  tmp <- read_xlsx(inp_path, sheet = i) |> as.data.table()
  dt  <- rbind(dt, tmp, fill = TRUE)  # fill = TRUE allows for different columns
}

# Adding physical capital 
inp_path<- "02_Input/JP_capital_accounts.xlsx"
sheet_names <- c("K_GFCF", "Kq_GFCF")
for (i in sheet_names) {
  tmp <- read_xlsx(inp_path, sheet = i) |> as.data.table()
  dt  <- rbind(dt, tmp, fill = TRUE)  # fill = TRUE allows for different columns
}

# Melt the data table with relevant columns
NACE_codes <- unique(dt[, .(geo_name, nace_r2_code, nace_r2_name)])
dt[, c("geo_code", "geo_name", "nace_r2_name") := NULL]
dt <- melt(dt, id.vars = c("nace_r2_code", "var"))
dt <- dcast(dt, "nace_r2_code + variable ~ var")
setnames(dt, c("nace_r2_code", "variable"), c("nace", "year"))
setkey(dt, "nace", "year")



# LAB_QI Data
ga <- as.data.table(read_xlsx("02_Input/JP_growth_accounts_extended.xlsx",  sheet = "LAB_QI"))

# Melt the data table with relevant columns
NACE_codes <- unique(ga[, .(geo_name, nace_r2_code, nace_r2_name)])
ga[, c("geo_code", "geo_name", "nace_r2_name") := NULL]
ga <- melt(ga, id.vars = c("nace_r2_code", "var"))
ga <- dcast(ga, "nace_r2_code + variable ~ var")
setnames(ga, c("nace_r2_code", "variable"), c("nace", "year"))
setkey(ga, "nace", "year")
dt <- merge.data.table(dt, ga, by = c("nace", "year"), all = T)

# Select years
dt[, year := as.numeric(as.character(year))]
dt <- dt[between(year, 1995, 2015)]

### Load pwt data for human capital
pwt_all <- read_xlsx("02_Input/pwt110.xlsx", sheet = "Data") |> as.data.table()
pwt_all <- pwt_all[between(year, 1995, 2015)]
pwt     <- pwt_all[country=="Japan", .(year, hc)][]
# Merge into dt
dt <- merge(dt, pwt, by="year", all.x=T)
setnames(dt, "hc", "hc_pwt")
setkey(dt, "nace", "year")

# Export
saveRDS(dt, "02_Input/dt_Japan.rds")
saveRDS(NACE_codes, "02_Input/NACE_codes.rds")








