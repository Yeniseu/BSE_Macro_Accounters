# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting

rm(list = ls())
library(data.table)  # sorry for the inconvenience:)
library(readxl)
library(writexl)
library(dplyr)  # sorry, I am a dplyr girl;)
options(print.max = 300, scipen = 50, digits = 2)

#### a: Loading the data set ----
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


#### b: Computing Output per worker ----

### Aggregate Output per Worker - TOTAL ECONOMY
# Using number of employees
png("03_Output/Exercise b/b_Japan_EMP_TOT.png", width = 800, height = 600, res = 120) # opening png 
dt[, y_pw  := VA_Q / EMP]
plot(dt[nace == "TOT", year], dt[nace == "TOT", y_pw], type = "l", lwd = 2,
     main = paste0("Japan - Output per Efficient Worker (TOTAL)"),
     xlab = "", ylab = "")
mtext("Note: Output per efficient worker calculated using number of employees.", side = 1, line = 4, cex = 0.8)
dev.off() # closing the png

# Using hours worked by person engaged 
png("03_Output/Exercise b/b_Japan_H_EMP_TOT.png", width = 800, height = 600, res = 120) # opening png 
dt[, y_phw := VA_Q / H_EMP]
plot(dt[nace == "TOT", year], dt[nace == "TOT", y_phw], type = "l", lwd = 2,
     main = paste0("Japan - Output per Efficient Worker (TOTAL)"),
     xlab = "", ylab = "")
mtext("Note: Output per efficient worker calculated using  total hours worked by person engaged.", side = 1, line = 4, cex = 0.8)
dev.off() # closing the png

# Using total hours worked by employees
png("03_Output/Exercise b/b_Japan_H_EMPE_TOT.png", width = 800, height = 600, res = 120) # opening png
dt[, y_phw_e := VA_Q / H_EMPE]
plot(dt[nace == "TOT", year], dt[nace == "TOT", y_phw_e], type = "l", lwd = 2,
     main = paste0("Japan - Output per Efficient Worker (TOTAL)"),
     xlab = "", ylab = "")
mtext("Note: Output per efficient worker calculated using  total hours worked by employees.", side = 1, line = 4, cex = 0.8)
dev.off() # closing the png


### Aggregate Output per Worker - MARKET ECONOMY 
# Using number of employees
png("03_Output/Exercise b/b_Japan_EMP_MARKET.png", width = 800, height = 600, res = 120) # opening png 
dt[, y_pw  := VA_Q / EMP]
plot(dt[nace == "MARKT", year], dt[nace == "MARKT", y_pw], type = "l", lwd = 2,
     main = paste0("Japan - Output per Efficient Worker (MARKET)"),
     xlab = "", ylab = "")
mtext("Note: Output per efficient worker calculated using number of employees.", side = 1, line = 4, cex = 0.8)
dev.off() # closing the png

# Using hours worked by person engaged 
png("03_Output/Exercise b/b_Japan_H_EMP_MARKET.png", width = 800, height = 600, res = 120) # opening png 
dt[, y_phw := VA_Q / H_EMP]
plot(dt[nace == "MARKT", year], dt[nace == "MARKT", y_phw], type = "l", lwd = 2,
     main = paste0("Japan - Output per Efficient Worker (MARKET)"),
     xlab = "", ylab = "")
mtext("Note: Output per efficient worker calculated using  total hours worked by person engaged.", side = 1, line = 4, cex = 0.8)
dev.off() # closing the png

# Using total hours worked by employees
png("03_Output/Exercise b/b_Japan_H_EMPE_MARKET.png", width = 800, height = 600, res = 120) # opening png
dt[, y_phw_e := VA_Q / H_EMPE]
plot(dt[nace == "MARKT", year], dt[nace == "MARKT", y_phw_e], type = "l", lwd = 2,
     main = paste0("Japan - Output per Efficient Worker (MARKET)"),
     xlab = "", ylab = "")
mtext("Note: Output per efficient worker calculated using  total hours worked by employees.", side = 1, line = 4, cex = 0.8)
dev.off() # closing the png




#### c Growth Accounting ----
growth_acc <- data.table(
  NACE = dt$nace[-1],   # remove first row to match diff()
  Year = dt$year[-1]
)

# Compute growth rates (first differences of logs)
growth_acc[, g_y  := diff(log(dt$VA_Q     / dt$EMP ))]
growth_acc[, g_KY := diff(log(dt$Kq_GFCF  / dt$VA_Q))]
growth_acc[, g_h  := diff(log(dt$Kq_Train / dt$EMP ))]
growth_acc[, g_hcpwt := diff(log(dt$hc_pwt))] # Alternative Human capital from PWT

# Compute contributions
growth_acc[, capital_contrib   := (1/3)/(2/3) * g_KY]
growth_acc[, TFP_contrib       := g_y - capital_contrib - g_h]
growth_acc[, TFP_contrib_hcpwt := g_y - capital_contrib - g_hcpwt]


# Export
growth_tot <- growth_acc %>% filter (growth_acc$NACE == "TOT")
write_xlsx(growth_tot, path = "03_Output/Exercise c/c_Japan.xlsx")
saveRDS(growth_acc, "02_Input/growth_acc_Japan.rds")
saveRDS(dt[, -c("y_pw", "y_phw", "y_phw_e")], "02_Input/dt_Japan.rds")
saveRDS(NACE_codes, "02_Input/NACE_codes.rds")

# Plot ;)
growth_tot <- growth_tot %>%filter(Year > 1995)
png("03_Output/Exercise c/c_Japan.png", width = 800, height = 600, res = 120) # opening png
plot(growth_tot$Year, growth_tot$g_y, type = "l", col = "black", lwd = 2,
     ylim = range(c(growth_tot$g_y, growth_tot$capital_contrib, growth_tot$TFP_contrib, growth_tot$g_h), na.rm = TRUE),
     xlab = "Year", ylab = "Growth rate",
     main = "GDP per worker growth and contributions")
lines(growth_tot$Year, growth_tot$capital_contrib, col = "purple", lwd = 2)
lines(growth_tot$Year, growth_tot$TFP_contrib, col = "lightblue", lwd = 2)
lines(growth_tot$Year, growth_tot$g_h, col = "pink", lwd = 2)
legend("topright", legend = c("Growth Output per Worker", "Capital contribution", "TFP contribution", "Human Capital Contribution"),
       col = c("black", "purple", "lightblue", "pink"), lwd = 2)
dev.off() # closing the png

# Plot Alternative (with human capital from PWT)
plot(growth_tot$Year, growth_tot$g_y, type = "l", col = "black", lwd = 2,
     ylim = range(c(growth_tot$g_y, growth_tot$capital_contrib, growth_tot$TFP_contrib_hcpwt, growth_tot$g_hcpwt), na.rm = TRUE),
     xlab = "Year", ylab = "Growth rate",
     main = "GDP per worker growth and contributions")
lines(growth_tot$Year, growth_tot$capital_contrib, col = "purple", lwd = 2)
lines(growth_tot$Year, growth_tot$TFP_contrib_hcpwt, col = "lightblue", lwd = 2)
lines(growth_tot$Year, growth_tot$g_hcpwt, col = "pink", lwd = 2)
legend("bottomleft", legend = c("Growth Output per Worker", "Capital contribution", "TFP contribution", "Human Capital Contribution"),
       col = c("black", "purple", "lightblue", "pink"), lwd = 2)



