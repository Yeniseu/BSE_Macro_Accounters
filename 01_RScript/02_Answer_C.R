# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting Q1 C

rm(list = ls())
library(data.table) # sorry for the inconvenience:)
library(readxl)
library(writexl)
library(dplyr) # sorry, I am a dplyr girl;)
library(kableExtra)
options(print.max = 300, scipen = 50, digits = 3)

#### Load data from previous script
dt         <- readRDS("02_Input/dt_Japan.rds")

#### Question C - Growth Accounting 
growth_acc <- copy(dt)
# Compute growth rates (first differences of logs)
dif1 <- function(x) {x-shift(x)}
growth_acc[, g_y     := dif1(log(VA_Q     / EMP ))*100, by = nace]
growth_acc[, g_KY    := dif1(log(Kq_GFCF  / VA_Q))*100, by = nace]
growth_acc[, g_h     := dif1(log(Kq_Train / EMP ))*100, by = nace]
growth_acc[, g_hcpwt := dif1(log(hc_pwt))*100] # Alternative Human capital from PWT

# Compute contributions
growth_acc[, capital_contrib   := 0.5 * g_KY]
growth_acc[, TFP_contrib       := g_y - capital_contrib - g_h]
growth_acc[, TFP_contrib_hcpwt := g_y - capital_contrib - g_hcpwt]

# Export
wanted_cols <- c("nace", "year", "g_y", "g_KY", "g_h", "g_hcpwt", "capital_contrib", "TFP_contrib", "TFP_contrib_hcpwt")
growth_tot <- growth_acc[nace=="TOT", ..wanted_cols]
write_xlsx(growth_tot, "03_Output/Exercise c/c_Japan.xlsx")


# Plot ;)
growth_tot <- growth_tot[year > 1995, ]
png("03_Output/Exercise c/c_Japan.png", width = 800, height = 600, res = 120) # opening png
plot(growth_tot$year, growth_tot$g_y, type = "l", col = "black", lwd = 2,
     ylim = range(c(growth_tot$g_y, growth_tot$capital_contrib, growth_tot$TFP_contrib, growth_tot$g_h), na.rm = TRUE),
     xlab = "Year", ylab = "Growth rate",
     main = "GDP per worker growth and contributions")
lines(growth_tot$year, growth_tot$capital_contrib, col = "purple", lwd = 2)
lines(growth_tot$year, growth_tot$TFP_contrib, col = "lightblue", lwd = 2)
lines(growth_tot$year, growth_tot$g_h, col = "pink", lwd = 2)
legend("topright", legend = c("Growth Output per Worker", "Capital contribution", "TFP contribution", "Human Capital Contribution"),
       col = c("black", "purple", "lightblue", "pink"), lwd = 2)
dev.off() # closing the png

# Plot Alternative (with human capital from PWT)
png("03_Output/Exercise c/c_Japan_hc_PWT.png", width = 800, height = 600, res = 120) # opening png
plot(growth_tot$year, growth_tot$g_y, type = "l", col = "black", lwd = 2,
     #ylim = range(c(growth_tot$g_y, growth_tot$capital_contrib, growth_tot$TFP_contrib, growth_tot$g_h), na.rm = TRUE),
     ylim = range(c(growth_tot$g_y, growth_tot$capital_contrib, growth_tot$TFP_contrib_hcpwt, growth_tot$g_hcpwt), na.rm = TRUE),
     xlab = "year", ylab = "Growth rate",
     main = "GDP per worker growth and contributions")
lines(growth_tot$year, growth_tot$capital_contrib, col = "purple", lwd = 2)
lines(growth_tot$year, growth_tot$TFP_contrib_hcpwt, col = "lightblue", lwd = 2)
lines(growth_tot$year, growth_tot$g_hcpwt, col = "pink", lwd = 2)
legend("bottomleft", legend = c("Growth Output per Worker", "Capital contribution", "TFP contribution", "Human Capital Contribution"),
       col = c("black", "purple", "lightblue", "pink"), lwd = 2)
dev.off() # closing the png




