# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting

rm(list = ls())
library(data.table)  # sorry for the inconvenience:)
library(readxl)
library(writexl)
library(dplyr)  # sorry, I am a dplyr girl;)
options(print.max = 300, scipen = 50, digits = 2)

source("01_RScript/00_Answer_A_Data.R")

#### Question b: Computing Output per worker

## 1: Starting with Level Comparison across TOT and MARKT ----

# EMP: Using number of employees  ----

png("03_Output/Exercise b/b_Japan_EMP_TOT_MARKET.png", width = 800, height = 600, res = 120)
dt[, y_pw := VA_Q / EMP]
dt_tot    <- dt[nace == "TOT"]
dt_market <- dt[nace == "MARKT"]
y_range <- range(c(dt_tot$y_pw, dt_market$y_pw), na.rm = TRUE) # Define common y-axis range
par(mar = c(5, 4, 6, 2))  # Increase top margin

# Plot TOTAL with full y-range
plot(dt_tot$year, dt_tot$y_pw, 
     type = "l", lwd = 2, lty = 1,
     ylim = y_range,
     # main = "Japan - Output per Worker",
     xlab = "Year", ylab = "Output per Worker")

# Add MARKET
lines(dt_market$year, dt_market$y_pw, lwd = 2, lty = 2)
par(xpd = TRUE) # Allow drawing in margin

legend("top",
       inset = c(0, -0.12),   # moves it slightly below the title
       legend = c("TOTAL", "MARKET"),
       lwd = 2,
       lty = c(1, 2),
       horiz = TRUE,
       bty = "n")


# mtext("Note: Output per efficient worker calculated using  total hours worked by person engaged.", side = 1, line = 4, cex = 0.8)

dev.off()

# H_EMP: Using hours worked by person engaged  ----

png("03_Output/Exercise b/b_Japan_H_EMP_TOT_MARKET.png", width = 800, height = 600, res = 120)
dt[, y_pw := VA_Q / H_EMP]
dt_tot    <- dt[nace == "TOT"]
dt_market <- dt[nace == "MARKT"]
y_range <- range(c(dt_tot$y_pw, dt_market$y_pw), na.rm = TRUE) # Define common y-axis range
par(mar = c(5, 4, 6, 2))  # Increase top margin

# Plot TOTAL with full y-range
plot(dt_tot$year, dt_tot$y_pw, 
     type = "l", lwd = 2, lty = 1,
     ylim = y_range,
    #  main = "Japan - Output per Hours Worked",
     xlab = "Year", ylab = "Output per Hours Worked")

# Add MARKET
lines(dt_market$year, dt_market$y_pw, lwd = 2, lty = 2)
par(xpd = TRUE) # Allow drawing in margin

legend("top",
       inset = c(0, -0.12),   # moves it slightly below the title
       legend = c("TOTAL", "MARKET"),
       lwd = 2,
       lty = c(1, 2),
       horiz = TRUE,
       bty = "n")


# mtext("Note: Output per efficient worker calculated using  total hours worked by person engaged.", side = 1, line = 4, cex = 0.8)

dev.off()


# H_EMPE: Using total hours worked by employees ----

png("03_Output/Exercise b/b_Japan_H_EMPE_TOT_MARKET.png", width = 800, height = 600, res = 120)
dt[, y_pw := VA_Q / H_EMPE]
dt_tot    <- dt[nace == "TOT"]
dt_market <- dt[nace == "MARKT"]
y_range <- range(c(dt_tot$y_pw, dt_market$y_pw), na.rm = TRUE) # Define common y-axis range
par(mar = c(5, 4, 6, 2))  # Increase top margin

# Plot TOTAL with full y-range
plot(dt_tot$year, dt_tot$y_pw, 
     type = "l", lwd = 2, lty = 1,
     ylim = y_range,
    # main = "Japan - Output per Hours Worked of Employees",
     xlab = "Year", ylab = "Output per Hours Worked")

# Add MARKET
lines(dt_market$year, dt_market$y_pw, lwd = 2, lty = 2)
par(xpd = TRUE) # Allow drawing in margin

legend("top",
       inset = c(0, -0.12),   # moves it slightly below the title
       legend = c("TOTAL", "MARKET"),
       lwd = 2,
       lty = c(1, 2),
       horiz = TRUE,
       bty = "n")


# mtext("Note: Output per efficient worker calculated using total hours worked by employees.", side = 1, line = 4, cex = 0.8)

dev.off()


## 2: Starting with Growth Rate across TOT and MARKT

######

# EMP: Using number of employees  ----

png("03_Output/Exercise b/b_Japan_EMP_GROWTH_TOT_MARKET.png",
    width = 800, height = 600, res = 120)

dt_growth <- copy(dt) # Copy to avoid modifying original
dif1 <- function(x) { x - shift(x) } # Define first difference
dt_growth[, y_pw := VA_Q / EMP] # Output per hour worked
dt_growth[, g_y_pw := dif1(log(y_pw)) * 100, by = nace] # Growth rate (log difference * 100)

# Split sectors
dt_tot    <- dt_growth[nace == "TOT"]
dt_market <- dt_growth[nace == "MARKT"]

y_range <- range(c(dt_tot$g_y_pw, dt_market$g_y_pw), na.rm = TRUE) # Common y-axis range

par(mar = c(5, 4, 6, 2))

# Plot TOTAL
plot(dt_tot$year, dt_tot$g_y_pw,
     type = "l", lwd = 2, lty = 1,
     ylim = y_range,
    # main = "Japan - Growth in Output per Worker",
     xlab = "Year", ylab = "Growth rate (%)")

# Add MARKET
lines(dt_market$year, dt_market$g_y_pw, lwd = 2, lty = 2)

par(xpd = TRUE)

legend("top",
       inset = c(0, -0.12),
       legend = c("TOTAL", "MARKET"),
       lwd = 2,
       lty = c(1, 2),
       horiz = TRUE,
       bty = "n")

dev.off()

# H_EMP: Using hours worked by person engaged  ----

png("03_Output/Exercise b/b_Japan_H_EMP_GROWTH_TOT_MARKET.png",
    width = 800, height = 600, res = 120)

dt_growth <- copy(dt) # Copy to avoid modifying original
dif1 <- function(x) { x - shift(x) } # Define first difference
dt_growth[, y_pw := VA_Q / H_EMP] # Output per hour worked
dt_growth[, g_y_pw := dif1(log(y_pw)) * 100, by = nace] # Growth rate (log difference * 100)

# Split sectors
dt_tot    <- dt_growth[nace == "TOT"]
dt_market <- dt_growth[nace == "MARKT"]

y_range <- range(c(dt_tot$g_y_pw, dt_market$g_y_pw), na.rm = TRUE) # Common y-axis range

par(mar = c(5, 4, 6, 2))

# Plot TOTAL
plot(dt_tot$year, dt_tot$g_y_pw,
     type = "l", lwd = 2, lty = 1,
     ylim = y_range,
    # main = "Japan - Growth in Output per Hours Worked",
     xlab = "Year", ylab = "Growth rate (%)")

# Add MARKET
lines(dt_market$year, dt_market$g_y_pw, lwd = 2, lty = 2)

par(xpd = TRUE)

legend("top",
       inset = c(0, -0.12),
       legend = c("TOTAL", "MARKET"),
       lwd = 2,
       lty = c(1, 2),
       horiz = TRUE,
       bty = "n")

dev.off()

# H_EMPE: Using total hours worked by employees ----

png("03_Output/Exercise b/b_Japan_H_EMPE_GROWTH_TOT_MARKET.png",
    width = 800, height = 600, res = 120)

dt_growth <- copy(dt) # Copy to avoid modifying original
dif1 <- function(x) { x - shift(x) } # Define first difference
dt_growth[, y_pw := VA_Q / H_EMPE] # Output per hour worked
dt_growth[, g_y_pw := dif1(log(y_pw)) * 100, by = nace] # Growth rate (log difference * 100)

# Split sectors
dt_tot    <- dt_growth[nace == "TOT"]
dt_market <- dt_growth[nace == "MARKT"]

y_range <- range(c(dt_tot$g_y_pw, dt_market$g_y_pw), na.rm = TRUE) # Common y-axis range

par(mar = c(5, 4, 6, 2))

# Plot TOTAL
plot(dt_tot$year, dt_tot$g_y_pw,
     type = "l", lwd = 2, lty = 1,
     ylim = y_range,
   #  main = "Japan - Growth in Output per Hours Worked of Employees",
     xlab = "Year", ylab = "Growth rate (%)")

# Add MARKET
lines(dt_market$year, dt_market$g_y_pw, lwd = 2, lty = 2)

par(xpd = TRUE)

legend("top",
       inset = c(0, -0.12),
       legend = c("TOTAL", "MARKET"),
       lwd = 2,
       lty = c(1, 2),
       horiz = TRUE,
       bty = "n")

dev.off()









