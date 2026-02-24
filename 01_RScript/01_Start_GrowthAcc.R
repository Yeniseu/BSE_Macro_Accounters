# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting


rm(list = ls())
library(data.table)
library(readxl)
options(print.max = 300, scipen = 50, digits = 3)

#### a
### Load & prepare

# Select Japan
inp_path    <- "02_Input/JP_national_accounts.xlsx"
sheet_names <- c("VA_Q", "VA_CP", "EMP", "COMP", "H_EMP", "H_EMPE")

dt <- data.table(NULL)
for (i in sheet_names) { # Read every sheet, rowbind them
  tmp <- read_xlsx(inp_path, sheet = i) |> as.data.table()
  dt <- rbind(dt, tmp)
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
dt <- dt[between(year,1995, 2015)]

#### b

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

png("03_Output/Exercise b/b_Japan_H_EMPE_TOT.png", width = 800, height = 600, res = 120) # opening png

# Using total hours worked by employees

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

png("03_Output/Exercise b/b_Japan_H_EMPE_MARKET.png", width = 800, height = 600, res = 120) # opening png

# Using total hours worked by employees

dt[, y_phw_e := VA_Q / H_EMPE]
plot(dt[nace == "MARKT", year], dt[nace == "MARKT", y_phw_e], type = "l", lwd = 2,
     main = paste0("Japan - Output per Efficient Worker (MARKET)"),
     xlab = "", ylab = "")
mtext("Note: Output per efficient worker calculated using  total hours worked by employees.", side = 1, line = 4, cex = 0.8)

dev.off() # closing the png





#### c

