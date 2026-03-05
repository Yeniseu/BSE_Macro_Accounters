# Compute Labour Composition (h) 

library(data.table)
library(readxl)

# 1 - LAB_QI / H_EMP

### Load & prepare EUKLEM Data
# H_EMP
inp_path    <- "02_Input/JP_national_accounts.xlsx"
sheet_names <- c("VA_Q", "VA_CP", "EMP", "COMP", "H_EMP", "H_EMPE")
dt <- data.table(NULL)
for (i in sheet_names) { # Read every sheet, rowbind them
  tmp <- read_xlsx(inp_path, sheet = i) |> as.data.table()
  dt  <- rbind(dt, tmp)
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

# LAB_QI Data
ga <- as.data.table(read_xlsx("02_Input/JP_growth_accounts_extended.xlsx",  sheet = "LAB_QI"))

# Melt the data table with relevant columns
NACE_codes <- unique(ga[, .(geo_name, nace_r2_code, nace_r2_name)])
ga[, c("geo_code", "geo_name", "nace_r2_name") := NULL]
ga <- melt(ga, id.vars = c("nace_r2_code", "var"))
ga <- dcast(ga, "nace_r2_code + variable ~ var")
setnames(ga, c("nace_r2_code", "variable"), c("nace", "year"))
setkey(ga, "nace", "year")

# Select years
ga[, year := as.numeric(as.character(year))]
ga <- ga[between(year, 1995, 2015)]

dt <- merge.data.table(dt, ga, by = c("nace", "year"), all = T)


dif1 <- function(x) {x-shift(x)}
#dt[, g_H     := dif1(log(LAB_QI))*100, by = nace]
#dt[, g_L     := dif1(log(H_EMP))*100, by = nace]
#dt[, g_h_dif := g_H - g_L]
dt[, h := LAB_QI / H_EMP]
dt[, g_h     := dif1(log(h))*100, by = nace]

head(dt, 20)

####


# 2 - CAPITAL

### Load & prepare EUKLEM Data
# Capital Accounts
inp_path    <- "02_Input/JP_capital_accounts.xlsx"
sheet_names <- c("Ip_IT", "Ip_CT", "Ip_Soft_DB", "Ip_TraEq", "Ip_OMach",
                 "Ip_OCon", "Ip_Rstruc", "Ip_Cult", "Ip_RD", "Ip_OIPP",
                 "Ip_GFCF", "K_IT", "K_CT", "K_Soft_DB", "K_TraEq", "K_OMach",
                 "K_OCon", "K_Rstruc", "K_Cult", "K_RD", "K_OIPP", "K_GFCF",
                 "Kq_IT", "Kq_CT", "Kq_Soft_DB", "Kq_TraEq", "Kq_OMach",
                 "Kq_OCon", "Kq_Rstruc", "Kq_Cult", "Kq_RD", "Kq_OIPP", 
                 "Kq_GFCF")
dt <- data.table(NULL)
for (i in sheet_names) { # Read every sheet, rowbind them
  tmp <- read_xlsx(inp_path, sheet = i) |> as.data.table()
  dt  <- rbind(dt, tmp)
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

colnames(dt)

# Compute Different Capital Stock Measures

var_list <- c("IT", "CT", "Soft_DB", "TraEq", "OMach", "OCon", "Rstruc", 
              "Cult", "RD", "OIPP", "GFCF")
nominal_list <- c("Kq_IT", "Kq_CT", "Kq_Soft_DB", "Kq_TraEq", "Kq_OMach",
                  "Kq_OCon", "Kq_Rstruc", "Kq_Cult", "Kq_RD", "Kq_OIPP")

# Sum nominal subitems and check if you found K_GFCF
dt[, K_GFCF_summed := rowSums(.SD), .SDcols = nominal_list]
dt[1:20, .(K_GFCF_summed,  K_GFCF)]
dt[, difference := (K_GFCF_summed - K_GFCF) / K_GFCF_summed]
mean(dt[, difference], na.rm = T)
dt[, K_GFCF_summed := NULL]
dt[, difference := NULL]

# Compute Different Capital Stock Measures

setnafill(dt, fill = 0, cols = setdiff(names(dt), "nace"))

dt[, K_Tang := K_GFCF - K_Soft_DB - K_RD - K_OIPP]
dt[, Kq_Tang := K_Tang / Ip_GFCF]

dt[, K_Tang := K_GFCF - K_Rstruc]
dt[, Kq_Tang := K_Tang / Ip_GFCF]



#TODO compute narrow definitions
#TODO divide them by the price indices. 
#TODO plug those in the c part. 



















