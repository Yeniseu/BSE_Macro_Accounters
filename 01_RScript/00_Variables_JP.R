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


























# 2

library(data.table)
library(readxl)

path_la <- "02_Input/JP_labour_accounts.xlsx"

E <- as.data.table(read_excel(path_la, sheet = "Share_E"))
W <- as.data.table(read_excel(path_la, sheet = "Share_W"))

year_cols <- intersect(as.character(1995:2015), names(E))

# Fix types BEFORE melting
E[, (year_cols) := lapply(.SD, as.numeric), .SDcols = year_cols]
W[, (year_cols) := lapply(.SD, as.numeric), .SDcols = year_cols]

E_long <- melt(E,
               id.vars = c("country","code","education","age","gender"),
               measure.vars = year_cols,
               variable.name = "year",
               value.name = "share_E")

W_long <- melt(W,
               id.vars = c("country","code","education","age","gender"),
               measure.vars = year_cols,
               variable.name = "year",
               value.name = "share_W")


E_long[, year := as.numeric(as.character(year))]
W_long[, year := as.numeric(as.character(year))]

LW <- merge(E_long, W_long,
            by = c("country","code","education","age","gender","year"))

setorder(LW, code, education, age, gender, year)

# Δ ln share_E
LW[, dln_sE := log(share_E) - shift(log(share_E), 1L),
   by = .(code, education, age, gender)]

# Average wage share
LW[, vbar := 0.5 * (share_W + shift(share_W, 1L)),
   by = .(code, education, age, gender)]

# Labour composition growth
LC <- LW[, .(dln_LC = sum(vbar * dln_sE, na.rm=TRUE)),
         by = .(code, year)]

setorder(LC, code, year)

# Construct level index
LC[, ln_LC := cumsum(fifelse(is.na(dln_LC), 0, dln_LC)), by = code]
LC[, h := exp(ln_LC), by = code]

LC[year > 2007, ]
dt[year > 2007, ]





E <- as.data.table(read_excel(path_la, sheet = "Share_E"))
W <- as.data.table(read_excel(path_la, sheet = "Share_W"))

# Melt years
year_cols <- intersect(as.character(1995:2015), names(E))

E_long <- melt(E,
               id.vars = c("country","code","education","age","gender"),
               measure.vars = year_cols,
               variable.name = "year",
               value.name = "share_E")

W_long <- melt(W,
               id.vars = c("country","code","education","age","gender"),
               measure.vars = year_cols,
               variable.name = "year",
               value.name = "share_W")

E_long[, year := as.integer(year)]
W_long[, year := as.integer(year)]
















