# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting Q1 EF

rm(list = ls())
library(data.table) # sorry for the inconvenience:)
library(readxl)
library(writexl)
library(dplyr) # sorry, I am a dplyr girl;)
library(kableExtra)
options(print.max = 300, scipen = 50, digits = 2)


#### Load data from previous script
dt         <- readRDS("02_Input/dt_sectors_JP.rds")
NACE_codes <- readRDS("02_Input/NACE_codes.rds")

#### Question E
#### Calculate Labor Share
# Alternative 1: Simple formula
dt[, L_share := COMP / VA_CP]
# Alternative 2: Using formula in the EU klems documentation
dt[, L_share := (COMP + (COMP / H_EMPE) * (H_EMP - H_EMPE)) / VA_CP] 
# If labor share is larger than 1, use simpler version, using only COMP
dt[, .N, by = L_share>1]
dt[L_share > 1, L_share := COMP / VA_CP]
dt[, alpha := 1 - mean(L_share, na.rm=T), by = "nace"]

### Look into all economy for sanity check
dt_all         <- readRDS("02_Input/dt_Japan.rds")
dt_all[, L_share := COMP / VA_CP]
dt_all[nace=="TOT"]  # Sanity Check
dt_all[, L_share := (COMP + (COMP / H_EMPE) * (H_EMP - H_EMPE)) / VA_CP] 
dt_all[nace=="TOT"]  # Sanity Check
dt_all[, alpha := 1 - mean(L_share, na.rm=T), by = "nace"]

#### Old sector codes, repeat

#### Output per workers and accounting
#dt[, y_pw  := VA_Q / EMP]
# Compute growth rates (first differences of logs)
dif1 <- function(x) {x-shift(x)}
dt[, g_y  := dif1(log(VA_Q     / EMP )), by = nace]
dt[, g_KY := dif1(log(Kq_GFCF  / VA_Q)), by = nace]
dt[, g_h  := dif1(log(Kq_Train / EMP )), by = nace]
dt[, g_hcpwt := dif1(log(hc_pwt)), by = nace] # if Kq_Train is unknown, input aggregate g_h
dt[is.nan(g_h), g_h := g_hcpwt]

# Compute contributions
dt[, KY_contrib   := (alpha)/(1-alpha) * g_KY]
dt[, TFP_contrib       := g_y - KY_contrib - g_h]


#### See Results
## Add sector importance, as share in the economy's output
dt[, VA_Q_share := VA_Q/sum(VA_Q, na.rm=T)*100, by=year]

## TFP Growth Ranking
table <- dt[, .(mean_tfp_y = mean(TFP_contrib, na.rm=T)*100, share=round(mean(VA_Q_share, na.rm=T),1)), by = nace][order(-mean_tfp_y)]
setnames(table, c("nace", "mean_tfp_y","share"), c("NACE Code", "TFP Growth (Mean)", "Output Share"))
caption <- "Average TFP Growth by Sector (Japan, 1995–2015)"
kbl(table, digits = 4, align ="c", caption = caption) |>
  kable_classic(full_width = F, lightable_options = c("striped", "hover")) |>
  row_spec(0, bold = T)


### Question F
## Aggregate from sectors
dt[, g_K := dif1(log(Kq_GFCF)), by = nace]
table <- dt[, .(g_y_aggreg  = weighted.mean(g_y        ,VA_Q_share, na.rm=T)*100,
                TFP_aggreg  = weighted.mean(TFP_contrib,VA_Q_share, na.rm=T)*100,
                g_KY_aggreg = weighted.mean(g_K        ,VA_Q_share, na.rm=T)*100), by=year]
table <- table[, lapply(.SD, round, 2)]
setnames(table, c("year","g_y_aggreg", "TFP_aggreg","g_KY_aggreg"),
                c("Year", "Income per Worker", "TFP Growth","Capital Growth"))
caption <- "Growth in Income per Worker, TFP and Capital Aggregated Using the Sectoral Data"
kbl(table[Year>1995,], digits = 4, align ="c", caption = caption) |>
  kable_classic(full_width = F, lightable_options = c("striped", "hover")) |>
  row_spec(0, bold = T)


## TOT data
dt_tot <- dt_all[nace == "TOT"]
dt_tot[, g_y  := dif1(log(VA_Q     / EMP )), by = nace]
dt_tot[, g_KY := dif1(log(Kq_GFCF  / VA_Q)), by = nace]
dt_tot[, g_h  := dif1(log(Kq_Train / EMP )), by = nace]
dt_tot[, g_K := dif1(log(Kq_GFCF)), by = nace]
#dt_tot[, g_hcpwt := dif1(log(hc_pwt)), by = nace] # if Kq_Train is unknown, input aggregate g_h
dt_tot[is.nan(g_h), g_h := g_hcpwt]
# Compute contributions
dt_tot[, KY_contrib   := (alpha)/(1-alpha) * g_KY]
dt_tot[, TFP_contrib  := g_y - KY_contrib - g_h]
table2 <- dt_tot[, .(g_y_aggreg  = g_y        *100,
                    TFP_aggreg  = TFP_contrib*100,
                    g_KY_aggreg = g_K        *100), by=year]
table2 <- table2[, lapply(.SD, round, 2)]
setnames(table2, c("year", "g_y_aggreg", "TFP_aggreg","g_KY_aggreg"),
                c("Year", "Income per Worker", "TFP Growth","Capital Growth"))
caption <- "Growth in Income per Worker, TFP and Capital From the Total Data"
kbl(table2[Year>1995,], digits = 4, align ="c", caption = caption) |>
  kable_classic(full_width = F, lightable_options = c("striped", "hover")) |>
  row_spec(0, bold = T)


cor(table$`Income per Worker`, table2$`Income per Worker`, use="complete.obs")
cor(table$`TFP Growth`       , table2$`TFP Growth`       , use="complete.obs")
cor(table$`Capital Growth`   , table2$`Capital Growth`   , use="complete.obs")

