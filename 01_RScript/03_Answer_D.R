# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting Q1 D,E

rm(list = ls())
library(data.table) # sorry for the inconvenience:)
library(readxl)
library(writexl)
library(dplyr) # sorry, I am a dplyr girl;)
library(kableExtra)
library(ggplot2)
library(viridis)
options(print.max = 300, scipen = 50, digits = 3)


#### Load data from previous script
dt         <- readRDS("02_Input/dt_Japan.rds")
NACE_codes <- readRDS("02_Input/NACE_codes.rds")

#### Select broad NACE sectors
dt[, unique(nace)]
NACE_wanted <- c("A", "B", "C", "D-E", "F", "G", "H", "I", "J", "K", "L", "M-N", "O-U")
# We don't have O-U, calculate it by summing or averaging sectors from O to U
dt_ou <- dt[nace %in% c("O-Q","RS","T", "U"), ]
dt_ou[, nace := "O-U"]
summable   <- setdiff(names(dt_ou), c("nace", "year", "hc_pwt"))
averageble <- c("hc_pwt")
tmp1 <- dt_ou[, lapply(.SD, sum , na.rm=T), by = c("year", "nace"), .SDcols=summable]
tmp2 <- dt_ou[, lapply(.SD, mean, na.rm=T), by = c("year", "nace"), .SDcols=averageble]
dt_ou <- cbind(tmp1, tmp2[, ..averageble])

# D-E is empty, but we have D and E, sum them
dt_de <- dt[nace %in% c("D","E"), ]
dt_de[, nace := "D-E"]
tmp1 <- dt_de[, lapply(.SD, sum , na.rm=T), by = c("year", "nace"), .SDcols=summable]
tmp2 <- dt_de[, lapply(.SD, mean, na.rm=T), by = c("year", "nace"), .SDcols=averageble]
dt_de <- cbind(tmp1, tmp2[, ..averageble])

# Merge O-U and D-E into dt and select the sectors wanted
dt <- dt[nace != "D-E"]
dt <- rbind(dt, dt_ou, dt_de)
dt <- dt[nace %in% NACE_wanted, ]
setkey(dt, nace, year)

# Export
saveRDS(dt, "02_Input/dt_sectors_JP.rds")

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
dt[, KY_contrib   := (1/3)/(2/3) * g_KY]
dt[, TFP_contrib       := g_y - KY_contrib - g_h]


#### See Results
### Add sector importance, as share in the economy's output
dt[, VA_Q_share := VA_Q/sum(VA_Q, na.rm=T)*100, by=year]

### Tables
## Output per Worker Growth Ranking
table <- dt[, .(mean_g_y = mean(g_y, na.rm=T)*100, share=round(mean(VA_Q_share, na.rm=T),1)), by = nace][order(-mean_g_y)]
setnames(table, c("nace", "mean_g_y","share"), c("NACE Code", "Output Per Worker Growth (Mean)", "Output Share"))
caption <- "Average Output per Worker Growth by Sector (Japan, 1995–2015)"
kbl(table, digits = 4, align ="c", caption = caption) |>
  kable_classic(full_width = F, lightable_options = c("striped", "hover")) |>
  row_spec(0, bold = T)

## TFP Growth Ranking
table <- dt[, .(mean_tfp_y = mean(TFP_contrib, na.rm=T)*100, share=round(mean(VA_Q_share, na.rm=T),1)), by = nace][order(-mean_tfp_y)]
setnames(table, c("nace", "mean_tfp_y","share"), c("NACE Code", "TFP Growth (Mean)", "Output Share"))
caption <- "Average TFP Growth by Sector (Japan, 1995–2015)"
kbl(table, digits = 4, align ="c", caption = caption) |>
  kable_classic(full_width = F, lightable_options = c("striped", "hover")) |>
  row_spec(0, bold = T)


### Charts
## Output per Worker Growth Ranking
#Cumulative chart starting from 1995=100
chart1 <- dt[, .(year, nace, g_y=g_y*100)]
chart1[, g_y_index :=  1+g_y/100]
chart1[year==1995, g_y_index := 100]
chart1[, g_y_index := cumprod(g_y_index), by = nace]

caption <- "Output per Worker Growth by Sector (Japan, 1995=100)"
ggplot(chart1, aes(x=year, y=g_y_index, color=nace, group=nace)) +
  geom_line(size = 1) + scale_color_viridis_d(option = "H") +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(title=caption, x="Year", y="Output Per Worker Index", color="NACE") +
  theme_minimal() + 
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise d/y_chart_sectoral.png", width=10, height=6)


## TFP Growth Ranking
chart2 <- dt[, .(year, nace, TFP=TFP_contrib*100)]
chart2[, TFP_index :=  1+TFP/100]
chart2[year==1995, TFP_index := 100]
chart2[, TFP_index := cumprod(TFP_index), by = nace]

caption <- "TFP Growth by Sector (Japan, 1995=100)"
ggplot(chart2, aes(x=year, y=TFP_index, color=nace, group=nace)) +
  geom_line(size = 1) + scale_color_viridis_d(option = "H") +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(title=caption, x="Year", y="TFP Index", color="NACE") +
  theme_minimal() + 
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise d/tfp_chart_sectoral.png", width=10, height=6)


