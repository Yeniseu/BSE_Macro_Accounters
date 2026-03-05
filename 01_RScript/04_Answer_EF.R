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
dt[, h_lc    := LAB_QI / H_EMP]
dt[, g_h_lc  := dif1(log(h_lc)), by = nace]

# Compute contributions
dt[, KY_contrib   := (alpha)/(1-alpha) * g_KY]
dt[, TFP_contrib  := g_y - KY_contrib - g_h_lc]


#### See Results
## Add sector importance, as share in the economy's output
dt[, VA_Q_share := VA_Q/sum(VA_Q, na.rm=T)*100, by=year]
## Table alpha
table_alpha <- unique(dt[, .("NACE Code" = nace, "Labor Share" = 1-alpha, "Capital Share" = alpha)])[order(`Labor Share`)]
caption <- "Average Labor and Capital Shares by Sector (Japan, 1995–2015)"
kbl(table_alpha, digits = 2, align ="c", caption = caption) |>
  kable_classic(full_width = F, lightable_options = c("striped", "hover")) |>
  row_spec(0, bold = T)


## TFP Growth Ranking
table <- dt[, .(mean_tfp_y = mean(TFP_contrib, na.rm=T)*100, share=round(mean(VA_Q_share, na.rm=T),1)), by = nace][order(-mean_tfp_y)]
setnames(table, c("nace", "mean_tfp_y","share"), c("NACE Code", "TFP Growth", "Output Share"))
caption <- "Average TFP Growth by Sector (Japan, 1995–2015)"
kbl(table, digits = 1, align ="c", caption = caption) |>
  kable_classic(full_width = F, lightable_options = c("striped", "hover")) |>
  row_spec(0, bold = T)


## TFP Growth Ranking
chart2 <- dt[, .(year, nace, TFP=TFP_contrib*100)]
chart2[, TFP_index :=  1+TFP/100]
chart2[year==1995, TFP_index := 100]
chart2[, TFP_index := cumprod(TFP_index), by = nace]

caption <- "TFP Growth by Sector (Japan, 1995=100)"
ggplot(chart2, aes(x=year, y=TFP_index, color=nace, group=nace)) +
  geom_line(size = 1) + scale_color_viridis_d(option = "H") +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="TFP Index", color="NACE") +
  #labs(title=caption, x="Year", y="Output Per Worker Index", color="NACE") +
  theme_minimal() + 
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise e/tfp_chart_sect_alpha.png", width=8, height=5)



# Sector L - Plot Cumulative 
chart_cum <- dt[nace == "L"]
chart_cum <- chart_cum[, .(year, g_y, KY_contrib, g_h_lc, TFP_contrib)]
col_names <- setdiff(names(chart_cum), "year")
chart_cum[, (col_names) := lapply(.SD, function(x) 1+x/100), .SDcols = col_names]
chart_cum[year == 1995, eval(col_names) := 100]
chart_cum[, (col_names) := lapply(.SD, function(x) cumprod(x)), .SDcols = col_names]
chart_cum <- melt(chart_cum, id.vars = "year")

caption <- "GDP Per Worker and Contributions (Japan, 1995=100)"
ggplot(chart_cum, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL,  guide = guide_legend(nrow = 2), labels = c(
    "g_y" = "Output Per Worker",
    "capital_contr" = "Capital Contr.",
    "TFP_contr_hcpwt" = "TFP Contr.",
    "g_hcpwt" = "Human Capital Contr."
  )) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Index Value", color="") +
  #labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise e/Growth_Acc_Cumulative_PWT.png", width=4, height=3)




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

mean(table$`Income per Worker`, na.rm=T)
mean(table2$`Income per Worker`, na.rm=T)


IPW <- cbind(table[, .(Year, "Aggregated Sectoral" = `Income per Worker`)], table2[, .("Country Level" = `Income per Worker`)])
IPW <- melt(IPW, id.vars="Year")
caption <- "Income Per Worker Growth Rates (Japan)"
ggplot(IPW[Year>1995], aes(x=Year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Points") +
  #labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise f/Aggregated_IPW.png", width=4, height=3)


TFP <- cbind(table[, .(Year, "Aggregated Sectoral" = `TFP Growth`)], table2[, .("Country Level" = `TFP Growth`)])
TFP <- melt(TFP, id.vars="Year")
caption <- "TFP Growth Rates (Japan)"
ggplot(TFP[Year>1995], aes(x=Year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Points") +
  #labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise f/Aggregated_TFP.png", width=4, height=3)

K <- cbind(table[, .(Year, "Aggregated Sectoral" = `Capital Growth`)], table2[, .("Country Level" = `Capital Growth`)])
K <- melt(K, id.vars="Year")
caption <- "Capital Growth Rates (Japan)"
ggplot(K[Year>1995], aes(x=Year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Points") +
  #labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise f/Aggregated_K.png", width=4, height=3)



