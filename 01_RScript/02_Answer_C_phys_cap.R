# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting Q1 C

rm(list = ls())
library(data.table) # sorry for the inconvenience:)
library(readxl)
library(writexl)
library(dplyr) # sorry, I am a dplyr girl;)
library(kableExtra)
library(ggplot2)
library(viridis) 
options(print.max = 300, scipen = 50, digits = 4)

#### Load data from previous script
dt <- readRDS("02_Input/dt_Japan.rds")

#### Question C - Growth Accounting 
growth_acc <- copy(dt)
# Compute growth rates (first differences of logs)
dif1 <- function(x) {x-shift(x)}
growth_acc[, g_y            := dif1(log(VA_Q          / EMP ))*100, by = nace]
growth_acc[, g_KY           := dif1(log(Kq_GFCF       / VA_Q))*100, by = nace]
growth_acc[, g_KY_NRes      := dif1(log(Kq_GFCF_NRes  / VA_Q))*100, by = nace]
growth_acc[, g_KY_Tang_NRes := dif1(log(Kq_Tang_NRes  / VA_Q))*100, by = nace]
growth_acc[, g_h     := dif1(log(Kq_Train / EMP ))*100, by = nace]
growth_acc[, g_hcpwt := dif1(log(hc_pwt))*100] # Alternative Human capital from PWT
growth_acc[, h_lc    := LAB_QI / H_EMP]
growth_acc[, g_h_lc  := dif1(log(h_lc))*100, by = nace]
# Compute contributions (total capital stock, different h)
growth_acc[, capital_contr   := 0.5 * g_KY]
growth_acc[, TFP_contr       := g_y - capital_contr - g_h]
growth_acc[, TFP_contr_hcpwt := g_y - capital_contr - g_hcpwt]
growth_acc[, TFP_contr_hlc   := g_y - capital_contr - g_h_lc]
# Compute contributions (different capital stock, hlc)
growth_acc[, capital_contr_Nres       := 0.5 * g_KY_NRes]
growth_acc[, TFP_contr_NRes           := g_y - capital_contr_Nres - g_h_lc]
growth_acc[, capital_contr_Tang_Nres  := 0.5 * g_KY_Tang_NRes]
growth_acc[, TFP_contr_Tang_NRes      := g_y - capital_contr_Tang_Nres - g_h_lc]

# Export
wanted_cols <- c("nace", "year", "g_y", "g_KY", "g_h", "g_hcpwt", "g_h_lc",
                 "capital_contr", "TFP_contr", "TFP_contr_hcpwt", "TFP_contr_hlc",
                 "g_KY_NRes", "g_KY_Tang_NRes", "capital_contr_Nres", 
                 "TFP_contr_NRes", "capital_contr_Tang_Nres", "TFP_contr_Tang_NRes")
growth_short <- growth_acc[nace=="TOT", ..wanted_cols]
#write_xlsx(growth_tot, "03_Output/Exercise c/c_Japan.xlsx")

# Plot ;)
# Compare the different indicators' line charts 
growth_tot <- growth_short[year > 1995, ]
#growth_tot <- growth_tot[, .(year, g_y, capital_contr, TFP_contr_hcpwt, g_hcpwt, 
#                             TFP_contr,  g_h, g_h_lc, TFP_contr_hlc)]
growth_tot <- growth_tot[, nace := NULL]
growth_tot <- melt(growth_tot, id.vars = "year")

growth_short
chart_compare1 <- growth_tot[variable %in% c("g_KY", "g_KY_NRes", "g_KY_Tang_NRes")]
caption <- "Different Physical Capital Indices Growth Rates (Japan, %)"
ggplot(chart_compare1, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL, labels = c(
    "g_KY" = "Total K",
    "g_KY_NRes" = "Non-Res K",
    "g_KY_Tang_NRes" = "Non-Res Tangible K"
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Growth Rate (%)", color="") +
  #labs(title=caption, x="Year", y="Growth Rate (%)", color="") +
  theme_minimal() + 
  theme(legend.position = "top", 
        plot.title = element_text(hjust = 0.5, face = "bold",),
        legend.text  = element_text(size = 8)) 
ggsave("03_Output/Exercise c/Physical_capital/chart_compare1.png", width=4, height=3)


#### Plot  Growth Accounting
### Growth Accounting 1: Total K
chart_acc1 <- growth_tot[variable %in% c("g_y", "capital_contr", "TFP_contr_hlc", "g_h_lc")]
caption <- "GDP Per Worker Growth and Contributions (Japan)"
ggplot(chart_acc1, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_manual(name = NULL, guide = guide_legend(nrow = 2),
                     values = c("g_y" = "#000000", "capital_contr"="#800080", "TFP_contr_hlc"="#ADD8E6", "g_h_lc"="#FFC0CB"),
                     labels = c(
                       "g_y" = "Output Per Worker",
                       "capital_contr" = "Capital Contr. (Total K)",
                       "TFP_contr_hlc" = "TFP Contr.",
                       "g_h_lc" = "Human Capital Contr."
                     )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Points") +
  #labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Physical_capital/Gr_Acc_Total_K.png", width=4, height=3)


### Growth Accounting 2: Non-Res K
chart_acc2 <- growth_tot[variable %in% c("g_y", "capital_contr_Nres", "TFP_contr_NRes", "g_h_lc")]
caption <- "GDP Per Worker Growth and Contributions (Japan)"
ggplot(chart_acc2, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_manual(name = NULL, guide = guide_legend(nrow = 2),
                     values = c("g_y" = "#000000", "capital_contr_Nres"="#800080", "TFP_contr_NRes"="#ADD8E6", "g_h_lc"="#FFC0CB"),
                     labels = c(
                       "g_y" = "Output Per Worker",
                       "capital_contr_Nres" = "Capital Contr. (NRes K)",
                       "TFP_contr_NRes" = "TFP Contr.",
                       "g_h_lc" = "Human Capital Contr."
                     )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Points") +
  #labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Physical_capital/Gr_Acc_NRes_K.png", width=4, height=3)


### Growth Accounting 3: Non-Res Tangible K
chart_acc3 <- growth_tot[variable %in% c("g_y", "capital_contr_Tang_Nres", "TFP_contr_Tang_NRes", "g_h_lc")]
caption <- "GDP Per Worker Growth and Contributions (Japan)"
ggplot(chart_acc3, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_manual(name = NULL, guide = guide_legend(nrow = 2),
                     values = c("g_y" = "#000000", "capital_contr_Tang_Nres"="#800080", "TFP_contr_Tang_NRes"="#ADD8E6", "g_h_lc"="#FFC0CB"),
                     labels = c(
                       "g_y" = "Output Per Worker",
                       "capital_contr_Tang_Nres" = "Capital Contr. (NRes Tang K)",
                       "TFP_contr_Tang_NRes" = "TFP Contr.",
                       "g_h_lc" = "Human Capital Contr."
                     )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Points") +
  #labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Physical_capital/Gr_Acc_Tang_Nres.png", width=4, height=3)








#### Cumulative charts starting from 1995=100
chart_cum <- growth_short[, .(year, g_y, g_h_lc, TFP_contr_hlc, capital_contr,
                              capital_contr_Nres, capital_contr_Tang_Nres,
                              TFP_contr_NRes, TFP_contr_Tang_NRes
                              )]
col_names <- setdiff(names(chart_cum), "year")
chart_cum[, (col_names) := lapply(.SD, function(x) 1+x/100), .SDcols = col_names]
chart_cum[year == 1995, eval(col_names) := 100]
chart_cum[, (col_names) := lapply(.SD, function(x) cumprod(x)), .SDcols = col_names]
chart_cum <- melt(chart_cum, id.vars = "year")

### Growth Accounting 1: Total K
chart_cum1 <- chart_cum[variable %in% c("g_y", "capital_contr", "TFP_contr_hlc", "g_h_lc")]
caption <- "Output Per Worker Growth by Sector (Japan, 1995=100)"
ggplot(chart_cum1, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_manual(name = NULL,  guide = guide_legend(nrow = 2),
                     values = c("g_y" = "#2c003e", "capital_contr"="#00c896", "TFP_contr_hlc"="#f0a202", "g_h_lc"="#8b0000"),
                     labels = c(
                       "g_y" = "Output Per Worker",
                       "capital_contr" = "Capital Contr. (Total K)",
                       "TFP_contr_hlc" = "TFP Contr.",
                       "g_h_lc" = "Human Capital Contr."
                     )) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Index Value", color="") +
  #labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Physical_capital/Growth_Acc_Cum_Total_K.png", width=4, height=3)

# Plot Cumulative 2 : Non-Res K
chart_cum2 <- chart_cum[variable %in% c("g_y", "capital_contr_Nres", "TFP_contr_NRes", "g_h_lc")]
caption <- "GDP Per Worker and Contributions (Japan, 1995=100)"
ggplot(chart_cum2, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_manual(name = NULL,  guide = guide_legend(nrow = 2),
                     values = c("g_y" = "#2c003e", "capital_contr_Nres"="#00c896", "TFP_contr_NRes"="#f0a202", "g_h_lc"="#8b0000"),
                     labels = c(
                       "g_y" = "Output Per Worker",
                       "capital_contr_Nres" = "Capital Contr. (NRes K)",
                       "TFP_contr_NRes" = "TFP Contr.",
                       "g_h_lc" = "Human Capital Contr."
                     )) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Index Value", color="") +
  #labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Physical_capital/Growth_Acc_Cum_NRes_K.png", width=4, height=3)


# Plot Cumulative 3: Non-Res Tangible K
chart_cum3 <- chart_cum[variable %in% c("g_y", "capital_contr_Tang_Nres", "TFP_contr_Tang_NRes", "g_h_lc")]
caption <- "GDP Per Worker and Contributions (Japan, 1995=100)"
ggplot(chart_cum3, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_manual(name = NULL,  guide = guide_legend(nrow = 2),
                        values = c("g_y" = "#2c003e", "capital_contr_Tang_Nres"="#00c896", "TFP_contr_Tang_NRes"="#f0a202", "g_h_lc"="#8b0000"),
                        labels = c(
                          "g_y" = "Output Per Worker",
                          "capital_contr_Tang_Nres" = "Capital Contr. (NRes Tang K)",
                          "TFP_contr_Tang_NRes" = "TFP Contr.",
                          "g_h_lc" = "Human Capital Contr."
  )) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Index Value", color="") +
  #labs(title=caption, x="Year", y="Index", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Physical_capital/Growth_Acc_Cumu_NRes_Tang_K.png", width=4, height=3)

