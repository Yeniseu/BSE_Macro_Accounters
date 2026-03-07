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
growth_acc[, g_y     := dif1(log(VA_Q     / EMP ))*100, by = nace]
growth_acc[, g_KY    := dif1(log(Kq_GFCF  / VA_Q))*100, by = nace]
growth_acc[, g_h     := dif1(log(Kq_Train / EMP ))*100, by = nace]
growth_acc[, g_hcpwt := dif1(log(hc_pwt))*100] # Alternative Human capital from PWT
growth_acc[, h_lc    := LAB_QI / H_EMP]
growth_acc[, g_h_lc  := dif1(log(h_lc))*100, by = nace]
# Compute contributions
growth_acc[, capital_contr   := 0.5 * g_KY]
growth_acc[, TFP_contr       := g_y - capital_contr - g_h]
growth_acc[, TFP_contr_hcpwt := g_y - capital_contr - g_hcpwt]
growth_acc[, TFP_contr_hlc   := g_y - capital_contr - g_h_lc]


# Export
wanted_cols <- c("nace", "year", "g_y", "g_KY", "g_h", "g_hcpwt", "g_h_lc", "capital_contr", 
                 "TFP_contr", "TFP_contr_hcpwt", "TFP_contr_hlc")
growth_short <- growth_acc[nace=="TOT", ..wanted_cols]
#write_xlsx(growth_tot, "03_Output/Exercise c/c_Japan.xlsx")

# Plot ;)
# Compare the different indicators' line charts 
growth_tot <- growth_short[year > 1995, ]
growth_tot <- growth_tot[, .(year, g_y, capital_contr, TFP_contr_hcpwt, g_hcpwt, 
                             TFP_contr,  g_h, g_h_lc, TFP_contr_hlc)]
growth_tot <- melt(growth_tot, id.vars = "year")

growth_short
chart_compare1 <- growth_tot[variable %in% c("g_h", "g_hcpwt", "g_h_lc")]
caption <- "Different Human Capital Indices Growth Rates (Japan, %)"
ggplot(chart_compare1, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL, labels = c(
    "g_h" = "Training HC",
    "g_hcpwt" = "PWT HC",
    "g_h_lc" = "Labor Composition HC"
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Growth Rate (%)", color="") +
  #labs(title=caption, x="Year", y="Growth Rate (%)", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/chart_compare1.png", width=4, height=3)

chart_compare2 <- growth_tot[variable %in% c("g_hcpwt", "g_h_lc")]
caption <- "Different Human Capital Indices Growth Rates (Japan, %)"
ggplot(chart_compare2, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL, labels = c(
    "g_hcpwt" = "PWT HC",
    "g_h_lc" = "Labor Composition HC"
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Growth Rate (%)", color="") +
  #labs(title=caption, x="Year", y="Growth Rate (%)", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/chart_compare2.png", width=4, height=3)







#### Plot  Growth Accounting
### Growth Accounting 1: Train HC
chart_acc1 <- growth_tot[variable %in% c("g_y", "capital_contr", "TFP_contr", "g_h")]
caption <- "GDP Per Worker Growth and Contributions (Japan)"
ggplot(chart_acc1, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL,  guide = guide_legend(nrow = 2), labels = c(
    "g_y" = "Output Per Worker",
    "capital_contr" = "Capital Contr.",
    "g_h" = "Human Capital Contr.",
    "TFP_contr" = "TFP Contr."
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="%-Points") +
  #labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_bw() + 
  theme(legend.position = "top", panel.border = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Gr_Acc_Tr.png", width=4, height=3)



### Growth Accounting 2: PWT HC
chart_acc2 <- growth_tot[variable %in% c("g_y", "capital_contr", "TFP_contr_hcpwt", "g_hcpwt")]
caption <- "GDP Per Worker Growth and Contributions (Japan)"
ggplot(chart_acc2, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL,  guide = guide_legend(nrow = 2), labels = c(
      "g_y" = "Output Per Worker",
      "capital_contr" = "Capital Contr.",
      "TFP_contr_hcpwt" = "TFP Contr.",
      "g_hcpwt" = "Human Capital Contr."
    )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="%-Points") +
  #labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_bw() + 
  theme(legend.position = "top", panel.border = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Gr_Acc_PWT.png", width=4, height=3)


### Growth Accounting 3: Labor Composition HC

chart_acc3 <- growth_tot[variable %in% c("g_y", "capital_contr", "TFP_contr_hlc", "g_h_lc")]

# Reorder factor levels
desired_order <- c("g_y", "capital_contr", "TFP_contr_hlc", "g_h_lc")
chart_acc3[, variable := factor(variable, levels = desired_order)]

caption <- "GDP Per Worker Growth and Contributions (Japan)"
ggplot(chart_acc3, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL,  guide = guide_legend(nrow = 2), labels = c(
       "g_y" = "Output Per Worker",
       "capital_contr" = "Capital Contr.",
       "TFP_contr_hlc" = "TFP Contr.",
       "g_h_lc" = "Human Capital Contr."
     )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="%-Points") +
  #labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_bw() + 
  theme(legend.position = "top", panel.border = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Gr_Acc_LC.png", width=4, height=3)







#### Cumulative charts starting from 1995=100
chart_cum <- growth_short[, .(year, g_y, capital_contr, TFP_contr_hcpwt, g_hcpwt, 
                              TFP_contr,  g_h, g_h_lc, TFP_contr_hlc)]
col_names <- setdiff(names(chart_cum), "year")
chart_cum[, (col_names) := lapply(.SD, function(x) 1+x/100), .SDcols = col_names]
chart_cum[year == 1995, eval(col_names) := 100]
chart_cum[, (col_names) := lapply(.SD, function(x) cumprod(x)), .SDcols = col_names]
chart_cum <- melt(chart_cum, id.vars = "year")

### Growth Accounting 1: Train HC
chart_cum1 <- chart_cum[variable %in% c("g_y", "capital_contr", "TFP_contr", "g_h")]
caption <- "Output Per Worker Growth by Sector (Japan, 1995=100)"
ggplot(chart_cum1, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL,  guide = guide_legend(nrow = 2), labels = c(
    "g_y" = "Output Per Worker",
    "capital_contr" = "Capital Contr.",
    "TFP_contr" = "TFP Contr.",
    "g_h" = "Human Capital Contr."
  )) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Index Value", color="") +
  #labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Growth_Acc_Cumulative_Tr.png", width=4, height=3)

# Plot Cumulative 2 (with human capital from PWT)
chart_cum2 <- chart_cum[variable %in% c("g_y", "capital_contr", "TFP_contr_hcpwt", "g_hcpwt")]
caption <- "GDP Per Worker and Contributions (Japan, 1995=100)"
ggplot(chart_cum2, aes(x=year, y=value, color=variable, group=variable)) +
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
ggsave("03_Output/Exercise c/Growth_Acc_Cumulative_PWT.png", width=4, height=3)

# Plot Cumulative 3 (hlc, human capital labor compositiob)
chart_cum3 <- chart_cum[variable %in% c("g_y", "capital_contr", "TFP_contr_hlc", "g_h_lc")]

desired_order <- c("g_y", "capital_contr", "TFP_contr_hlc", "g_h_lc")
chart_cum3[, variable := factor(variable, levels = desired_order)]

caption <- "GDP Per Worker and Contributions (Japan, 1995=100)"
ggplot(chart_cum3, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H",  name = NULL,  guide = guide_legend(nrow = 2), labels = c(
    "g_y" = "Output Per Worker",
    "capital_contr" = "Capital Contr.",
    "TFP_contr_hlc" = "TFP Contr.",
    "g_h_lc" = "Human Capital Contr."
  )) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(x="Year", y="Index Value", color="") +
  #labs(title=caption, x="Year", y="Index", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Growth_Acc_Cumulative_LC.png", width=4, height=3)


