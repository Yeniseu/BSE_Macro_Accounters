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
growth_acc[, capital_contrib   := 0.5 * g_KY]
growth_acc[, TFP_contrib       := g_y - capital_contrib - g_h]
growth_acc[, TFP_contrib_hcpwt := g_y - capital_contrib - g_hcpwt]
growth_acc[, TFP_contrib_hlc   := g_y - capital_contrib - g_h_lc]


# Export
wanted_cols <- c("nace", "year", "g_y", "g_KY", "g_h", "g_hcpwt", "g_h_lc", "capital_contrib", 
                 "TFP_contrib", "TFP_contrib_hcpwt", "TFP_contrib_hlc")
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



### Cumulative charts starting from 1995=100
growth_tot
chart_cum <- growth_tot[, .(year, g_y, capital_contrib, TFP_contrib_hcpwt, g_hcpwt, 
                            TFP_contrib,  g_h, g_h_lc, TFP_contrib_hlc)]
chart_cum <- rbind(data.table(year = 1995), chart_cum, fill=T)
col_names <- setdiff(names(chart_cum), "year")
chart_cum[, (col_names) := lapply(.SD, function(x) 1+x/100), .SDcols = col_names]
chart_cum[year == 1995, eval(col_names) := 100]
chart_cum[, (col_names) := lapply(.SD, function(x) cumprod(x)), .SDcols = col_names]
chart_cum <- melt(chart_cum, id.vars = "year")

# Plot Cumulative 1
chart_cum1 <- chart_cum[variable %in% c("g_y", "capital_contrib", "TFP_contrib", "g_h")]
caption <- "Output per Worker Growth by Sector (Japan, 1995=100)"
ggplot(chart_cum1, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + scale_color_viridis_d(option = "H", labels = c(
    "g_y" = "Output per Worker",
    "capital_contrib" = "Capital Contribution",
    "TFP_contrib" = "TFP Contribution",
    "g_h" = "Human Capital Contribution"
  )) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(title=caption, x="Year", y="Output Per Worker Index") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Growth_Acc_Cumulative.png", width=7, height=4)

# Plot Cumulative 2 (with human capital from PWT)
chart_cum2 <- chart_cum[variable %in% c("g_y", "capital_contrib", "TFP_contrib_hcpwt", "g_hcpwt")]
caption <- "GDP per Worker and Contributions (Japan, 1995=100)"
ggplot(chart_cum2, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H", labels = c(
    "g_y" = "Output per Worker",
    "capital_contrib" = "Capital Contribution",
    "TFP_contrib_hcpwt" = "TFP Contribution",
    "g_hcpwt" = "Human Capital Contribution"
  )) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Growth_Acc_Cumulative_hcPWT.png", width=7, height=4)

# Plot Cumulative 3 (hlc, human capital labor compositiob)
chart_cum3 <- chart_cum[variable %in% c("g_y", "capital_contrib", "TFP_contrib_hlc", "g_h_lc")]
caption <- "GDP per Worker and Contributions (Japan, 1995=100)"
ggplot(chart_cum3, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line(size = 1) + 
  scale_color_viridis_d(option = "H", labels = c(
    "g_y" = "Output per Worker",
    "capital_contrib" = "Capital Contribution",
    "TFP_contrib_hcpwt" = "TFP Contribution",
    "g_hcpwt" = "Human Capital Contribution"
  )) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.5, color = "black") +
  labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("03_Output/Exercise c/Growth_Acc_Cumulative_hclc.png", width=7, height=4)




g_h_lc TFP_contrib_hlc

