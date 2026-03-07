# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting

rm(list = ls())
library(data.table)  # sorry for the inconvenience:)
library(readxl)
library(writexl)
library(dplyr)  # sorry, I am a dplyr girl;)
options(print.max = 300, scipen = 50, digits = 2)
library(ggplot2)
library(viridis)

source("01_RScript/00_Answer_A_Data.R")

#### Question b: Computing Output per worker

## 1: Starting with Level Comparison across TOT and MARKT ----

# EMP: Using number of employees  ----

dt[, y_pw := VA_Q / EMP]
dt_plot <- dt[nace %in% c("TOT", "MARKT")]
ggplot(dt_plot, aes(x = year, y = y_pw, color = nace, group = nace)) +
  geom_line(size = 1) +
  scale_color_viridis_d(
    option = "H",
    name = NULL,
    labels = c("TOT" = "TOTAL", "MARKT" = "MARKET")
  ) +
  labs(x = "Year", y = "Output per Worker") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),    # axis tick labels
    axis.title = element_text(size = 15),   # axis titles
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("03_Output/Exercise b/b_Japan_EMP_TOT_MARKET.png",width = 800, height = 600,units = "px",dpi = 120)


# H_EMP: Using hours worked by person engaged  ----

dt[, y_pw := VA_Q / H_EMP]
dt_plot <- dt[nace %in% c("TOT", "MARKT")]
ggplot(dt_plot, aes(x = year, y = y_pw, color = nace, group = nace)) +
  geom_line(size = 1) +
  scale_color_viridis_d(
    option = "H",
    name = NULL,
    labels = c("TOT" = "TOTAL", "MARKT" = "MARKET")
  ) +
  labs(x = "Year", y = "Output per Hours Worked") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),    # axis tick labels
    axis.title = element_text(size = 15),   # axis titles
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("03_Output/Exercise b/b_Japan_H_EMP_TOT_MARKET.png",width = 800, height = 600,units = "px",dpi = 120)


# H_EMPE: Using total hours worked by employees ----

dt[, y_pw := VA_Q / H_EMPE]
dt_plot <- dt[nace %in% c("TOT", "MARKT")]
ggplot(dt_plot, aes(x = year, y = y_pw, color = nace, group = nace)) +
  geom_line(size = 1) +
  scale_color_viridis_d(
    option = "H",
    name = NULL,
    labels = c("TOT" = "TOTAL", "MARKT" = "MARKET")
  ) +
  labs(x = "Year", y = "Output per Hours Worked") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),    # axis tick labels
    axis.title = element_text(size = 15),   # axis titles
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("03_Output/Exercise b/b_Japan_H_EMPE_TOT_MARKET.png",width = 800, height = 600,units = "px",dpi = 120)

## 2: Starting with Growth Rate across TOT and MARKT

######

# EMP: Using number of employees  ----

dt_growth <- dt
dif1 <- function(x) { x - shift(x) }
dt_growth[, y_pw := VA_Q / EMP]
dt_growth[, g_y_pw := dif1(log(y_pw)) * 100, by = nace]
dt_plot <- dt_growth[nace %in% c("TOT", "MARKT")]

p <- ggplot(dt_plot, aes(x = year, y = g_y_pw, color = nace, group = nace)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "black") +
  scale_color_viridis_d(
    option = "H", name = NULL,
    labels = c("TOT" = "TOTAL", "MARKT" = "MARKET")) +
  labs(x = "Year", y = "Growth rate (%)") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),    # axis tick labels
    axis.title = element_text(size = 15),   # axis titles
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("03_Output/Exercise b/b_Japan_EMP_GROWTH_TOT_MARKET.png",plot = p,width = 800,height = 600,units = "px",dpi = 120)

# H_EMP: Using hours worked by person engaged  ----

dt_growth <- dt
dif1 <- function(x) { x - shift(x) }
dt_growth[, y_pw := VA_Q / H_EMP]
dt_growth[, g_y_pw := dif1(log(y_pw)) * 100, by = nace]
dt_plot <- dt_growth[nace %in% c("TOT", "MARKT")]

p <- ggplot(dt_plot, aes(x = year, y = g_y_pw, color = nace, group = nace)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "black") +
  scale_color_viridis_d(
    option = "H", name = NULL,
    labels = c("TOT" = "TOTAL", "MARKT" = "MARKET")) +
  labs(x = "Year", y = "Growth rate (%)") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),    # axis tick labels
    axis.title = element_text(size = 15),   # axis titles
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("03_Output/Exercise b/b_Japan_H_EMP_GROWTH_TOT_MARKET.png",plot = p,width = 800,height = 600,units = "px",dpi = 120)


# H_EMPE: Using total hours worked by employees ----

dt_growth <- dt
dif1 <- function(x) { x - shift(x) }
dt_growth[, y_pw := VA_Q / H_EMPE]
dt_growth[, g_y_pw := dif1(log(y_pw)) * 100, by = nace]
dt_plot <- dt_growth[nace %in% c("TOT", "MARKT")]

p <- ggplot(dt_plot, aes(x = year, y = g_y_pw, color = nace, group = nace)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "black") +
  scale_color_viridis_d(
    option = "H", name = NULL,
    labels = c("TOT" = "TOTAL", "MARKT" = "MARKET")) +
  labs(x = "Year", y = "Growth rate (%)") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),    # axis tick labels
    axis.title = element_text(size = 15),   # axis titles
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("03_Output/Exercise b/b_Japan_H_EMPE_GROWTH_TOT_MARKET.png",plot = p,width = 800,height = 600,units = "px",dpi = 120)








