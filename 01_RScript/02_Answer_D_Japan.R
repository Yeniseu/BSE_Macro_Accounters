# Authors: Ellie Walters, Ece Taşan Özel, Orhun Özel, Lea Röller
# Date   : 23/02/2026
# Scope  : Growth Accounting Q1 D,E

rm(list = ls())
library(data.table) # sorry for the inconvenience:)
library(readxl)
library(writexl)
library(dplyr) # sorry, I am a dplyr girl;)
options(print.max = 300, scipen = 50, digits = 2)


#### Load data from previous script
dt         <- readRDS("02_Input/dt_Japan.rds")
growth_acc <- readRDS("02_Input/growth_acc_Japan.rds")
NACE_codes <- readRDS("02_Input/NACE_codes.rds")

#### Select broad NACE sectors
dt[, unique(nace)]
NACE_wanted <- c("A", "B", "C", "D-E", "F", "G", "H", "I", "J", "K", "L", "M-N", "O-U")
# We don't have O-U, calculate it by summing or averaging sectors from O to U
dt_ou <- dt[nace %in% c("O-Q","RS","T", "U"), ]
dt_ou[, nace := "O-U"]
summable   <- setdiff(names(dt_ou), c("nace", "year", "hc_pwt"))
averageble <- c("hc_pwt")
tmp1 <- dt_ou[, lapply(.SD, sum, na.rm=T), by = c("year", "nace"), .SDcols=summable]
tmp2 <- dt_ou[, lapply(.SD, mean, na.rm=T), by = c("year", "nace"), .SDcols=averageble]
dt_ou <- cbind(tmp1, tmp2[, ..averageble])
# Merge O-U into dt and select the sectors wanted
dt <- rbind(dt, dt_ou)
dt <- dt[nace %in% NACE_wanted, ]


#### Output per workers
dt[]
