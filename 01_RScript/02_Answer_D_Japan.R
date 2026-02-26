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
# We don't have O-U, calculate it
dt_ou <- dt[nace %in% c("O-Q","RS","T", "U"), ]
dt_ou[, nace := "O-U"]
dt_ou[, sum(.SD), by = c("year", "nace")]


new_nace <- NACE_codes[, .(nace=nace_r2_code, nace_new=nace_r2_code)]
new_nace[grepl("^C", nace), nace_new:="C"]
new_nace[nace%in% c("D", "E"), nace_new :="D-E"]
new_nace[nace%in% c("O", "Q"), nace_new :="D-E"]


O-Q
R-S
M-N 
L68A