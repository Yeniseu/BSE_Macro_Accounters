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
growth_acc <- readRDS("02_Input/growth_acc_Japan.rds")