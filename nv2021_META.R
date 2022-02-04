setwd("")
#install.packages(c("tidyverse", "psych", "meta", "metaviz"))
library(tidyverse)
library(psych)
library(meta)
library(metaviz)

rm(list = ls())

df <- read.csv("nv2021_btw.csv")

ma <- metagen(
        TE = g_s,
        seTE = g_sSE,
        data = df,
        method.tau = "ML"
        )
summary(ma)
funnel(ma)
forest(ma)

