# Unified Trade Up Analysis

library(readxl)
library(tidyverse)
library(lattice)

CBTU <-
  read_excel("CBTUR - Roll Out Year Q1 PtC Data.xlsx",
    sheet = "Clean Data",
    skip = 0
  )

for (i in c("Baseline Q", "Q")) {
  for (j in 1:4) {
    CBTU[paste0("Trading Ratio ", i, j)] <- 100 * CBTU[paste0("Trading Income ", i, j)] /
      (CBTU[paste0("Trading Income ", i, j)] + CBTU[paste0("Non-trading Income ", i, j)])
  }
}

for (j in 1:4) {
  CBTU[paste0("Trading Ratio Change Q", j)] <-
    (CBTU[paste0("Trading Ratio Q", j)] - CBTU[paste0("Trading Ratio Baseline Q", j)])
}

for (i in c("Baseline ", "")) {
  CBTU[paste0("Trading Ratio ", i, "Cumulative")] <- 100 *
    rowSums(CBTU[grepl(paste0("Trading Income ", i, "Q"), names(CBTU))], na.rm = TRUE) /
    (rowSums(CBTU[grepl(paste0("Trading Income ", i, "Q"), names(CBTU))], na.rm = TRUE) +
      rowSums(CBTU[grepl(paste0("Non-trading Income ", i, "Q"), names(CBTU))], na.rm = TRUE))
}

CBTU["Trading Ratio Change Cumulative"] <-
  CBTU["Trading Ratio Cumulative"] -
  CBTU["Trading Ratio Baseline Cumulative"]

CBTU %>% group_by(Scheme) %>% summarise(
  n = n(),
  median_BASELINE_C = median(`Trading Ratio Baseline Cumulative`, na.rm = TRUE),
  median_TRADEUP_C = median(`Trading Ratio Cumulative`, na.rm = TRUE),
  median_Change_C = median(`Trading Ratio Change Cumulative`, na.rm = TRUE)
)

CBTU %>% group_by(Scheme) %>% summarise(
  n = n(),
  median_BASELINE_2 = median(`Trading Ratio Baseline Q2`, na.rm = TRUE),
  median_TRADEUP_2 = median(`Trading Ratio Q2`, na.rm = TRUE),
  median_Change_2 = median(`Trading Ratio Change Q2`, na.rm = TRUE)
)

OUT <- (CBTU %>%
  group_by(Scheme) %>%
  summarise_at(
    vars(starts_with("Trading Ratio")),
    funs(median = median(., na.rm = TRUE))
  ))

OUT <- OUT %>%
  gather(var, val, 2:ncol(OUT)) %>%
  spread(names(OUT)[1], val)

CBTU %>%
  ggplot() +
  geom_histogram(aes(`Trading Ratio Q2`, fill = `Scheme`), position = "dodge")

histogram(~ `Trading Ratio Change Cumulative` | interaction(as.factor(CBTU$Location), as.factor(CBTU$Scheme)),
          type = "count", data = CBTU, nint = 5, layout = c(2,5))
histogram(~ `Trading Ratio Change Cumulative` | as.factor(Scheme),
          data = CBTU, type = "count", nint = 5, labels = TRUE, layout = c(1,2))
kruskal.test(`Trading Ratio Change Cumulative` ~ as.factor(Scheme), data = CBTU)
wilcox.test(`Trading Ratio Change Cumulative` ~ as.factor(Scheme), data = CBTU)

CBTU %>%
  ggplot(aes(Scheme, `Trading Ratio Change Cumulative`)) +
  geom_boxplot() +
  geom_point()
