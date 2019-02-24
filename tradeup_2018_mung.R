# Unified Trade Up Analysis

library(readxl)
library(tidyverse)
library(lattice)

CBTU <-
  read_excel("CBTU PtC - Roll Out Year Q4 110219.xlsx",
    sheet = "Clean Data",
    skip = 0
  )

CBTU_melt1 <- CBTU %>%
  mutate(Quarter = 1) %>%
  select(-contains("Q2"), -contains("Q3"), -contains("Q4")) %>%
  rename("Trading Income Baseline" = "Trading Income Baseline Q1",
         "Non-trading Income Baseline" = "Non-trading Income Baseline Q1",
         "Trading Income" = "Trading Income Q1",
         "Non-trading Income" = "Non-trading Income Q1",
         "Eligible To Claim" = "Eligible To Claim Q1")

CBTU_melt2 <- CBTU %>%
  mutate(Quarter = 2) %>%
  select(-contains("Q1"), -contains("Q3"), -contains("Q4")) %>%
  rename("Trading Income Baseline" = "Trading Income Baseline Q2",
         "Non-trading Income Baseline" = "Non-trading Income Baseline Q2",
         "Trading Income" = "Trading Income Q2",
         "Non-trading Income" = "Non-trading Income Q2",
         "Eligible To Claim" = "Eligible To Claim Q2")

CBTU_melt3 <- CBTU %>%
  mutate(Quarter = 3) %>%
  select(-contains("Q1"), -contains("Q2"), -contains("Q4")) %>%
  rename("Trading Income Baseline" = "Trading Income Baseline Q3",
         "Non-trading Income Baseline" = "Non-trading Income Baseline Q3",
         "Trading Income" = "Trading Income Q3",
         "Non-trading Income" = "Non-trading Income Q3",
         "Eligible To Claim" = "Eligible To Claim Q3")

CBTU_melt4 <- CBTU %>%
  mutate(Quarter = 4) %>%
  select(-contains("Q1"), -contains("Q2"), -contains("Q3")) %>%
  rename("Trading Income Baseline" = "Trading Income Baseline Q4",
         "Non-trading Income Baseline" = "Non-trading Income Baseline Q4",
         "Trading Income" = "Trading Income Q4",
         "Non-trading Income" = "Non-trading Income Q4",
         "Eligible To Claim" = "Eligible To Claim Q4")

CBTU_melt = bind_rows(CBTU_melt1, CBTU_melt2, CBTU_melt3, CBTU_melt4)
rm(CBTU_melt1, CBTU_melt2, CBTU_melt3, CBTU_melt4)
CBTU_melt$`Trading Ratio Baseline` <-
  100 * CBTU_melt$`Trading Income Baseline` /
  (CBTU_melt$`Trading Income Baseline` + CBTU_melt$`Non-trading Income Baseline`)
CBTU_melt$`Trading Ratio` <-
  100 * CBTU_melt$`Trading Income` /
  (CBTU_melt$`Trading Income` + CBTU_melt$`Non-trading Income`)
CBTU_melt$`Trading Ratio Change` <-
  CBTU_melt$`Trading Ratio` - CBTU_melt$`Trading Ratio Baseline`

CBTU_melt %>% group_by(Scheme, Quarter) %>% summarise(
  n = n(),
  median_BASELINE_C = median(`Trading Ratio Baseline`, na.rm = TRUE),
  median_TRADEUP_C = median(`Trading Ratio`, na.rm = TRUE),
  median_Change_C = median(`Trading Ratio Change`, na.rm = TRUE)
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
