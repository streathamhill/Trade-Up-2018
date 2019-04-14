#### Title: Unified Trade Up Analysis ####

source("Common Tools.R")
library(readxl)

#### Data import and manipulation ####

CBTU <-
  read_excel("CBTU PtC - Roll Out Year Q4 110219.xlsx",
    sheet = "Clean Data",
    skip = 0
  ) %>%
  mutate(
    Scheme = case_when(
      Scheme == "C" ~ "Control",
      Scheme == "MT" ~ "Match Trading"
    )
  )

# Calculate quarterly trading ratios and annual cumulative ratios
# for the baseline and programme years

for (i in c("Baseline ", "")) {
  for (j in 1:4) {
    CBTU[paste0("Trading Ratio ", i, "Q", j)] <- 100 * CBTU[paste0("Trading Income ", i, "Q", j)] /
      (CBTU[paste0("Trading Income ", i, "Q", j)] + CBTU[paste0("Non-trading Income ", i, "Q", j)])
  }
  CBTU[paste0("Trading Ratio ", i, "Cumulative")] <- 100 *
    rowSums(CBTU[grepl(paste0("Trading Income ", i, "Q"), names(CBTU))], na.rm = FALSE) /
    (rowSums(CBTU[grepl(paste0("Trading Income ", i, "Q"), names(CBTU))], na.rm = FALSE) +
      rowSums(CBTU[grepl(paste0("Non-trading Income ", i, "Q"), names(CBTU))], na.rm = FALSE))
}

# Calculate the change in quarterly and annual ratios
# between the two years

for (j in 1:4) {
  CBTU[paste0("Trading Ratio Change Q", j)] <-
    (CBTU[paste0("Trading Ratio Q", j)] - CBTU[paste0("Trading Ratio Baseline Q", j)])
  CBTU[paste0("Eligible To Claim Ratio Q", j)] <-
    (CBTU[paste0("Eligible To Claim Q", j)] / (CBTU[paste0("Trading Income Q", j)] + CBTU[paste0("Non-trading Income Q", j)])) * 100
}

CBTU["Trading Ratio Change Cumulative"] <-
  CBTU["Trading Ratio Cumulative"] -
  CBTU["Trading Ratio Baseline Cumulative"]

#### Exploratory Data Analysis ####

CBTU %>% group_by(Scheme) %>% summarise(
  n = n(),
  mean_BASELINE_C = mean(`Trading Ratio Baseline Cumulative`, na.rm = TRUE),
  mean_TRADEUP_C = mean(`Trading Ratio Cumulative`, na.rm = TRUE),
  mean_Change_C = mean(`Trading Ratio Change Cumulative`, na.rm = TRUE)
)

CBTU %>% group_by(Scheme) %>% summarise(
  n = n(),
  median_BASELINE_C = median(`Trading Ratio Baseline Cumulative`, na.rm = TRUE),
  median_TRADEUP_C = median(`Trading Ratio Cumulative`, na.rm = TRUE),
  median_Change_C = median(`Trading Ratio Change Cumulative`, na.rm = TRUE)
)

kruskal.test(`Trading Ratio Cumulative` ~ as.factor(Scheme), data = CBTU)
wilcox.test(`Trading Ratio Cumulative` ~ as.factor(Scheme), data = CBTU)

kruskal.test(`Trading Ratio Change Cumulative` ~ as.factor(Scheme), data = CBTU)
wilcox.test(`Trading Ratio Change Cumulative` ~ as.factor(Scheme), data = CBTU)

#### Plots ####

CBTU %>%
  ggplot() +
  geom_histogram(aes(
    x = `Trading Ratio Change Cumulative`,
    colour = 24, fill = Location
  ),
  bins = 10, show.legend = FALSE
  ) +
  labs(
    title = "TradeUp 2018: Cumulative performance distribution",
    subtitle = "(Compared with the equivalent trading ratio in the baseline year)",
    x = "", y = "Percentage point change",
    caption = "(based on data from the School for Social Entrepreneurs)"
  ) +
  facet_wrap(vars(Scheme, Location), ncol = 5) +
  theme_ptc()

CBTU %>%
  ggplot() +
  geom_histogram(aes(
    x = `Trading Ratio Change Cumulative`,
    colour = 24, fill = Location
  ),
  bins = 5
  ) +
  guides(colour = FALSE) +
  labs(
    title = "TradeUp 2018: Cumulative performance distribution",
    subtitle = "(Compared with the equivalent trading ratio in the baseline year)",
    x = "", y = "Percentage point change",
    caption = "(based on data from the School for Social Entrepreneurs)"
  ) +
  facet_wrap(vars(Scheme)) +
  theme_ptc()

CBTU %>%
  ggplot(aes(Scheme, `Trading Ratio Change Cumulative`)) +
  geom_boxplot() +
  geom_point(aes(colour = `Scheme`), show.legend = FALSE) +
  labs(
    title = "TradeUp 2018: Cumulative performance",
    subtitle = "(compared with the equivalent trading ratio in the baseline year)",
    x = "", y = "Percentage point change",
    caption = "(based on data from the School for Social Entrepreneurs)"
  ) +
  theme_ptc()

CBTU_AVG <- CBTU %>%
  group_by(Scheme) %>%
  summarise_at(
    vars(starts_with("Trading Ratio")),
    list(median = ~median(., na.rm = TRUE))
  ) %>%
  gather(`Ratio Type`, Value, 2:ncol(.)) %>%
  spread(Scheme, Value)

CBTU_AVG %>%
  filter(str_detect(`Ratio Type`, "Trading Ratio Q")) %>%
  rename("MT" = "Match Trading") %>%
  ggplot(aes(x = `Ratio Type`)) +
  geom_point(aes(y = Control, group = "Control", colour = "Control"), size = 2) +
  geom_line(aes(y = Control, group = "Control", colour = "Control"), size = 1.25) +
  geom_point(aes(y = MT, group = "Match Trading", colour = "Match Trading"), size = 2) +
  geom_line(aes(y = MT, group = "Match Trading", colour = "Match Trading"), size = 1.25) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  labs(
    title = "TradeUp 2018: Quarterly median performance",
    subtitle = "(absolute trading ratio in 2018)",
    colour = "", x = "", y = "Percentage",
    caption = "(based on data from the School for Social Entrepreneurs)"
  ) +
  theme_ptc()

CBTU_AVG %>%
  filter(str_detect(`Ratio Type`, "Trading Ratio Change Q")) %>%
  rename("MT" = "Match Trading") %>%
  ggplot(aes(x = `Ratio Type`)) +
  geom_point(aes(y = Control, group = "Control", colour = "Control"), size = 2) +
  geom_line(aes(y = Control, group = "Control", colour = "Control"), size = 1.25) +
  geom_point(aes(y = MT, group = "Match Trading", colour = "Match Trading"), size = 2) +
  geom_line(aes(y = MT, group = "Match Trading", colour = "Match Trading"), size = 1.25) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  labs(
    title = "TradeUp 2018: Quarterly median performance",
    subtitle = "(compared with the equivalent trading ratio in the baseline year)",
    colour = "", x = "", y = "Percentage point change",
    caption = "(based on data from the School for Social Entrepreneurs)"
  ) +
  geom_hline(aes(yintercept = 0)) +
  theme_ptc()

CBTU %>%
  select(Project, Scheme, Location,
         contains("Trading Ratio Change Q")) %>%
  gather(Quarter, `Trading Ratio Change`, 4:ncol(.)) %>%
  mutate(Quarter = str_sub(Quarter, -2)) %>%
  filter(Scheme == "Control") %>%
  ggplot(aes(x = Quarter, y = `Trading Ratio Change`, group = Project)) +
  geom_point(aes(colour = Location), show.legend = FALSE) +
  geom_line(aes(colour = Location), show.legend = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  labs(
    title = "TradeUp 2018: Quarterly median performance",
    subtitle = "(Control Group, compared with the equivalent trading ratio in the baseline year)",
    x = "", y = "Percentage point change",
    caption = "(based on data from the School for Social Entrepreneurs)"
  ) +
  facet_grid(~Location) +
  theme_ptc()

CBTU %>%
  select(Project, Scheme, Location,
         contains("Trading Ratio Change Q")) %>%
  gather(Quarter, `Trading Ratio Change`, 4:ncol(.)) %>%
  mutate(Quarter = str_sub(Quarter, -2)) %>%
  filter(Scheme == "Match Trading") %>%
  ggplot(aes(x = Quarter, y = `Trading Ratio Change`, group = Project)) +
  geom_point(aes(colour = Location), show.legend = FALSE) +
  geom_line(aes(colour = Location), show.legend = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  labs(
    title = "TradeUp 2018: Quarterly median performance",
    subtitle = "(Match Trading Group, compared with the equivalent trading ratio in the baseline year)",
    x = "", y = "Percentage point change",
    caption = "(based on data from the School for Social Entrepreneurs)"
  ) +
  facet_wrap(~Location, ncol = 4) +
  theme_ptc()

CBTU %>%
  select(Project, Scheme, Location,
         contains("Trading Ratio Change Q"),
         contains("Eligible To Claim Ratio Q")) %>%
  gather(`Ratio Type`, Value, 4:ncol(.)) %>%
  mutate(Quarter = str_sub(`Ratio Type`, -2)) %>%
  mutate(`Ratio Type` = str_sub(`Ratio Type`, 1, -4)) %>%
  replace_na(list(Value = 0)) %>%
  spread(`Ratio Type`, Value) %>%
  ggplot() +
  geom_point(aes(x = `Eligible To Claim Ratio`, y = `Trading Ratio Change`,
    colour = Location)
  ) +
  geom_hline(aes(yintercept = 0)) +
  labs(
    title = "TradeUp 2018: Quarterly performance",
    subtitle = "(absolute ratios in 2018)",
    x = "Eligible To Claim Ratio",
    y = "Trading Ratio Change",
    caption = "(based on data from the School for Social Entrepreneurs)"
  ) +
  facet_wrap(vars(Scheme, Quarter), ncol = 4) +
  xlim(0, 100) +
  theme_ptc()
