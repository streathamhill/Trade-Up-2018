---
title: "2018 Trade Up Analysis (Bayesian)"
author: "Richard Harries"
date: '`r format(Sys.Date(), "%A, %d %B %Y")`'
output: html_document
---
  
```{r setup, include=TRUE}
#------------------------------------------------------------------------------- 
# Import libraries
library(kableExtra)
library(tidyverse)
library(readxl)
library(tidybayes)

#------------------------------------------------------------------------------- 
# Create the dataset
CBTU_2017 <-
  read_excel("MT CBTUD PTC 170918.xlsx",
    sheet = "Sheet1",
    skip = 0
  ) %>%
  mutate(
    Year = 2017,
    Location = case_when(
      Location == "L" ~ "London",
      Location == "NW" ~ "Liverpool"
    ),
    Scheme = case_when(
      Scheme == "C" ~ "Control",
      Scheme == "MT" ~ "Match Trading"
    )
  ) %>%
  select(-c("Sector", "Beneficiary", "Impact Area 1", "Impact Area 2"))

CBTU_2018 <-
  read_excel("CBTU PtC - Roll Out Year Q4 110219.xlsx",
    sheet = "Clean Data",
    skip = 0
  ) %>%
  mutate(
    Year = 2018,
    Scheme = case_when(
      Scheme == "C" ~ "Control",
      Scheme == "MT" ~ "Match Trading"
    )
  ) %>%
  select(-c("Sector", "Beneficiary", "Impact Area 1", "Impact Area 2"))

CBTU <- full_join(CBTU_2017, CBTU_2018)

#------------------------------------------------------------------------------- 
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

#------------------------------------------------------------------------------- 
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

#------------------------------------------------------------------------------- 

ABC <- CBTU %>%
  select(c(`Trading Ratio Change Cumulative`, `Scheme`)) %>%
  drop_na()

ABC %>%
  ggplot(aes(`Trading Ratio Change Cumulative`, `Scheme`)) +
  geom_point(alpha = 0.5) +
  ylab("Group")

CBTU_NA <- CBTU %>%
  group_by(Scheme, Year) %>%
  summarise_at(
    vars(starts_with("Trading Ratio Change")),
    list(missing = ~ sum(is.na(.)) / length(.))
  )

CBTU %>%
  select(
  contains("Trading Ratio Change")
) %>%
  summarise(
    x = funs((sum(is.na(.))))
  )

ABC %>%
  group_by(Scheme) %>%
  summarise(
    n = n(),
    mean_Change_C = mean(`Trading Ratio Change Cumulative`, na.rm = TRUE),
    median_Change_C = median(`Trading Ratio Change Cumulative`, na.rm = TRUE)
  ) %>%
  kable(
    col.names = c(
      "Group", "n",
      "Mean",
      "Median"
    ),
    digits = 1, format = "html"
  ) %>%
  add_header_above(c(" " = 2, "Percentage point change" = 2)) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

#------------------------------------------------------------------------------- 

graphFileType <- "jpeg"
fileNameRoot <- "TwoGroupTUrobustHet-"
RopeMuDiff <- c(-0.5, 0.5)
RopeSdDiff <- c(-0.5, 0.5)
RopeEff <- c(-0.1, 0.1)

#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ymet-Xnom2grp-MrobustHet.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
#mcmcCoda <- genMCMC(
#  datFrm = as.data.frame(ABC),
#  numSavedSteps = 50000, saveName = "TwoGroupTUrobustHet-"
#  yName = "Trading Ratio Change Cumulative", xName = "Scheme",
)
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
#parameterNames <- varnames(mcmcCoda) # get all parameter names
#for (parName in parameterNames) {
#  diagMCMC(
#    codaObject = mcmcCoda, parName = parName,
#    saveName = fileNameRoot, saveType = graphFileType
#  )
#}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
#summaryInfo <- smryMCMC(mcmcCoda,
#  RopeMuDiff = RopeMuDiff,
#  RopeSdDiff = RopeSdDiff, RopeEff = RopeEff,
#)
##  saveName = NULL

#summaryInfo %>%
#  kable(col.names = c(
#      "HDImass", "HDIlow", "HDIhigh", "CompVal",
#      "Mean", "Median", "Mode", "ESS",
#      "PcntGtCompVal", "ROPElow", "ROPEhigh",
#      "PcntLtROPE", "PcntInROPE", "PcntGtROPE"),
#    digits = 1, format = "html"
#  ) %>% 

#  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
# Display posterior information:
#plotMCMC(mcmcCoda,
#  datFrm = as.data.frame(ABC),
#  yName = "Trading Ratio Change Cumulative", xName = "Scheme",
#  RopeMuDiff = NULL, RopeSdDiff = NULL, RopeEff = NULL,
#  pairsPlot = FALSE, saveName = NULL, saveType = graphFileType
#)
#------------------------------------------------------------------------------- 

#X11( width=7 , height=7 , type="cairo")
mcmcMat = as.matrix(mcmcCoda,chains=TRUE)
mu1 = mcmcMat[,"mu[1]"]
mu2 = mcmcMat[,"mu[2]"]
xlim = range( c( mu1 , mu2 ) )
#plotPost( mu2-mu1 , compVal=0 ,
#                     xlab=bquote(mu[2] - mu[1]) , cex.lab = 1.75 , 
#                     main="Difference of Means:\nMatch Trading - Control" , col="skyblue" )
```