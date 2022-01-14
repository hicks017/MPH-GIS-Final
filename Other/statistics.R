# Setup
library(tidyverse)
df <- readxl::read_xlsx("./SDTracts_GrocBuff_Summarize_TableToExcel.xlsx")
df <- df %>% rename(
  totalpop = TotalPopulation,
  chol_prev = HIGHCHOL_CrudePrev,
  prev_95_ci = HIGHCHOL_Crude95CI,
  tract_area = Tract_Sq_Miles,
  grocer_count = Polygon_Count
)

# Changing degrees of longitude to miles east from most westward point in SD
df$east10miles <- (df$Longitude + (-1*min(df$Longitude)))*58.2/10
attach(df)

# Descriptive Statistics

## Median high cholesterol prevalence: 28.5% (range = 8.8-45.0%)
## Median number of nearby grocery stores: 10 (range = 0-52)
summary(df)
qplot(chol_prev)
qplot(grocer_count)

## 8 Census tracts did not have any grocery stores
df %>% subset(grocer_count == 0) %>% nrow()
df %>% subset(grocer_count==0) %>% summary()

# Analyitics Statistics

## Main study correlation R = -0.32
qplot(grocer_count, chol_prev)
lm(chol_prev ~ grocer_count) %>% summary()
cor(chol_prev, grocer_count)

## MLR R = 0.39; R2 = 0.15; grocer estimate = -0.15
lm(chol_prev ~ grocer_count + totalpop + east10miles + tract_area) %>% summary()
lm(chol_prev ~ grocer_count) %>% confint()

## Prevalence per grocery store
### Min = 0.481; Median = 3.03; Max = 39.9
df$prev_per_groc <- df$chol_prev / df$grocer_count
prev_finite <- df$prev_per_groc %>% subset(is.finite(df$prev_per_groc))
summary(prev_finite)
qplot(prev_finite)
### 90% of tracts have less than or equal to 10 prevalence poitns per grocer
df %>% subset(df$prev_per_groc<=10) %>% nrow() / nrow(df)