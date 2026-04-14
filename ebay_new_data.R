library(corrplot)
library(tidyverse)

df <- read.csv("~/CERGE-EI/Other computers/Moje PC/CERGE-EI/Research/platforms/ebay/merged.csv")
summary(df)



df_numeric <- df %>% dplyr::select(where(is.numeric ))  %>%
  dplyr::select(where(~ stats::var(.x, na.rm = TRUE) > 0))

M<-cor(df_numeric, use = "pairwise.complete.obs")
corrplot(M, method = 'square',  diag = FALSE)

lm(fee_percent_estimate ~ backend_monetization_score+item_price_numeric, df) %>% summary()
