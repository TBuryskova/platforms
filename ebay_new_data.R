library(corrplot)
library(tidyverse)
library(RColorBrewer)

df <- read.csv("~/CERGE-EI/Other computers/Moje PC/CERGE-EI/Research/platforms/ebay/merged.csv") %>%
  mutate(fee_paid = as.numeric(item_price_numeric * fee_percent_estimate)) %>%
  mutate(
    label = factor(
      label,
      levels = c("low", "mid-low", "mid-high", "high"),
      ordered = TRUE
    )
  ) %>%
  filter(fee_category_detected != "Coins & Paper Money (except Bullion)") %>%
  filter(fee_category_detected != "Collectibles")

summary(df)

category_levels <- df %>%
  distinct(fee_category_detected, label) %>%
  arrange(label, fee_category_detected) %>%
  pull(fee_category_detected)

df$fee_category_detected <- factor(
  df$fee_category_detected,
  levels = category_levels
)

df_numeric <- df %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(where(~ stats::var(.x, na.rm = TRUE) > 0))

M <- cor(df_numeric, use = "pairwise.complete.obs")
corrplot(M, method = "square", diag = FALSE)

# choose cut points
x_mid <- 45
y_mid <- 8

# background for theoretical regions
quad_df <- data.frame(
  xmin = c(-Inf, x_mid, -Inf, x_mid),
  xmax = c(x_mid, Inf, x_mid, Inf),
  ymin = c(y_mid, y_mid, -Inf, -Inf),
  ymax = c(Inf, Inf, y_mid, y_mid),
  label = factor(
    c("low", "mid-low", "high", "mid-high"),
    levels = c("low", "mid-low", "mid-high", "high"),
    ordered = TRUE
  ),
  x = c(x_mid - 8, x_mid + 8, x_mid - 8, x_mid + 8),
  y = c(y_mid + 3, y_mid + 3, y_mid - 3, y_mid - 3)
)

# label -> color
label_cols <- setNames(
  brewer.pal(4, "Spectral")[4:1],
  c("low", "mid-low", "mid-high", "high")
)

# category -> color, inherited from that category's label
cat_cols <- df %>%
  distinct(fee_category_detected, label) %>%
  mutate(col = label_cols[as.character(label)]) %>%
  { setNames(.$col, .$fee_category_detected) }

ggplot(
  df,
  aes(
    x = visible_monetization_score,
    y = fee_percent_estimate,
    color = fee_category_detected,
    shape = fee_category_detected
  )
) +
  geom_rect(
    data = quad_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = label),
    inherit.aes = FALSE,
    alpha = 0.08
  ) +
  geom_vline(xintercept = x_mid, color = "white", linewidth = 3) +
  geom_hline(yintercept = y_mid, color = "white", linewidth = 3) +
  geom_point(
    position = position_jitter(width = 1, height = 0.2),
    size = 5,
    alpha = 0.85,
    stroke = 1.5
  ) +
  scale_shape_manual(
    values = c(0, 3, 6, 9, 12, 8, 21),
    name = "Fee-relevant category"
  ) +
  scale_color_manual(
    values = cat_cols,
    name = "Fee-relevant category"
  ) +
  scale_fill_brewer(
    guide = "none",
    palette = "Spectral",
    direction = -1
  ) +
  geom_text(
    data = quad_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 10
  ) +
  labs(
    x = "Visible monetization score (α)",
    y = "Fee (%) (w)"
  ) +
  theme_classic(base_size = 14)