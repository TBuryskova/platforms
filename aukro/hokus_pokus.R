library(readxl)
library(corrplot)
library(tidyverse)
df <- read_excel("data aukro.xlsx")



# -----------------------------
# 1. Build indices
# -----------------------------
df <- df %>%
  mutate(
    distraction_index = rowMeans(scale(select(.,third_party,banners,sticky, interruption, viewport_area
    )), na.rm = TRUE),
    
    visual_index = rowMeans(scale(select(.,banners,sticky,viewport_area
    )), na.rm = TRUE),
    viewport_area=viewport_area/20
  ) %>% mutate(complexity_index_sq=complexity_index^2)

# -----------------------------
# 2. Reshape data for facetting
# -----------------------------
plot_df <- df %>%
  select(
    category,
    distraction_index,
    visual_index,
    fee_private,
    fee_business, complexity, complexity_index
  ) %>%
  pivot_longer(
    cols = c(distraction_index, visual_index),
    names_to = "index_type",
    values_to = "index_value"
  ) %>%
  pivot_longer(
    cols = c( fee_private,
              fee_business),
    names_to = "fee_type",
    values_to = "fee_value"
  ) %>%
  mutate(
    index_type = recode(
      index_type,
      distraction_index = "Distraction index",
      visual_index = "Visual index"
    ),
    fee_type = recode(
      fee_type,
      fee_private = "fee_private",
      fee_business = "fee_business"
    )
  )

# -----------------------------
# 3. Enter theoretical curve points
#    Replace these with your own points
# -----------------------------
theory_df <- data.frame(
  x = c(
    0.00,  # vertical drop start
    0.00,  # vertical drop end
    0.05,
    0.15,
    0.24,
    0.30,
    0.33,
    0.35,
    0.38,
    0.41
  ),
  y = c(
    0.467, # top point
    0.420, # after vertical drop
    0.405,
    0.365,
    0.337,
    0.305,
    0.288,
    0.272,
    0.260,
    0.249
  )
)

# Optional: keep the original sequence if it matters for the shape.
# If your line looks odd, try sorting by x:
# theory_df <- theory_df[order(theory_df$x), ]

# -----------------------------
# 4. Helper function: rescale one vector to a new range
# -----------------------------
rescale_to <- function(x, new_min, new_max) {
  old_min <- min(x, na.rm = TRUE)
  old_max <- max(x, na.rm = TRUE)
  
  if (old_max == old_min) {
    return(rep(mean(c(new_min, new_max)), length(x)))
  }
  
  (x - old_min) / (old_max - old_min) * (new_max - new_min) + new_min
}

# -----------------------------
# 5. Build a rescaled theory curve for each facet
# -----------------------------
facet_ranges <- plot_df %>%
  group_by(index_type, fee_type) %>%
  summarise(
    xmin = min(index_value, na.rm = TRUE),
    xmax = max(index_value, na.rm = TRUE),
    ymin = min(fee_value, na.rm = TRUE),
    ymax = max(fee_value, na.rm = TRUE),
    .groups = "drop"
  )

theory_facet <- bind_rows(lapply(seq_len(nrow(facet_ranges)), function(i) {
  r <- facet_ranges[i, ]
  
  data.frame(
    index_type = r$index_type,
    fee_type   = r$fee_type,
    x = rescale_to(theory_df$x, r$xmin, r$xmax),
    y = rescale_to(theory_df$y, r$ymin, r$ymax)
  )
}))

# -----------------------------
# 6. Plot
# -----------------------------
p <- ggplot(df, aes(x = fee_business, y = viewport_area, color=complexity)) +
  geom_point(size = 2) +
 
  labs(
    x = "Ads",
    y = "Fee",
    title = ""
  ) +
  theme_minimal()

print(p)

q <- ggplot(df, aes(x = viewport_area, y = fee_private)) +
  geom_point(size = 2) +
  theme_minimal() 

print(q)

r <- ggplot(df, aes(x = complexity_index, y = fee_private, label=category)) +
  geom_point(size = 2) +
  theme_minimal() + geom_text()

print(r)

w <- ggplot(df, aes(x = (complexity_index), y = viewport_area, label=category)) +
  geom_point(size = 2) +
  theme_minimal() + geom_text(hjust = 0, nudge_x = 0.05)

print(w)


lm(fee_private~complexity_index, df) %>% summary()
model <- lm(viewport_area ~ complexity_index + I(complexity_index^2), data = df)
summary(model)

b1 <- coef(model)["complexity_index"]
b2 <- coef(model)["I(complexity_index^2)"]

turning_point <- -b1 / (2 * b2)
turning_point