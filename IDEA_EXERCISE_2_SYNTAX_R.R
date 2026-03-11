# ============================================================
# R Translation of Stata Analysis Script
# Dataset: synth_random10.dta
# ============================================================

# --- Install required packages (run once) ---
install.packages("haven")       # Read .dta files
install.packages("dplyr")       # Data manipulation
install.packages("ggplot2")     # Visualization
install.packages("margins")     # Marginal effects (equivalent to Stata's margins)
install.packages("prediction")  # Predicted probabilities (companion to margins)
install.packages("scales")      # Percent formatting

# --- Load required libraries ---
library(haven)
library(dplyr)
library(ggplot2)
library(margins)
library(prediction)
library(scales)


# ============================================================
# LOAD DATA
# ============================================================

df <- read_dta("synth_random10.dta")


# ============================================================
# ANALYTICAL SAMPLE
# ============================================================

df <- df %>% rename(age_group = age_group_synth)
df <- df %>% filter(age_group <= 9)   # Ages 19 to 64
table(df$age_group)


# ============================================================
# RECODE TARGET VARIABLES
# ============================================================

# --- Condition of dwelling ---
# 0 = No repair needed; 1 = Repair needed
df <- df %>%
  mutate(con_dwelling = case_when(
    rpair_synth == 1 ~ 0,
    rpair_synth %in% c(2, 3) ~ 1,
    TRUE ~ NA_real_
  ))
table(df$con_dwelling)
table(df$rpair_synth)

# --- Immigrant status ---
# 0 = Non-immigrant; 1 = Immigrant
df <- df %>%
  mutate(immigrant = case_when(
    immder_synth %in% c(1, 2) ~ 1,
    immder_synth == 3 ~ 0,
    TRUE ~ NA_real_
  ))
table(df$immigrant)
table(df$immder_synth)

# --- Visible minority status ---
# 0 = Non-visible minority; 1 = Visible minority
df <- df %>%
  mutate(vis_min = case_when(
    dvismin_synth == 14 ~ 0,
    dvismin_synth %in% 1:13 ~ 1,
    TRUE ~ NA_real_
  ))
table(df$vis_min)

# --- Parental status ---
# 0 = Non-parent; 1 = Parent
df <- df %>%
  mutate(parent = case_when(
    kid_group_synth == 1 ~ 0,
    kid_group_synth %in% c(2, 3) ~ 1,
    TRUE ~ NA_real_
  ))
table(df$parent)
table(df$kid_group_synth)

# --- Marital status ---
# 0 = Single; 1 = Married
df <- df %>%
  mutate(marital_status = case_when(
    marst_synth %in% c(1, 4, 5) ~ 0,
    marst_synth %in% c(2, 3) ~ 1,
    TRUE ~ NA_real_
  ))
table(df$marital_status)
table(df$marst_synth)

# --- Gender ---
# 0 = Female; 1 = Male
df <- df %>%
  mutate(gender = case_when(
    sex_synth == 1 ~ 0,
    sex_synth == 2 ~ 1,
    TRUE ~ NA_real_
  ))
table(df$gender)
table(df$sex_synth)

# --- Income ---
# 0 = Low income; 1 = Non-low income
df <- df %>%
  mutate(income = case_when(
    loinca_synth == 1 ~ 1,
    loinca_synth == 2 ~ 0,
    TRUE ~ NA_real_
  ))
table(df$income)
table(df$loinca_synth)

# --- Education ---
# 1 = High school or less; 2 = Trades/College; 3 = Bachelor; 4 = Graduate
df <- df %>%
  mutate(education = case_when(
    hcdd_synth %in% c(1, 2) ~ 1,
    hcdd_synth %in% c(3, 4, 5, 6, 7) ~ 2,
    hcdd_synth %in% c(8, 9) ~ 3,
    hcdd_synth %in% c(10, 11, 12, 13) ~ 4,
    TRUE ~ NA_real_
  ))
table(df$education)
table(df$hcdd_synth)


# ============================================================
# HELPER: ORANGE GRADIENT PALETTE
# Assigns colours: highest bar = bright orange, lowest = light orange
# ============================================================

orange_palette <- function(probs) {
  n <- length(probs)
  bright <- "#FF6B00"   # Bright orange  (highest bar)
  light  <- "#FFD4A8"   # Light orange   (lowest bar)
  
  # Rank bars: rank 1 = lowest → light, rank n = highest → bright
  ranks  <- rank(probs, ties.method = "average")
  t_vals <- (ranks - 1) / max(ranks - 1, 1)   # 0 (lowest) → 1 (highest)
  
  # Linear interpolation in RGB space
  r_bright <- col2rgb(bright); r_light <- col2rgb(light)
  cols <- sapply(t_vals, function(t) {
    r <- round(r_light[1] + t * (r_bright[1] - r_light[1]))
    g <- round(r_light[2] + t * (r_bright[2] - r_light[2]))
    b <- round(r_light[3] + t * (r_bright[3] - r_light[3]))
    rgb(r, g, b, maxColorValue = 255)
  })
  cols
}


# ============================================================
# INTERSECTION #1 — Immigrant status × Visible minority status
# ============================================================

df <- df %>%
  mutate(
    imm_vis = case_when(
      immigrant == 0 & vis_min == 0 ~ 1,   # White natives
      immigrant == 1 & vis_min == 0 ~ 2,   # White immigrants
      immigrant == 0 & vis_min == 1 ~ 3,   # Non-white natives
      immigrant == 1 & vis_min == 1 ~ 4,   # Non-white immigrants
      TRUE ~ NA_real_
    ),
    imm_vis = factor(imm_vis,
                     levels = 1:4,
                     labels = c("White natives",
                                "White immigrants",
                                "Non-white natives",
                                "Non-white immigrants"))
  )
table(df$imm_vis)

# Convert control variables to factors for the model
df <- df %>%
  mutate(
    imm_vis        = factor(imm_vis),
    parent_f       = factor(parent),
    marital_f      = factor(marital_status),
    gender_f       = factor(gender),
    income_f       = factor(income),
    education_f    = factor(education),
    age_group_f    = factor(age_group),
    immigrant_f    = factor(immigrant),
    vis_min_f      = factor(vis_min)
  )

# --- Logistic regression #1 ---
model1 <- glm(
  con_dwelling ~ imm_vis + parent_f + marital_f + gender_f +
    income_f + education_f + age_group_f,
  data   = df,
  family = binomial(link = "logit")
)
summary(model1)

# --- Predicted probabilities (equivalent to Stata's margins imm_vis) ---
marg1 <- margins(model1, variables = "imm_vis")

pred1 <- prediction(model1,
                    at = list(imm_vis = levels(df$imm_vis))) %>%
  summary()

# Build a tidy data frame of predicted probabilities + 95% CI
pred1_df <- data.frame(
  group = levels(df$imm_vis),
  prob  = pred1$Prediction,
  lower = pred1$lower,
  upper = pred1$upper
)

# --- Visualization #1 ---
# Sort by prob first so colour assignment and bar order always match
pred1_df <- pred1_df %>%
  arrange(prob) %>%
  mutate(
    fill_col = orange_palette(prob),
    group    = factor(group, levels = group)
  )

ggplot(pred1_df, aes(x = group, y = prob, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15, linewidth = 0.7, colour = "grey30") +
  geom_text(aes(label = scales::percent(prob, accuracy = 0.1)),
            vjust = -0.6, size = 3.8, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Predicted Probability of Dwelling Needing Repair",
    subtitle = "Intersection #1: Immigrant Status × Visible Minority Status",
    x        = NULL,
    y        = "Predicted Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10)
  )


# ============================================================
# INTERSECTION #2 — Marital status × Parental status
# ============================================================

df <- df %>%
  mutate(
    child_marriage = case_when(
      marital_status == 0 & parent == 0 ~ 1,   # Single, no children
      marital_status == 1 & parent == 0 ~ 2,   # Married, no children
      marital_status == 0 & parent == 1 ~ 3,   # Single with children
      marital_status == 1 & parent == 1 ~ 4,   # Married with children
      TRUE ~ NA_real_
    ),
    child_marriage = factor(child_marriage,
                            levels = 1:4,
                            labels = c("Single, no children",
                                       "Married, no children",
                                       "Single with children",
                                       "Married with children"))
  )
table(df$child_marriage)

# --- Logistic regression #2 ---
model2 <- glm(
  con_dwelling ~ child_marriage + immigrant_f + vis_min_f + gender_f +
    income_f + education_f + age_group_f,
  data   = df,
  family = binomial(link = "logit")
)
summary(model2)

# --- Predicted probabilities ---
pred2 <- prediction(model2,
                    at = list(child_marriage = levels(df$child_marriage))) %>%
  summary()

pred2_df <- data.frame(
  group = levels(df$child_marriage),
  prob  = pred2$Prediction,
  lower = pred2$lower,
  upper = pred2$upper
)

# --- Visualization #2 ---
# Sort by prob first so colour assignment and bar order always match
pred2_df <- pred2_df %>%
  arrange(prob) %>%
  mutate(
    fill_col = orange_palette(prob),
    group    = factor(group, levels = group)
  )

ggplot(pred2_df, aes(x = group, y = prob, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15, linewidth = 0.7, colour = "grey30") +
  geom_text(aes(label = scales::percent(prob, accuracy = 0.1)),
            vjust = -0.6, size = 3.8, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Predicted Probability of Dwelling Needing Repair",
    subtitle = "Intersection #2: Marital Status × Parental Status",
    x        = NULL,
    y        = "Predicted Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10)
  )


# ============================================================
# INTERSECTION #3 — Gender × Marital status
# ============================================================

df <- df %>%
  mutate(
    gender_marriage = case_when(
      gender == 0 & marital_status == 0 ~ 1,   # Single women
      gender == 1 & marital_status == 0 ~ 2,   # Single men
      gender == 0 & marital_status == 1 ~ 3,   # Married women
      gender == 1 & marital_status == 1 ~ 4,   # Married men
      TRUE ~ NA_real_
    ),
    gender_marriage = factor(gender_marriage,
                             levels = 1:4,
                             labels = c("Single women",
                                        "Single men",
                                        "Married women",
                                        "Married men"))
  )
table(df$gender_marriage)

# --- Logistic regression #3 ---
model3 <- glm(
  con_dwelling ~ gender_marriage + immigrant_f + vis_min_f + parent_f +
    income_f + education_f,
  data   = df,
  family = binomial(link = "logit")
)
summary(model3)

# --- Predicted probabilities ---
pred3 <- prediction(model3,
                    at = list(gender_marriage = levels(df$gender_marriage))) %>%
  summary()

pred3_df <- data.frame(
  group = levels(df$gender_marriage),
  prob  = pred3$Prediction,
  lower = pred3$lower,
  upper = pred3$upper
)

# --- Visualization #3 ---
# Sort by prob first so colour assignment and bar order always match
pred3_df <- pred3_df %>%
  arrange(prob) %>%
  mutate(
    fill_col = orange_palette(prob),
    group    = factor(group, levels = group)
  )

ggplot(pred3_df, aes(x = group, y = prob, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15, linewidth = 0.7, colour = "grey30") +
  geom_text(aes(label = scales::percent(prob, accuracy = 0.1)),
            vjust = -0.6, size = 3.8, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Predicted Probability of Dwelling Needing Repair",
    subtitle = "Intersection #3: Gender × Marital Status",
    x        = NULL,
    y        = "Predicted Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10)
  )
