# ============================================================================
# DOMINANCE ANALYSIS FOR ITEM ANALYSIS
# Comparing Primary Scale (4 items) with Secondary Scale (validation)
# ============================================================================

# Install required packages
# install.packages("dominanceanalysis")
# install.packages("tidyverse")
# install.packages("lavaan")

library(dominanceanalysis)
library(tidyverse)
library(lavaan)

# ============================================================================
# 1. GENERATE SAMPLE DATA
# ============================================================================

set.seed(123)
n <- 300

# Primary Scale (4 items) - measuring construct X
primary_scale <- data.frame(
  item1 = rnorm(n, mean = 3.5, sd = 1.2),
  item2 = rnorm(n, mean = 3.6, sd = 1.1),
  item3 = rnorm(n, mean = 3.4, sd = 1.3),
  item4 = rnorm(n, mean = 3.7, sd = 1.0)
)

# Secondary Scale (4 items) - validation scale, similar construct
secondary_scale <- data.frame(
  item5 = rnorm(n, mean = 3.5, sd = 1.2),
  item6 = rnorm(n, mean = 3.6, sd = 1.1),
  item7 = rnorm(n, mean = 3.4, sd = 1.3),
  item8 = rnorm(n, mean = 3.7, sd = 1.0)
)

# Outcome variable
outcome <- (rowMeans(primary_scale) + rowMeans(secondary_scale)) / 2 + rnorm(n, 0, 0.5)

# Combine all data
data <- cbind(primary_scale, secondary_scale, outcome)

# ============================================================================
# 2. DOMINANCE ANALYSIS - PRIMARY SCALE (4 items)
# ============================================================================

cat("\n========== DOMINANCE ANALYSIS: PRIMARY SCALE (4 ITEMS) ==========\n\n")

# Fit linear model with primary scale
model_primary <- lm(outcome ~ item1 + item2 + item3 + item4, data = data)

# Perform dominance analysis
da_primary <- dominanceAnalysis(model_primary)

# Print results
print(da_primary)

# Extract General Dominance scores
gd_primary <- da_primary$general.dominance
cat("\nGeneral Dominance Scores (Primary Scale):\n")
print(round(gd_primary, 4))

# Ranking items by importance
ranking_primary <- data.frame(
  Item = names(gd_primary),
  General_Dominance = round(gd_primary, 4),
  Rank = rank(-gd_primary)
) %>% arrange(Rank)

cat("\nRanking Items (Primary Scale):\n")
print(ranking_primary)

# ============================================================================
# 3. DOMINANCE ANALYSIS - SECONDARY SCALE (4 items - validation)
# ============================================================================

cat("\n========== DOMINANCE ANALYSIS: SECONDARY SCALE (4 ITEMS) ==========\n\n")

# Fit linear model with secondary scale
model_secondary <- lm(outcome ~ item5 + item6 + item7 + item8, data = data)

# Perform dominance analysis
da_secondary <- dominanceAnalysis(model_secondary)

# Print results
print(da_secondary)

# Extract General Dominance scores
gd_secondary <- da_secondary$general.dominance
cat("\nGeneral Dominance Scores (Secondary Scale):\n")
print(round(gd_secondary, 4))

# Ranking items by importance
ranking_secondary <- data.frame(
  Item = names(gd_secondary),
  General_Dominance = round(gd_secondary, 4),
  Rank = rank(-gd_secondary)
) %>% arrange(Rank)

cat("\nRanking Items (Secondary Scale):\n")
print(ranking_secondary)

# ============================================================================
# 4. COMBINED DOMINANCE ANALYSIS (All 8 items)
# ============================================================================

cat("\n========== DOMINANCE ANALYSIS: COMBINED (8 ITEMS) ==========\n\n")

# Fit model with all items
model_combined <- lm(outcome ~ item1 + item2 + item3 + item4 + 
                               item5 + item6 + item7 + item8, data = data)

# Perform dominance analysis
da_combined <- dominanceAnalysis(model_combined)

# Print results
print(da_combined)

# Extract General Dominance scores
gd_combined <- da_combined$general.dominance
cat("\nGeneral Dominance Scores (Combined):\n")
print(round(gd_combined, 4))

# Ranking all items
ranking_combined <- data.frame(
  Item = names(gd_combined),
  General_Dominance = round(gd_combined, 4),
  Scale = c(rep("Primary", 4), rep("Secondary", 4)),
  Rank = rank(-gd_combined)
) %>% arrange(Rank)

cat("\nRanking All Items (Combined):\n")
print(ranking_combined)

# ============================================================================
# 5. COMPARATIVE VISUALIZATION
# ============================================================================

# Plot comparison
comparison_plot <- ranking_combined %>%
  ggplot(aes(x = reorder(Item, General_Dominance), 
             y = General_Dominance, 
             fill = Scale)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Dominance Analysis: Primary vs Secondary Scale",
       x = "Items",
       y = "General Dominance Score",
       fill = "Scale") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(comparison_plot)

# ============================================================================
# 6. CONSISTENCY CHECK: Correlation of Rankings
# ============================================================================

cat("\n========== CONSISTENCY CHECK ==========\n\n")

# Extract rankings for each scale
primary_ranks <- ranking_primary %>% 
  select(Item, Rank) %>% 
  rename(Primary_Rank = Rank)

secondary_ranks <- ranking_secondary %>% 
  select(Item, Rank) %>% 
  rename(Secondary_Rank = Rank) %>%
  mutate(Item = gsub("item", "item", Item))

# Compare rankings
consistency <- data.frame(
  Primary_Item = primary_ranks$Item,
  Primary_Rank = primary_ranks$Rank,
  Primary_GD = ranking_primary$General_Dominance,
  Secondary_Rank = ranking_secondary$Rank,
  Secondary_GD = ranking_secondary$General_Dominance
)

cat("Consistency of Item Rankings:\n")
print(consistency)

# Spearman correlation of rankings
spearman_corr <- cor(consistency$Primary_Rank, 
                     consistency$Secondary_Rank, 
                     method = "spearman")
cat("\nSpearman Correlation of Rankings:", round(spearman_corr, 4), "\n")

# ============================================================================
# 7. SUMMARY TABLE
# ============================================================================

cat("\n========== SUMMARY: DOMINANCE ANALYSIS RESULTS ==========\n\n")

summary_table <- data.frame(
  Metric = c("R-squared (Primary)", 
             "R-squared (Secondary)", 
             "R-squared (Combined)",
             "Mean GD (Primary)",
             "Mean GD (Secondary)",
             "Ranking Consistency (Spearman)"),
  Value = c(round(summary(model_primary)$r.squared, 4),
           round(summary(model_secondary)$r.squared, 4),
           round(summary(model_combined)$r.squared, 4),
           round(mean(gd_primary), 4),
           round(mean(gd_secondary), 4),
           round(spearman_corr, 4))
)

print(summary_table)

cat("\n========== END OF ANALYSIS ==========\n")
