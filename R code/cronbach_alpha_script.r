################################################################################
# Cronbach's Alpha Analysis for Genetic Knowledge Confidence Scale
#
# Description: Calculates internal consistency (Cronbach's alpha) for the 
#              6-item Genetic Knowledge Confidence scale used in genetic 
#              literacy research
#
# Author: [Your Name]
# Date: September 2025
# R Version: [Your R Version]
#
# Required packages: psych
#
# Data: N = 4,058 participants from SPARK and general population cohorts
#       Scale: 6 items measuring confidence in genetic knowledge
#       Response format: 5-point Likert scale
################################################################################

# Load required libraries ------------------------------------------------------
library(psych)

# Load data --------------------------------------------------------------------
# Note: Replace with your actual file path
# Option 1: CSV file
# GKC_data_for_Cronbachalpha <- read.csv("path/to/your/GKC_data_for_Cronbachalpha.csv")

# Option 2: Excel file (requires readxl package)
# library(readxl)
# GKC_data_for_Cronbachalpha <- read_excel("path/to/your/GKC_data_for_Cronbachalpha.xlsx")

# Option 3: RDS file (if saved from R)
# GKC_data_for_Cronbachalpha <- readRDS("path/to/your/GKC_data_for_Cronbachalpha.rds")

# Select confidence items ------------------------------------------------------
confidence_items <- GKC_data_for_Cronbachalpha[, c("confidence_1", "confidence_2", 
                                                     "confidence_3", "confidence_4", 
                                                     "confidence_5", "confidence_6")]

# Check data quality -----------------------------------------------------------
cat("========== DATA QUALITY CHECK ==========\n")
cat("Total sample size:", nrow(confidence_items), "\n")
cat("\nMissing values per item:\n")
print(sapply(confidence_items, function(x) sum(is.na(x))))
cat("\nComplete cases:", sum(complete.cases(confidence_items)), 
    "out of", nrow(confidence_items), "\n")

# Calculate Cronbach's alpha ---------------------------------------------------
alpha_result <- alpha(confidence_items)

# Display full results ---------------------------------------------------------
cat("\n========== FULL RELIABILITY ANALYSIS ==========\n")
print(alpha_result)

# Summary statistics -----------------------------------------------------------
cat("\n========== SUMMARY STATISTICS ==========\n")
cat("Cronbach's Alpha:", round(alpha_result$total$raw_alpha, 3), "\n")
cat("Standardized Alpha:", round(alpha_result$total$std.alpha, 3), "\n")
cat("95% CI Lower:", round(alpha_result$feldt$lower.ci, 3), "\n")
cat("95% CI Upper:", round(alpha_result$feldt$upper.ci, 3), "\n")
cat("Average inter-item correlation:", round(alpha_result$total$average_r, 3), "\n")
cat("Scale mean:", round(alpha_result$total$mean, 2), "\n")
cat("Scale SD:", round(alpha_result$total$sd, 2), "\n")

# Reliability if item dropped --------------------------------------------------
cat("\n========== RELIABILITY IF ITEM DROPPED ==========\n")
cat("Shows how alpha would change if each item were removed:\n\n")
item_drop_table <- round(alpha_result$alpha.drop[, c("raw_alpha", "std.alpha")], 3)
print(item_drop_table)

# Item-total correlations ------------------------------------------------------
cat("\n========== ITEM-TOTAL CORRELATIONS ==========\n")
cat("All correlations should be > 0.30 for acceptable items:\n\n")
item_stats <- round(alpha_result$item.stats[, c("r.cor", "r.drop", "mean", "sd")], 3)
print(item_stats)

# Generate statement for manuscript -------------------------------------------
cat("\n========== FOR YOUR MANUSCRIPT ==========\n")
cat("Methods section text:\n")
cat("The Genetic Knowledge Confidence scale (6 items) demonstrated",
    ifelse(alpha_result$total$raw_alpha >= 0.9, "excellent",
           ifelse(alpha_result$total$raw_alpha >= 0.8, "good",
                  ifelse(alpha_result$total$raw_alpha >= 0.7, "acceptable", 
                         "questionable"))),
    "internal consistency (Cronbach's α =", 
    sprintf("%.2f", alpha_result$total$raw_alpha),
    ", 95% CI [",
    sprintf("%.2f", alpha_result$feldt$lower.ci),
    ",",
    sprintf("%.2f", alpha_result$feldt$upper.ci),
    "]).\n")

# Optional: Analyze by cohort --------------------------------------------------
if("cohort" %in% names(GKC_data_for_Cronbachalpha)) {
  cat("\n========== RELIABILITY BY COHORT ==========\n")
  cohorts <- unique(GKC_data_for_Cronbachalpha$cohort)
  
  for(c in cohorts) {
    subset_data <- confidence_items[GKC_data_for_Cronbachalpha$cohort == c, ]
    
    if(nrow(subset_data) > 10) {  # Only calculate if sufficient sample size
      alpha_subset <- alpha(subset_data, warnings = FALSE)
      cat(c, "cohort:\n")
      cat("  N =", nrow(subset_data), "\n")
      cat("  Cronbach's α =", sprintf("%.3f", alpha_subset$total$raw_alpha), "\n")
      cat("  95% CI: [", sprintf("%.3f", alpha_subset$feldt$lower.ci),
          ",", sprintf("%.3f", alpha_subset$feldt$upper.ci), "]\n\n")
    }
  }
}

# Export results for supplementary materials ----------------------------------
# Create summary table
summary_table <- data.frame(
  Metric = c("Cronbach's Alpha", 
             "Standardized Alpha",
             "95% CI Lower",
             "95% CI Upper",
             "Average Inter-item r",
             "N (complete cases)"),
  Value = c(round(alpha_result$total$raw_alpha, 3),
            round(alpha_result$total$std.alpha, 3),
            round(alpha_result$feldt$lower.ci, 3),
            round(alpha_result$feldt$upper.ci, 3),
            round(alpha_result$total$average_r, 3),
            sum(complete.cases(confidence_items)))
)

# Save results
write.csv(summary_table, "GKC_reliability_summary.csv", row.names = FALSE)
write.csv(item_stats, "GKC_item_statistics.csv", row.names = TRUE)
write.csv(item_drop_table, "GKC_reliability_if_dropped.csv", row.names = TRUE)

cat("\n========== FILES SAVED ==========\n")
cat("Results exported to:\n")
cat("  - GKC_reliability_summary.csv\n")
cat("  - GKC_item_statistics.csv\n")
cat("  - GKC_reliability_if_dropped.csv\n")

# Session info for reproducibility --------------------------------------------
cat("\n========== SESSION INFO ==========\n")
sessionInfo()