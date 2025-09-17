# regression_tables.R
# Creates Word tables of regression results for GL1, GL2, GL3 and
# runs Tukey-adjusted label mapping for factors chosen via `var`.

# ---- Setup ----
# Required package: jtools (for export_summs)
if (!requireNamespace("jtools", quietly = TRUE)) {
  stop("Package 'jtools' is required. Install with install.packages('jtools').")
}
library(jtools)

# Set your output directory for .docx tables
out_dir <- file.path("statistics", "")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Expect a data.frame `combined` in your environment with columns:
# GL1, GL2, GL3, popl, edgrpl, and one of: relig (affiliation), religiosity, polphill2, etc.
# Example (commented):
# combined <- read.csv("path/to/combined.csv")

# ---- Helpers ----
pretty <- function(p){
  # Formats p-values: <0.001 if small; otherwise 3 decimals
  ifelse(p >= 0 & p < 0.001, "<0.001", format(round(p, digits = 3), nsmall = 3))
}

# Master coefficient-name map with placeholders "get(var)" for the variable you pass to run_models()
all_coefs <- c(
  "Intercept" = "(Intercept)",
  "Population - SPARK" = "poplSPARK",
  "Education - High School" = "edgrplHigh School",
  "Education - Undergraduate" = "edgrplUndergraduate",
  "Education - Graduate" = "edgrplGraduate",

  # Single-factor levels (these lines will have "get(var)" replaced with the actual var name)
  "Religious" = "get(var)Religious",
  "Religiosity - Religious, actively practicing" = "get(var)Religious, actively practicing",
  "Religiosity - Religious, not actively practicing" = "get(var)Religious, not actively practicing",
  "Science" = "get(var)Science",
  "Political Ideology - Conservative" = "get(var)Conservative",
  "Political Ideology - Liberal" = "get(var)Liberal",
  "Perceived Gene Importance" = "get(var)",
  "Important to Know" = "get(var)",
  "Gene Scenario Confidence" = "get(var)",
  "Gene Knowledge Confidence" = "get(var)",

  # Interactions with Education (also replaced)
  "Education - High School*Religious" = "edgrplHigh School:get(var)Religious",
  "Education - Undergraduate*Religious" = "edgrplUndergraduate:get(var)Religious",
  "Education - Graduate*Religious" = "edgrplGraduate:get(var)Religious",

  "Education - High School*Religiosity - Religious, not actively practicing" =
    "edgrplHigh School:get(var)Religious, not actively practicing",
  "Education - High School*Religiosity - Religious, actively practicing" =
    "edgrplHigh School:get(var)Religious, actively practicing",
  "Education - Undergraduate*Religiosity - Religious, not actively practicing" =
    "edgrplUndergraduate:get(var)Religious, not actively practicing",
  "Education - Undergraduate*Religiosity - Religious, actively practicing" =
    "edgrplUndergraduate:get(var)Religious, actively practicing",
  "Education - Graduate*Religiosity - Religious, not actively practicing" =
    "edgrplGraduate:get(var)Religious, not actively practicing",
  "Education - Graduate*Religiosity - Religious, actively practicing" =
    "edgrplGraduate:get(var)Religious, actively practicing",

  "Education - High School*Political Ideology - Conservative" =
    "edgrplHigh School:get(var)Conservative",
  "Education - High School*Political Ideology - Liberal" =
    "edgrplHigh School:get(var)Liberal",
  "Education - Undergraduate*Political Ideology - Conservative" =
    "edgrplUndergraduate:get(var)Conservative",
  "Education - Undergraduate*Political Ideology - Liberal" =
    "edgrplUndergraduate:get(var)Liberal",
  "Education - Graduate*Political Ideology - Conservative" =
    "edgrplGraduate:get(var)Conservative",
  "Education - Graduate*Political Ideology - Liberal" =
    "edgrplGraduate:get(var)Liberal"
)

# Replace "get(var)" with the actual variable name in the coefficient map
expand_coef_map <- function(var, labels_needed) {
  # var should be the *column name* in `combined` as a string
  # e.g., "relig", "religiosity", "polphill2", etc.
  cm <- all_coefs
  cm <- gsub("get\\(var\\)", var, cm, fixed = FALSE)
  cm[labels_needed]
}

# ---- Main function ----
# var: string name of the predictor to add (e.g., "relig", "religiosity", "polphill2")
# lab: character vector of labels to include from all_coefs for that var (e.g., c("Religious") or the factor level labels)
run_models <- function(var, lab){

  if (var == "popl") {
    m1 <- lm(GL1 ~ popl, data = combined)
    m2 <- lm(GL2 ~ popl, data = combined)
    m3 <- lm(GL3 ~ popl, data = combined)
    use_coefs <- c("Intercept", "Population - SPARK")

    coefs_map <- all_coefs[use_coefs]

  } else if (var == "edgrpl") {
    m1 <- lm(GL1 ~ popl + edgrpl, data = combined)
    m2 <- lm(GL2 ~ popl + edgrpl, data = combined)
    m3 <- lm(GL3 ~ popl + edgrpl, data = combined)
    use_coefs <- c("Intercept", "Population - SPARK",
                   "Education - High School", "Education - Undergraduate", "Education - Graduate")

    coefs_map <- all_coefs[use_coefs]

  } else {
    # NOTE: using as.formula to safely insert the variable name into the model
    f1 <- as.formula(paste0("GL1 ~ popl + edgrpl + ", var))
    f2 <- as.formula(paste0("GL2 ~ popl + edgrpl + ", var))
    f3 <- as.formula(paste0("GL3 ~ popl + edgrpl + ", var))

    m1 <- lm(f1, data = combined)
    m2 <- lm(f2, data = combined)
    m3 <- lm(f3, data = combined)

    use_coefs <- c("Intercept", "Population - SPARK",
                   "Education - High School", "Education - Undergraduate", "Education - Graduate",
                   lab)

    coefs_map <- expand_coef_map(var, use_coefs)
  }

  # Export .docx table with jtools::export_summs
  res <- jtools::export_summs(
    m1, m2, m3,
    error_format  = "({std.error}) p: {p.value}",
    number_format = list(pretty),
    error_pos     = "same",
    ci_level      = 0.95,
    statistics    = c(
      N = "nobs",
      R2 = "r.squared",
      "Overall F statistic" = "statistic",
      "Overall F p-value"   = "p.value"
    ),
    model.names = c("Familiarity Score", "Knowledge Score", "Skills Score"),
    coefs       = coefs_map,
    to.file     = "docx",
    file.name   = paste0(out_dir, "Tab_", var, ".docx")
  )

  return(res)
}

# ---- Example usage (uncomment and adapt) ----
# run_models("popl", lab = character(0))
# run_models("edgrpl", lab = character(0))
# run_models("relig", lab = c("Religious"))
# run_models("religiosity",
#            lab = c("Religiosity - Religious, actively practicing",
#                    "Religiosity - Religious, not actively practicing"))
# run_models("polphill2",
#            lab = c("Political Ideology - Conservative",
#                    "Political Ideology - Liberal"))
