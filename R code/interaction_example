combined_sub <- combined %>%
  filter(!if_any(c(popl, edgrpl, religiosity_cat, Religion1, polphill2, GL1, GL2, GL3), ~ is.na(.x)))

all_coefs2 <- c("Intercept" = "(Intercept)",
               "Population - SPARK" = "poplSPARK", 
               "Education - High School" = "edgrplHigh School",
               "Education - Undergraduate" = "edgrplUndergraduate", 
               "Education - Graduate" = "edgrplGraduate", 
               "Religious" = "Religion1Religious", 
               "Religiosity - Religious, actively practicing" = "religiosity_catReligious, actively practicing", 
               "Religiosity - Religious, not actively practicing" = "religiosity_catReligious, not actively practicing", 
               "Political Ideology - Conservative" = "polphill2Conservative",
               "Political Ideology - Liberal" = "polphill2Liberal")

use_coefs <- c("Intercept", "Population - SPARK", 
               "Education - High School", "Education - Undergraduate", "Education - Graduate",
               "Religiosity - Religious, actively practicing", "Religiosity - Religious, not actively practicing", 
               "Political Ideology - Conservative", "Political Ideology - Liberal")

reslist <- lapply(c("GL1", "GL2", "GL3"), function(y){
  
  full_model <- lm(get(y) ~ popl + edgrpl + religiosity_cat + polphill2, data=combined_sub)
  #summary(full_model)
  
  polphil_only <- lm(get(y) ~ popl + edgrpl + polphill2, data=combined_sub)
  #summary(polphil_only)
  
  relig_only <- lm(get(y) ~ popl + edgrpl + religiosity_cat, data=combined_sub)
  #summary(relig_only)
  
  res <- export_summs(full_model, polphil_only, relig_only, 
                 error_format = "({std.error}) p: {p.value}",
                 number_format = list(pretty), #"%.3f",
                 error_pos = "same", 
                 ci_level = 0.95,
                 statistics = c(N = "nobs", 
                                R2 = "r.squared", 
                                "Overall F statistic" = "statistic", 
                                "Overall F p-value" = "p.value"),
                 model.names = c("Full Model", "Political Ideology Only", "Religiosity Only"), 
                 coefs = all_coefs2[use_coefs], 
                 to.file = "docx", 
                 file.name = paste0(out_dir, "Tab_", y, "_religiosity_political.docx"))
  
  return(res)
})

h4("Familiarity Score")