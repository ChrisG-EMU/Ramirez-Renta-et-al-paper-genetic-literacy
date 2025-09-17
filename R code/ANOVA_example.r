run_models_education_interact <- function(var, lab, tablelab){
  m1 <- lm(GL1 ~ popl + edgrpl + get(var) + edgrpl*get(var), data=combined) #, contrasts = list(edgrpl = "contr.sum", `get(var)` = "contr.sum")) 
  m2 <- lm(GL2 ~ popl + edgrpl + get(var) + edgrpl*get(var), data=combined) #, contrasts = list(edgrpl = "contr.sum", `get(var)` = "contr.sum"))
  m3 <- lm(GL3 ~ popl + edgrpl + get(var) + edgrpl*get(var), data=combined) #, contrasts = list(edgrpl = "contr.sum", `get(var)` = "contr.sum"))
  
  use_coefs <- c("Intercept", "Population - SPARK", 
                 "Education - High School", "Education - Undergraduate", "Education - Graduate",
                 lab, paste(c("Education - High School", "Education - Undergraduate", "Education - Graduate"), lab, sep="*"))
  
  res <- export_summs(m1, m2, m3, 
               error_format = "({std.error}) p: {p.value}",
               number_format = list(pretty), 
               error_pos = "same", 
               ci_level = 0.95,
               statistics = c(N = "nobs", 
                              R2 = "r.squared", 
                              "Overall F statistic" = "statistic", 
                              "Overall F p-value" = "p.value"),
               model.names = c("Familiarity Score", "Knowledge Score", "Skills Score"), 
               coefs = all_coefs[use_coefs], 
               to.file = "docx", 
               file.name = paste0(out_dir, "Tab_interaction_education_", var, ".docx"))

  # ANOVA to test for interactions
  # Type II follows marginality (effect after all other effects but ignores higher-order relatives)
  # Type III violates marginality (effect after all other effects - including interaction terms)
  # Should not be a difference in Type II and Type III for the interaction effects, but will differ for main effects.
  # Type III will match results from usual lm outputs testing each individual coefficient, but should not interpret main effect results
  a1 <- Anova(m1, type="II")
  a2 <- Anova(m2, type="II")
  a3 <- Anova(m3, type="II")
  
  coefs <- c("popl" = "Population", 
             "edgrpl" = "Education Level", 
             "get(var)" = tablelab,
             "edgrpl:get(var)" = paste0("Education Level*", tablelab))
  
  tab <- cbind(coefs[rownames(a1)], pretty(a1$`Pr(>F)`), pretty(a2$`Pr(>F)`), pretty(a3$`Pr(>F)`))

  
  kable_out <- kable(tab[1:4,], align="lccc", format = "html", row.names = F, 
        col.names = c("Variable", "Familiarity Score", "Knowledge Score", "Skills Score")) %>%
    kable_styling()
  
  return(list(res=res, kable_out=kable_out))
}