### Analyzing the absenteeism & presenteeism data using metafor package ###
library(metafor)

# Perform meta-analysis for Absenteeism
absenteeism_results <- rma(yi = absenteeism_presenteeism$absenteeism_mean,
                           sei = absenteeism_presenteeism$absenteeism_se,
                           data = absenteeism_presenteeism)

# Perform meta-analysis for Presenteeism
presenteeism_results <- rma(yi = absenteeism_presenteeism$presenteeism_mean,
                            sei = absenteeism_presenteeism$presenteeism_se,
                            data = absenteeism_presenteeism)

# Extract confidence intervals for Absenteeism & Presenteeism
absenteeism_ci <- confint(absenteeism_results)
presenteeism_ci <- confint(presenteeism_results)

# Extract numeric confidence intervals for Absenteeism & Presenteeism
absenteeism_ci_numeric <- as.numeric(absenteeism_ci$random[2, c("ci.lb", "ci.ub")])
presenteeism_ci_numeric <- as.numeric(presenteeism_ci$random[2, c("ci.lb", "ci.ub")])

# Print the results
cat("Absenteeism: Mean =", round(absenteeism_results$b, 2), ", 95% CI =", 
    round(absenteeism_ci_numeric[1], 2), "-", round(absenteeism_ci_numeric[2], 2), "\n")
cat("Presenteeism: Mean =", round(presenteeism_results$b, 2), ", 95% CI =", 
    round(presenteeism_ci_numeric[1], 2), "-", round(presenteeism_ci_numeric[2], 2), "\n")