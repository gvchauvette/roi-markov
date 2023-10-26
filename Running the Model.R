# Load these packages that are utilized. heemod is the primary package for running the Markov Model
library(heemod)

# Run the Markov model from .csv files - location should be set to drive where files are located, make sure file names match
# This is a function in heemod that runs a model from tabular input. It looks at the reference file
# which contains two columns, one for data (state, tm, parameters, output, options) and one for 
# the corresponding file locations for that data to use to make the model. 

#The Wage is labelled as i in the reference document so that we can easily change its value during analysis
i <- 5000

# Make the model
table_mod <- run_model_tabular(
  location = "/Users/tatet/Desktop/R Directory",
  reference = "REFERENCE5.28.23.csv",
  run_dsa = TRUE,      #Deterministic Sensitivity Analysis
  run_psa = FALSE,     #Probabilistic Sensitivity Analysis
  run_demo = FALSE,
  save = FALSE,
  overwrite = FALSE
)

# With the model made, the run_model() function runs strategies in that model
# Notably, strategies (here NoTreatment vs Treatment) have the same state and state value names
# but differ through transition probabilities between states and values tied to states
result <- run_model(
  NoTreatment = table_mod$models$NoTreatment,
  Treatment   = table_mod$models$Treatment,
  parameters = table_mod$model_runs$parameters, #parameters are pulled 
  method = "end",
  cycles = 15,
  cost = cost,
  effect = utility
)

# Print summary of the results
summary(result)