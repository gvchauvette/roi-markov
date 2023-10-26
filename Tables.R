# Load these packages that are utilized. 
# heemod is the primary package for running the Markov Model
# ggplot2 allows us to plot our data well
library(heemod)
library(ggplot2)

#
#
#
##########  Graphing Percentage Depressed Each Month With CBT Treatment or With No Treatment  ##########################################################################################

# For this table, the wage does not matter, so we arbitrarily set it's value to 5000 
# since it is labelled as i in the reference document
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

# These take the counts of each state during each monthly cycle in the model
NT1 <- get_counts(result)[get_counts(result)[,2]==1 & get_counts(result)[,1]=="NoTreatment",]
NT2 <- get_counts(result)[get_counts(result)[,2]==2 & get_counts(result)[,1]=="NoTreatment",]
NT3 <- get_counts(result)[get_counts(result)[,2]==3 & get_counts(result)[,1]=="NoTreatment",]
NT4 <- get_counts(result)[get_counts(result)[,2]==4 & get_counts(result)[,1]=="NoTreatment",]
NT5 <- get_counts(result)[get_counts(result)[,2]==5 & get_counts(result)[,1]=="NoTreatment",]
NT6 <- get_counts(result)[get_counts(result)[,2]==6 & get_counts(result)[,1]=="NoTreatment",]
NT7 <- get_counts(result)[get_counts(result)[,2]==7 & get_counts(result)[,1]=="NoTreatment",]
NT8 <- get_counts(result)[get_counts(result)[,2]==8 & get_counts(result)[,1]=="NoTreatment",]
NT9 <- get_counts(result)[get_counts(result)[,2]==9 & get_counts(result)[,1]=="NoTreatment",]
NT10 <- get_counts(result)[get_counts(result)[,2]==10 & get_counts(result)[,1]=="NoTreatment",]
NT11 <- get_counts(result)[get_counts(result)[,2]==11 & get_counts(result)[,1]=="NoTreatment",]
NT12 <- get_counts(result)[get_counts(result)[,2]==12 & get_counts(result)[,1]=="NoTreatment",]
NT13 <- get_counts(result)[get_counts(result)[,2]==13 & get_counts(result)[,1]=="NoTreatment",]
NT14 <- get_counts(result)[get_counts(result)[,2]==14 & get_counts(result)[,1]=="NoTreatment",]
NT15 <- get_counts(result)[get_counts(result)[,2]==15 & get_counts(result)[,1]=="NoTreatment",]

T1 <- get_counts(result)[get_counts(result)[,2]==1 & get_counts(result)[,1]=="Treatment",]
T2 <- get_counts(result)[get_counts(result)[,2]==2 & get_counts(result)[,1]=="Treatment",]
T3 <- get_counts(result)[get_counts(result)[,2]==3 & get_counts(result)[,1]=="Treatment",]
T4 <- get_counts(result)[get_counts(result)[,2]==4 & get_counts(result)[,1]=="Treatment",]
T5 <- get_counts(result)[get_counts(result)[,2]==5 & get_counts(result)[,1]=="Treatment",]
T6 <- get_counts(result)[get_counts(result)[,2]==6 & get_counts(result)[,1]=="Treatment",]
T7 <- get_counts(result)[get_counts(result)[,2]==7 & get_counts(result)[,1]=="Treatment",]
T8 <- get_counts(result)[get_counts(result)[,2]==8 & get_counts(result)[,1]=="Treatment",]
T9 <- get_counts(result)[get_counts(result)[,2]==9 & get_counts(result)[,1]=="Treatment",]
T10 <- get_counts(result)[get_counts(result)[,2]==10 & get_counts(result)[,1]=="Treatment",]
T11 <- get_counts(result)[get_counts(result)[,2]==11 & get_counts(result)[,1]=="Treatment",]
T12 <- get_counts(result)[get_counts(result)[,2]==12 & get_counts(result)[,1]=="Treatment",]
T13 <- get_counts(result)[get_counts(result)[,2]==13 & get_counts(result)[,1]=="Treatment",]
T14 <- get_counts(result)[get_counts(result)[,2]==14 & get_counts(result)[,1]=="Treatment",]
T15 <- get_counts(result)[get_counts(result)[,2]==15 & get_counts(result)[,1]=="Treatment",]

##################  MEAN  ###############################
  # In the above model, we use the mean parameter values, the lowest parameter values in
  # the 95% CIs and the highest parameter values in the 95% CIs.
  # We start with the mean values.

### Treatment ###

# Create an empty data frame with 15 rows and 16 columns
mean_treatment_DR_count <- data.frame(matrix(nrow = 7, ncol = 15))

# Set the row names
  rownames(mean_treatment_DR_count) <- c("Month","ARRemission", "AADepression", 
                                         "AARemission", "CDepression", "CRemission", "ARDepression")

# Set the values of each new column to the 4th column of each tibble, which are the state counts
  mean_treatment_DR_count[1, 1:15] <- 1:15
  mean_treatment_DR_count[2:7, 1:15] <- cbind(T1[, 4], T2[, 4], T3[, 4], T4[, 4], 
                                              T5[, 4], T6[, 4], T7[, 4], T8[, 4], T9[, 4], 
                                              T10[, 4], T11[, 4], T12[, 4], T13[, 4], T14[, 4], T15[, 4])

# Making the data long instead of wide
mean_treatment_DR_count <- t(mean_treatment_DR_count)

# Making a new table summing up states to depression and remission
mean_treatment_DR_sum <- data.frame(
  Month = mean_treatment_DR_count[, 1], 
  Column2_Sum = rowSums(mean_treatment_DR_count[, c(2, 4, 6)]), 
  Column3_Sum = rowSums(mean_treatment_DR_count[, c(3, 5, 7)])
)

# Renaming columns accordingly
colnames(mean_treatment_DR_sum) <- c("Month", "Remission", "Depression")

### NoTreatment ###

# Create an empty data frame with 15 rows and 16 columns
mean_notreatment_DR_count <- data.frame(matrix(nrow = 7, ncol = 15))

# Set the row names
rownames(mean_notreatment_DR_count) <- c("Month", "ARRemission", "AADepression", 
                                         "AARemission", "CDepression", "CRemission", "ARDepression")

# Set the values of each new column to the 4th column of each tibble, which are the state counts
mean_notreatment_DR_count[1, 1:15] <- 1:15
mean_notreatment_DR_count[2:7, 1:15] <- cbind(NT1[, 4], NT2[, 4], NT3[, 4], NT4[, 4], 
                                              NT5[, 4], NT6[, 4], NT7[, 4], NT8[, 4], NT9[, 4], 
                                              NT10[, 4], NT11[, 4], NT12[, 4], NT13[, 4], NT14[, 4], NT15[, 4])

# Making the data long instead of wide
mean_notreatment_DR_count <- t(mean_notreatment_DR_count)

# Making a new table summing up depression and remission
mean_notreatment_DR_sum <- data.frame(
  Month = mean_notreatment_DR_count[, 1], 
  Remission = rowSums(mean_notreatment_DR_count[, c(2, 4, 6)]), 
  Depression = rowSums(mean_notreatment_DR_count[, c(3, 5, 7)])
)

# Renaming columns accordingly
colnames(mean_notreatment_DR_sum) <- c("Month", "Remission", "Depression")


##################  LOW  ###############################

### Treatment ###

# Create an empty data frame with 15 rows and 16 columns
low_treatment_DR_count <- data.frame(matrix(nrow = 7, ncol = 15))

# Set the row names
rownames(low_treatment_DR_count) <- c("Month","ARRemission", "AADepression", 
                                      "AARemission", "CDepression", "CRemission", "ARDepression")

# Set the values of each new column to the 4th column of each tibble, which are the state counts
low_treatment_DR_count[1, 1:15] <- 1:15
low_treatment_DR_count[2:7, 1:15] <- cbind(T1[, 4], T2[, 4], T3[, 4], T4[, 4], 
                                           T5[, 4], T6[, 4], T7[, 4], T8[, 4], T9[, 4], 
                                           T10[, 4], T11[, 4], T12[, 4], T13[, 4], T14[, 4], T15[, 4])

# Making the data long instead of wide
low_treatment_DR_count <- t(low_treatment_DR_count)

# Making a new table summing up depression and remission
low_treatment_DR_sum <- data.frame(
  Month = low_treatment_DR_count[, 1], 
  Column2_Sum = rowSums(low_treatment_DR_count[, c(2, 4, 6)]), 
  Column3_Sum = rowSums(low_treatment_DR_count[, c(3, 5, 7)])
)

# Renaming columns accordingly
colnames(low_treatment_DR_sum) <- c("Month", "Remission", "Depression")

### NoTreatment ###

# Create an empty data frame with 15 rows and 16 columns
low_notreatment_DR_count <- data.frame(matrix(nrow = 7, ncol = 15))

# Set the row names
rownames(low_notreatment_DR_count) <- c("Month", "ARRemission", "AADepression", 
                                        "AARemission", "CDepression", "CRemission", "ARDepression")

# Set the values of each new column to the 4th column of each tibble, which are the state counts
low_notreatment_DR_count[1, 1:15] <- 1:15
low_notreatment_DR_count[2:7, 1:15] <- cbind(NT1[, 4], NT2[, 4], NT3[, 4], NT4[, 4], 
                                             NT5[, 4], NT6[, 4], NT7[, 4], NT8[, 4], NT9[, 4], 
                                             NT10[, 4], NT11[, 4], NT12[, 4], NT13[, 4], NT14[, 4], NT15[, 4])

# Making the data long instead of wide
low_notreatment_DR_count <- t(low_notreatment_DR_count)

# Making a new table summing up depression and remission
low_notreatment_DR_sum <- data.frame(
  Month = low_notreatment_DR_count[, 1], 
  Remission = rowSums(low_notreatment_DR_count[, c(2, 4, 6)]), 
  Depression = rowSums(low_notreatment_DR_count[, c(3, 5, 7)])
)

# Renaming columns accordingly
colnames(low_notreatment_DR_sum) <- c("Month", "Remission", "Depression")

##################  HIGH  ###############################

### Treatment ###

# Create an empty data frame with 15 rows and 16 columns
high_treatment_DR_count <- data.frame(matrix(nrow = 7, ncol = 15))

# Set the row names
rownames(high_treatment_DR_count) <- c("Month","ARRemission", "AADepression", 
                                       "AARemission", "CDepression", "CRemission", "ARDepression")

# Set the values of each new column to the 4th column of each tibble, which are the state counts
high_treatment_DR_count[1, 1:15] <- 1:15
high_treatment_DR_count[2:7, 1:15] <- cbind(T1[, 4], T2[, 4], T3[, 4], T4[, 4], 
                                            T5[, 4], T6[, 4], T7[, 4], T8[, 4], T9[, 4], 
                                            T10[, 4], T11[, 4], T12[, 4], T13[, 4], T14[, 4], T15[, 4])

# Making the data long instead of wide
high_treatment_DR_count <- t(high_treatment_DR_count)

# Making a new table summing up depression and remission
high_treatment_DR_sum <- data.frame(
  Month = high_treatment_DR_count[, 1], 
  Column2_Sum = rowSums(high_treatment_DR_count[, c(2, 4, 6)]), 
  Column3_Sum = rowSums(high_treatment_DR_count[, c(3, 5, 7)])
)

# Renaming columns accordingly
colnames(high_treatment_DR_sum) <- c("Month", "Remission", "Depression")

### NoTreatment ###

# Create an empty data frame with 15 rows and 16 columns
high_notreatment_DR_count <- data.frame(matrix(nrow = 7, ncol = 15))

# Set the row names
rownames(high_notreatment_DR_count) <- c("Month", "ARRemission", "AADepression", 
                                         "AARemission", "CDepression", "CRemission", "ARDepression")

# Set the values of each new column to the 4th column of each tibble, which are the state counts
high_notreatment_DR_count[1, 1:15] <- 1:15
high_notreatment_DR_count[2:7, 1:15] <- cbind(NT1[, 4], NT2[, 4], NT3[, 4], NT4[, 4], 
                                              NT5[, 4], NT6[, 4], NT7[, 4], NT8[, 4], NT9[, 4], 
                                              NT10[, 4], NT11[, 4], NT12[, 4], NT13[, 4], NT14[, 4], NT15[, 4])

# Making the data long instead of wide
high_notreatment_DR_count <- t(high_notreatment_DR_count)

# Making a new table summing up depression and remission
high_notreatment_DR_sum <- data.frame(
  Month = high_notreatment_DR_count[, 1], 
  Remission = rowSums(high_notreatment_DR_count[, c(2, 4, 6)]), 
  Depression = rowSums(high_notreatment_DR_count[, c(3, 5, 7)])
)

# Renaming columns accordingly
colnames(high_notreatment_DR_sum) <- c("Month", "Remission", "Depression")


###############  Create Master Tables from Mean, Low, High Tables  ####################

# Creating master_treatment_DR_sum table and dividing values by 10, since 1000
# people were assessed. This gives us 100 people which directly translates to percentages
master_DR_sum <- data.frame(
  Month = mean_treatment_DR_sum$Month,
  Treatment_Depressed_Mean = mean_treatment_DR_sum$Depression/10,
  Treatment_Depressed_Low = low_treatment_DR_sum$Depression/10,
  Treatment_Depressed_High = high_treatment_DR_sum$Depression/10,
  NoTreatment_Depressed_Mean = mean_notreatment_DR_sum$Depression/10,
  NoTreatment_Depressed_Low = low_notreatment_DR_sum$Depression/10,
  NoTreatment_Depressed_High = high_notreatment_DR_sum$Depression/10
)

# Adding a column for the start of the period assessed, Month 0
master_DR_sum <- rbind(
  data.frame(
    Month = 0,
    Treatment_Depressed_Mean = 100,
    Treatment_Depressed_Low = 100,
    Treatment_Depressed_High = 100,
    NoTreatment_Depressed_Mean = 100,
    NoTreatment_Depressed_Low = 100,
    NoTreatment_Depressed_High = 100
  ),
  master_DR_sum
)

# Checking data
print(master_DR_sum)

# Plotting the data
ggplot(data = master_DR_sum) +
  geom_line(aes(x = Month, y = Treatment_Depressed_Mean, color = "CBT Treatment"), size = 1) +
  geom_ribbon(aes(x = Month, ymin = Treatment_Depressed_Low, ymax = Treatment_Depressed_High, fill = "CBT Treatment"), alpha = 0.2) +
  geom_line(aes(x = Month, y = NoTreatment_Depressed_Mean, color = "No Treatment"), size = 1) +
  geom_ribbon(aes(x = Month, ymin = NoTreatment_Depressed_Low, ymax = NoTreatment_Depressed_High, fill = "No Treatment"), alpha = 0.2) +
  labs(x = "Month", y = "Percent Depressed",
       title = "Percentage Depressed Each Month With CBT Treatment or With No Treatment") +
  scale_x_continuous(breaks = seq(0, 15, by = 1), minor_breaks = seq(0, 15, by = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = paste0(seq(0, 100, by = 10), "%")) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold")) +
  geom_text(aes(x = Month, y = Treatment_Depressed_Mean, label = ifelse(Month %in% c(3, 6, 9, 12, 15), paste0(format(round(Treatment_Depressed_Mean, 1), nsmall = 1), "%"), "")),
            vjust = 3, size = 5) +
  geom_text(aes(x = Month, y = NoTreatment_Depressed_Mean, label = ifelse(Month %in% c(3, 6, 9, 12, 15), paste0(format(round(NoTreatment_Depressed_Mean, 1), nsmall = 1), "%"), "")),
            vjust = -3, size = 5) +
  geom_text(aes(x = Month, y = NoTreatment_Depressed_Mean, label = ifelse(Month %in% c(0), paste0(format(round(NoTreatment_Depressed_Mean, 1), nsmall = 1), "%"), "")),
            vjust = -1, size = 5) +
  geom_point(aes(x = Month, y = Treatment_Depressed_Mean, color = "CBT Treatment"), size = 3) +
  geom_point(aes(x = Month, y = NoTreatment_Depressed_Mean, color = "No Treatment"), size = 3) +
  scale_color_manual(values = c("navy", "gold"), guide = guide_legend(title = "Treatment")) +
  scale_fill_manual(values = c("navy", "gold"), guide = guide_legend(title = "Treatment"))
######################################################################################################################################################################################
#
#
#



#
#
#
#############  Average Monthly Cost Reduction for Business for Facilitating CBT Treatment Vs No Treatment By Monthly Wage Per Employee  ##################################################

##################  MEAN  ###############################
  # We use the mean parameter values, the lowest parameter values in
  # the 95% CIs and the highest parameter values in the 95% CIs.
  # We start with the mean values.

wage <- seq(0, 40, by = 10)
ROI_mean <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(ROI_mean) <- c("NoTreatment", "Treatment")

for (i in wage) {

  # Making the model
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
  
  # For each run of the model at a different wage, we are taking the cost of a depressed person who is receiving
  # CBT treatment and those not receiving treatment and adding them to the data frame ROI_mean
  ROI_mean[paste0("Wage $", i),] <- c(result$run_model$cost)
}

# Making a new data frame from ROI_mean that subtracts the cost of treatment from the cost of no treatment
# and divides those values by 1000 to get the costs for a single individual
ROI_mean_difference <- data.frame(monthly_wage = seq(0, 40, by = 10), 
                                  mean_cost_to_business_NoTreatment_minus_Treatment = ROI_mean$Treatment/1000 - ROI_mean$NoTreatment/1000)

##################  LOW  ###############################

wage <- seq(0, 40, by = 10)
ROI_low <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(ROI_low) <- c("NoTreatment", "Treatment")


for (i in wage) {
  
  # Making the model
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
  
  # For each run of the model at a different wage, we are taking the cost of a depressed person who is receiving
  # CBT treatment and those not receiving treatment and adding them to the data frame ROI_mean
  ROI_low[paste0("Wage $", i),] <- c(result$run_model$cost)
}

# Making a new data frame from ROI_mean that subtracts the cost of treatment from the cost of no treatment
# and divides those values by 1000 to get the costs for a single individual
ROI_low_difference <- data.frame(monthly_wage = seq(0, 40, by = 10), 
                                  low_cost_to_business_NoTreatment_minus_Treatment = ROI_low$Treatment/1000 - ROI_low$NoTreatment/1000)


##################  HIGH  ###############################

wage <- seq(0, 40, by = 10)
ROI_high <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(ROI_high) <- c("NoTreatment", "Treatment")


for (i in wage) {
  
  # Making the model
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
  
  # For each run of the model at a different wage, we are taking the cost of a depressed person who is receiving
  # CBT treatment and those not receiving treatment and adding them to the data frame ROI_mean
  ROI_high[paste0("Wage $", i),] <- c(result$run_model$cost)
}

# Making a new data frame from ROI_mean that subtracts the cost of treatment from the cost of no treatment
# and divides those values by 1000 to get the costs for a single individual
ROI_high_difference <- data.frame(monthly_wage = seq(0, 40, by = 10), 
                                 high_cost_to_business_NoTreatment_minus_Treatment = ROI_high$Treatment/1000 - ROI_high$NoTreatment/1000)


###############  Create Master Table from Mean, Low, High Tables  ####################

master_ROI_difference <- data.frame(
  Monthly_Wage = ROI_high_difference$monthly_wage,
  ROI_mean_difference = ROI_mean_difference$mean_cost_to_business_NoTreatment_minus_Treatment/15,
  ROI_low_difference = ROI_low_difference$low_cost_to_business_NoTreatment_minus_Treatment/15,
  ROI_high_difference = ROI_high_difference$high_cost_to_business_NoTreatment_minus_Treatment/15
)

# Using the master table, it became clear that the relationship between wage and cost-reduction was linear.
# With that, 3 functions were made for the mean, low, and high parameter values
ROI_mean_difference_function <- lm(ROI_mean_difference ~ Monthly_Wage, data = master_ROI_difference)
  slope_mean <- coef(ROI_mean_difference_function)["Monthly_Wage"]
  intercept_mean <- coef(ROI_mean_difference_function)["(Intercept)"]
  
ROI_low_difference_function <- lm(ROI_low_difference ~ Monthly_Wage, data = master_ROI_difference)
  slope_low <- coef(ROI_low_difference_function)["Monthly_Wage"]
  intercept_low <- coef(ROI_low_difference_function)["(Intercept)"]

ROI_high_difference_function <- lm(ROI_high_difference ~ Monthly_Wage, data = master_ROI_difference)
  slope_high <- coef(ROI_high_difference_function)["Monthly_Wage"]
  intercept_high <- coef(ROI_high_difference_function)["(Intercept)"]

  # Create the additional rows with values equal to a sequence from 50 to 16000 by 10s in the Monthly_Wage column
  additional_rows <- data.frame(
    Monthly_Wage = seq(50, 16000, by = 10),
    ROI_mean_difference = rep(1, 1596),
    ROI_high_difference = rep(1, 1596),
    ROI_low_difference = rep(1, 1596)
  )
  
  # Combine the original and additional rows
  master_ROI_difference <- rbind(master_ROI_difference, additional_rows)
  
  
  
  # Making a larger data.frame using the same method for the chief executives wage
  master_ROI_difference_ce <- data.frame(
    Monthly_Wage_ce = ROI_high_difference$monthly_wage,
    ROI_mean_difference_ce = ROI_mean_difference$mean_cost_to_business_NoTreatment_minus_Treatment/15,
    ROI_low_difference_ce = ROI_low_difference$low_cost_to_business_NoTreatment_minus_Treatment/15,
    ROI_high_difference_ce = ROI_high_difference$high_cost_to_business_NoTreatment_minus_Treatment/15
  )
  
  ROI_mean_difference_function_ce <- lm(ROI_mean_difference_ce ~ Monthly_Wage_ce, data = master_ROI_difference_ce)
  slope_mean_ce <- coef(ROI_mean_difference_function_ce)["Monthly_Wage_ce"]
  intercept_mean_ce <- coef(ROI_mean_difference_function_ce)["(Intercept)"]
  
  ROI_low_difference_function_ce <- lm(ROI_low_difference_ce ~ Monthly_Wage_ce, data = master_ROI_difference_ce)
  slope_low_ce <- coef(ROI_low_difference_function_ce)["Monthly_Wage_ce"]
  intercept_low_ce <- coef(ROI_low_difference_function_ce)["(Intercept)"]
  
  ROI_high_difference_function_ce <- lm(ROI_high_difference_ce ~ Monthly_Wage_ce, data = master_ROI_difference_ce)
  slope_high_ce <- coef(ROI_high_difference_function_ce)["Monthly_Wage_ce"]
  intercept_high_ce <- coef(ROI_high_difference_function_ce)["(Intercept)"]
  
  additional_rows_ce <- data.frame(
    Monthly_Wage_ce = seq(50, 25000, by = 10),
    ROI_mean_difference_ce = rep(1, 2496),
    ROI_high_difference_ce = rep(1, 2496),
    ROI_low_difference_ce = rep(1, 2496)
  )
  
  master_ROI_difference_ce <- rbind(master_ROI_difference_ce, additional_rows_ce)
  
  
  
# Plotting the data with horizontal line at cost reduction difference equal to 0
  ggplot(master_ROI_difference_ce, aes(x = Monthly_Wage_ce, y = ROI_mean_difference_ce)) +
    geom_segment(aes(x = 0, y = intercept_mean_ce, xend = 25000, yend = intercept_mean_ce + slope_mean_ce * 25000),
                 color = "gold", linewidth = 1.5) +
    geom_ribbon(aes(ymin = intercept_low_ce + slope_low_ce * Monthly_Wage_ce,
                    ymax = intercept_high_ce + slope_high_ce * Monthly_Wage_ce),
                fill = "gold", alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "navy", linewidth = 1) +
    geom_text(aes(x = -intercept_mean / slope_mean, y = 0,
                  label = paste("$", sprintf("%.2f", -intercept_mean / slope_mean))),
              vjust = -1, hjust = .5, color = "navy", size = 6) +
    geom_point(aes(x = -intercept_mean / slope_mean, y = 0), color = "navy", size = 3) +
    geom_point(aes(x = -intercept_low / slope_low, y = 0), color = "navy", size = 3) +
    geom_point(aes(x = -intercept_high / slope_high, y = 0), color = "navy", size = 3) +
    geom_text(aes(x = -intercept_low / slope_low, y = 0,
                  label = paste("$", sprintf("%.2f", -intercept_low / slope_low))),
              vjust = -1, hjust = .5, color = "navy", size = 6) +
    geom_text(aes(x = -intercept_high / slope_high, y = 0,
                  label = paste("$", sprintf("%.2f", -intercept_high / slope_high))),
              vjust = 2, hjust = 1, color = "navy", size = 6) +
    labs(x = "Monthly Wage ($)",
         y = "Average Monthly Cost Reduction for Business for CBT Treatment Vs No Treatment ($)",
         title = "Average Monthly Cost Reduction for Business for Facilitating CBT Treatment Vs No Treatment By Monthly Wage Per Employee") +
    scale_x_continuous(breaks = seq(0, 25000, by = 1000),
                       minor_breaks = seq(0, 25000, by = 1000)) +
    scale_y_continuous(breaks = seq(-2000, 200, by = 100),
                       labels = seq(-2000, 200, by = 100)) +
    theme_minimal() +
    theme(text = element_text(size = 18),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          plot.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"))
  
# Making plot with vertical lines at particular wages
   ggplot(master_ROI_difference_ce, aes(x = Monthly_Wage_ce, y = ROI_mean_difference_ce)) +
     geom_segment(aes(x = 0, y = intercept_mean_ce, xend = 25000, yend = intercept_mean_ce + slope_mean_ce * 25000),
                  color = "gold", linewidth = 1.5) +
     geom_ribbon(aes(ymin = intercept_low_ce + slope_low_ce * Monthly_Wage_ce,
                     ymax = intercept_high_ce + slope_high_ce * Monthly_Wage_ce),
                 fill = "gold", alpha = 0.4) +
     
     geom_vline(xintercept = 1256.67, linetype = "dashed", color = "navy", linewidth = .5, alpha = 0.8) +
     geom_point(aes(x = 1256.67, y = intercept_mean + slope_mean * 1256.67), color = "navy", size = 1, alpha = 0.6) +
     geom_point(aes(x = 1256.67, y = intercept_low + slope_low * 1256.67), color = "navy", size = 1, alpha = 0.6) +
     geom_point(aes(x = 1256.67, y = intercept_high + slope_high * 1256.67), color = "navy", size = 1, alpha = 0.6) +
     annotate("text", x = 1256.67, y = -1200, label = "Federal Minimum Wage",
              vjust = -1, hjust = 1, color = "navy", angle = 90) +
     
     geom_vline(xintercept = 2777.50, linetype = "dashed", color = "maroon", linewidth = .5, alpha = 0.8) +
     geom_point(aes(x = 2777.50, y = intercept_mean + slope_mean * 2777.50), color = "maroon", size = 1, alpha = 0.6) +
     geom_point(aes(x = 2777.50, y = intercept_low + slope_low * 2777.50), color = "maroon", size = 1, alpha = 0.6) +
     geom_point(aes(x = 2777.50, y = intercept_high + slope_high * 2777.50), color = "maroon", size = 1, alpha = 0.6) +
     annotate("text", x = 2777.50, y = -1200, label = "25th Percentile Wage",
              vjust = -1, hjust = 1, color = "maroon", angle = 90) +
     
     
     geom_vline(xintercept = 3859.17, linetype = "dashed", color = "navy", linewidth = .5, alpha = 0.8) +
     geom_point(aes(x = 3859.17, y = intercept_mean + slope_mean * 3859.17), color = "navy", size = 1, alpha = 0.6) +
     geom_point(aes(x = 3859.17, y = intercept_low + slope_low * 3859.17), color = "navy", size = 1, alpha = 0.6) +
     geom_point(aes(x = 3859.17, y = intercept_high + slope_high * 3859.17), color = "navy", size = 1, alpha = 0.6) +
     annotate("text", x = 3859.17, y = -1200, label = "Median Wage",
              vjust = -1, hjust = 1, color = "navy", angle = 90) +
     
     geom_vline(xintercept = 5158.33, linetype = "dashed", color = "maroon", linewidth = .5, alpha = 0.8) +
     geom_point(aes(x = 5158.33, y = intercept_mean + slope_mean * 5158.33), color = "maroon", size = 1, alpha = 0.6) +
     geom_point(aes(x = 5158.33, y = intercept_low + slope_low * 5158.33), color = "maroon", size = 1, alpha = 0.6) +
     geom_point(aes(x = 5158.33, y = intercept_high + slope_high * 5158.33), color = "maroon", size = 1, alpha = 0.6) +
     annotate("text", x = 5158.33, y = -1200, label = "Mean Wage",
              vjust = -1, hjust = 1, color = "maroon", angle = 90) +
     
     geom_vline(xintercept = 6121.67, linetype = "dashed", color = "navy", linewidth = .5, alpha = 0.8) +
     geom_point(aes(x = 6121.67, y = intercept_mean + slope_mean * 6121.67), color = "navy", size = 1, alpha = 0.6) +
     geom_point(aes(x = 6121.67, y = intercept_low + slope_low * 6121.67), color = "navy", size = 1, alpha = 0.6) +
     geom_point(aes(x = 6121.67, y = intercept_high + slope_high * 6121.67), color = "navy", size = 1, alpha = 0.6) +
     annotate("text", x = 6121.67, y = -1200, label = "75th Percentile Wage",
              vjust = -1, hjust = 1, color = "navy", angle = 90) +
     
     geom_vline(xintercept = 9190.83, linetype = "dashed", color = "maroon", linewidth = .5, alpha = 0.8) +
     geom_point(aes(x = 9190.83, y = intercept_mean + slope_mean * 9190.83), color = "maroon", size = 1, alpha = 0.6) +
     geom_point(aes(x = 9190.83, y = intercept_low + slope_low * 9190.83), color = "maroon", size = 1, alpha = 0.6) +
     geom_point(aes(x = 9190.83, y = intercept_high + slope_high * 9190.83), color = "maroon", size = 1, alpha = 0.6) +
     annotate("text", x = 9190.83, y = -1200, label = "95th Percentile (MBA) Wage",
              vjust = -1, hjust = 1, color = "maroon", angle = 90) +

     geom_vline(xintercept = 20536.67, linetype = "dashed", color = "navy", linewidth = .5, alpha = 0.8) +
     geom_point(aes(x = 20536.67, y = intercept_mean_ce + slope_mean_ce * 20536.67), color = "navy", size = 1, alpha = 0.6) +
     geom_point(aes(x = 20536.67, y = intercept_low_ce + slope_low_ce * 20536.67), color = "navy", size = 1, alpha = 0.6) +
     geom_point(aes(x = 20536.67, y = intercept_high_ce + slope_high_ce * 20536.67), color = "navy", size = 1, alpha = 0.6) +
     annotate("text", x = 20536.67, y = -1200, label = "Median Chief Executive Wage",
              vjust = -1, hjust = 1, color = "navy", angle = 90) +

     annotate("text", x = 1256.67, y = intercept_mean + slope_mean * 1256.67, label = paste0(ifelse(intercept_mean + slope_mean * 1256.67 < 0, "-$", "$"), abs(round(intercept_mean + slope_mean * 1256.67, 2))), hjust = 1.2, color = "navy") +
     annotate("text", x = 1256.67, y = intercept_low + slope_low * 1256.67, label = paste0(ifelse(intercept_low + slope_low * 1256.67 < 0, "-$", "$"), abs(round(intercept_low + slope_low * 1256.67, 2))), hjust = 1.2, color = "navy") +
     annotate("text", x = 1256.67, y = intercept_high + slope_high * 1256.67, label = paste0(ifelse(intercept_high + slope_high * 1256.67 < 0, "-$", "$"), abs(round(intercept_high + slope_high * 1256.67, 2))), hjust = 1.2, color = "navy") +
     
     annotate("text", x = 2777.50, y = intercept_mean + slope_mean * 2777.50, label = paste0(ifelse(intercept_mean + slope_mean * 2777.50 < 0, "-$", "$"), abs(round(intercept_mean + slope_mean * 2777.50, 2))), hjust = 1.2, color = "maroon") +
     annotate("text", x = 2777.50, y = intercept_low + slope_low * 2777.50, label = paste0(ifelse(intercept_low + slope_low * 2777.50 < 0, "-$", "$"), abs(round(intercept_low + slope_low * 2777.50, 2))), hjust = 1.2, color = "maroon") +
     annotate("text", x = 2777.50, y = intercept_high + slope_high * 2777.50, label = paste0(ifelse(intercept_high + slope_high * 2777.50 < 0, "-$", "$"), abs(round(intercept_high + slope_high * 2777.50, 2))), hjust = 1.2, color = "maroon") +
     
     annotate("text", x = 3859.17, y = intercept_mean + slope_mean * 3859.17, label = paste0(ifelse(intercept_mean + slope_mean * 3859.17 < 0, "-$", "$"), abs(round(intercept_mean + slope_mean * 3859.17, 2))), hjust = -.2, color = "navy") +
     annotate("text", x = 3859.17, y = intercept_low + slope_low * 3859.17, label = paste0(ifelse(intercept_low + slope_low * 3859.17 < 0, "-$", "$"), abs(round(intercept_low + slope_low * 3859.17, 2))), hjust = -.2, color = "navy") +
     annotate("text", x = 3859.17, y = intercept_high + slope_high * 3859.17, label = paste0(ifelse(intercept_high + slope_high * 3859.17 < 0, "-$", "$"), abs(round(intercept_high + slope_high * 3859.17, 2))), hjust = -.2, color = "navy") +
     
     annotate("text", x = 5158.33, y = intercept_mean + slope_mean * 5158.33, label = paste0(ifelse(intercept_mean + slope_mean * 5158.33 < 0, "-$", "$"), abs(round(intercept_mean + slope_mean * 5158.33, 2))), hjust = -.2, color = "maroon") +
     annotate("text", x = 5158.33, y = intercept_low + slope_low * 5158.33, label = paste0(ifelse(intercept_low + slope_low * 5158.33 < 0, "-$", "$"), abs(round(intercept_low + slope_low * 5158.33, 2))), hjust = -.2, color = "maroon") +
     annotate("text", x = 5158.33, y = intercept_high + slope_high * 5158.33, label = paste0(ifelse(intercept_high + slope_high * 5158.33 < 0, "-$", "$"), abs(round(intercept_high + slope_high * 5158.33, 2))), hjust = -.2, color = "maroon") +
     
     annotate("text", x = 6121.67, y = intercept_mean + slope_mean * 6121.67, label = paste0(ifelse(intercept_mean + slope_mean * 6121.67 < 0, "-$", "$"), abs(round(intercept_mean + slope_mean * 6121.67, 2))), hjust = -.2, color = "navy") +
     annotate("text", x = 6121.67, y = intercept_low + slope_low * 6121.67, label = paste0(ifelse(intercept_low + slope_low * 6121.67 < 0, "-$", "$"), abs(round(intercept_low + slope_low * 6121.67, 2))), hjust = -.2, color = "navy") +
     annotate("text", x = 6121.67, y = intercept_high + slope_high * 6121.67, label = paste0(ifelse(intercept_high + slope_high * 6121.67 < 0, "-$", "$"), abs(round(intercept_high + slope_high * 6121.67, 2))), hjust = -.2, color = "navy") +
     
     annotate("text", x = 9190.83, y = intercept_mean + slope_mean * 9190.83, label = paste0(ifelse(intercept_mean + slope_mean * 9190.83 < 0, "-$", "$"), abs(round(intercept_mean + slope_mean * 9190.83, 2))), hjust = -.2, color = "maroon") +
     annotate("text", x = 9190.83, y = intercept_low + slope_low * 9190.83, label = paste0(ifelse(intercept_low + slope_low * 9190.83 < 0, "-$", "$"), abs(round(intercept_low + slope_low * 9190.83, 2))), hjust = -.2, color = "maroon") +
     annotate("text", x = 9190.83, y = intercept_high + slope_high * 9190.83, label = paste0(ifelse(intercept_high + slope_high * 9190.83 < 0, "-$", "$"), abs(round(intercept_high + slope_high * 9190.83, 2))), hjust = -.2, color = "maroon") +
     
     annotate("text", x = 20536.67, y = intercept_mean_ce + slope_mean_ce * 20536.67, label = paste0(ifelse(intercept_mean_ce + slope_mean_ce * 20536.67 < 0, "-$", "$"), abs(round(intercept_mean_ce + slope_mean_ce * 20536.67, 2))), hjust = -.2, color = "navy") +
     annotate("text", x = 20536.67, y = intercept_low_ce + slope_low_ce * 20536.67, label = paste0(ifelse(intercept_low_ce + slope_low_ce * 20536.67 < 0, "-$", "$"), abs(round(intercept_low_ce + slope_low_ce * 20536.67, 2))), hjust = -.2, color = "navy") +
     annotate("text", x = 20536.67, y = intercept_high_ce + slope_high_ce * 20536.67, label = paste0(ifelse(intercept_high_ce + slope_high_ce * 20536.67 < 0, "-$", "$"), abs(round(intercept_high_ce + slope_high_ce * 20536.67, 2))), hjust = -.2, color = "navy") +
     labs(x = "Monthly Wage ($)",
          y = "Average Monthly Cost Reduction for Business for CBT Treatment Vs No Treatment ($)",
          title = "Average Monthly Cost Reduction for Business for Facilitating CBT Treatment Vs No Treatment By Monthly Wage Per Employee") +
     scale_x_continuous(breaks = seq(0, 25000, by = 1000),
                        minor_breaks = seq(0, 25000, by = 1000)) +
     scale_y_continuous(breaks = seq(-2000, 200, by = 100),
                        labels = seq(-2000, 200, by = 100)) +
     theme_minimal() +
     theme(text = element_text(size = 18),
           legend.title = element_blank(),
           legend.text = element_text(size = 18),
           plot.title = element_text(size = 18, face = "bold"),
           axis.title = element_text(size = 14, face = "bold"))
  
###############################################################################################################################################################################
#
#
#

  
  
#
#
#
#####################  Average Cost to Business Each Month for Facilitating CBT Treatment Vs No Treatment  ####################################################################################################################
  
##################  MEAN  ###############################
  # We use the mean parameter values, the lowest parameter values in
  # the 95% CIs and the highest parameter values in the 95% CIs.
  # We start with the mean values.

# choose the wage here
i <- 3859.17

# Running the model
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

# The easiest way that I found to store obtain this data is to use a special plot that is
# part of the heemod package and then extract the values from the plot itself and put it into 
# the data frame monthly_costs_mean.

# So first we plot the data
line_plot <- plot(result, type = "values", panel = "by_value", free_y = TRUE) +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )

# Then we extract the data from the plot
line_data <- ggplot_build(line_plot)$data[[1]]

# and put it in a format ready to construct a data frame and divide the costs by 1000 to give us costs per person
x <- 1:15
no_treatment <- line_data$y[1:15] / 1000
treatment <- line_data$y[16:30] / 1000

# Create data frame
monthly_costs_mean <- data.frame(Cycle = x, NoTreatment_mean = no_treatment, Treatment_mean = treatment)


##################  LOW  ############################### 

# Running the model
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

# The easiest way that I found to store obtain this data is to use a special plot that is
# part of the heemod package and then extract the values from the plot itself and put it into 
# the data frame monthly_costs_mean.

# So first we plot the data
line_plot <- plot(result, type = "values", panel = "by_value", free_y = TRUE) +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )

# Then we extract the data from the plot
line_data <- ggplot_build(line_plot)$data[[1]]

# and put it in a format ready to construct a data frame and divide the costs by 1000 to give us costs per person
x <- 1:15
no_treatment <- line_data$y[1:15] / 1000
treatment <- line_data$y[16:30] / 1000

# Create dataframe
monthly_costs_low <- data.frame(Cycle = x, NoTreatment_low = no_treatment, Treatment_low = treatment)


##################  HIGH  ###############################

# Running the model
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

# The easiest way that I found to store obtain this data is to use a special plot that is
# part of the heemod package and then extract the values from the plot itself and put it into 
# the data frame monthly_costs_mean.

# So first we plot the data
line_plot <- plot(result, type = "values", panel = "by_value", free_y = TRUE) +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )

# Then we extract the data from the plot
line_data <- ggplot_build(line_plot)$data[[1]]

# and put it in a format ready to construct a data frame and divide the costs by 1000 to give us costs per person
x <- 1:15
no_treatment <- line_data$y[1:15] / 1000
treatment <- line_data$y[16:30] / 1000

# Create dataframe
monthly_costs_high <- data.frame(Cycle = x, NoTreatment_high = no_treatment, Treatment_high = treatment)


###############  Create Master Table from Mean, Low, High Tables  ####################

master_monthly_costs_ce <- data.frame(
  Month = monthly_costs_mean$Cycle,
  NoTreatment_mean = monthly_costs_mean$NoTreatment_mean,
  Treatment_mean = monthly_costs_mean$Treatment_mean,
  NoTreatment_low = monthly_costs_low$NoTreatment_low,
  Treatment_low = monthly_costs_low$Treatment_low,
  NoTreatment_high = monthly_costs_high$NoTreatment_high,
  Treatment_high = monthly_costs_high$Treatment_high
)

# Plotting the data

ggplot(data = master_monthly_costs_90thp) +
  geom_line(aes(x = Month, y = NoTreatment_mean, color = "No Treatment"), show.legend = TRUE) +
  geom_ribbon(aes(x = Month, ymin = NoTreatment_low, ymax = NoTreatment_high, fill = "No Treatment"), alpha = 0.2, show.legend = TRUE) +
  geom_line(aes(x = Month, y = Treatment_mean, color = "CBT Treatment"), show.legend = TRUE) +
  geom_ribbon(aes(x = Month, ymin = Treatment_low, ymax = Treatment_high, fill = "CBT Treatment"), alpha = 0.2, show.legend = TRUE) +
  geom_text(aes(x = Month, y = NoTreatment_mean, label = ifelse(Month %in% c(6, 9, 12, 15), paste0("$", format(round(NoTreatment_mean, 1), nsmall = 2)), "")),
            vjust = -3, size = 5) +
  geom_text(aes(x = Month, y = Treatment_mean, label = ifelse(Month %in% c(6, 9, 12, 15), paste0("$", format(round(Treatment_mean, 1), nsmall = 2)), "")),
            vjust = 3, size = 5) +
  geom_text(aes(x = Month, y = Treatment_mean, label = ifelse(Month %in% c(3), paste0("$", format(round(Treatment_mean, 1), nsmall = 2)), "")),
            vjust = 3, size = 5) +
  geom_text(aes(x = Month, y = NoTreatment_mean, label = ifelse(Month %in% c(3), paste0("$", format(round(NoTreatment_mean, 1), nsmall = 2)), "")),
            vjust = -3, size = 5) +
  geom_point(aes(x = Month, y = Treatment_mean, color = "CBT Treatment"), size = 3) +
  geom_point(aes(x = Month, y = NoTreatment_mean, color = "No Treatment"), size = 3) +
  labs(x = "Month",
       y = "Average Cost to Business Each Month ($)",
       title = "Average Cost to Business Each Month for Facilitating CBT Treatment Vs No Treatment") +
  scale_color_manual(values = c("navy", "gold"), guide = guide_legend(title = "Treatment")) +
  scale_fill_manual(values = c("navy", "gold"), guide = guide_legend(title = "Treatment")) +
  scale_x_continuous(breaks = 1:15,
                     minor_breaks = 1:15) +
  scale_y_continuous(breaks = seq(0, 13000, by = 500),
                     labels = seq(0, 13000, by = 500)) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))
###############################################################################################################################################################################
#
#
#



#
#
#
##################### Plot Showing Which Parameters Had the Greatest Affects on the Results  ################################################################################
plot(
  table_mod$dsa,
  type = "difference",
  shorten_labels = TRUE,
  result = "cost"
)