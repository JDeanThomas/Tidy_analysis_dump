ncbirths_complete_habit <- ncbirths %>%
  filter(!is.na(habit))
diff_mean_obs <- ncbirths_complete_habit %>%
  group_by(habit) %>%
  summarize(mean_weight = mean(weight)) %>%
  pull() %>%
  diff()
n_replicates <- 1000

diff_mean_ht <- ncbirths_complete_habit %>% 
  specify(weight ~ habit) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = n_replicates, type = "permute") %>%
  calculate(stat = "diff in means", order = c("nonsmoker", "smoker")) 
  
  
diff_mean_ht %>%
  # Identify simulated test statistics at least as extreme as observed
  filter(stat <= diff_mean_obs) %>%
  # Calculate p-value
  summarize(
    one_sided_p_val = n() / n_replicates,
    two_sided_p_val = 2 * one_sided_p_val
  )
  

# Calculate the 98% CI via percentile method
diff_med_ci %>%
summarize(
  l = quantile(stat, .01),
  u = quantile(stat, .99)
)