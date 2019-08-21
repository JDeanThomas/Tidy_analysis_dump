ex1_props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(stat = mean(vote == "yes"))
ex2_props <- all_polls %>%
  filter(poll == 1) %>%
  select(vote) %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")
  
# Calculate variability of p-hat
ex1_props %>% 
  summarize(variability = sd(stat))
  
# Calculate variability of p-hat*
ex2_props %>% 
  summarize(variability = sd(stat))
  

# Combine data from both experiments
both_ex_props <- bind_rows(ex1_props, ex2_props, .id = "experiment")

# Using both_ex_props, plot stat colored by experiment
ggplot(both_ex_props, aes(stat, color = experiment)) + 
# Add a density layer with bandwidth 0.1
geom_density(bw = 0.1)


# Proportion of yes votes by poll
props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote == "yes"))

# The true population proportion of yes votes
true_prop_yes <- 0.6

# Proportion of polls within 2SE
props %>%
  # Add column: is prop_yes in 2SE of 0.6
  mutate(is_in_conf_int = abs(prop_yes - true_prop_yes) < 2 * sd(prop_yes)) %>%
  # Calculate  proportion in conf int
  summarize(prop_in_conf_int = mean(is_in_conf_int))

  
p_hat <- one_poll %>%
  # Calculate proportion of yes votes
  summarize(stat <- mean(vote == 'yes')) %>%
  pull()

# Create an interval of plausible values
one_poll_boot %>%
  summarize(
    # Lower bound is p_hat minus 2 std errs
    lower = p_hat - 2 * sd(stat),
    # Upper bound is p_hat plus 2 std errs
    upper = p_hat + 2 * sd(stat)
  )

  
# Bootstrap t-confidence interval
one_poll_boot %>%
summarize(
  lower = p_hat - 2 * sd(stat),
  upper = p_hat + 2 * sd(stat)
)

# Manually calculate a 95% percentile interval
one_poll_boot %>%
summarize(
  lower = mean(stat, p = .95),
  upper = mean(stat, p = .025)
)


one_poll_boot %>%
  summarize(
    lower <- quantile(stat, 0.025),
    upper <- quantile(stat, 0.975)
  )
  
# Calculate the same interval, using infer package
percentile_ci <- one_poll_boot %>% 
  get_confidence_interval(level=.95)
  
percentile_ci


one_poll_boot %>% 
  # Visualize in-between the endpoints given by percentile_ci
  visualize(endpoints=percentile_ci, direction='between')
  
  
calc_t_conf_int <- function(resampled_dataset) {
resampled_dataset %>%
  summarize(
    lower = p_hat - 2 * sd(stat),
    upper = p_hat + 2 * sd(stat)
  )
}

# Bootstrap t-confidence interval for 300 resamples
calc_t_conf_int(one_poll_boot)

calc_p_hat <- function(dataset) {
  dataset %>%
    summarize(stat = mean(vote == "yes")) %>%
    pull()
}

calc_t_conf_int <- function(resampled_dataset, p_hat) {
  resampled_dataset %>%
    summarize(
      lower = p_hat - 2 * sd(stat),
      upper = p_hat + 2 * sd(stat)
    )
}

# Find proportion of yes votes from original population
p_hat <- calc_p_hat(one_poll)

p_hat  

# Calculate bootstrap t-confidence interval (original 0.6 param)
calc_t_conf_int(one_poll_boot, p_hat)

# Find proportion of yes votes from new population
p_hat_0.8 <- calc_p_hat(one_poll_0.8)

p_hat_0.8  
  
# Calculate the bootstrap t-confidence interval (new 0.8 param)
calc_t_conf_int(one_poll_boot_0.8, p_hat_0.8)