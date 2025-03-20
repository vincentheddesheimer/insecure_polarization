perform_event_study <- function(
    data, yname, idname, tname, gname, 
    xformla = ~1, 
    run_sensitivity = FALSE, 
    post_treatment_periods = 0.8, 
    Mbarvec = seq(from = 0.5, to = 1, by = 0.5)) {
  
  # Perform Callaway and Sant'Anna event study with specified covariates
  est_did <- did::att_gt(
    yname = yname, 
    tname = tname, 
    idname = idname, 
    gname = gname, 
    data = data, 
    base_period = "universal",
    clustervars = idname,
    xformla = xformla # Use specified covariates or ~1 if none
  )
  
  n <- est_did$n
  
  # Aggregate results to dynamic ATT
  est_did <- did::aggte(est_did, type = "dynamic", na.rm = TRUE)
  
  # Extract event study coefficients
  tidy_did <- broom::tidy(est_did) |> 
    select(-term) |>
    rename(term = event.time) |>
    mutate(dv = yname, n = n)
  
  # Check if sensitivity analysis should be run
  if (run_sensitivity) {
    # Initialize a list to store sensitivity results for each post-treatment period
    sensitivity_results_list <- list()
    
    # Loop over each post-treatment period
    for (e in post_treatment_periods) {
      # Run sensitivity analysis for the current period
      sensitivity_results <- honest_did(
        es = est_did, 
        e = e,
        type = "relative_magnitude",
        Mbarvec = Mbarvec
      )
      
      # Extract sensitivity results
      robust_ci <- sensitivity_results$robust_ci
      
      # Pivot wider and add the period information
      robust_ci_wide <- robust_ci %>%
        pivot_wider(names_from = Mbar, values_from = c(lb, ub)) %>%
        mutate(term = e)
      
      # Append the results to the list
      sensitivity_results_list <- append(sensitivity_results_list, list(robust_ci_wide))
    }
    
    # Combine all sensitivity results into a single tidy dataframe
    final_sensitivity_results <- bind_rows(sensitivity_results_list)
    
    # Merge the sensitivity results into the tidy event study results
    final_results <- tidy_did %>%
      left_join(final_sensitivity_results, by = "term")
  } else {
    # If sensitivity analysis is not run, return the tidy event study results
    final_results <- tidy_did
  }
  
  return(final_results)
}