### Heddesheimer: PanelMatch Function
# vincent.heddesheimer@princeton.edu

# This is a custom function to run PanelMatch on a dataset.

# Load packages
pacman::p_load(tidyverse, data.table, PanelMatch, stringr)

# Function
run_panelmatch <- function(data,
                           treatment,
                           outcome,
                           covariates,
                           lag = 2,
                           lead = 0:1,
                           unit_id = "id",
                           time_id = "t",
                           refinement.method = "CBPS.weight",
                           figure_path = "output/figures/panelmatch/",
                           dataframes_path = "output/dataframes/panelmatch/",
                           results_width = 5,
                           results_height = 5.5,
                           covariate_balance_width = 7,
                           covariate_balance_height = 8,
                           forbid_treatment_reversal = FALSE,
                           se_method = "conditional") {
  tryCatch({
    # Match individuals on covariates and lagged outcome
    cov_formula <-
      as.formula(paste("~", paste(covariates, collapse = " + ")))
    
    # Adding the lag of the outcome variable
    lagged_outcome <- sprintf("I(lag(%s, 1:%d))", outcome, lag)
    match_formula <-
      as.formula(paste("~", paste(
        c(covariates, lagged_outcome), collapse = " + "
      )))
    
    # Create matched set
    match <- PanelMatch(
      lag = lag,
      time.id = time_id,
      unit.id = unit_id,
      covs.formula = match_formula,
      treatment = treatment,
      refinement.method = refinement.method,
      data = data,
      match.missing = TRUE,
      qoi = "att",
      outcome.var = outcome,
      lead = lead,
      forbid.treatment.reversal = forbid_treatment_reversal
    )
    
    # Create a scatter plot to analyze covariate balance
    pdf(paste0(figure_path,
               treatment,
               "_",
               outcome,
               "_cb_scat.pdf"))
    
    balance_scatter(
      matched_set_list = list(match$att),
      data = data,
      covariates = c(covariates, outcome)
    )
    # Close the pdf file
    dev.off()
    
    # Create covariate balance before treatment
    # Note that this includes covariates as well as outcome
    cb <- as_tibble(
      get_covariate_balance(
        match$att,
        data = data,
        covariates = c(covariates, outcome),
        plot = FALSE
      ),
      rownames = "t"
    ) |>
      pivot_longer(
        cols = c(covariates, outcome),
        names_to = "covariate",
        values_to = "covbal"
      ) |>
      mutate(
        t = as.integer(str_replace(t, "t_", "-")),
        treatment = treatment,
        outcome = outcome
      )
    
    # cb df
    covariate_balance_df <- rbind(covariate_balance_df, cb)
    
    # Create Covariance Balance Plot
    # Create a named color vector where the outcome is "black" and all other covariates are "grey"
    all_vars <- c(covariates, outcome)
    color_mapping <-
      setNames(rep("grey", length(all_vars)), all_vars)
    color_mapping[outcome] <- "black"
    
    # Plot
    cb_plot <- cb |>
      ggplot(aes(x = t, y = covbal, color = covariate)) +
      geom_line() +
      scale_color_manual(values = color_mapping) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_vline(xintercept = -1, linetype = "dashed") +
      scale_x_continuous(breaks = scales::breaks_extended(n = lag)) +
      scale_y_continuous(limits = c(-.5, .5)) +
      theme_hanno() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(y = "Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")
    
    # Save
    ggsave(
      filename = paste0(treatment, "_", outcome, "_cb_pre.pdf"),
      plot = cb_plot,
      path = figure_path,
      height = covariate_balance_height,
      width = covariate_balance_width
    )
    
    # Estimate ATT
    est <-
      PanelEstimate(sets = match,
                    data = data,
                    se.method = se_method)
    est90 <-
      PanelEstimate(
        sets = match,
        data = data,
        se.method = se_method,
        confidence.level = .90
      )
    
    # Extract relevant portions of the summary data frames for each lead
    summary_est <- summary(est)$summary
    summary_est90 <- summary(est90)$summary
    
    lead_rows <-
      lead + 1  # Assuming leads start from 0 and the summary is indexed starting from 1
    tmp_results <- data.frame(
      treatment = rep(treatment, length(lead)),
      outcome = rep(outcome, length(lead)),
      t = rep(lead, each = 1),
      # Now dynamic
      estimate = summary_est[lead_rows, 1],
      conf.low = summary_est[lead_rows, 3],
      conf.high = summary_est[lead_rows, 4],
      conf.low90 = summary_est90[lead_rows, 3],
      conf.high90 = summary_est90[lead_rows, 4],
      stringsAsFactors = FALSE
    )
    
    # Add these results to the overall dataframe
    results_df <- rbind(results_df, tmp_results)
    
    # Plot results
    plot_data <- data.frame(
      t = lead,
      estimate = summary_est[lead_rows, 1],
      conf.low = summary_est[lead_rows, 3],
      conf.high = summary_est[lead_rows, 4],
      conf.low90 = summary_est90[lead_rows, 3],
      conf.high90 = summary_est90[lead_rows, 4]
    )
    
    # Plotting...
    plot <- plot_data |>
      ggplot(aes(x = t, y = estimate)) +
      geom_point(aes(x = t, y = estimate), size = 2) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                    width = 0,
                    linewidth = .5) +
      geom_errorbar(
        aes(ymin = conf.low90, ymax = conf.high90),
        width = 0,
        linewidth = 1.25
      ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "red",
        linewidth = .25,
        alpha = 0.75
      ) +
      labs(y = "ATT", x = "Relative Time") +
      scale_x_continuous(breaks = lead) +
      haschaR::theme_hanno()
    
    ggsave(
      filename = paste0(treatment, "_", outcome, "_att.pdf"),
      plot = plot,
      path = figure_path,
      width = 5,
      height = 5.5
    )
    
  }, error = function(e) {
    warning(paste("Caught an error:", e))
    return(NULL)
  })
  
  # Return the results data frames
  return(list(results_df = results_df, covariate_balance_df = covariate_balance_df))
}