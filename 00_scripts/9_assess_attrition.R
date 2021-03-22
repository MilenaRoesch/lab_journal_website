dept_job_role_tbl %>%
  group_by(Department, JobRole, Attrition) 

# Function to convert counts to percentages. 
count_to_pct <- function(data, ..., col = n) {
  
  # capture the dots
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  ret <- data %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    ungroup()
  
  return(ret)
  
}

dept_job_role_tbl %>%
  count(JobRole, Attrition) %>%
  count_to_pct(JobRole)

dept_job_role_tbl %>%
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole)  

assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
    
    attrition_col_expr <- enquo(attrition_col)
    
    data %>%
      
      # Use parenthesis () to give tidy eval evaluation priority
      filter((!! attrition_col_expr) %in% attrition_value) %>%
      arrange(desc(pct)) %>%
      mutate(
        # Function inputs in numeric format (e.g. baseline_pct = 0.088 don't require tidy eval)
        above_industry_avg = case_when(
          pct > baseline_pct ~ "Yes",
          TRUE ~ "No"
        )
      )
  }

