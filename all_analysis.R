### Data Analysis Clean

# 1. Import Raw Data and Fix Errors.
source("data_carpentry/01_fix_errors.R")

# 2. Source Functions
source("functions/functions.R")

# 3. Put dataframes into a list
source("data_carpentry/02_list_dataframes.R")

# 4. Overall dominance characteristics
source("analysis_code/01_dominance_overall.R")

# Sex differences in dominance characteristics 
source("data_carpentry/05_male_v_female.R")

# 5. Rank ordering individuals by David's Scores.
source("analysis_code/02_ranking.R")

# 6. Plot sociomatrices
source("analysis_code/03_matrix_plot.R")

# 7. Get and plot glicko scores
source("analysis_code/08_glickos.R")

# 8. Overall dominance characteristics by day
source("analysis_code/04_dominance_overall_by_day.R")

# 9. Estrus analysis
source("data_carpentry/03_estrus_states.R")

# 10. Dominance by behavior
source("analysis_code/05_dominance_by_behavior.R")

# 11. Body Weight assessment
source("analysis_code/06_body_weight.R")

# 12. Daily Glicko rating and estrus status 
source("data_carpentry/04_startends.R")
source("analysis_code/07_startends.R")

# 13. Analyze the hourly occurence rate of each behavior (fighting, chasing, mounting) given/received
source("analysis_code/09_brms_each_behavior.R")

# 14. Analyze estrus state data
source("analysis_code/10_brms_estrus.R")

# 15. Analyze the effect of social status (Dom-Sub) on plasma CORT and estradiol levels
source("analysis_code/11_hormones.R")

# 16. Analyze gene expression difference between dominant and subordinate females using MCMC.qpcr package
source("analysis_code/12_mcmc.qpcr_gene_expression.R") 
