
# Load data
setwd("12_2018_Runs")
source("R code/InputData_HWAssessment_InitialAssessments_Oct2018.R")
library(HumpbackSIR)

# Reference
Core.1901.1938 <- Core.Catches2[Core.Catches2$Year %in% c(1830:1938), ]
Core.1939.1945 <- Core.Catches2[Core.Catches2$Year %in% c(1939:1945), ]
Core.1946.present <- Core.Catches2[Core.Catches2$Year > 1945, ]
Core.Catches.Reorg <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg <- as.matrix(merge(Core.Catches.Reorg, Core.1946.present, by = "Year", all = TRUE))
Core.Catches.Reorg[which(is.na(Core.Catches.Reorg))] = 0
Core.Catches.Reorg <- as.data.frame(Core.Catches.Reorg)

Rel.Abundance.Pavanato$Index <- 2
rel_abund_ref <- rbind(Rel.Abundance.Branch, Rel.Abundance.Pavanato)
Rel.Abundance.Pavanato$Index <- 1

################################################################################
# Reference
################################################################################
file_name <- "Reference/Reference"
sir_reference <- list()
for(i in 1:2){
  sir_reference[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                      n_resamples = 1000,
                                      priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                               N_obs = make_prior(runif, 500, 40000)),
                                      catch_multipliers = make_multiplier_list(
                                        make_prior(rbest, 0.139, 0.3),
                                        make_prior(runif, 1.25, 1.42), 
                                        make_prior(rnorm, 1.0185, 0.0028)),
                                      premodern_catch_multipliers = make_multiplier_list(
                                        make_prior(rnorm, 1.71, 0.073)),
                                      target.Yr = 2008,
                                      num.haplotypes = 0,
                                      output.Yrs = c(2012, 2006, 2019, 2030),
                                      abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                      rel.abundance = rel_abund_ref,
                                      rel.abundance.key = TRUE, # Indices of abundance
                                      count.data = Count.Data,
                                      count.data.key = FALSE,
                                      growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                      growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                      catch.data = Core.Catches.Reorg,
                                      premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                      control = sir_control(threshold = 1e-25, progress_bar = TRUE),
                                      realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_reference[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_reference[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_reference, file = paste0(file_name, ".Rdata"))



################################################################################
# SDATA Runs
################################################################################
# SData 1
file_name <- "SData 1/SData 1"
sir_sdata_1 <- list()
for(i in 1:2){
  sir_sdata_1[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2012,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2008, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-25, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_1[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_1[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_sdata_1, file = paste0(file_name, ".Rdata"))




# SData 2
file_name <- "SData 2/SData 2"
sir_sdata_2 <- list()
for(i in 1:2){
  sir_sdata_2[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = FALSE, # No ndices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-7, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_2[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_2[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_sdata_2, file = paste0(file_name, ".Rdata"))




# SData 3
Rel.Abundance.Wedekin$Index <- 2
rel_abund3 <- rbind(Rel.Abundance.Branch, Rel.Abundance.Wedekin)
Rel.Abundance.Wedekin$Index <- 1
file_name <- "SData 3/SData 3"
sir_sdata_3 <- list()
for(i in 1:2){
  sir_sdata_3[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund3,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-31, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_3[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_3[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_sdata_3, file = paste0(file_name, ".Rdata"))




# SData 4
file_name <- "SData 4/SData 4"
sir_sdata_4 <- list()
for(i in 1:2){
  sir_sdata_4[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = Rel.Abundance.Pavanato,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-16, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_4[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_4[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_sdata_4, file = paste0(file_name, ".Rdata"))




# SData 5
Rel.Abundance.Wedekin$Index <- 1
file_name <- "SData 5/SData 5"
sir_sdata_5 <- list()
for(i in 1:2){
  sir_sdata_5[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = Rel.Abundance.Wedekin,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-25, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_5[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_5[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_sdata_5, file = paste0(file_name, ".Rdata"))



# SData 6
Rel.Abundance.Branch$Index <- 1
file_name <- "SData 6/SData 6"
sir_sdata_6 <- list()
for(i in 1:2){
  sir_sdata_6[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = Rel.Abundance.Branch,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-14, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_6[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_6[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_sdata_6, file = paste0(file_name, ".Rdata"))




# SData 7
# -- Make beta prior
fit_beta <- function(log_params){
  alpha = exp(log_params[1])
  beta = exp(log_params[2])
  
  # Mean = 0.086
  mean = alpha / (alpha + beta)
  
  # Upper CI = 0.114
  upper_ci <- qbeta( 0.975, alpha, beta)
  
  # Lower CI = 0.0.05
  lower_ci <- qbeta( 0.025, alpha, beta)
  
  sum_squares = ((0.086 - mean)^2)*4 + (0.114 - upper_ci)^2 + (0.05 - lower_ci)^2
  
  return(sum_squares)
}

fit <- optim( c(-1, -1), fit_beta)

alpha = exp(fit$par)[1]
beta = exp(fit$par)[2]
alpha / (alpha + beta)# mean
qbeta( 0.975, alpha, beta)#upper
qbeta( 0.025, alpha, beta) #lower


file_name <- "SData 7/SData 7"
sir_sdata_7 <- list()
for(i in 1:2){
  sir_sdata_7[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(rbeta, alpha, beta),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_7[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_7[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_sdata_7, file = paste0(file_name, ".Rdata"))




################################################################################
# Scatch Runs
################################################################################
# Scatch 1
file_name <- "SCatch 1/SCatch 1"
sir_catch_1 <- list()
for(i in 1:2){
  sir_catch_1[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(1),
                                      make_prior(1), 
                                      make_prior(1)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(1)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = NULL,
                                    control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_1[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_1[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)      
save(sir_catch_1, file = paste0(file_name, ".Rdata"))




# Scatch 2
file_name <- "SCatch 2/SCatch 2"
sir_catch_2 <- list()
for(i in 1:2){
  sir_catch_2[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(1)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = NULL,
                                    control = sir_control(threshold = 10 * 1e-22, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_2[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_2[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_catch_2, file = paste0(file_name, ".Rdata"))


# Scatch 3
file_name <- "Scatch 3/Scatch 3"
sir_catch_3 <- list()
for(i in 1:2){
  sir_catch_3[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(1),
                                      make_prior(1), 
                                      make_prior(1)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(1)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_3[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_3[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)   
save(sir_catch_3, file = paste0(file_name, ".Rdata"))



# Scatch 4 - Conservative
file_name <- "Scatch 4/Scatch 4"
sir_catch_4 <- list()
for(i in 1:2){
  sir_catch_4[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.0185 , 0.028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches2,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-22, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_4[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_4[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_catch_4, file = paste0(file_name, ".Rdata"))


# Scatch 5 - Liberal
Core.1901.1930 <- Core.Catches2[Core.Catches2$Year %in% c(1830:1930), ]
Core.1931.after <- Core.Catches2[Core.Catches2$Year > 1931, ]
Core.Catches.Reorg2 <- as.matrix(merge(Core.1901.1930, Core.1931.after, by = "Year", all = TRUE))
Core.Catches.Reorg2[which(is.na(Core.Catches.Reorg2))] = 0
Core.Catches.Reorg2 <- as.data.frame(Core.Catches.Reorg2)

file_name <- "Scatch 5/Scatch 5"
sir_catch_5 <- list()
for(i in 1:2){
  sir_catch_5[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(rnorm, 1.0185 , 0.028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg2,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-22, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_5[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_5[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_catch_5, file = paste0(file_name, ".Rdata"))


# Scatch 6
Core.Catches3 <- Core.Catches2
Core.Catches3$Catch <- Core.Catches3$Catch + Falkland.Catches2$Catch

Core.1901.1938 <- Core.Catches3[Core.Catches3$Year %in% c(1901:1938), ]
Core.1939.1945 <- Core.Catches3[Core.Catches3$Year %in% c(1939:1945), ]
Core.1946.present <- Core.Catches3[Core.Catches3$Year > 1945, ]
Core.Catches.Reorg3 <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg3 <- merge(Core.Catches.Reorg3, Core.1946.present, by = "Year", all = TRUE)

file_name <- "Scatch 6/Scatch 6"
sir_catch_6 <- list()
for(i in 1:2){
  sir_catch_6[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg3,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_6[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_6[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_catch_6, file = paste0(file_name, ".Rdata"))




# Scatch 7
Core.1901.1938 <- Fringe.Catches2[Fringe.Catches2$Year %in% c(1901:1938), ]
Core.1939.1945 <- Fringe.Catches2[Fringe.Catches2$Year %in% c(1939:1945), ]
Core.1946.present <- Fringe.Catches2[Fringe.Catches2$Year > 1945, ]
Core.Catches.Reorg4 <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg4 <- merge(Core.Catches.Reorg4, Core.1946.present, by = "Year", all = TRUE)

file_name <- "Scatch 7/Scatch 7"
sir_catch_7 <- list()
for(i in 1:2){
  sir_catch_7[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg4,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_7[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_7[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_catch_7, file = paste0(file_name, ".Rdata"))


# Scatch 8
Core.1901.1938 <- Overlap.Catches2[Overlap.Catches2$Year %in% c(1901:1938), ]
Core.1939.1945 <- Overlap.Catches2[Overlap.Catches2$Year %in% c(1939:1945), ]
Core.1946.present <- Overlap.Catches2[Overlap.Catches2$Year > 1945, ]
Core.Catches.Reorg5 <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg5 <- merge(Core.Catches.Reorg5, Core.1946.present, by = "Year", all = TRUE)

file_name <- "Scatch 8/Scatch 8"
sir_catch_8 <- list()
for(i in 1:2){
  sir_catch_8[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 1000,
                                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                             N_obs = make_prior(runif, 500, 40000)),
                                    catch_multipliers = make_multiplier_list(
                                      make_prior(rbest, 0.139, 0.3),
                                      make_prior(runif, 1.25, 1.42), 
                                      make_prior(rnorm, 1.0185, 0.0028)),
                                    premodern_catch_multipliers = make_multiplier_list(
                                      make_prior(rnorm, 1.71, 0.073)),
                                    target.Yr = 2008,
                                    num.haplotypes = 0,
                                    output.Yrs = c(2012, 2006, 2019, 2030),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg5,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_8[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_8[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_catch_8, file = paste0(file_name, ".Rdata"))


################################################################################
# GC Runs
################################################################################
# GC 1
file_name <- "GC 1/GC 1"
sir_gc_1 <- list()
for(i in 1:2){
  sir_gc_1[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                 n_resamples = 1000,
                                 priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                          N_obs = make_prior(runif, 500, 40000)),
                                 catch_multipliers = make_multiplier_list(
                                   make_prior(rbest, 0.139, 0.3),
                                   make_prior(runif, 1.25, 1.42), 
                                   make_prior(rnorm, 1.0185, 0.0028)),
                                 premodern_catch_multipliers = make_multiplier_list(
                                   make_prior(rnorm, 1.71, 0.073)),
                                 target.Yr = 2008,
                                 num.haplotypes = 54,
                                 output.Yrs = c(2012, 2006, 2019, 2030),
                                 abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                 rel.abundance = rel_abund_ref,
                                 rel.abundance.key = TRUE, # Indices of abundance
                                 count.data = Count.Data,
                                 count.data.key = FALSE,
                                 growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                 growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                 catch.data = Core.Catches.Reorg,
                                 premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                 control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                 realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_gc_1[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_gc_1[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_gc_1, file = paste0(file_name, ".Rdata"))




# GC 2
file_name <- "GC 2/GC 2"
sir_gc_2 <- list()
for(i in 1:2){
  sir_gc_2[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                 n_resamples = 1000,
                                 priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                          N_obs = make_prior(runif, 500, 40000)),
                                 catch_multipliers = make_multiplier_list(
                                   make_prior(rbest, 0.139, 0.3),
                                   make_prior(runif, 1.25, 1.42), 
                                   make_prior(rnorm, 1.0185, 0.0028)),
                                 premodern_catch_multipliers = make_multiplier_list(
                                   make_prior(rnorm, 1.71, 0.073)),
                                 target.Yr = 2008,
                                 num.haplotypes = 5,
                                 output.Yrs = c(2012, 2006, 2019, 2030),
                                 abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                 rel.abundance = rel_abund_ref,
                                 rel.abundance.key = TRUE, # Indices of abundance
                                 count.data = Count.Data,
                                 count.data.key = FALSE,
                                 growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                 growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                 catch.data = Core.Catches.Reorg,
                                 premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                 control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                 realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_gc_2[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_gc_2[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
save(sir_gc_2, file = paste0(file_name, ".Rdata"))



################################################################################
# MSYR Runs
################################################################################
NmsyKz <- function(z,NmsyK) { 1-(z+1)*NmsyK^z }
z70 <- uniroot(NmsyKz,NmsyK=0.7,lower=1,upper=100)$root
z80 <- uniroot(NmsyKz,NmsyK=0.8,lower=1,upper=100)$root

# MSYR 1
file_name <- "MSYR 1/MSYR 1"
sir_msyr_1 <- list()
for(i in 1:2){
  sir_msyr_1[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                   n_resamples = 1000,
                                   priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                            N_obs = make_prior(runif, 500, 40000),
                                                            z = make_prior(z70)),
                                   catch_multipliers = make_multiplier_list(
                                     make_prior(rbest, 0.139, 0.3),
                                     make_prior(runif, 1.25, 1.42), 
                                     make_prior(rnorm, 1.0185, 0.0028)),
                                   premodern_catch_multipliers = make_multiplier_list(
                                     make_prior(rnorm, 1.71, 0.073)),
                                   target.Yr = 2008,
                                   num.haplotypes = 0,
                                   output.Yrs = c(2012, 2006, 2019, 2030),
                                   abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                   rel.abundance = rel_abund_ref,
                                   rel.abundance.key = TRUE, # Indices of abundance
                                   count.data = Count.Data,
                                   count.data.key = FALSE,
                                   growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                   growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                   catch.data = Core.Catches.Reorg,
                                   premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                   control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                   realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_msyr_1[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_msyr_1[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)

file_name <- "MSYR 1/MSYR 1"
plot_trajectory(sir_msyr_1[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_msyr_1[[1]]), priors = list(sir_reference[[2]], sir_msyr_1[[2]]),  file_name = file_name,  lower = c(NA, 22000, 0, NA, 16000, NA, NA, NA, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 23000, NA, .05, NA, NA, NA))
plot_ioa(sir_msyr_1[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_msyr_1[[1]],  file_name = file_name)

save(sir_msyr_1, file = paste0(file_name, ".Rdata"))



# MSYR 2
file_name <- "MSYR 2/MSYR 2"
sir_msyr_2 <- list()
for(i in 1:2){
  sir_msyr_2[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                   n_resamples = 1000,
                                   priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                            N_obs = make_prior(runif, 500, 40000),
                                                            z = make_prior(z80)),
                                   catch_multipliers = make_multiplier_list(
                                     make_prior(rbest, 0.139, 0.3),
                                     make_prior(runif, 1.25, 1.42), 
                                     make_prior(rnorm, 1.0185, 0.0028)),
                                   premodern_catch_multipliers = make_multiplier_list(
                                     make_prior(rnorm, 1.71, 0.073)),
                                   target.Yr = 2008,
                                   num.haplotypes = 0,
                                   output.Yrs = c(2012, 2006, 2019, 2030),
                                   abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                   rel.abundance = rel_abund_ref,
                                   rel.abundance.key = TRUE, # Indices of abundance
                                   count.data = Count.Data,
                                   count.data.key = FALSE,
                                   growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                   growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                   catch.data = Core.Catches.Reorg,
                                   premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                   control = sir_control(threshold = 10 * 1e-23, progress_bar = TRUE),
                                   realized_prior = ifelse(i == 1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_msyr_2[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_msyr_2[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)

file_name <- "MSYR 2/MSYR 2"
plot_trajectory(sir_msyr_2[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_msyr_2[[1]]), priors = list(sir_reference[[2]], sir_msyr_2[[2]]),  file_name = file_name,  lower = c(NA, 22000, 0, NA, 16000, NA, NA, NA, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 23000, NA, .05, NA, NA, NA))
plot_ioa(sir_msyr_2[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_msyr_2[[1]],  file_name = file_name)

save(sir_msyr_2, file = paste0(file_name, ".Rdata"))

