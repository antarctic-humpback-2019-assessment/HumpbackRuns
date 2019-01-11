
# Load data
setwd("12_2018_Runs")
source("InputData_HWAssessment_InitialAssessments_Oct2018.R")
library(HumpbackSIR)

# Reference
Core.1901.1938 <- Core.Catches2[Core.Catches2$Year %in% c(1901:1938), ]
Core.1939.1945 <- Core.Catches2[Core.Catches2$Year %in% c(1939:1945), ]
Core.1946.present <- Core.Catches2[Core.Catches2$Year > 1945, ]
Core.Catches.Reorg <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg <- merge(Core.Catches.Reorg, Core.1946.present, by = "Year", all = TRUE)

Rel.Abundance.Branch$ Index <- 2
rel_abund_ref <- rbind(Rel.Abundance.Pavanato, Rel.Abundance.Branch)


################################################################################
# Reference
################################################################################
file_name <- "Reference/Reference"
for(i in 1:2){
  sir_reference[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                      n_resamples = 10000,
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
                                      output.Yrs = c(2012, 2018),
                                      abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                      rel.abundance = rel_abund_ref,
                                      rel.abundance.key = TRUE, # Indices of abundance
                                      count.data = Count.Data,
                                      count.data.key = FALSE,
                                      growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                      growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                      catch.data = Core.Catches.Reorg,
                                      premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                      control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                      realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_reference[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_reference[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_reference[[1]],  file_name = file_name)
plot_density(sir_reference,  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_reference, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_reference[[1]],  file_name = file_name)


################################################################################
# SDATA Runs
################################################################################
# SData 1
file_name <- "SData 1/SData 1"
sir_sdata_1 <- list()
for(i in 1:2){
  sir_sdata_1[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2008, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_1[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_1[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_sdata_1[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_sdata_1),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_sdata_1, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_sdata_1,  file_name = file_name)



# SData 2
file_name <- "SData 2/SData 2"
sir_sdata_2 <- list()
for(i in 1:2){
  sir_sdata_2[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = FALSE, # No ndices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-7, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_2[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_2[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_sdata_2[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_sdata_2),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_sdata_2, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_sdata_2,  file_name = file_name)



# SData 3
rel_abund3 <- rbind(Rel.Abundance.Wedekin, Rel.Abundance.Branch)
file_name <- "SData 3/SData 3"
sir_sdata_3 <- list()
for(i in 1:2){
  sir_sdata_3[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund3,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-32, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_3[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_3[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_sdata_3[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_sdata_3),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_sdata_3, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_sdata_3,  file_name = file_name)



# SData 4
file_name <- "SData 4/SData 4"
sir_sdata_4 <- list()
for(i in 1:2){
  sir_sdata_4[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = Rel.Abundance.Pavanato,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-17, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_4[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_4[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_sdata_4[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_sdata_4),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_sdata_4, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_sdata_4,  file_name = file_name)


# SData 5
Rel.Abundance.Branch$Index <- 1
file_name <- "SData 5/SData 5"
sir_sdata_5 <- list()
for(i in 1:2){
  sir_sdata_5[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = Rel.Abundance.Branch,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-14, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_5[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_5[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_sdata_5[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_sdata_5),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_sdata_5, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_sdata_5,  file_name = file_name)



# SData 6
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


file_name <- "SData 6/SData 6"
sir_sdata_6 <- list()
for(i in 1:2){
  sir_sdata_6[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = Rel.Abundance.Branch,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-14, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_sdata_6[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_sdata_6[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_sdata_6[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_sdata_6),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_sdata_6, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_sdata_6,  file_name = file_name)



################################################################################
# Scatch Runs
################################################################################
# Scatch 1
file_name <- "SCatch 1/SCatch 1"
sir_catch_1 <- list()
for(i in 1:2){
  sir_catch_1[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_1[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_1[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_catch_1[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_catch_1),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_catch_1, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_catch_1,  file_name = file_name)



# Scatch 2
file_name <- "SCatch 2/SCatch 2"
sir_catch_2 <- list()
for(i in 1:2){
  sir_catch_2[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = NULL,
                                    control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_2[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_2[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_catch_2[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_catch_2),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_catch_2, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_catch_2,  file_name = file_name)


# Scatch 3
Core.Catches3 <- Core.Catches2
Core.Catches3$Catch <- Core.Catches3$Catch + Falkland.Catches2$Catch

Core.1901.1938 <- Core.Catches3[Core.Catches3$Year %in% c(1901:1938), ]
Core.1939.1945 <- Core.Catches3[Core.Catches3$Year %in% c(1939:1945), ]
Core.1946.present <- Core.Catches3[Core.Catches3$Year > 1945, ]
Core.Catches.Reorg3 <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg3 <- merge(Core.Catches.Reorg3, Core.1946.present, by = "Year", all = TRUE)


file_name <- "SCatch 3/SCatch 3"
sir_catch_3 <- list()
for(i in 1:2){
  sir_catch_3[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg3,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_3[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_3[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_catch_3[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_catch_3),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_catch_3, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_catch_3,  file_name = file_name)




# Scatch 4
Core.1901.1938 <- Fringe.Catches2[Fringe.Catches2$Year %in% c(1901:1938), ]
Core.1939.1945 <- Fringe.Catches2[Fringe.Catches2$Year %in% c(1939:1945), ]
Core.1946.present <- Fringe.Catches2[Fringe.Catches2$Year > 1945, ]
Core.Catches.Reorg4 <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg4 <- merge(Core.Catches.Reorg4, Core.1946.present, by = "Year", all = TRUE)

file_name <- "SCatch 4/SCatch 4"
sir_catch_4 <- list()
for(i in 1:2){
  sir_catch_4[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg4,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_4[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_4[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_catch_4[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_catch_4),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_catch_4, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_catch_4,  file_name = file_name)



# Scatch 5
Core.1901.1938 <- Overlap.Catches2[Overlap.Catches2$Year %in% c(1901:1938), ]
Core.1939.1945 <- Overlap.Catches2[Overlap.Catches2$Year %in% c(1939:1945), ]
Core.1946.present <- Overlap.Catches2[Overlap.Catches2$Year > 1945, ]
Core.Catches.Reorg5 <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg5 <- merge(Core.Catches.Reorg5, Core.1946.present, by = "Year", all = TRUE)

file_name <- "SCatch 5/SCatch 5"
sir_catch_5 <- list()
for(i in 1:2){
  sir_catch_5[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg5,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_catch_5[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_catch_5[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_catch_5[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_catch_5),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_catch_5, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_catch_5,  file_name = file_name)




################################################################################
# GC Runs
################################################################################
# GC 1
file_name <- "GC 1/GC 1"
sir_gc_1 <- list()
for(i in 1:2){
  sir_gc_1[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                    n_resamples = 10000,
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
                                    output.Yrs = c(2012, 2018),
                                    abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                    rel.abundance = rel_abund_ref,
                                    rel.abundance.key = TRUE, # Indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                    realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_gc_1[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_gc_1[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_gc_1[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_gc_1),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_gc_1, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_gc_1,  file_name = file_name)



# GC 2
file_name <- "GC 2/GC 2"
sir_gc_2 <- list()
for(i in 1:2){
  sir_gc_2[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                 n_resamples = 10000,
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
                                 output.Yrs = c(2012, 2018),
                                 abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                 rel.abundance = rel_abund_ref,
                                 rel.abundance.key = TRUE, # Indices of abundance
                                 count.data = Count.Data,
                                 count.data.key = FALSE,
                                 growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                 growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                 catch.data = Core.Catches.Reorg,
                                 premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                 control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                 realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_gc_2[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_gc_2[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_gc_2[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_gc_2),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_gc_2, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_gc_2,  file_name = file_name)


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
                                 n_resamples = 10000,
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
                                 output.Yrs = c(2012, 2018),
                                 abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                 rel.abundance = rel_abund_ref,
                                 rel.abundance.key = TRUE, # Indices of abundance
                                 count.data = Count.Data,
                                 count.data.key = FALSE,
                                 growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                 growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                 catch.data = Core.Catches.Reorg,
                                 premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                 control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                 realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_msyr_1[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_msyr_1[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_msyr_1[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_msyr_1),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_msyr_1, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_msyr_1,  file_name = file_name)


# MSYR 2
file_name <- "MSYR 2/MSYR 2"
sir_msyr_2 <- list()
for(i in 1:2){
  sir_msyr_2[[i]] <-  HUMPBACK.SIR(file_name = paste0(file_name, c("","prior")[i]),
                                   n_resamples = 10000,
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
                                   output.Yrs = c(2012, 2018),
                                   abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                                   rel.abundance = rel_abund_ref,
                                   rel.abundance.key = TRUE, # Indices of abundance
                                   count.data = Count.Data,
                                   count.data.key = FALSE,
                                   growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                   growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                   catch.data = Core.Catches.Reorg,
                                   premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                   control = sir_control(threshold = 1e-23, progress_bar = TRUE),
                                   realized_prior = ifelse(1, "FALSE", "TRUE"))
}
resample_summary_reference <- summary_sir(sir_msyr_2[[1]]$resamples_output, object = "Resample_Summary", file_name = file_name)
trajectory_summary_reference <- summary_sir(sir_msyr_2[[1]]$resamples_trajectories, object = "Trajectory_Summary", file_name = file_name)
plot_trajectory(sir_msyr_2[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(c(sir_reference[[1]], sir_msyr_2),  file_name = file_name, multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_msyr_2, file = paste0(file_name, ".Rdata"))
zerbini_table(sir_msyr_2,  file_name = file_name)



