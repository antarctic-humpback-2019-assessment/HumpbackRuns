
# Load data
setwd("12_2018_Runs")
source("InputData_HWAssessment_InitialAssessments_Oct2018.R")
library(HumpbackSIR)

# Reference
Core.1901.1938 <- Core.Catches[Core.Catches$Year %in% c(1901:1938), ]
Core.1939.1945 <- Core.Catches[Core.Catches$Year %in% c(1939:1945), ]
Core.1946.present <- Core.Catches[Core.Catches$Year > 1945, ]
Core.Catches.Reorg <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg <- merge(Core.Catches.Reorg, Core.1946.present, by = "Year", all = TRUE)

sir_reference <- HUMPBACK.SIR(file_name = "Reference/Reference",
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
                              rel.abundance = rbind(Rel.Abundance.Pavanato, Rel.Abundance.Branch),
                              rel.abundance.key = TRUE, # No indices of abundance
                              count.data = Count.Data,
                              count.data.key = FALSE,
                              growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                              growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                              catch.data = Core.Catches.Reorg,
                              premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                              control = sir_control(threshold = 10 * 6e-9, progress_bar = TRUE))
resample_summary_reference <- summary_sir(sir_reference$resamples_output, object = "Resample_Summary", file_name = "Reference/Reference")
trajectory_summary_reference <- summary_sir(sir_reference$resamples_trajectories, object = "Trajectory_Summary", file_name = "Reference/Reference")
plot_trajectory(sir_reference,  file_name = "Reference/Reference")
plot_density(list(sir_base, sir_reference),  file_name = "Reference/Reference", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_reference, file = "Reference/sir_scatch6.RData")
zerbini_table(sir_reference,  file_name = "Reference/Reference")

sir_reference_prior <- HUMPBACK.SIR(file_name = "Reference/Reference",
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
                                    rel.abundance = rbind(Rel.Abundance.Pavanato, Rel.Abundance.Branch),
                                    rel.abundance.key = TRUE, # No indices of abundance
                                    count.data = Count.Data,
                                    count.data.key = FALSE,
                                    growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                                    catch.data = Core.Catches.Reorg,
                                    premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                                    control = sir_control(threshold = 10 * 6e-9, progress_bar = TRUE),
                                    realized_prior = TRUE)


################################################################################
# MSYR Runs
################################################################################

NmsyKz <- function(z,NmsyK) { 1-(z+1)*NmsyK^z }
z70 <- uniroot(NmsyKz,NmsyK=0.7,lower=1,upper=100)$root
z80 <- uniroot(NmsyKz,NmsyK=0.8,lower=1,upper=100)$root

# MSYR 1
sir_MSYR_1 <- HUMPBACK.SIR(file_name = "MSYR 1/MSYR 1",
                           n_resamples = 10000,
                           priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                    N_obs = make_prior(runif, 500, 40000),
                                                    z = make_prior(z70)),
                           catch_multipliers = make_multiplier_list(make_prior(1)),
                           premodern_catch_multipliers = make_multiplier_list(make_prior(1)),
                           target.Yr = 2008,
                           num.haplotypes = 0,
                           output.Yrs = c(2018),
                           abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                           rel.abundance = Rel.Abundance.Wedekin,
                           rel.abundance.key = FALSE, # No indices of abundance
                           count.data = Count.Data,
                           count.data.key = FALSE,
                           growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                           growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                           catch.data = Core.Catches2,
                           premodern_catch_data = NULL,
                           control = sir_control(threshold = 10 * 4e-8, progress_bar = TRUE))
resample_summary_MSYR_1 <- summary_sir(sir_MSYR_1$resamples_output, object = "Resample_Summary", file_name = "MSYR 1/MSYR 1")
trajectory_summary_MSYR_1 <- summary_sir(sir_MSYR_1$resamples_trajectories, object = "Trajectory_Summary", file_name = "MSYR 1/MSYR 1")
plot_trajectory(sir_MSYR_1,  file_name = "MSYR 1/MSYR 1")
plot_density(list(sir_base, sir_MSYR_1),  file_name = "MSYR 1/MSYR 1", multiple_sirs = TRUE, lower = c(NA, 20000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
save(sir_MSYR_1, file = "MSYR 1/sir_MSYR1.RData")
zerbini_table(sir_MSYR_1,  file_name = "MSYR 1/MSYR 1")



# MSYR 2
sir_MSYR_2 <- HUMPBACK.SIR(file_name = "MSYR 2/MSYR 2",
                           n_resamples = 10000,
                           priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                    N_obs = make_prior(runif, 500, 40000),
                                                    z = make_prior(z80)),
                           catch_multipliers = make_multiplier_list(make_prior(1)),
                           premodern_catch_multipliers = make_multiplier_list(make_prior(1)),
                           target.Yr = 2008,
                           num.haplotypes = 0,
                           output.Yrs = c(2018),
                           abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                           rel.abundance = Rel.Abundance.Wedekin,
                           rel.abundance.key = FALSE, # No indices of abundance
                           count.data = Count.Data,
                           count.data.key = FALSE,
                           growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                           growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                           catch.data = Core.Catches2,
                           premodern_catch_data = NULL,
                           control = sir_control(threshold = 10 * 4e-8, progress_bar = TRUE))
resample_summary_MSYR_2 <- summary_sir(sir_MSYR_2$resamples_output, object = "Resample_Summary", file_name = "MSYR 2/MSYR 2")
trajectory_summary_MSYR_2 <- summary_sir(sir_MSYR_2$resamples_trajectories, object = "Trajectory_Summary", file_name = "MSYR 2/MSYR 2")
plot_trajectory(sir_MSYR_2,  file_name = "MSYR 2/MSYR 2")
plot_density(list(sir_base, sir_MSYR_2),  file_name = "MSYR 2/MSYR 2", multiple_sirs = TRUE, lower = c(NA, 18000, 0, NA, 19000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, 24000, 0.1, NA, NA))
save(sir_MSYR_2, file = "MSYR 2/sir_MSYR2.RData")
zerbini_table(sir_MSYR_2,  file_name = "MSYR 2/MSYR 2")
