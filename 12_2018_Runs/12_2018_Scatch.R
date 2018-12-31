
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
# SCATCH Runs
################################################################################

# SCatch 1
sir_scatch_1 <- HUMPBACK.SIR(file_name = "SCatch 1/SCatch 1",
                             n_resamples = 10000,
                             priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                      N_obs = make_prior(runif, 500, 40000)),
                             catch_multipliers = make_multiplier_list(make_prior(1)),
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
                             control = sir_control(threshold = 10 * 5e-9, progress_bar = TRUE))
resample_summary_scatch_1 <- summary_sir(sir_scatch_1$resamples_output, object = "Resample_Summary", file_name = "SCatch 1/SCatch 1")
trajectory_summary_scatch_1 <- summary_sir(sir_scatch_1$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 1/SCatch 1")
plot_trajectory(sir_scatch_1,  file_name = "SCatch 1/SCatch 1")
plot_density(list(sir_base, sir_scatch_1),  file_name = "SCatch 1/SCatch 1", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
save(sir_scatch_1, file = "SCatch 1/sir_scatch1.RData")
zerbini_table(sir_scatch_1,  file_name = "SCatch 1/SCatch 1")




# SCatch 2
sir_scatch_2 <- HUMPBACK.SIR(file_name = "SCatch 2/SCatch 2",
                             n_resamples = 10000,
                             priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                      N_obs = make_prior(runif, 500, 40000)),
                             catch_multipliers = make_multiplier_list(make_prior(1), make_prior(1)),
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
                             catch.data = merge(Core.Catches2, Falkland.Catches2, by = "Year", all = T)  ,
                             control = sir_control(threshold = 10 * 5e-9, progress_bar = TRUE))
resample_summary_scatch_2 <- summary_sir(sir_scatch_2$resamples_output, object = "Resample_Summary", file_name = "SCatch 2/SCatch 2")
trajectory_summary_scatch_2 <- summary_sir(sir_scatch_2$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 2/SCatch 2")
plot_trajectory(sir_scatch_2,  file_name = "SCatch 2/SCatch 2")
plot_density(list(sir_base, sir_scatch_2),  file_name = "SCatch 2/SCatch 2", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
save(sir_scatch_2, file = "SCatch 2/sir_scatch2.RData")
zerbini_table(sir_scatch_2,  file_name = "SCatch 2/SCatch 2")




# SCatch 3
sir_scatch_3 <- HUMPBACK.SIR(file_name = "SCatch 3/SCatch 3",
                             n_resamples = 10000,
                             priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                      N_obs = make_prior(runif, 500, 40000)),
                             catch_multipliers = make_multiplier_list(make_prior(1)),
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
                             catch.data = Fringe.Catches2,
                             control = sir_control(threshold = 10 * 3e-8, progress_bar = TRUE))
resample_summary_scatch_3 <- summary_sir(sir_scatch_3$resamples_output, object = "Resample_Summary", file_name = "SCatch 3/SCatch 3")
trajectory_summary_scatch_3 <- summary_sir(sir_scatch_3$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 3/SCatch 3")
plot_trajectory(sir_scatch_3,  file_name = "SCatch 3/SCatch 3")
plot_density(list(sir_base, sir_scatch_3),  file_name = "SCatch 3/SCatch 3", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
save(sir_scatch_3, file = "SCatch 3/sir_scatch3.RData")
zerbini_table(sir_scatch_3,  file_name = "SCatch 3/SCatch 3")




# SCatch 4
sir_scatch_4 <- HUMPBACK.SIR(file_name = "SCatch 4/SCatch 4",
                             n_resamples = 10000,
                             priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                      N_obs = make_prior(runif, 500, 40000)),
                             catch_multipliers = make_multiplier_list(make_prior(1)),
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
                             catch.data = Overlap.Catches2,
                             control = sir_control(threshold = 10 * 6e-9, progress_bar = TRUE))
resample_summary_scatch_4 <- summary_sir(sir_scatch_4$resamples_output, object = "Resample_Summary", file_name = "SCatch 4/SCatch 4")
trajectory_summary_scatch_4 <- summary_sir(sir_scatch_4$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 4/SCatch 4")
plot_trajectory(sir_scatch_4,  file_name = "SCatch 4/SCatch 4")
plot_density(list(sir_base, sir_scatch_4),  file_name = "SCatch 4/SCatch 4", multiple_sirs = TRUE, lower = c(NA, 18000, 0, NA, 18000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
save(sir_scatch_4, file = "SCatch 4/sir_scatch4.RData")
zerbini_table(sir_scatch_4,  file_name = "SCatch 4/SCatch 4")



# SCatch 5
sir_scatch_5 <- HUMPBACK.SIR(file_name = "SCatch 5/SCatch 5",
                             n_resamples = 10000,
                             priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                      N_obs = make_prior(runif, 500, 40000)),
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
                             catch.data = Core.Catches,
                             premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                             control = sir_control(threshold = 10 * 6e-9, progress_bar = TRUE))
resample_summary_scatch_5 <- summary_sir(sir_scatch_5$resamples_output, object = "Resample_Summary", file_name = "SCatch 5/SCatch 5")
trajectory_summary_scatch_5 <- summary_sir(sir_scatch_5$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 5/SCatch 5")
plot_trajectory(sir_scatch_5,  file_name = "SCatch 5/SCatch 5")
plot_density(list(sir_base, sir_scatch_5),  file_name = "SCatch 5/SCatch 5", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, 24500, 0.1, NA, NA))
save(sir_scatch_5, file = "SCatch 5/sir_scatch5.RData")
zerbini_table(sir_scatch_5,  file_name = "SCatch 5/SCatch 5")


# SCatch 6
Core.1901.1938 <- Core.Catches[Core.Catches$Year %in% c(1901:1938), ]
Core.1939.1945 <- Core.Catches[Core.Catches$Year %in% c(1939:1945), ]
Core.1946.present <- Core.Catches[Core.Catches$Year > 1945, ]
Core.Catches.Reorg <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg <- merge(Core.Catches.Reorg, Core.1946.present, by = "Year", all = TRUE)

sir_scatch_6 <- HUMPBACK.SIR(file_name = "SCatch 6/SCatch 6",
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
                             output.Yrs = c(2018),
                             abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                             rel.abundance = Rel.Abundance.Wedekin,
                             rel.abundance.key = FALSE, # No indices of abundance
                             count.data = Count.Data,
                             count.data.key = FALSE,
                             growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                             growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                             catch.data = Core.Catches.Reorg,
                             premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                             control = sir_control(threshold = 10 * 6e-9, progress_bar = TRUE))
resample_summary_scatch_6 <- summary_sir(sir_scatch_6$resamples_output, object = "Resample_Summary", file_name = "SCatch 6/SCatch 6")
trajectory_summary_scatch_6 <- summary_sir(sir_scatch_6$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 6/SCatch 6")
plot_trajectory(sir_scatch_6,  file_name = "SCatch 6/SCatch 6")
plot_density(list(sir_base, sir_scatch_6),  file_name = "SCatch 6/SCatch 6", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.35, 0.6), upper = c(NA, 40000, 2000, NA, 28000, 0.1, NA, NA))
save(sir_scatch_6, file = "SCatch 6/sir_scatch6.RData")
zerbini_table(sir_scatch_6,  file_name = "SCatch 6/SCatch 6")


plot_density(list(sir_base, sir_scatch_1, sir_scatch_2, sir_scatch_3, sir_scatch_4, sir_scatch_5, sir_scatch_6),  file_name = "Reference_vs_scatch1-6", multiple_sirs = T)
