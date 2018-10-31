
# Load data
source("InputData_HWAssessment_InitialAssessments_Oct2018.R")
library(HumpbackSIR)

# Reference
sir_base <- HUMPBACK.SIR(file_name = "Reference/reference",
                         n_resamples = 10000,
                         priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                  N_obs = make_prior(runif, 500, 40000)),
                         catch_multipliers = make_multiplier_list(make_prior(1)),
                         target.Yr = 2008,
                         num.haplotypes = 0,
                         output.Yrs = c(2018),
                         abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                         rel.abundance = Rel.Abundance.Pavanato,
                         rel.abundance.key = FALSE, # No indices of abundance
                         count.data = Count.Data,
                         count.data.key = FALSE,
                         growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                         growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                         catch.data = Core.Catches,
                         premodern_catch_data = NULL,
                         control = sir_control(threshold = 4e-9, progress_bar = TRUE))
resample_summary_base <- summary_sir(sir_base$resamples_output, object = "Resample_Summary", file_name = "Reference/Scenario__base")
resample_summary_base$output_summary[,1:2]
trajectory_summary_base <- summary_sir(sir_base$resamples_trajectories, object = "Trajectory_Summary", file_name = "Reference/Scenario__base")
plot_trajectory(sir_base,  file_name = "Reference/Scenario__base")
plot_density(sir_base,  file_name = "Reference/Scenario__base")
save(sir_base, file = "Reference/sir__base.RData")
zerbini_table( sir_base, file_name = "Reference/Scenario__base" )


################################################################################
# SDATA Runs
################################################################################

# SData 1
sir_sdata_1 <- HUMPBACK.SIR(file_name = "SData 1/SData 1",
                            n_resamples = 10000,
                            priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                     N_obs = make_prior(runif, 500, 40000)),
                            catch_multipliers = make_multiplier_list(make_prior(1)),
                            premodern_catch_multipliers = make_multiplier_list(make_prior(1)),
                            target.Yr = 2012,
                            num.haplotypes = 0,
                            output.Yrs = c(2018),
                            abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                            rel.abundance = Rel.Abundance.Pavanato,
                            rel.abundance.key = FALSE, # No indices of abundance
                            count.data = Count.Data,
                            count.data.key = FALSE,
                            growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                            growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                            catch.data = Core.Catches,
                            control = sir_control(threshold = 4e-9, progress_bar = TRUE))
resample_summary_sdata_1 <- summary_sir(sir_sdata_1$resamples_output, object = "Resample_Summary", file_name = "SData 1/SData 1")
trajectory_summary_sdata_1 <- summary_sir(sir_sdata_1$resamples_trajectories, object = "Trajectory_Summary", file_name = "SData 1/SData 1")
plot_trajectory(sir_sdata_1,  file_name = "SData 1/SData 1")
plot_density(sir_sdata_1,  file_name = "SData 1/SData 1")
save(sir_sdata_1, file = "SData 1/sir_sdata1.RData")
zerbini_table(sir_sdata_1,  file_name = "SData 1/SData 1")



# SData 2
sir_sdata_2 <- HUMPBACK.SIR(file_name = "SData 2/SData 2",
                            n_resamples = 10000,
                            priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                     N_obs = make_prior(runif, 500, 40000)),
                            catch_multipliers = make_multiplier_list(make_prior(1)),
                            target.Yr = 2008,
                            num.haplotypes = 0,
                            output.Yrs = c(2018),
                            abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                            rel.abundance = Rel.Abundance.Pavanato,
                            rel.abundance.key = TRUE, # No indices of abundance
                            count.data = Count.Data,
                            count.data.key = FALSE,
                            growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                            growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                            catch.data = Core.Catches,
                            control = sir_control(threshold = 2e-18, progress_bar = TRUE))
resample_summary_sdata_2 <- summary_sir(sir_sdata_2$resamples_output, object = "Resample_Summary", file_name = "SData 2/SData 2")
trajectory_summary_sdata_2 <- summary_sir(sir_sdata_2$resamples_trajectories, object = "Trajectory_Summary", file_name = "SData 2/SData 2")
plot_trajectory(sir_sdata_2,  file_name = "SData 2/SData 2")
plot_density(sir_sdata_2,  file_name = "SData 2/SData 2")
plot_ioa(sir_sdata_2,  file_name = "SData 2/SData 2")
save(sir_sdata_2, file = "SData 2/sir_sdata2.RData")
zerbini_table(sir_sdata_2,  file_name = "SData 2/SData 2")



# SData 3
sir_sdata_3 <- HUMPBACK.SIR(file_name = "SData 3/SData 3",
                            n_resamples = 10000,
                            priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                     N_obs = make_prior(runif, 500, 40000)),
                            catch_multipliers = make_multiplier_list(make_prior(1)),
                            target.Yr = 2008,
                            num.haplotypes = 0,
                            output.Yrs = c(2018),
                            abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                            rel.abundance = Rel.Abundance.Wedekin,
                            rel.abundance.key = TRUE, # No indices of abundance
                            count.data = Count.Data,
                            count.data.key = FALSE,
                            growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                            growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                            catch.data = Core.Catches,
                            control = sir_control(threshold = 2e-27, progress_bar = TRUE))
resample_summary_sdata_3 <- summary_sir(sir_sdata_3$resamples_output, object = "Resample_Summary", file_name = "SData 3/SData 3")
trajectory_summary_sdata_3 <- summary_sir(sir_sdata_3$resamples_trajectories, object = "Trajectory_Summary", file_name = "SData 3/SData 3")
plot_trajectory(sir_sdata_3,  file_name = "SData 3/SData 3")
plot_density(sir_sdata_3,  file_name = "SData 3/SData 3")
plot_ioa(sir_sdata_3,  file_name = "SData 3/SData 3")
save(sir_sdata_3, file = "SData 3/sir_sdata3.RData")
zerbini_table(sir_sdata_3,  file_name = "SData 3/SData 3")

plot_density(list(sir_base, sir_sdata_2, sir_sdata_3),  file_name = "Reference_vs_sdata1-3", multiple_sirs = TRUE)

################################################################################
# SCATCH Runs
################################################################################

# SCatch 1
sir_scatch_1 <- HUMPBACK.SIR(file_name = "SCatch 1/SCatch 1",
                             n_resamples = 10000,
                             priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                      N_obs = make_prior(runif, 500, 40000)),
                             catch_multipliers = make_multiplier_list(make_prior(1), make_prior(1)),
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
                             catch.data = merge(Core.Catches, Falkland.Catches, by = "Year", all = T),
                             premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                             control = sir_control(threshold = 5e-9, progress_bar = TRUE))
resample_summary_scatch_1 <- summary_sir(sir_scatch_1$resamples_output, object = "Resample_Summary", file_name = "SCatch 1/SCatch 1")
trajectory_summary_scatch_1 <- summary_sir(sir_scatch_1$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 1/SCatch 1")
plot_trajectory(sir_scatch_1,  file_name = "SCatch 1/SCatch 1")
plot_density(sir_scatch_1,  file_name = "SCatch 1/SCatch 1")
save(sir_scatch_1, file = "SCatch 1/sir_scatch1.RData")
zerbini_table(sir_scatch_1,  file_name = "SCatch 1/SCatch 1")




# SCatch 2
sir_scatch_2 <- HUMPBACK.SIR(file_name = "SCatch 2/SCatch 2",
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
                             catch.data = Fringe.Catches,
                             premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                             control = sir_control(threshold = 5e-9, progress_bar = TRUE))
resample_summary_scatch_2 <- summary_sir(sir_scatch_2$resamples_output, object = "Resample_Summary", file_name = "SCatch 2/SCatch 2")
trajectory_summary_scatch_2 <- summary_sir(sir_scatch_2$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 2/SCatch 2")
plot_trajectory(sir_scatch_2,  file_name = "SCatch 2/SCatch 2")
plot_density(sir_scatch_2,  file_name = "SCatch 2/SCatch 2")
save(sir_scatch_2, file = "SCatch 2/sir_scatch2.RData")
zerbini_table(sir_scatch_2,  file_name = "SCatch 2/SCatch 2")




# SCatch 3
sir_scatch_3 <- HUMPBACK.SIR(file_name = "SCatch 3/SCatch 3",
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
                             catch.data = Overlap.Catches,
                             premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T),
                             control = sir_control(threshold = 3e-9, progress_bar = TRUE))
resample_summary_scatch_3 <- summary_sir(sir_scatch_3$resamples_output, object = "Resample_Summary", file_name = "SCatch 3/SCatch 3")
trajectory_summary_scatch_3 <- summary_sir(sir_scatch_3$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 3/SCatch 3")
plot_trajectory(sir_scatch_3,  file_name = "SCatch 3/SCatch 3")
plot_density(sir_scatch_3,  file_name = "SCatch 3/SCatch 3")
save(sir_scatch_3, file = "SCatch 3/sir_scatch3.RData")
zerbini_table(sir_scatch_3,  file_name = "SCatch 3/SCatch 3")




# SCatch 4
sir_scatch_4 <- HUMPBACK.SIR(file_name = "SCatch 4/SCatch 4",
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
                             control = sir_control(threshold = 6e-9, progress_bar = TRUE))
resample_summary_scatch_4 <- summary_sir(sir_scatch_4$resamples_output, object = "Resample_Summary", file_name = "SCatch 4/SCatch 4")
trajectory_summary_scatch_4 <- summary_sir(sir_scatch_4$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 4/SCatch 4")
plot_trajectory(sir_scatch_4,  file_name = "SCatch 4/SCatch 4")
plot_density(sir_scatch_4,  file_name = "SCatch 4/SCatch 4")
save(sir_scatch_4, file = "SCatch 4/sir_scatch4.RData")
zerbini_table(sir_scatch_4,  file_name = "SCatch 4/SCatch 4")




# SCatch 5
Core.1901.1938 <- Core.Catches[Core.Catches$Year %in% c(1901:1938), ]
Core.1939.1945 <- Core.Catches[Core.Catches$Year %in% c(1939:1945), ]
Core.1946.present <- Core.Catches[Core.Catches$Year > 1945, ]
Core.Catches.Reorg <- merge(Core.1901.1938, Core.1939.1945, by = "Year", all = TRUE)
Core.Catches.Reorg <- merge(Core.Catches.Reorg, Core.1946.present, by = "Year", all = TRUE)

sir_scatch_5 <- HUMPBACK.SIR(file_name = "SCatch 5/SCatch 5",
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
                             control = sir_control(threshold = 6e-9, progress_bar = TRUE))
resample_summary_scatch_5 <- summary_sir(sir_scatch_5$resamples_output, object = "Resample_Summary", file_name = "SCatch 5/SCatch 5")
trajectory_summary_scatch_5 <- summary_sir(sir_scatch_5$resamples_trajectories, object = "Trajectory_Summary", file_name = "SCatch 5/SCatch 5")
plot_trajectory(sir_scatch_5,  file_name = "SCatch 5/SCatch 5")
plot_density(sir_scatch_5,  file_name = "SCatch 5/SCatch 5")
save(sir_scatch_1, file = "SCatch 5/sir_scatch5.RData")
zerbini_table(sir_scatch_5,  file_name = "SCatch 5/SCatch 5")

plot_density(list(sir_base, sir_scatch_1, sir_scatch_2, sir_scatch_3, sir_scatch_4, sir_scatch_5),  file_name = "Reference_vs_scatch1-5", multiple_sirs = T)

################################################################################
# GENETIC Constraint Runs
################################################################################

# SCatch 1
sir_GC_1 <- HUMPBACK.SIR(file_name = "GC 1/GC 1",
                         n_resamples = 10000,
                         priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                  N_obs = make_prior(runif, 500, 40000)),
                         catch_multipliers = make_multiplier_list(make_prior(1)),
                         premodern_catch_multipliers = make_multiplier_list(make_prior(1)),
                         target.Yr = 2008,
                         num.haplotypes = 66,
                         output.Yrs = c(2018),
                         abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                         rel.abundance = Rel.Abundance.Wedekin,
                         rel.abundance.key = FALSE, # No indices of abundance
                         count.data = Count.Data,
                         count.data.key = FALSE,
                         growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                         growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                         catch.data = Core.Catches,
                         premodern_catch_data = NULL,
                         control = sir_control(threshold = 4e-9, progress_bar = TRUE))
resample_summary_GC_1 <- summary_sir(sir_GC_1$resamples_output, object = "Resample_Summary", file_name = "GC 1/GC 1")
trajectory_summary_GC_1 <- summary_sir(sir_GC_1$resamples_trajectories, object = "Trajectory_Summary", file_name = "GC 1/GC 1")
plot_trajectory(sir_GC_1,  file_name = "GC 1/GC 1")
plot_density(sir_GC_1,  file_name = "GC 1/GC 1")
save(sir_GC_1, file = "GC 1/sir_gc1.RData")
zerbini_table(sir_GC_1,  file_name = "GC 1/GC 1")
plot_density(list(sir_base, sir_GC_1),  file_name = "Reference_vs_sgc", multiple_sirs = T)
