
# Load data
source("R code/InputData_HWAssessment_InitialAssessments_Oct2018.R")
library(HumpbackSIR)

################################################################################
# Prior Runs
################################################################################
# Prior 1
sir_prior_1 <- HUMPBACK.SIR(file_name = "Prior 1/Prior 1",
                         n_resamples = 10000,
                         priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                  N_obs = make_prior(runif, 500, 40000)),
                         catch_multipliers = make_multiplier_list(make_prior(1)),
                         target.Yr = 2008,
                         num.haplotypes = 0,
                         output.Yrs = c(2012, 2018),
                         abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                         rel.abundance = Rel.Abundance.Pavanato,
                         rel.abundance.key = FALSE, # No indices of abundance
                         count.data = Count.Data,
                         count.data.key = FALSE,
                         growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                         growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                         catch.data = Core.Catches2,
                         premodern_catch_data = NULL,
                         control = sir_control(threshold = 10 * 4e-9, progress_bar = TRUE),
                         realized_prior = TRUE)
resample_summary_prior_1 <- summary_sir(sir_prior_1$resamples_output, object = "Resample_Summary", file_name = "Prior 1/Scenario__prior_1")
resample_summary_prior_1$output_summary[,1:2]
trajectory_summary_prior_1 <- summary_sir(sir_prior_1$resamples_trajectories, object = "Trajectory_Summary", file_name = "Prior 1/Scenario_prior_1")
plot_trajectory(sir_prior_1,  file_name = "Prior 1/Scenario_prior_1")
plot_density(list(sir_prior_1, sir_prior_1),  file_name = "Prior 1/Scenario_prior_1", multiple_sirs = TRUE)
save(sir_prior_1, file = "Prior 1/sir_prior_1.RData")
zerbini_table( sir_prior_1, file_name = "Prior 1/Scenario_prior_1" )



# Prior 2
sir_prior_2 <- HUMPBACK.SIR(file_name = "Prior 2/Prior 2",
                            n_resamples = 10000,
                            priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                     N_obs = make_prior(runif, 500, 40000)),
                            catch_multipliers = make_multiplier_list(make_prior(1)),
                            premodern_catch_multipliers = make_multiplier_list(make_prior(1)),
                            target.Yr = 2012,
                            num.haplotypes = 0,
                            output.Yrs = c(2008, 2018),
                            abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                            rel.abundance = Rel.Abundance.Pavanato,
                            rel.abundance.key = FALSE, # No indices of abundance
                            count.data = Count.Data,
                            count.data.key = FALSE,
                            growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                            growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                            catch.data = Core.Catches2,
                            control = sir_control(threshold = 10 * 4e-9, progress_bar = TRUE),
                            realized_prior = TRUE)
resample_summary_prior_2 <- summary_sir(sir_prior_2$resamples_output, object = "Resample_Summary", file_name = "Prior 2/Scenario__prior_2")
resample_summary_prior_2$output_summary[,1:2]
trajectory_summary_prior_2 <- summary_sir(sir_prior_2$resamples_trajectories, object = "Trajectory_Summary", file_name = "Prior 2/Scenario_prior_2")
plot_trajectory(sir_prior_2,  file_name = "Prior 2/Scenario_prior_2")
plot_density(list(sir_prior_1, sir_prior_2),  file_name = "Prior 2/Scenario_prior_2", multiple_sirs = TRUE)
save(sir_prior_2, file = "Prior 2/sir_prior_2.RData")
zerbini_table( sir_prior_2, file_name = "Prior 2/Scenario_prior_2" )
plot_density(list(sir_prior_1, sir_prior_2),  file_name = "Realized prior", multiple_sirs = TRUE)

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
                         catch.data = Core.Catches2,
                         premodern_catch_data = NULL,
                         control = sir_control(threshold = 10 * 4e-9, progress_bar = TRUE))
resample_summary_base <- summary_sir(sir_base$resamples_output, object = "Resample_Summary", file_name = "Reference/Scenario__base")
resample_summary_base$output_summary[,1:2]
trajectory_summary_base <- summary_sir(sir_base$resamples_trajectories, object = "Trajectory_Summary", file_name = "Reference/Scenario__base")
plot_trajectory(sir_base,  file_name = "Reference/Scenario__base")
plot_density(list(sir_base, sir_base),  file_name = "Reference/Scenario__base", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
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
                            output.Yrs = c(2008, 2018),
                            abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                            rel.abundance = Rel.Abundance.Pavanato,
                            rel.abundance.key = FALSE, # No indices of abundance
                            count.data = Count.Data,
                            count.data.key = FALSE,
                            growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                            growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                            catch.data = Core.Catches2,
                            control = sir_control(threshold = 10 * 4e-9, progress_bar = TRUE))
resample_summary_sdata_1 <- summary_sir(sir_sdata_1$resamples_output, object = "Resample_Summary", file_name = "SData 1/SData 1")
trajectory_summary_sdata_1 <- summary_sir(sir_sdata_1$resamples_trajectories, object = "Trajectory_Summary", file_name = "SData 1/SData 1")
plot_trajectory(sir_sdata_1,  file_name = "SData 1/SData 1")
plot_density(list(sir_base2, sir_sdata_1),  file_name = "SData 1/SData 1", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, NA, 0.1, NA, NA))
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
                            output.Yrs = c(2012, 2018),
                            abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                            rel.abundance = Rel.Abundance.Pavanato,
                            rel.abundance.key = TRUE, # Yes indices of abundance
                            count.data = Count.Data,
                            count.data.key = FALSE,
                            growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                            growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                            catch.data = Core.Catches2,
                            control = sir_control(threshold = 10 * 2e-18, progress_bar = TRUE))
resample_summary_sdata_2 <- summary_sir(sir_sdata_2$resamples_output, object = "Resample_Summary", file_name = "SData 2/SData 2")
trajectory_summary_sdata_2 <- summary_sir(sir_sdata_2$resamples_trajectories, object = "Trajectory_Summary", file_name = "SData 2/SData 2")
plot_trajectory(sir_sdata_2,  file_name = "SData 2/SData 2")
plot_density(list(sir_base, sir_sdata_2),  file_name = "SData 2/SData 2", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
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
                            output.Yrs = c(2012, 2018),
                            abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                            rel.abundance = Rel.Abundance.Wedekin,
                            rel.abundance.key = TRUE, # Yes indices of abundance
                            count.data = Count.Data,
                            count.data.key = FALSE,
                            growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                            growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                            catch.data = Core.Catches2,
                            control = sir_control(threshold = 10 * 2e-27, progress_bar = TRUE))
resample_summary_sdata_3 <- summary_sir(sir_sdata_3$resamples_output, object = "Resample_Summary", file_name = "SData 3/SData 3")
trajectory_summary_sdata_3 <- summary_sir(sir_sdata_3$resamples_trajectories, object = "Trajectory_Summary", file_name = "SData 3/SData 3")
plot_trajectory(sir_sdata_3,  file_name = "SData 3/SData 3")
plot_density(list(sir_base, sir_sdata_3),  file_name = "SData 3/SData 3", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
plot_ioa(sir_sdata_3,  file_name = "SData 3/SData 3")
save(sir_sdata_3, file = "SData 3/sir_sdata3.RData")
zerbini_table(sir_sdata_3,  file_name = "SData 3/SData 3")



# SData 4
sir_sdata_4 <- HUMPBACK.SIR(file_name = "SData 4/SData 4",
                            n_resamples = 10000,
                            priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                     N_obs = make_prior(runif, 500, 40000)),
                            catch_multipliers = make_multiplier_list(make_prior(1)),
                            target.Yr = 2008,
                            num.haplotypes = 0,
                            output.Yrs = c(2012, 2018),
                            abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                            rel.abundance = Rel.Abundance.Branch,
                            rel.abundance.key = TRUE, # Yes indices of abundance
                            count.data = Count.Data,
                            count.data.key = FALSE,
                            growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                            growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                            catch.data = Core.Catches2,
                            control = sir_control(threshold = 10 * 2e-15, progress_bar = TRUE))
resample_summary_sdata_4 <- summary_sir(sir_sdata_4$resamples_output, object = "Resample_Summary", file_name = "SData 4/SData 4")
trajectory_summary_sdata_4 <- summary_sir(sir_sdata_4$resamples_trajectories, object = "Trajectory_Summary", file_name = "SData 4/SData 4")
plot_trajectory(sir_sdata_4,  file_name = "SData 4/SData 4")
plot_density(list(sir_base, sir_sdata_4),  file_name = "SData 4/SData 4", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
plot_ioa(sir_sdata_4,  file_name = "SData 4/SData 4")
save(sir_sdata_4, file = "SData 4/sir_sdata4.RData")
zerbini_table(sir_sdata_4,  file_name = "SData 4/SData 4")




# SData 5
# Approximate distribution
sdlow <- uniroot( function(x){qnorm(0.025, mean = 0.086, sd = x)} - 0.05, interval = c(0.001, 5) )$root
sdhigh <- uniroot( function(x){qnorm(0.975, mean = 0.086, sd = x)} - 0.114, interval = c(0.001, 5) )$root
sd_approx <- mean(c(sdlow, sdhigh))

qnorm(c(0.025, 0.975), mean = 0.086, sd = sdlow)
qnorm(c(0.025, 0.975), mean = 0.086, sd = sdhigh)
qnorm(c(0.025, 0.975), mean = 0.086, sd = sd_approx)

# sdlow <- uniroot( function(x){qlnorm(0.025, meanlog = log(0.086), sdlog = x)} - 0.05, interval = c(0.001, 5) )$root
# sdhigh <- uniroot( function(x){qlnorm(0.975, meanlog = log(0.086), sdlog = x)} - 0.114, interval = c(0.001, 5) )$root
# sd_approx <- mean(c(sdlow, sdhigh))
# 
# qlnorm(c(0.025, 0.975), meanlog = log(0.086), sdlog = sdlow)
# qlnorm(c(0.025, 0.975), meanlog = log(0.086), sdlog = sdhigh)
# qlnorm(c(0.025, 0.975), meanlog = log(0.086), sdlog = sd_approx)

sir_sdata_5 <- HUMPBACK.SIR(file_name = "SData 5/SData 5",
                            n_resamples = 10000,
                            priors = make_prior_list(r_max = make_prior(rnorm, 0.086, sd_approx),
                                                     N_obs = make_prior(runif, 500, 40000)),
                            catch_multipliers = make_multiplier_list(make_prior(1)),
                            target.Yr = 2008,
                            num.haplotypes = 0,
                            output.Yrs = c(2012, 2018),
                            abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                            rel.abundance = Rel.Abundance.Wedekin,
                            rel.abundance.key = FALSE, # No indices of abundance
                            count.data = Count.Data,
                            count.data.key = FALSE,
                            growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                            growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                            catch.data = Core.Catches2,
                            control = sir_control(threshold = 10 * 2e-8, progress_bar = TRUE))
resample_summary_sdata_5 <- summary_sir(sir_sdata_5$resamples_output, object = "Resample_Summary", file_name = "SData 5/SData 5")
trajectory_summary_sdata_5 <- summary_sir(sir_sdata_5$resamples_trajectories, object = "Trajectory_Summary", file_name = "SData 5/SData 5")
plot_trajectory(sir_sdata_5,  file_name = "SData 5/SData 5")
plot_density(list(sir_base, sir_sdata_5),  file_name = "SData 5/SData 5", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
plot_ioa(sir_sdata_5,  file_name = "SData 5/SData 5")
save(sir_sdata_5, file = "SData 5/sir_sdata5.RData")
zerbini_table(sir_sdata_5,  file_name = "SData 5/SData 5")



plot_density(list(sir_base, sir_sdata_2, sir_sdata_3, sir_sdata_4, sir_sdata_5),  file_name = "Reference_vs_sdata_2-5", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))

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

################################################################################
# GENETIC Constraint Runs
################################################################################

# GC 1
sir_GC_1 <- HUMPBACK.SIR(file_name = "GC 1/GC 1",
                         n_resamples = 10000,
                         priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                  N_obs = make_prior(runif, 500, 40000)),
                         catch_multipliers = make_multiplier_list(make_prior(1)),
                         premodern_catch_multipliers = make_multiplier_list(make_prior(1)),
                         target.Yr = 2008,
                         num.haplotypes = 54,
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
                         control = sir_control(threshold = 10 * 4e-9, progress_bar = TRUE))
resample_summary_GC_1 <- summary_sir(sir_GC_1$resamples_output, object = "Resample_Summary", file_name = "GC 1/GC 1")
trajectory_summary_GC_1 <- summary_sir(sir_GC_1$resamples_trajectories, object = "Trajectory_Summary", file_name = "GC 1/GC 1")
plot_trajectory(sir_GC_1,  file_name = "GC 1/GC 1")
plot_density(list(sir_base, sir_GC_1),  file_name = "GC 1/GC 1", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
save(sir_GC_1, file = "GC 1/sir_gc1.RData")
zerbini_table(sir_GC_1,  file_name = "GC 1/GC 1")



# GC 2
sir_GC_2 <- HUMPBACK.SIR(file_name = "GC 2/GC 2",
                         n_resamples = 10000,
                         priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                                  N_obs = make_prior(runif, 500, 40000)),
                         catch_multipliers = make_multiplier_list(make_prior(1)),
                         premodern_catch_multipliers = make_multiplier_list(make_prior(1)),
                         target.Yr = 2008,
                         num.haplotypes = 5,
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
                         control = sir_control(threshold = 10 * 4e-9, progress_bar = TRUE))
resample_summary_GC_2 <- summary_sir(sir_GC_2$resamples_output, object = "Resample_Summary", file_name = "GC 2/GC 2")
trajectory_summary_GC_2 <- summary_sir(sir_GC_2$resamples_trajectories, object = "Trajectory_Summary", file_name = "GC 2/GC 2")
plot_trajectory(sir_GC_2,  file_name = "GC 2/GC 2")
plot_density(list(sir_base, sir_GC_2),  file_name = "GC 2/GC 2", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))
save(sir_GC_2, file = "GC 2/sir_gc2.RData")
zerbini_table(sir_GC_2,  file_name = "GC 2/GC 2")


plot_density(list(sir_base, sir_GC_1, sir_GC_2),  file_name = "base_vs_GC_1-2", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))



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

################################################################################
# Summary fits
################################################################################
plot_density(list(sir_base, sir_sdata_1, sir_sdata_2, sir_sdata_3, sir_sdata_4, sir_sdata_5),  file_name = "base_vs_Sdata_1-5", multiple_sirs = TRUE, upper = c(NA, 28000, 2000, NA,NA,NA,.07,NA,NA,NA), lower = c(NA, 22000, 0, NA, NA, 19000, 0, NA, .5, .8))

plot_density(list(sir_scatch_1, sir_scatch_2, sir_scatch_3, sir_scatch_4, sir_scatch_5, sir_scatch_6),  file_name = "base_vs_Scatch_1-6", multiple_sirs = TRUE, lower = c(NA, 18000, 0, NA, 18000, 0, 0.3, 0.7), upper = c(NA, 34000, 2000, NA, 27000, 0.1, NA, NA))

plot_density(list(sir_base, sir_GC_1, sir_GC_2),  file_name = "base_vs_GC_1-2", multiple_sirs = TRUE, lower = c(NA, 22000, 0, NA, 20000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))

plot_density(list(sir_base, sir_MSYR_1, sir_MSYR_2),  file_name = "base_vs_MSYR_1-2", multiple_sirs = TRUE, lower = c(NA, 18000, 0, NA, 19000, 0, 0.5, 0.7), upper = c(NA, 30000, 2000, NA, NA, 0.1, NA, NA))

