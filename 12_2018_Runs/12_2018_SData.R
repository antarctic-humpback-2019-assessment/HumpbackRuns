
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
