setwd("C:/Users/Grant Adams/Google Drive/2018_Zerbini_et_al/Runs/9_26_18_Runs")

#Data for Humpback Assessment Model - Preliminary Runs Sep 2018
#Sent to Grant Adams and John Best on 25 Sep 2018
##################################################################

#2005 Absoulute Abundance
Abs.Abundance.2005 <- data.frame(Year=c(2005),
                                 N.obs=c(6404),
                                 CV.obs=c(0.12))

#2008 Absolute Abundance
Abs.Abundance.2008 <- data.frame(Year=c(2008),
                                 N.obs=c(14264),
                                 CV.obs=c(0.084))

#2008 Absolute Abundance
Abs.Abundance.2012 <- data.frame(Year=c(2012),
                                 N.obs=c(20389),
                                 CV.obs=c(0.071))


#Index of Abundance from Pavanato et al. 2017, Ecol. Modelling, Table 3
Rel.Abundance.Pavanato <- data.frame(
    Index = rep(1,3),
    Year=c(2008, 2011, 2015),
    IA.obs=c(7689, 8652, 12123),
    CV.IA.obs=c(0.078, 0.066, 0.07))

#Catch Series Modern
Core.Catches <- data.frame(Year=seq(1901,2018),
                           Catch=c(0, 0, 0, 180, 288, 240, 1261, 1849, 3391, 6468, 5832, 2881, 999, 1155,
                                   1697, 447, 121, 129, 111, 102, 9, 364, 133, 266, 254, 7, 0, 19, 51, 107,
                                   18, 23, 132, 57, 48, 105, 242, 0, 2, 36, 13, 0, 4, 60, 238, 30, 35, 48,
                                   83, 698, 45, 34, 140, 44, 96, 167, 61, 16, 15, 27, 13, 24, 12, 0, 52, 0,
                                   189, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0 ,0, 0))

PreModern.Catch <- data.frame(Year=seq(1830,1924),
                              Catch=c(rep(400, 10), #1830-1839
                                      rep(402.8, 10), #1840-1849
                                      rep(400, 10), #1850-1859
                                      rep(418.1, 10), #1860-1869
                                      rep(400, 24), #1870-1893
                                      448, #1894
                                      rep(400, 6), #1895-1900
                                      rep(581.7, 2), #1901-1902
                                      400, #1903
                                      581.7, #1904 (first year with pre-modern + modern)
                                      581.7, #1905 (pre-modern + modern)
                                      400, #1906
                                      400, #1907
                                      406.5, #1908
                                      228.03, #1909
                                      246.62, #1910
                                      rep(30, 14))) #1911-1924
Core.Catches$Period <- 1

# Combine catches
Catch_Combined <- merge( PreModern.Catch , Core.Catches, by = "Year", all.x = T, all.y = T)
Catch_Combined$Period <- 1
Catch_Combined$Catch.x[which(is.na(Catch_Combined$Catch.x))] <- 0
Catch_Combined$Catch.y[which(is.na(Catch_Combined$Catch.y))] <- 0
Catch_Combined$Catch <- Catch_Combined$Catch.x + Catch_Combined$Catch.y
Catch_Combined <- subset(Catch_Combined, select = -c(Catch.y, Catch.x))

# Base case
sir_base <- HUMPBACK.SIR(file_name = "Base",
                         n_resamples = 10000,
                         priors = make_prior_list(),
                         catch_multipliers = make_multiplier_list(),
                         target.Yr = 2005,
                         num.haplotypes = 0,
                         output.Yrs = c(2005, 2018),
                         abs.abundance = Abs.Abundance.2005,
                         rel.abundance = Rel.Abundance,
                         rel.abundance.key = FALSE,
                         count.data = Count.Data,
                         count.data.key = FALSE,
                         growth.rate.obs = c(0.074, 0.033, TRUE),
                         growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                         catch.data = Core.Catches,
                         control = sir_control(threshold = 5e-3, progress_bar = T))
resample_summary_base <- summary_sir(sir_base$resamples_output, object = "Resample_Summary", file_name = "Scenario__base")
resample_summary_base$output_summary[,1:2]
trajectory_summary_base <- summary_sir(sir_base$resamples_trajectories, object = "Trajectory_Summary", file_name = "Scenario__base")
plot_sir(sir_base,  file_name = "Scenario__base")
zerbini_table( resample_summary_base, file_name = "Scenario__base" )

# Scenario 1
sir1 <- HUMPBACK.SIR(file_name = "Preliminary_Run_Scenario_1",
                    n_resamples = 10000,
                    priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                             N_obs = make_prior(runif, 500, 40000)),
                    catch_multipliers = make_multiplier_list(make_prior(1)),
                    target.Yr = 2005,
                    num.haplotypes = 0,
                    output.Yrs = c(2005, 2018),
                    abs.abundance = Abs.Abundance.2005,
                    rel.abundance = Rel.Abundance.Pavanato,
                    rel.abundance.key = FALSE, # No indices of abundance
                    count.data = Count.Data,
                    count.data.key = FALSE,
                    growth.rate.obs = c(0.074, 0.033, TRUE), # Include growth rate
                    growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                    catch.data = Core.Catches,
                    control = sir_control(threshold = 1e-3, progress_bar = TRUE))
resample_summary1 <- summary_sir(sir1$resamples_output, object = "Resample_Summary", file_name = "Scenario_1")
resample_summary1$output_summary[,1:2]
trajectory_summary1 <- summary_sir(sir1$resamples_trajectories, object = "Trajectory_Summary", file_name = "Scenario_1")
plot_sir(sir1,  file_name = "Scenario_1")
zerbini_table( resample_summary1, file_name = "Scenario_1" )


# Scenario 2
sir2 <- HUMPBACK.SIR(file_name = "Preliminary_Run_Scenario_2",
                     n_resamples = 10000,
                     priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                              N_obs = make_prior(runif, 500, 40000)),
                     catch_multipliers = make_multiplier_list(make_prior(1)),
                     target.Yr = 2008,
                     num.haplotypes = 0,
                     output.Yrs = c(2008, 2018),
                     abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                     rel.abundance = Rel.Abundance.Pavanato,
                     rel.abundance.key = FALSE, # No indices of abundance
                     count.data = Count.Data,
                     count.data.key = FALSE,
                     growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                     growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                     catch.data = Core.Catches,
                     control = sir_control(threshold = 4e-9, progress_bar = TRUE))
resample_summary2 <- summary_sir(sir2$resamples_output, object = "Resample_Summary", file_name = "Scenario_2")
trajectory_summary2 <- summary_sir(sir2$resamples_trajectories, object = "Trajectory_Summary", file_name = "Scenario_2")
plot_sir(sir2,  file_name = "Scenario_2")
zerbini_table( resample_summary2, file_name = "Scenario_2" )


# Scenario 3
sir3 <- HUMPBACK.SIR(file_name = "Preliminary_Run_Scenario_3",
                     n_resamples = 10000,
                     priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                              N_obs = make_prior(runif, 500, 40000)),
                     catch_multipliers = make_multiplier_list(make_prior(1)),
                     target.Yr = 2008,
                     num.haplotypes = 0,
                     output.Yrs = c(2008, 2018),
                     abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                     rel.abundance = Rel.Abundance.Pavanato,
                     rel.abundance.key = FALSE, # No indices of abundance
                     count.data = Count.Data,
                     count.data.key = FALSE,
                     growth.rate.obs = c(0.074, 0.033, TRUE), # Include growth rate
                     growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                     catch.data = Core.Catches,
                     control = sir_control(threshold = 2e-07, progress_bar = TRUE))
resample_summary3 <- summary_sir(sir3$resamples_output, object = "Resample_Summary", file_name = "Scenario_3")
trajectory_summary3 <- summary_sir(sir3$resamples_trajectories, object = "Trajectory_Summary", file_name = "Scenario_3")
plot_sir(sir3,  file_name = "Scenario_3")
zerbini_table( resample_summary3, file_name = "Scenario_3" )


# Scenario 4
sir4 <- HUMPBACK.SIR(file_name = "Preliminary_Run_Scenario_4",
                     n_resamples = 10000,
                     priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                              N_obs = make_prior(runif, 500, 40000)),
                     catch_multipliers = make_multiplier_list(make_prior(1)),
                     target.Yr = 2008,
                     num.haplotypes = 0,
                     output.Yrs = c(2008, 2018),
                     abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                     rel.abundance = Rel.Abundance.Pavanato,
                     rel.abundance.key = TRUE, # No indices of abundance
                     count.data = Count.Data,
                     count.data.key = FALSE,
                     growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                     growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                     catch.data = Core.Catches,
                     control = sir_control(threshold = 2e-18, progress_bar = TRUE))
 resample_summary4 <- summary_sir(sir4$resamples_output, object = "Resample_Summary", file_name = "Scenario_4")
trajectory_summary4 <- summary_sir(sir4$resamples_trajectories, object = "Trajectory_Summary", file_name = "Scenario_4")
plot_sir(sir4,  file_name = "Scenario_4")
zerbini_table( resample_summary4, file_name = "Scenario_4" )

# Scenario 5
sir5 <- HUMPBACK.SIR(file_name = "Preliminary_Run_Scenario_5",
                     n_resamples = 10000,
                     priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                              N_obs = make_prior(runif, 500, 40000)),
                     catch_multipliers = make_multiplier_list(make_prior(1)),
                     target.Yr = 2008,
                     num.haplotypes = 0,
                     output.Yrs = c(2008, 2018),
                     abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                     rel.abundance = Rel.Abundance.Pavanato,
                     rel.abundance.key = FALSE, # Do not include indices of abundance
                     count.data = Count.Data,
                     count.data.key = FALSE,
                     growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                     growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                     catch.data = Catch_Combined,
                     control = sir_control(threshold = 1e-8, progress_bar = TRUE))
resample_summary5 <- summary_sir(sir5$resamples_output, object = "Resample_Summary", file_name = "Scenario_5")
trajectory_summary5 <- summary_sir(sir5$resamples_trajectories, object = "Trajectory_Summary", file_name = "Scenario_5")
plot_sir(sir5,  file_name = "Scenario_5")
zerbini_table( resample_summary5, file_name = "Scenario_5" )


# Scenario 6
Catch_Combined2 <- Catch_Combined
Catch_Combined2$Period[which(Catch_Combined2$Year > 1935)] <- 2

sir6 <- HUMPBACK.SIR(file_name = "Preliminary_Run_Scenario_6",
                     n_resamples = 10000,
                     priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                                              N_obs = make_prior(runif, 500, 40000)),
                     catch_multipliers = make_multiplier_list(make_prior(runif, 1, 1.12), make_prior(1)),
                     target.Yr = 2008,
                     num.haplotypes = 0,
                     output.Yrs = c(2008, 2018),
                     abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012),
                     rel.abundance = Rel.Abundance.Pavanato,
                     rel.abundance.key = FALSE, # Do not include indices of abundance
                     count.data =   Count.Data,
                     count.data.key = FALSE,
                     growth.rate.obs = c(0.074, 0.033, FALSE), # Do not include growth rate
                     growth.rate.Yrs = c(1995, 1996, 1997, 1998),
                     catch.data = Catch_Combined2,
                     control = sir_control(threshold = 1e-8, progress_bar = TRUE))
resample_summary6 <- summary_sir(sir6$resamples_output, object = "Resample_Summary", file_name = "Scenario_6")
trajectory_summary6 <- summary_sir(sir6$resamples_trajectories, object = "Trajectory_Summary", file_name = "Scenario_6")
plot_sir(sir6,  file_name = "Scenario_6")
zerbini_table( resample_summary6, file_name = "Scenario_6" )
