

file_name <- "Reference/Reference"
plot_trajectory(sir_reference[[1]],  file_name = file_name)
plot_trajectory(sir_reference[[2]],  file_name = paste0(file_name, "prior"))
plot_density(SIR = list(sir_reference[[1]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA), priors = list(sir_reference[[2]]), inc_reference = FALSE)
plot_ioa(sir_reference[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto") )
zerbini_table(sir_reference[[1]],  file_name = file_name)


file_name <- "SData 1/SData 1"
plot_trajectory(SIR = sir_sdata_1[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_sdata_1[[1]]), priors = list(sir_reference[[2]], sir_sdata_1[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(SIR = sir_sdata_1[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_sdata_1[[1]],  file_name = file_name)


file_name <- "SData 2/SData 2"
plot_trajectory(sir_sdata_2[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_sdata_2[[1]]), priors = list(sir_reference[[2]], sir_sdata_2[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
zerbini_table(sir_sdata_2[[1]],  file_name = file_name)


file_name <- "SData 3/SData 3"
plot_trajectory(sir_sdata_3[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_sdata_3[[1]]), priors = list(sir_reference[[2]], sir_sdata_3[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_sdata_3[[1]],  file_name = file_name, ioa_names = c("Branch", "Wedekin"))
zerbini_table(sir_sdata_3[[1]],  file_name = file_name)


file_name <- "SData 4/SData 4"
plot_trajectory(sir_sdata_4[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_sdata_4[[1]]), priors = list(sir_reference[[2]], sir_sdata_4[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_sdata_4[[1]],  file_name = file_name, ioa_names = c("Pavanto"))
zerbini_table(sir_sdata_4[[1]],  file_name = file_name)


file_name <- "SData 5/SData 5"
plot_trajectory(sir_sdata_5[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_sdata_5[[1]]), priors = list(sir_reference[[2]], sir_sdata_5[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_sdata_5[[1]],  file_name = file_name, ioa_names = c("Pavanto"))
zerbini_table(sir_sdata_5[[1]],  file_name = file_name)


file_name <- "SData 6/SData 6"
plot_trajectory(sir_sdata_6[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_sdata_6[[1]]), priors = list(sir_reference[[2]], sir_sdata_6[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_sdata_6[[1]],  file_name = file_name, ioa_names = c("Branch"))
zerbini_table(sir_sdata_6[[1]],  file_name = file_name)


file_name <- "SData 7/SData 7"
plot_trajectory(sir_sdata_7[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_sdata_7[[1]]), priors = list(sir_reference[[2]], sir_sdata_7[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_sdata_7[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_sdata_7[[1]],  file_name = file_name)


file_name <- "SCatch 1/SCatch 1"
plot_trajectory(sir_catch_1[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_catch_1[[1]]), priors = list(sir_reference[[2]], sir_catch_1[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_catch_1[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_catch_1[[1]],  file_name = file_name)         


file_name <- "SCatch 2/SCatch 2"
plot_trajectory(sir_catch_2[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_catch_2[[1]]), priors = list(sir_reference[[2]], sir_catch_2[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_catch_2[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_catch_2[[1]],  file_name = file_name)


file_name <- "Scatch 3/Scatch 3"
plot_trajectory(sir_catch_3[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_catch_3[[1]]), priors = list(sir_reference[[2]], sir_catch_3[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_catch_3[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_catch_3[[1]],  file_name = file_name)  


file_name <- "Scatch 4/Scatch 4"
plot_trajectory(sir_catch_4[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_catch_4[[1]]), priors = list(sir_reference[[2]], sir_catch_4[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_catch_4[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_catch_4[[1]],  file_name = file_name)


##########################################
file_name <- "Scatch 5/Scatch 5"
plot_trajectory(sir_catch_5[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_catch_5[[1]]), priors = list(sir_reference[[2]], sir_catch_5[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_catch_5[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_catch_5[[1]],  file_name = file_name)


file_name <- "Scatch 6/Scatch 6"
plot_trajectory(sir_catch_6[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_catch_6[[1]]), priors = list(sir_reference[[2]], sir_catch_6[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_catch_6[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_catch_6[[1]],  file_name = file_name)



file_name <- "Scatch 7/Scatch 7"
plot_trajectory(sir_catch_7[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_catch_7[[1]]), priors = list(sir_reference[[2]], sir_catch_7[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_catch_7[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_catch_7[[1]],  file_name = file_name)



file_name <- "Scatch 8/Scatch 8"
plot_trajectory(sir_catch_8[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_catch_8[[1]]), priors = list(sir_reference[[2]], sir_catch_8[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_catch_8[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_catch_8[[1]],  file_name = file_name)



file_name <- "GC 1/GC 1"
plot_trajectory(sir_gc_1[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_gc_1[[1]]), priors = list(sir_reference[[2]], sir_gc_1[[2]]),  file_name = file_name, lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_gc_1[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_gc_1[[1]],  file_name = file_name)



file_name <- "GC 2/GC 2"
plot_trajectory(sir_gc_2[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_gc_2[[1]]), priors = list(sir_reference[[2]], sir_gc_2[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_gc_2[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_gc_2[[1]],  file_name = file_name)


file_name <- "MSYR 1/MSYR 1"
plot_trajectory(sir_msyr_1[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_msyr_1[[1]]), priors = list(sir_reference[[2]], sir_msyr_1[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_msyr_1[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_msyr_1[[1]],  file_name = file_name)


file_name <- "MSYR 2/MSYR 2"
plot_trajectory(sir_msyr_2[[1]], Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], sir_msyr_2[[1]]), priors = list(sir_reference[[2]], sir_msyr_2[[2]]),  file_name = file_name,  lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, NA, NA, NA, NA,  0.06, NA, NA, NA, NA, NA))
plot_ioa(sir_msyr_2[[1]],  file_name = file_name, ioa_names = c("Branch", "Pavanto"))
zerbini_table(sir_msyr_2[[1]],  file_name = file_name)