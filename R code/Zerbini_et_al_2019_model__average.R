library(HumpbackSIR)

# Load all the models
file_names <- c("Reference/Reference",
                "SData 1/SData 1",
                "SData 2/SData 2",
                "SData 3/SData 3",
                "SData 4/SData 4",
                "SData 5/SData 5",
                "SData 6/SData 6",
                "SData 7/SData 7",
                "SCatch 1/SCatch 1",
                "SCatch 2/SCatch 2",
                "SCatch 3/SCatch 3",
                "SCatch 4/SCatch 4",
                "SCatch 5/SCatch 5",
                "SCatch 6/SCatch 6",
                "SCatch 7/SCatch 7",
                "GC 1/GC 1",
                "GC 2/GC 2",
                "MSYR 1/MSYR 1",
                "MSYR 2/MSYR 2",
                "Model average/model_average")

for(i in 1:length(file_names)){
  load(file = paste0("Model runs/",file_names[i], ".Rdata"))
}
                
                

#############################################################
#### Model averaging
#############################################################
# Get bayes factor for models with comparable likelihoods
bayes_f <- bayes_factor(SIR = list(sir_reference[[1]], 
                                   sir_sdata_1[[1]], 
                                   sir_sdata_7[[1]], 
                                   sir_catch_4[[1]],
                                   sir_catch_5[[1]],
                                   sir_catch_6[[1]],
                                   sir_catch_7[[1]],
                                   sir_msyr_1[[1]],
                                   sir_msyr_2[[1]]))


# Create a new model based on bayes factors
new_mod <- weight_model(SIR = list(sir_reference[[1]], 
                                   sir_sdata_1[[1]], 
                                   sir_sdata_7[[1]], 
                                   sir_catch_4[[1]],
                                   sir_catch_5[[1]],
                                   sir_catch_6[[1]],
                                   sir_catch_7[[1]],
                                   sir_msyr_1[[1]],
                                   sir_msyr_2[[1]]), 
                        bayes_factor = bayes_f)

# For plotting make a vector of bayes factors, set NA for models that cant be compared (different likelihood)
bayes_vec <- round(c(bayes_f[1], bayes_f[2], NA, NA, NA, NA, NA, bayes_f[3], NA, NA, NA, bayes_f[4], bayes_f[5], bayes_f[6], bayes_f[7], NA, NA, bayes_f[8], bayes_f[9], NA), 2)


# Compare Aposteriors of all
compare_posteriors(
  reference_sir = TRUE, 
  SIR = list(sir_reference[[1]],
             sir_sdata_1[[1]], 
             sir_sdata_2[[1]], 
             sir_sdata_3[[1]],
             sir_sdata_4[[1]],
             sir_sdata_5[[1]],
             sir_sdata_6[[1]],
             sir_sdata_7[[1]],
             sir_catch_1[[1]],
             sir_catch_2[[1]],
             sir_catch_3[[1]],
             sir_catch_4[[1]],
             sir_catch_5[[1]],
             sir_catch_6[[1]],
             sir_catch_7[[1]],
             sir_gc_1[[1]],
             sir_gc_2[[1]],
             sir_msyr_1[[1]],
             sir_msyr_2[[1]],
             new_mod), 
  model_names = c( "R", paste0("D ", 1:7), paste0("C ", 1:7), paste0("G ", 1:2), paste0("M ", 1:2), "MA"), 
  bayes_factor = bayes_vec,
  file_name = "Cross scenario comparison/Figure_3_",
  years = c(2008, 2019))

# Plot and get parameter values from Model Average
file_name <- "Model runs/Model average/model_average"
plot_trajectory(new_mod, Reference = sir_reference[[1]],  file_name = file_name)
plot_density(SIR = list(sir_reference[[1]], new_mod), priors = list(sir_reference[[2]]),  file_name = file_name,  lower = c(NA, 20000, NA, NA, NA, 15000, NA, 21000, NA, NA, NA, NA, 0.5, 0.85), upper = c(NA, NA, 2000, NA, 20500, NA, NA, NA,  0.06, NA, NA, NA, 1, 1))
plot_ioa(new_mod,  file_name = file_name, ioa_names = c("FG", "BG1"))
zerbini_table(new_mod,  file_name = file_name)
save(new_mod, file = paste0(file_name, ".Rdata"))
