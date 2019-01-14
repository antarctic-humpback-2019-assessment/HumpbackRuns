file_name = paste0("Reference/Reference", c("","prior")[i])
n_resamples = 10000
priors = make_prior_list(r_max = make_prior(runif, 0, 0.118),
                         N_obs = make_prior(runif, 500, 40000))
catch_multipliers = make_multiplier_list(
  make_prior(rbest, 0.139, 0.3),
  make_prior(runif, 1.25, 1.42), 
  make_prior(rnorm, 1.0185, 0.0028))
premodern_catch_multipliers = make_multiplier_list(
  make_prior(rnorm, 1.71, 0.073))
target.Yr = 2008
num.haplotypes = 0
output.Yrs = c(2012, 2018)
abs.abundance = rbind(Abs.Abundance.2008, Abs.Abundance.2012)
rel.abundance = rel_abund
rel.abundance.key = TRUE # Indices of abundance
count.data = Count.Data
count.data.key = FALSE
growth.rate.obs = c(0.074, 0.033, FALSE) # Do not include growth rate
growth.rate.Yrs = c(1995, 1996, 1997, 1998)
catch.data = Core.Catches.Reorg
premodern_catch_data = merge(PreModern.Catch.Min, PreModern.Catch.Max, by = "Year", all = T)
control = sir_control(threshold = 1e-26, progress_bar = TRUE, verbose = 3)
realized_prior = ifelse(1, "FALSE", "TRUE")








begin.time <- Sys.time()

################################
# Assigning variables
################################
target.Yr <- target.Yr
## Use the first year of the projection is set as the first year in the
## catch series
start_yr <- min(catch.data$Year,
                ifelse(is.null(premodern_catch_data),  catch.data$Year, premodern_catch_data$Year))
## The last year of the projection is set as the last year in the catch or
## abundance series, whichever is most recent
end_yr <- max(tail(catch.data$Year, 1),
              max(abs.abundance$Year),
              max(rel.abundance$Year),
              output.Yrs,
              ifelse(is.null(premodern_catch_data),  catch.data$Year, premodern_catch_data$Year)) #FIXME: note should be able to handle if premodern_catch_data is NULL
## Setting the target year for the bisection method
bisection.Yrs <- target.Yr-start_yr + 1
## Setting the years to project
projection.Yrs <- end_yr-start_yr + 1
Year <- seq(start_yr, end_yr, by = 1)

# Expand the catch time series and fill missing values with 0
catch.data <- merge(data.frame(Year), catch.data, by="Year", all = TRUE)
catch.data[is.na(catch.data)] <- 0

if(!is.null(premodern_catch_data)){
  # Expand and fill with 0 for years with no data
  premodern_catch_data <- merge(data.frame(Year), premodern_catch_data, by="Year", all = TRUE)
  premodern_catch_data[is.na(premodern_catch_data)] <- 0
  
  # Extract catch series
  premodern_catch_original <- as.matrix(
    premodern_catch_data[,grep("catch", colnames(premodern_catch_data), ignore.case = T)])
  
  # Get min and max of premodern
  premodern_catch_min <- apply(premodern_catch_original, 1, FUN=min)
  premodern_catch_max <- apply(premodern_catch_original, 1, FUN=max)
  premodern_catch_dif <- premodern_catch_max - premodern_catch_min
}

## Assigning the catch data
catch_original <- as.matrix(catch.data[,grep("catch", colnames(catch.data), ignore.case = T)]) # Extract catch series
n_catch_period<- ncol(catch_original)

# Catch multiplier check
if(length(catch_multipliers) != n_catch_period){
  stop("Number of catch multipliers (",
       length(catch_multipliers),
       ") does not equal number of catch periods (",
       n_catch_period,
       "), using no multiplier.")
}

if(!is.null(premodern_catch_data)){
  if(length(premodern_catch_multipliers) == 0 ){#FIXME: remove if making it flexible for multiple premodern catch multipliers
    print("No premodern catch multipliers supplied, assuming none")
    premodern_catch_multipliers <- make_multiplier_list(c_mult_1 = make_prior(1))
  }
  
  if(length(premodern_catch_multipliers) > 1){#FIXME: remove if making it flexible for multiple premodern catch multipliers
    print("Multiple premodern catch multipliers supplied, assuming none")
    premodern_catch_multipliers <- make_multiplier_list(c_mult_1 = make_prior(1))
  }
}

## Determining the number of Indices of Abundance available
num.IA <- max(rel.abundance$Index)

## Determining the number of Count Data sets available
num.Count <- max(count.data$Index)

## Computing the value of sigma as in Zerbini et al. 2011
rel.abundance$Sigma <- sqrt(log(1 + rel.abundance$CV.IA.obs^2))

## Computing the value of sigma for the count data as in Zerbini et al. (2011)
count.data$Sigma <- sqrt(log(1 + count.data$CV.IA.obs^2))

## Computing the value of sigma as in Zerbini et al. 2011
abs.abundance$Sigma <- sqrt(log(1 + abs.abundance$CV.obs^2))

## Computing the minimum viable population, if num.haplotypes=0, assumes no MVP
MVP <- 3 * num.haplotypes

## Start the loop
i <- 0
## Keep track of number of draws
draw <- 1
Cumulative.Likelihood <- 0

#Creating output vectors
#-------------------------------------
sir_names <- c("r_max", "K", paste0("catch_multiplier_", 1:length(catch_multipliers)) ,
               paste0("premodern_catch_multiplier_", 1:length(premodern_catch_multipliers)),
               "sample.N.obs", "add_CV", "sample_premodern_catch", "Nmin", "YearMin",
               "violate_MVP", paste0("N", target.Yr), paste0("N", output.Yrs),
               paste0("ROI_IA", unique(rel.abundance$Index)),
               paste0("q_IA", unique(rel.abundance$Index)),
               paste0("ROI_Count", unique(count.data$Index)),
               paste0("q_Count", unique(count.data$Index)),
               "NLL.IAs", "NLL.Count", "NLL.N", "NLL.GR", "NLL", "Likelihood",
               "Max_Dep",paste0("status", target.Yr), paste("status", output.Yrs, sep = ""), "draw", "save")

resamples_output <- matrix(NA, nrow = n_resamples, ncol = length(sir_names))
resamples_trajectories <- matrix(NA, nrow = n_resamples, ncol = projection.Yrs)
catch_trajectories <- matrix(NA, nrow = n_resamples, ncol = projection.Yrs)
colnames(catch_trajectories) =  paste0("Catch_", Year)

if (control$progress_bar) {
  pb <- txtProgressBar(min = 0, max = n_resamples, style = 3)
}

#Initiating the SIR loop
while (i < n_resamples) {
  #Sampling from Priors
  #-------------------------------
  save <- FALSE #variable to indicate whether a specific draw is kept
  
  # Sampling for catch_multiplier
  sample_catch_multiplier <- sapply(catch_multipliers, function(x) x$rfn())
  catches <- rep(0, length(Year))
  for(p in 1:length(sample_catch_multiplier)){
    catches <- catches + (catch_original[,p] * sample_catch_multiplier[p]) # Multiply catches by multiplier and add
  }
  
  # Sample historic catch
  if(!is.null(premodern_catch_data)){
    sample_premodern_catch_multiplier <- sapply(premodern_catch_multipliers, function(x) x$rfn())
    sample_premodern_catch <- priors$premodern_catch_sample$rfn()
    
    catches <- catches + (premodern_catch_min + premodern_catch_dif * sample_premodern_catch) * sample_premodern_catch_multiplier
  } else{
    sample_premodern_catch_multiplier <- -999
    sample_premodern_catch <- -999
  }
  
  #Sampling for r_max
  sample.r_max <- priors$r_max$rfn()
  
  ## Sampling from the N.obs prior
  sample.N.obs <- priors$N_obs$rfn()
  
  ## Prior on additional CV
  if (priors$add_CV$use) {
    sample.add_CV <- priors$add_CV$rfn()
  } else {
    sample.add_CV <- 0
  }
  
  ## Sample from prior for `z` (usually constant)
  sample.z <- priors$z$rfn()
  
  ## Sampling from q priors if q.prior is TRUE; priors on q for indices of
  ## abundance
  if (priors$q_IA$use) {
    q.sample.IA <- replicate(num.IA, priors$q_IA$rfn())
  } else {
    ## FIXME: -9999 is probably not a good sentinel value here; NA?
    q.sample.IA <- rep(-9999, length(unique(rel.abundance$Index)))
  }
  
  ##priors on q for count data
  if (priors$q_count$use) {
    q.sample.Count <- replicate(num.Count, priors$q_count$rfn())
  } else {
    ## FIXME: Sentinel -9999 again
    q.sample.Count <- rep(-9999, length(unique(count.data$Index)))
  }
  
  sample.K <- LOGISTIC.BISECTION.K(K.low = control$K_bisect_lim[1],
                                   K.high = control$K_bisect_lim[2],
                                   r_max = sample.r_max,
                                   z = sample.z,
                                   num_Yrs = bisection.Yrs,
                                   start_yr = start_yr,
                                   target.Pop = sample.N.obs,
                                   catches = catches,
                                   MVP = MVP,
                                   tol = control$K_bisect_tol)
  
  #Computing the predicted abundances with the samples from the priors
  #----------------------------------------
  Pred_N <- GENERALIZED_LOGISTIC(r_max = sample.r_max,
                                 K = sample.K,
                                 N1 = sample.K,
                                 z = sample.z,
                                 start_yr = start_yr,
                                 num_Yrs = projection.Yrs,
                                 catches = catches,
                                 MVP = MVP)
  
  
  #Computing the predicted ROI for the IAs and Count data, if applicable
  #----------------------------------------
  #For IAs
  if (rel.abundance.key) {
    Pred.ROI.IA <- COMPUTING.ROI(data = rel.abundance,
                                 Pred_N = Pred_N$Pred_N,
                                 start_yr = start_yr)
  } else {
    Pred.ROI.IA <- rep(0, num.IA)
  }
  
  #For Count Data
  if (count.data.key) {
    Pred.ROI.Count <- COMPUTING.ROI(data = count.data,
                                    Pred_N = Pred_N$Pred_N,
                                    start_yr = start_yr)
  } else {
    Pred.ROI.Count <- rep(0, num.Count)
  }
  
  #Calculate Analytical Qs if rel.abundance.key is TRUE
  #---------------------------------------------------------
  if (rel.abundance.key) {
    if (!priors$q_IA$use) {
      q.sample.IA <- CALC.ANALYTIC.Q(rel.abundance,
                                     Pred_N$Pred_N,
                                     start_yr,
                                     sample.add_CV,
                                     num.IA)
    } else {
      q.sample.IA <- q.sample.IA
    }
  }
  
  #browser()
  
  ## Calculate Analytical Qs if count.data.key is TRUE
  ## (NOT USED YET - AZerbini, Feb 2013)
  if (count.data.key) {
    if (!priors$q_count$use) {
      q.sample.Count <- CALC.ANALYTIC.Q(count.data,
                                        Pred_N$Pred_N,
                                        start_yr,
                                        sample.add_CV,
                                        num.Count)
    } else {
      q.sample.Count <- q.sample.Count
    }
  }
  
  if (control$verbose > 3) {
    message("r_max = ", sample.r_max,
            " N.obs = ", sample.N.obs,
            " K = ", sample.K,
            " Pred_N.target = ", Pred_N$Pred_N[bisection.Yrs],
            " q.IAs = ", q.sample.IA,
            " q.Count = ", q.sample.Count)
  }
  
  #Compute the likelihoods
  #--------------------------------
  # (1) relative indices (if rel.abundance.key is TRUE)
  if (rel.abundance.key) {
    lnlike.IAs <- LNLIKE.IAs(rel.abundance,
                             Pred_N$Pred_N,
                             start_yr,
                             q.sample.IA,
                             sample.add_CV,
                             TRUE)
  } else {
    lnlike.IAs <- 0
  }
  
  # (2) count data (if count.data.key is TRUE)
  if (count.data.key) {
    lnlike.Count <- LNLIKE.IAs(count.data,
                               Pred_N$Pred_N,
                               start_yr,
                               q.sample.Count,
                               sample.add_CV,
                               log=TRUE)
  } else {
    lnlike.Count <- 0
  }
  
  # (3) absolute abundance
  lnlike.Ns <- LNLIKE.Ns(abs.abundance,
                         Pred_N$Pred_N,
                         start_yr,
                         sample.add_CV,
                         log=TRUE)
  
  # (4) growth rate if applicable
  if (growth.rate.obs[3]) {
    Pred.GR <- PRED.GROWTH.RATE(growth.rate.Yrs=growth.rate.Yrs,
                                Pred_N=Pred_N$Pred_N,
                                start_yr=start_yr)
    lnlike.GR <- LNLIKE.GR(Obs.GR=growth.rate.obs[1],
                           Pred.GR=Pred.GR,
                           GR.SD.Obs=growth.rate.obs[2])
  } else {
    lnlike.GR <- 0
  }
  
  if (control$verbose > 2) {
    message("lnlike.IAs = ", lnlike.IAs,
            " lnlike.Count = ", lnlike.Count,
            " lnlike.Ns = ", lnlike.Ns,
            " lnlike.GR = ", lnlike.GR)
  }
  
  ## These use the likelihoods in Zerbini et al. (2011)
  NLL <- lnlike.IAs[[1]] + lnlike.Count[[1]] + lnlike.Ns[[1]] + lnlike.GR[[1]]
  Likelihood <- exp(-NLL)
  if (control$verbose > 1) {
    message("NLL = ", NLL,
            " Likelihood = ", Likelihood)
  }
  
  if (Pred_N$Violate_Min_Viable_Pop) {
    Likelihood <- 0
    if (control$verbose > 0) {
      message("MVP violated on draw", draw)
    }
  }
  
  
  
  Cumulative.Likelihood <- Cumulative.Likelihood + Likelihood
  
  # Trick to just extract realized prior
  if(realized_prior){
    Cumulative.Likelihood <- 2 * control$threshold
  }
  
  if (!Pred_N$Violate_Min_Viable_Pop) {
    
    while (Cumulative.Likelihood > control$threshold & i < n_resamples) {
      if (control$verbose > 0) {
        message("sample = ", i, " draw = ", draw)
      }
      if (control$verbose > 1) {
        message("draw = ", draw,
                " Likelihood = ", Likelihood,
                " Cumulative = ", Cumulative.Likelihood)
      }
      save <- TRUE
      Cumulative.Likelihood <- Cumulative.Likelihood-control$threshold
      resamples_trajectories[i+1,] <- Pred_N$Pred_N
      catch_trajectories[i+1,] <- catches
      resamples_output[i+1,] <- c(sample.r_max,
                                  sample.K,
                                  sample_catch_multiplier,
                                  sample_premodern_catch_multiplier,
                                  sample.N.obs,
                                  sample.add_CV,
                                  sample_premodern_catch,
                                  Pred_N$Min_Pop,
                                  Pred_N$Min_Yr,
                                  Pred_N$Violate_Min_Viable_Pop,
                                  c(Pred_N$Pred_N[target.Yr - start_yr + 1]),
                                  c(Pred_N$Pred_N[output.Yrs - start_yr + 1]),
                                  Pred.ROI.IA,
                                  q.sample.IA,
                                  Pred.ROI.Count,
                                  q.sample.Count,
                                  lnlike.IAs[[1]],
                                  lnlike.Count[[1]],
                                  lnlike.Ns[[1]],
                                  lnlike.GR[[1]],
                                  NLL,
                                  Likelihood,
                                  Pred_N$Min_Pop / sample.K,
                                  c(Pred_N$Pred_N[target.Yr - start_yr + 1] /
                                      sample.K),
                                  c(Pred_N$Pred_N[output.Yrs - start_yr + 1] /
                                      sample.K),
                                  draw,
                                  save)
      i <- i+1
      if (control$progress_bar) {
        setTxtProgressBar(pb, i)
      }
    }
  }
  draw <- draw+1
}

# Save outputs
resamples_output <- data.frame(resamples_output)
names(resamples_output) <- sir_names
write.csv(resamples_output,
          paste0(file_name, "_", "resamples_output.csv"))

resamples_trajectories <- data.frame(resamples_trajectories)
names(resamples_trajectories) <- paste0("N_", Year)
write.csv(resamples_trajectories,
          paste0(file_name, "_", "resamples_trajectories.csv"))

catch_trajectories <- data.frame(catch_trajectories)
names(catch_trajectories) <- paste0("Catch_", Year)
write.csv(catch_trajectories,
          paste0(file_name, "_", "catch_trajectories.csv"))

resamples.per.samples <- draw / n_resamples
if(resamples.per.samples < 3){
  warning("Number of resamples per sample is ",
          round(resamples.per.samples, 1),
          ", use higher threshold value.")
} else if (resamples.per.samples > 20) {
  warning("Number of resamples per sample is ",
          round(resamples.per.samples, 1),
          ", use lower threshold value.")
}

end.time <- Sys.time()
if (control$verbose > 0) {
  message("Time to Compute = ", (end.time-begin.time))
}

return_list <- list(call = call,
                    file_name = file_name,
                    Date.Time = Sys.time(),
                    Time.to.compute.in.minutes = paste((end.time-begin.time) / 60),
                    threshold = control$threshold,
                    Ratio.Resamples.per.Sample = paste("1 resample",
                                                       ":",
                                                       resamples.per.samples,
                                                       "samples"),
                    resamples_output = resamples_output,
                    resamples_trajectories = resamples_trajectories,
                    catch_trajectories = catch_trajectories,
                    inputs = list(draws = draw,
                                  n_resamples = n_resamples,
                                  prior_r_max = priors$r_max,
                                  catch_multipliers = catch_multipliers,
                                  priors_N_obs = priors$N_obs,
                                  target.Yr = target.Yr,
                                  MVP = paste("num.haplotypes = ",
                                              num.haplotypes,
                                              "MVP = ",
                                              3 * num.haplotypes),
                                  tolerance = control$K_bisect_tol,
                                  output.Years = output.Yrs,
                                  abs.abundance = abs.abundance,
                                  catch.data = catch.data,
                                  realized_prior = realized_prior))
if(rel.abundance.key){ return_list$inputs$rel.abundance = rel.abundance}
if(count.data.key){ return_list$inputs$count.data = count.data}
