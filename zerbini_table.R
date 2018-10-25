#' Function to write tables of logistic model parameter outputs for HumpbackSIR similar to Table 5 from Zerbini et al (2011).
#'
#' @param resample_summary Resample summary from hympbackSIR
#' @param file_name Desired filename to where csv file will be saved
zerbini_table <- function( resample_summary, file_name = "Scenario_1" ){


    results <- matrix(NA, nrow = 6, ncol = 6)
    names(results) <- c("Mean", "Median", "2.5% CI", "97.5% CI", "5% CI", "95% CI")
    results[1,] <- round(resample_summary$output_summary$r_max[1:6], 3)
    results[2,] <- format(round(resample_summary$output_summary$K[1:6], 0),big.mark=",",scientific=FALSE)
    results[3,] <- format(round(resample_summary$output_summary$Nmin[1:6], 0),big.mark=",",scientific=FALSE)

    n_columns <- grep("N", colnames(resample_summary$output_summary))
    results[4,] <- format(round(resample_summary$output_summary[1:6, n_columns[3]], 0),big.mark=",",scientific=FALSE)
    results[5,] <- format(round(resample_summary$output_summary[1:6, n_columns[4]], 0),big.mark=",",scientific=FALSE)
    results[6,] <- format(round(resample_summary$output_summary$q_IA1[1:6], 3),big.mark=",",scientific=FALSE)
    results <- data.frame(results)
    colnames(results) <- c("Mean", "Median", "2.5% CI", "97.5% CI", "5% CI", "95% CI")
    rownames(results) <- c("r", "K", "Nmin", colnames(resample_summary$output_summary)[n_columns[3:4]], "q_IA1")

    write.csv(results, file = paste0(file_name, "_zerbini_table_5.csv"))
}