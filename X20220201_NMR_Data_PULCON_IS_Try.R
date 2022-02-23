#-----TOPSPIN & MNOVA DATA-----

#--import data
library(readxl)
X20220201_NMR_Data_PULCON_IS_Try <- read_excel("BPS1/RP1/NMR_data/Quantified_data/20220201_NMR_Data_PULCON_IS_Try.xlsx")
peaks_df <- X20220201_NMR_Data_PULCON_IS_Try # rename file

#--mean of each sample  
means_df <- cbind(peaks_df, new_col = rowMeans(peaks_df[, 5:9])) # implement mean of each sample in table
names(means_df)[names(means_df) == "new_col"] <- "Average [mmol/l Tryptophan]" # rename column

#--calculate standard deviation (SD) 
library(matrixStats)
concentrations_matrix <- data.matrix(means_df[, c(5:9)], rownames.force = NA) # convert df into matrix
means_sd_df <- cbind(means_df, new_col = as.data.frame(rowSds(concentrations_matrix))) # calculate SD of each sample and convert to df
names(means_sd_df)[names(means_sd_df) == "rowSds(concentrations_matrix)"] <- "Stdev" # rename column

#--calculate relative standard deviation (RSD)
means_sd_rsd_df <- cbind(means_sd_df, new_col = (means_sd_df$Stdev / means_sd_df$`Average [mmol/l Tryptophan]`) * 100) # calculate RSD for each sample
names(means_sd_rsd_df)[names(means_sd_rsd_df) == "new_col"] <- "RSD (%)" # rename column

#--calculate relative error (RE)
all_samples_df <- cbind(means_sd_rsd_df, new_col = ((means_sd_rsd_df$`Average [mmol/l Tryptophan]` - means_sd_rsd_df$`Concentration [mmol/l Tryptophan]`)
                                                    / means_sd_rsd_df$`Concentration [mmol/l Tryptophan]`) * 100) # calculate RSD for each sample
names(all_samples_df)[names(all_samples_df) == "new_col"] <- "RE (%)" # rename column
print(all_samples_df)


#-----Table average of average-----

#--creating vectors of table elements 
Software_vector <- rep(c("Topspin", "Mnova"), each = 15)
Method_vector <- rep(c("Internal standard", "PULCON", "PULCON"), each = 5, times = 2)
Sample_vector <- rep(c(4.007674656, 2.02081900018009, 1.01040950009004, 0.505204750045022, 0.252602375022511, 4.00767465581934, 2.02081900018009, 1.01040950009004, 0.505204750045022, 0.252602375022511, 4.00734981457954, 2.00367490728977, 1.00183745364488, 0.500918726822442, 0.250459363411221), times = 2)
Sample2_vector <- rep(c("4_Trp_I", "4_Trp_I", "2_Trp_I", "2_Trp_I", "1_Trp_I", "1_Trp_I", "0.5_Trp_I", "0.5_Trp_I", "0.25_Trp_I", "0.25_Trp_I", "4_Trp_P", "4_Trp_P", "2_Trp_P", "2_Trp_P", "1_Trp_P", "1_Trp_P", "0.5_Trp_P", "0.5_Trp_P", "0.25_Trp_P", "0.25_Trp_P", "4_Trp_P", "4_Trp_P", "2_Trp_P", "2_Trp_P", "1_Trp_P", "1_Trp_P", "0.5_Trp_P", "0.5_Trp_P", "0.25_Trp_P", "0.25_Trp_P"), times=2)
Mean_means_vector <- colMeans(matrix(all_samples_df$`Average [mmol/l Tryptophan]`, nrow = 2))
SD2_vector <- (all_samples_df$Stdev)^2

mean_means_df <- data.frame(cbind(Sample2_vector, Software_vector, Method_vector, Sample_vector, Mean_means_vector)) # combining vectors into df
mean_means_df$Sample_vector <- as.numeric(as.character(mean_means_df$Sample_vector)) # changing characters into numeric variables  
mean_means_df$Mean_means_vector <- as.numeric(as.character(mean_means_df$Mean_means_vector))

library(dplyr)
mean_means_df <- rename(mean_means_df, c(
  "Average [mmol/l Tryptophan]" = "Mean_means_vector", # rename columns
  "Concentration [mmol/l Tryptophan]" = "Sample_vector", "Software" = "Software_vector", "Method" = "Method_vector"
))

#--calculate RSD


#--calculate relative error (RE)
mean_means_rsd_re_df <- cbind(mean_means_df, new_col = ((mean_means_df$`Average [mmol/l Tryptophan]` - mean_means_df$`Concentration [mmol/l Tryptophan]`)
                                                        / mean_means_df$`Concentration [mmol/l Tryptophan]`) * 100) # calculate Re for each sample
names(mean_means_rsd_re_df)[names(mean_means_rsd_re_df) == "new_col"] <- "RE (%)" # rename column
print(mean_means_rsd_re_df)

#--remove 4mM samples with sample preparation mistake 
average_samples_df <- mean_means_rsd_re_df[-c(1, 6, 16, 21), ]

## ---all_samples_df and average_samples_df contain fully processed tables---##
#' @title copy to cb 
#' @description this functions copies df's from R to clipboard 
#' @param dataframe, decimals, max size of clipboard 
#' @importFrom none  
copy_to_cb <- function(x, sep = "\t", dec = ".", max.size = (200 * 1000)) {
  write.table(x, paste0("clipboard-", formatC(max.size, format = "f", digits = 0)),
              sep = sep, row.names = FALSE, dec = dec
  )
}

#--function to copy tables to clipboard
copy_to_cb(all_samples_df) # copy all_samples_df to clipboard
copy_to_cb(average_samples_df) # copy average_samples_df to clipboard

