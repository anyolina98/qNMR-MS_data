#--import data
library(readxl)
peaks_df <- read_excel("~/BPS1/RP1/NMR_data/Quantified_data/2022/20220414_NMR_data_PULCON.xlsx")

#--mean of each sample  
means_vector <- rowMeans(peaks_df[, 3:8],na.rm = TRUE) # create vector with mean of each row 
means_df <- cbind(peaks_df, new_col = means_vector) # implement mean of each sample in table
names(means_df)[names(means_df) == "new_col"] <- "Average concentration [mM/L]" # rename column

#--calculate standard deviation (SD) 
library(matrixStats)
concentrations_matrix <- data.matrix(means_df[, 3:8], rownames.force = NA) # convert df into matrix
means_sd_df <- cbind(means_df, new_col = as.data.frame(rowSds(concentrations_matrix, na.rm = TRUE))) # calculate SD of each sample and convert to df
names(means_sd_df)[names(means_sd_df) == "rowSds(concentrations_matrix, na.rm = TRUE)"] <- "Stdev" # rename column

#--calculate relative standard deviation (RSD)
means_sd_rsd_df <- cbind(means_sd_df, new_col = (means_sd_df$Stdev / means_sd_df$`Average concentration [mM/L]`) * 100) # calculate RSD for each sample
names(means_sd_rsd_df)[names(means_sd_rsd_df) == "new_col"] <- "RSD (%)" # rename column

#--calculate relative error (RE)
all_samples_df <- cbind(means_sd_rsd_df, new_col = ((means_sd_rsd_df$`Average concentration [mM/L]` - means_sd_rsd_df$`Concentration [mM/L]`) / means_sd_rsd_df$`Concentration [mM/L]`) * 100) # calculate RE for each sample
names(all_samples_df)[names(all_samples_df) == "new_col"] <- "RE (%)" # rename column

#--create df without measured values for amines  
amines_df <- all_samples_df[, -c(3:8)] # 20 amines df 

## ---amines_df contains fully processed tables---##
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
copy_to_cb(amines_df) # copy amines_df to clipboard
