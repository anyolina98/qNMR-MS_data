
#--import data
library(readxl)
peaks_df <- read_excel("~/BPS1/RP1/NMR_data/Quantified_data/20220302_NMR_Data_PULCON_20_AA.xlsx")

#--mean of each sample  
means_vector <- rowMeans(peaks_df[, 4:10],na.rm = TRUE) # create vector with mean of each row 
means_df <- cbind(peaks_df, new_col = means_vector) # implement mean of each sample in table
names(means_df)[names(means_df) == "new_col"] <- "Average concentration [mM/L]" # rename column

#--calculate relative error (RE)
all_samples_df <- cbind(means_df, new_col = ((means_df$`Average concentration [mM/L]` - means_df$`Concentration [mM/L]`) / means_df$`Concentration [mM/L]`) * 100) # calculate RE for each sample
names(all_samples_df)[names(all_samples_df) == "new_col"] <- "RE (%)" # rename column
print(all_samples_df) # print finished df

#--create df without measured values for amines  
amines_df <- all_samples_df[-c(21:32) ,] # 20 amines df 


#-----Table average of average-----

#--creating vectors of table elements   
Sample_vector <- rep(c("Acetonitrile"), times = 6) # sample name vector
Parameters_vector <- rep(c("Fixed", "Automated"), each = 3) # parameters vectors
Concentration_vector <- rep(c(2.127486805, 1.063743402, 0.531871701), times = 2) # expected concentration vector 
Average_vector <- colMeans(matrix(acn_df$`Average concentration [mM/L]`, nrow = 2)) # vector with average measured concentrations for duplicates

acn_samples_df <- data.frame(cbind(Sample_vector, Parameters_vector, Concentration_vector, Average_vector))
acn_samples_df$`Concentration [mM/L ACN]` <- as.numeric(as.character(acn_samples_df$`Concentration [mM/L ACN]`)) # changing characters into numeric variables
acn_samples_df$`Average [mM/L ACN]` <- as.numeric(as.character(acn_samples_df$`Average [mM/L ACN]`))


library(dplyr) # rename columns
acn_samples_df <- rename(acn_samples_df, c(
  "Average [mM/L ACN]" = "Average_vector",
  "Concentration [mM/L ACN]" = "Concentration_vector", "Acquisition parameters" = "Parameters_vector", "Compound" = "Sample_vector"
))

#--calculate relative error (RE)
acn_samples_df <- cbind(acn_samples_df, new_col = ((acn_samples_df$`Average [mM/L ACN]` - acn_samples_df$`Concentration [mM/L ACN]`)
                                                      / acn_samples_df$`Concentration [mM/L ACN]`) * 100) # calculate Re for each sample
names(acn_samples_df)[names(acn_samples_df) == "new_col"] <- "RE (%)" # rename column


## ---amines_df and acn_samples_df contain fully processed tables---##
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
copy_to_cb(acn_samples_df) # copy acn_samples_df to clipboard
