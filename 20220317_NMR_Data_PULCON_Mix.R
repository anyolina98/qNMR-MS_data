

#--import data
library(readxl)
peaks_df <<- read_excel("~/BPS1/RP1/NMR_data/Quantified_data/2022/20220317_NMR_Data_PULCON_20_AA.xlsx")

#--mean of each sample  
means_vector <- rowMeans(peaks_df[, 3:7],na.rm = TRUE) # create vector with mean of each row 
means_df <- cbind(peaks_df, new_col = means_vector) # implement mean of each sample in table
names(means_df)[names(means_df) == "new_col"] <- "Average concentration [mM/L]" # rename column

#--calculate relative error (RE)
all_samples_df <- cbind(means_df, new_col = ((means_df$`Average concentration [mM/L]` - means_df$`Concentration [mM/L]`) / means_df$`Concentration [mM/L]`) * 100) # calculate RE for each sample
names(all_samples_df)[names(all_samples_df) == "new_col"] <- "RE (%)" # rename column

#--create df without measured values for amines  
amines_df <- all_samples_df[, -c(3:7)] # 20 amines df 

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
