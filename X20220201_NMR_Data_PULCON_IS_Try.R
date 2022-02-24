#-----TOPSPIN & MNOVA DATA-----

#--import data
library(readxl)
X20220201_NMR_Data_PULCON_IS_Trp <- read_excel("~/BPS1/RP1/NMR_data/Quantified_data/20220201_NMR_Data_PULCON_IS_Trp.xlsx")
peaks_df <- X20220201_NMR_Data_PULCON_IS_Trp # rename file

#--mean of each sample  
means_df <- cbind(peaks_df, new_col = rowMeans(peaks_df[, 5:9])) # implement mean of each sample in table
names(means_df)[names(means_df) == "new_col"] <- "Average [mM Tryptophan]" # rename column

#--calculate standard deviation (SD) 
library(matrixStats)
concentrations_matrix <- data.matrix(means_df[, c(5:9)], rownames.force = NA) # convert df into matrix
means_sd_df <- cbind(means_df, new_col = as.data.frame(rowSds(concentrations_matrix))) # calculate SD of each sample and convert to df
names(means_sd_df)[names(means_sd_df) == "rowSds(concentrations_matrix)"] <- "Stdev" # rename column

#--calculate relative standard deviation (RSD)
means_sd_rsd_df <- cbind(means_sd_df, new_col = (means_sd_df$Stdev / means_sd_df$`Average [mmol/l Tryptophan]`) * 100) # calculate RSD for each sample
names(means_sd_rsd_df)[names(means_sd_rsd_df) == "new_col"] <- "RSD (%)" # rename column

#--calculate relative error (RE)
all_samples_df <- cbind(means_sd_rsd_df, new_col = ((means_sd_rsd_df$`Average [mmol/l Tryptophan]` - means_sd_rsd_df$`Concentration [mmol/l Trpptophan]`) / means_sd_rsd_df$`Concentration [mmol/l Trpptophan]`) * 100) # calculate RSD for each sample
names(all_samples_df)[names(all_samples_df) == "new_col"] <- "RE (%)" # rename column
print(all_samples_df) #print finished df 


#-----Table average of average-----

#--creating vectors of table elements 
Software_vector <- rep(c("Topspin", "Mnova"), each = 15) #create vector for software
Method_vector <- rep(c("Internal standard", "PULCON", "PULCON"), each = 5, times = 2) #create vector for method
Sample_vector <- rep(c(4.007674656, 2.02081900018009, 1.01040950009004, 0.505204750045022, 0.252602375022511, 4.00767465581934, 2.02081900018009, 1.01040950009004, 0.505204750045022, 0.252602375022511, 4.00734981457954, 2.00367490728977, 1.00183745364488, 0.500918726822442, 0.250459363411221), times = 2) #create vector with expected concentrations
Mean_means_vector <- colMeans(matrix(all_samples_df$`Average [mmol/l Tryptophan]`, nrow = 2)) #create vector with average measured concentrations for duplicates 

SD_vector <- (all_samples_df$Stdev)^2 #create vector for squared SD
Sample2_vector <- rep(1:30, each = 2) #create vector with numbers for each duplicate 
SD_df <- data.frame(cbind(Sample2_vector, SD_vector)) #create df
SD_df$SD_vector <- as.numeric(as.character(SD_df$SD_vector)) #change character into numeric value

library(dplyr) #create df for SD of the average concentrations per duplicate 
SD1_df <- SD_df %>% 
  group_by(Sample2_vector) %>% 
  summarise(Stdev = sqrt(sum(SD_vector)/2)) %>% 
  select(-c(Sample2_vector))

mean_sd_df <- data.frame(cbind(Software_vector, Method_vector, Sample_vector, Mean_means_vector, SD1_df))  # combining vectors into df
mean_sd_df$Sample_vector <- as.numeric(as.character(mean_sd_df$Sample_vector)) # changing characters into numeric variables  
mean_sd_df$Mean_means_vector <- as.numeric(as.character(mean_sd_df$Mean_means_vector)) 
mean_sd_df$Stdev <- as.numeric(as.character(mean_sd_df$Stdev))
                            
library(dplyr) # rename columns
mean_sd_df <- rename(mean_sd_df, c(
   "Average [mM Tryptophan]" = "Mean_means_vector", 
   "Concentration [mM Tryptophan]" = "Sample_vector", "Software" = "Software_vector", "Method" = "Method_vector"))

#--calculate RSD
mean_sd_rsd_df <- cbind(mean_sd_df, new_col = (mean_sd_df$Stdev / mean_sd_df$`Average [mM Tryptophan]`) * 100) # calculate RSD for each sample
names(mean_sd_rsd_df)[names(mean_sd_rsd_df) == "new_col"] <- "RSD (%)" # rename column

#--calculate relative error (RE)
mean_sd_rsd_re_df <- cbind(mean_sd_rsd_df, new_col = ((mean_sd_rsd_df$`Average [mM Tryptophan]` - mean_sd_rsd_df$`Concentration [mM Tryptophan]`)
                                                        / mean_sd_rsd_df$`Concentration [mM Tryptophan]`) * 100) # calculate Re for each sample
names(mean_sd_rsd_re_df)[names(mean_sd_rsd_re_df) == "new_col"] <- "RE (%)" # rename column

#--remove 4mM samples with sample preparation mistake 
average_samples_df <- mean_sd_rsd_re_df[-c(1, 6, 16, 21), ]
print(average_samples_df) #print finished df

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


#------Linear regression equations-----#

library(ggplot2)
library(tidyverse)
theme_set(theme_bw())

group_vector <- c(1,1,1,1,2,2,2,2,3,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,6) #create group vector 
average_samples_df_ggplot <- data.frame(cbind(average_samples_df, Group = group_vector))# add groups in order to subset the data into four different plots


ggplot(subset(average_samples_df_ggplot, Group %in% 1), aes(x = Concentration..mM.Tryptophan. , y = Average..mM.Tryptophan. )) + 
  geom_point() + 
    labs(x = 'Expected concentration [mM Trytophan]', y = 'Average measured concentration [mM Trytophan]') + 
       geom_smooth(formula = y ~ x, method = 'lm', se = FALSE)
