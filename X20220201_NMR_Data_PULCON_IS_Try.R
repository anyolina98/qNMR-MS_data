#-----TOPSPIN & MNOVA DATA-----

#--import data
library(readxl)
X20220201_NMR_Data_PULCON_IS_Trp <- read_excel("~/BPS1/RP1/NMR_data/Quantified_data/20220201_NMR_Data_PULCON_IS_Trp.xlsx")
peaks_df <- X20220201_NMR_Data_PULCON_IS_Trp # rename file

#--mean of each sample  
means_df <- cbind(peaks_df, new_col = rowMeans(peaks_df[, 5:9])) # implement mean of each sample in table
names(means_df)[names(means_df) == "new_col"] <- "Average [mM/L Tryptophan]" # rename column

#--calculate standard deviation (SD) 
library(matrixStats)
concentrations_matrix <- data.matrix(means_df[, c(5:9)], rownames.force = NA) # convert df into matrix
means_sd_df <- cbind(means_df, new_col = as.data.frame(rowSds(concentrations_matrix))) # calculate SD of each sample and convert to df
names(means_sd_df)[names(means_sd_df) == "rowSds(concentrations_matrix)"] <- "Stdev" # rename column

#--calculate relative standard deviation (RSD)
means_sd_rsd_df <- cbind(means_sd_df, new_col = (means_sd_df$Stdev / means_sd_df$`Average [mM/L Tryptophan]`) * 100) # calculate RSD for each sample
names(means_sd_rsd_df)[names(means_sd_rsd_df) == "new_col"] <- "RSD (%)" # rename column

#--calculate relative error (RE)
all_samples_df <- cbind(means_sd_rsd_df, new_col = ((means_sd_rsd_df$`Average [mM/L Tryptophan]` - means_sd_rsd_df$`Concentration [mM/L Tryptophan]`) / means_sd_rsd_df$`Concentration [mM/L Tryptophan]`) * 100) # calculate RSD for each sample
names(all_samples_df)[names(all_samples_df) == "new_col"] <- "RE (%)" # rename column
print(all_samples_df) # print finished df


#-----Table average of average-----

#--creating vectors of table elements 
Software_vector <- rep(c("Topspin", "Mnova"), each = 15) # create vector for software
Method_vector <- rep(c("Internal standard", "PULCON", "PULCON"), each = 5, times = 2) # create vector for method
Sample_vector <- rep(c(4.007674656, 2.02081900018009, 1.01040950009004, 0.505204750045022, 0.252602375022511, 4.00767465581934, 2.02081900018009, 1.01040950009004, 0.505204750045022, 0.252602375022511, 4.00734981457954, 2.00367490728977, 1.00183745364488, 0.500918726822442, 0.250459363411221), times = 2) # create vector with expected concentrations
Mean_means_vector <- colMeans(matrix(all_samples_df$`Average [mM/L Tryptophan]`, nrow = 2)) # create vector with average measured concentrations for duplicates

SD_vector <- (all_samples_df$Stdev)^2 # create vector for squared SD
Sample2_vector <- rep(1:30, each = 2) # create vector with numbers for each duplicate
SD_df <- data.frame(cbind(Sample2_vector, SD_vector)) # create df
SD_df$SD_vector <- as.numeric(as.character(SD_df$SD_vector)) # change character into numeric value

library(dplyr) # create df for SD of the average concentrations per duplicate
SD1_df <- SD_df %>%
  group_by(Sample2_vector) %>%
  summarise(Stdev = sqrt(sum(SD_vector) / 2)) %>%
  select(-c(Sample2_vector))

mean_sd_df <- data.frame(cbind(Software_vector, Method_vector, Sample_vector, Mean_means_vector, SD1_df)) # combining vectors into df
mean_sd_df$Sample_vector <- as.numeric(as.character(mean_sd_df$Sample_vector)) # changing characters into numeric variables
mean_sd_df$Mean_means_vector <- as.numeric(as.character(mean_sd_df$Mean_means_vector))
mean_sd_df$Stdev <- as.numeric(as.character(mean_sd_df$Stdev))

library(dplyr) # rename columns
mean_sd_df <- rename(mean_sd_df, c(
  "Average [mM/L Tryptophan]" = "Mean_means_vector",
  "Concentration [mM/L Tryptophan]" = "Sample_vector", "Software" = "Software_vector", "Method" = "Method_vector"
))

#--calculate RSD
mean_sd_rsd_df <- cbind(mean_sd_df, new_col = (mean_sd_df$Stdev / mean_sd_df$`Average [mM/L Tryptophan]`) * 100) # calculate RSD for each sample
names(mean_sd_rsd_df)[names(mean_sd_rsd_df) == "new_col"] <- "RSD (%)" # rename column

#--calculate relative error (RE)
mean_sd_rsd_re_df <- cbind(mean_sd_rsd_df, new_col = ((mean_sd_rsd_df$`Average [mM/L Tryptophan]` - mean_sd_rsd_df$`Concentration [mM/L Tryptophan]`)
/ mean_sd_rsd_df$`Concentration [mM/L Tryptophan]`) * 100) # calculate Re for each sample
names(mean_sd_rsd_re_df)[names(mean_sd_rsd_re_df) == "new_col"] <- "RE (%)" # rename column

#--remove 4mM/L samples with sample preparation mistake 
average_samples_df <- mean_sd_rsd_re_df[-c(1, 6, 16, 21), ]
print(average_samples_df) # print finished df

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
theme_set(theme_classic(base_size = 10, base_family = "serif", base_line_size = 0.7))

group_vector <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6) # create group vector
average_samples_df_ggplot <- data.frame(cbind(average_samples_df, Group = group_vector)) # add groups in order to make 4 subsets of the data
df_Top_IS <- average_samples_df_ggplot[average_samples_df_ggplot$Group %in% "1", ] # generate subset for Topspin + IS
df_Top_PU <- average_samples_df_ggplot[average_samples_df_ggplot$Group %in% "2", ] # generate subset for Topspin + PULCON
df_Mno_IS <- average_samples_df_ggplot[average_samples_df_ggplot$Group %in% "4", ] # generate subset for Mnova + IS
df_Mno_PU <- average_samples_df_ggplot[average_samples_df_ggplot$Group %in% "5", ] # generate subset for Mnova + PULCON

lmod_Top_IS <- lm(Average..mM.L.Tryptophan. ~ Concentration..mM.L.Tryptophan., df_Top_IS) # linear regression model for Topspin + IS
summary(lmod_Top_IS) # summary of model gives R-squared and equation

ggplot(df_Top_IS, aes(x = Concentration..mM.L.Tryptophan., y = Average..mM.L.Tryptophan.)) +
  geom_point(shape = 16, fill = "black", color = "black", size = 2.3) +
  geom_errorbar(aes(ymin = Average..mM.L.Tryptophan. - Stdev, ymax = Average..mM.L.Tryptophan. + Stdev), width = 0.1) +
  labs(x = "Expected concentration [mM/L Trytophan]", y = "Average measured concentration [mM/L Trytophan]") +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "black", size = 0.71) +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 10), axis.text.y = element_text(face = "bold", color = "black", size = 10)) +
  annotate("text", x = c(1.3, 1.3), y = c(2, 1.93), label = c("y = 1.0086x - 0.006", "Adjusted R-squared = 0.9999"), size = 3, family = "serif")

lmod_Top_PU <- lm(Average..mM.L.Tryptophan. ~ Concentration..mM.L.Tryptophan., df_Top_PU) # linear regression model for Topspin + PULCON
summary(lmod_Top_PU) # sumM/Lary of model gives R-squared and equation

ggplot(df_Top_PU, aes(x = Concentration..mM.L.Tryptophan., y = Average..mM.L.Tryptophan.)) +
  geom_point(shape = 16, fill = "black", color = "black", size = 2.3) +
  geom_errorbar(aes(ymin = Average..mM.L.Tryptophan. - Stdev, ymax = Average..mM.L.Tryptophan. + Stdev), width = 0.1) +
  labs(x = "Expected concentration [mM/L Trytophan]", y = "Average measured concentration [mM/L Trytophan]") +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "black", size = 0.71) +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 10), axis.text.y = element_text(face = "bold", color = "black", size = 10)) +
  annotate("text", x = c(1.3, 1.3), y = c(2, 1.93), label = c("y = 1.0311x - 0.0192", "Adjusted R-squared = 0.9999"), size = 3, family = "serif")

lmod_Mno_IS <- lm(Average..mM.L.Tryptophan. ~ Concentration..mM.L.Tryptophan., df_Mno_IS) # linear regression model for Mnova + IS
summary(lmod_Mno_IS) # sumM/Lary of model gives R-squared and equation

ggplot(df_Mno_IS, aes(x = Concentration..mM.L.Tryptophan., y = Average..mM.L.Tryptophan.)) +
  geom_point(shape = 16, fill = "black", color = "black", size = 2.3) +
  geom_errorbar(aes(ymin = Average..mM.L.Tryptophan. - Stdev, ymax = Average..mM.L.Tryptophan. + Stdev), width = 0.1) +
  labs(x = "Expected concentration [mM/L Trytophan]", y = "Average measured concentration [mM/L Trytophan]") +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "black", size = 0.71) +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 10), axis.text.y = element_text(face = "bold", color = "black", size = 10)) +
  annotate("text", x = c(1.3, 1.3), y = c(2, 1.93), label = c("y = 0.9785x + 0.0362", "Adjusted R-squared = 0.9995"), size = 3, family = "serif")

lmod_Mno_PU <- lm(Average..mM.L.Tryptophan. ~ Concentration..mM.L.Tryptophan., df_Mno_PU) # linear regression model for Mnova + PULCON
summary(lmod_Mno_PU) # sumM/Lary of model gives R-squared and equation

ggplot(df_Mno_PU, aes(x = Concentration..mM.L.Tryptophan., y = Average..mM.L.Tryptophan.)) +
  geom_point(shape = 16, fill = "black", color = "black", size = 2.3) +
  geom_errorbar(aes(ymin = Average..mM.L.Tryptophan. - Stdev, ymax = Average..mM.L.Tryptophan. + Stdev), width = 0.1) +
  labs(x = "Expected concentration [mM/L Trytophan]", y = "Average measured concentration [mM/L Trytophan]") +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "black", size = 0.71) +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 10), axis.text.y = element_text(face = "bold", color = "black", size = 10)) +
  annotate("text", x = c(1.3, 1.3), y = c(2, 1.93), label = c("y = 1.0182x - 0.0035", "Adjusted R-squared = 0.9996"), size = 3, family = "serif")
