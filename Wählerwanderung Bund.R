
###########################
#
# Wählerwanderungsanalyse Bund 
#
##############################
#load tidyverse and other packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dslabs")) install.packages("dslabs")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("titanic")) install.packages("titanic")
if (!require("gtools")) install.packages("gtools")
if (!require("rvest")) install.packages("rvest")
if (!require("purrr")) install.packages("purrr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyr")) install.packages("tidyr")
if (!require("scales")) install.packages("scales")
if (!require("tidytext")) install.packages("tidytext")
if (!require("pdftools")) install.packages("pdftools")
if (!require("mosaic")) install.packages("mosaic")
if (!require("readr")) install.packages("readr")
if (!require("gplots")) install.packages("gplots")
if (!require("readxl")) install.packages("readxl")
if (!require("rmarkdown")) install.packages("rmarkdown")
if (!require("sf")) install.packages("sf")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("geodata")) install.packages("geodata")
if (!require("tidygeocoder")) install.packages("tidygeocoder")
if (!require("st")) install.packages("st")
if (!require("osmdata")) install.packages("osmdata")
if (!require("osmextract")) install.packages("osmextract")
if (!require("eiPack")) install.packages("eiPack")



# now load the libraries
library(tidyverse) #load also ggplot2
library(dslabs)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(titanic)
library(gtools)
library(rvest)
library(purrr)
library(lubridate)
library(tidyr)
library(scales)
library(tidytext)
library(pdftools)

library(mosaic) 
library(readr)
library(gplots)
library(readxl)
library(rmarkdown)

library(sf)
library(rnaturalearth)
library(geodata)
library(tidygeocoder)
library(st)
library(osmdata)
library(osmextract)
library(eiPack)

library(glmnet)

btwa <- read_excel("btw25_kerg.xlsx")

#replace all NA values with 0 in btwa dataframe 
btwa[is.na(btwa)] <- 0



btw_clean <- btwa %>%
  mutate( Gültig_a21 = round(Gültig_a21 * Berechtigt_a25/Berechtigt_a21),
          SPD_a21 = round(SPD_a21 * Berechtigt_a25/Berechtigt_a21),
          CDU_a21 = round(CDU_a21 * Berechtigt_a25/Berechtigt_a21),
          Grüne_a21 = round(Grüne_a21 * Berechtigt_a25/Berechtigt_a21),
          FDP_a21 = round(FDP_a21 * Berechtigt_a25/Berechtigt_a21),
          AfD_a21 = round(AfD_a21 * Berechtigt_a25/Berechtigt_a21),
          Linke_a21 = round(Linke_a21 * Berechtigt_a25/Berechtigt_a21),
          BSW_a21 = round(BSW_a21 * Berechtigt_a25/Berechtigt_a21),
          Sonstige_a21 = Gültig_a21 - SPD_a21 - CDU_a21 - Grüne_a21 - FDP_a21 - AfD_a21 - Linke_a21 - BSW_a21,
          Nichtwähler_a21 = Berechtigt_a25 - Gültig_a21, # use Berechtigt_a25 deliberately
          Sonstige_a25 = Gültig_a25 - SPD_a25 - CDU_a25 - Grüne_a25 - FDP_a25 - AfD_a25 - Linke_a25 - BSW_a25,
          Nichtwähler_a25 = Berechtigt_a25 - Gültig_a25) %>%
  mutate(across(where(is.numeric), round)) %>%
  mutate(across(where(is.numeric), as.integer)) %>%
  select(SPD_a21, CDU_a21, Grüne_a21, FDP_a21, AfD_a21, Linke_a21, BSW_a21, Sonstige_a21, Nichtwähler_a21,
         SPD_a25, CDU_a25, Grüne_a25, FDP_a25, AfD_a25, Linke_a25, BSW_a25, Sonstige_a25, Nichtwähler_a25)



# Calculate row totals for 2021 and 2025
row_totals <- rowSums(btw_clean[, c("SPD_a21", "CDU_a21", "Grüne_a21", "FDP_a21", "AfD_a21", "Linke_a21", "BSW_a21", "Sonstige_a21", "Nichtwähler_a21")])
column_totals <- rowSums(btw_clean[, c("SPD_a25", "CDU_a25", "Grüne_a25", "FDP_a25", "AfD_a25", "Linke_a25", "BSW_a25", "Sonstige_a25", "Nichtwähler_a25")])

# Check if totals match
all(row_totals == column_totals)

# Example: Define your RxC formula
formula <- cbind(SPD_a21, CDU_a21, Grüne_a21, FDP_a21, AfD_a21, Linke_a21, BSW_a21, Sonstige_a21, Nichtwähler_a21) ~
  cbind(SPD_a25, CDU_a25, Grüne_a25, FDP_a25, AfD_a25, Linke_a25, BSW_a25, Sonstige_a25, Nichtwähler_a25)

# ei_result <- ei.MD.bayes(
#   formula = formula,
#   data = btw_clean,
#   sample = 100,      # Number of MCMC samples
#   burnin = 5000,       # Number of burn-in iterations
#   thin = 100,           # Thinning interval to reduce autocorrelation
#   ret.mcmc = T,
#   ret.beta = 'r',
#   verbose =1000
# )
ei_result <- ei.MD.bayes(
  formula = formula,
  data = btw_clean,
  sample = 1000,      # Number of MCMC samples
  burnin = 50000,       # Number of burn-in iterations
  thin = 1000,           # Thinning interval to reduce autocorrelation
  ret.mcmc = T,
  ret.beta = 'r',
  verbose =10000
)




transition_matrix <- ei_result$draws$Beta

# Extract parameter names
param_names <- attr(transition_matrix, "dimnames")[[2]]

# Convert transition_matrix to data frame
transition_df <- as.data.frame(transition_matrix)

# Add parameter names as column names
colnames(transition_df) <- param_names

# Pivot longer to associate parameters with samples
long_transition_df <- pivot_longer(
  transition_df,
  cols = everything(),
  names_to = "Parameter",
  values_to = "Sample"
)

# Extract Origin and Destination Groups:
long_transition_df <- long_transition_df %>%
  mutate(
    Destination = sub("beta\\.([^.]+)\\..*\\..*", "\\1", Parameter),  # Extract Destination group
    Origin = sub("beta\\.[^.]+\\.([^.]+)\\..*", "\\1", Parameter)     # Extract Origin group
  )


# Summarize Posterior Distributions:
voter_movements <- long_transition_df %>%
  group_by(Origin, Destination) %>%
  summarize(
    Mean = mean(Sample),
    .groups = "drop"
  )

voter_matrix <- voter_movements %>%
  pivot_wider(names_from = Destination, values_from = Mean) %>%
  column_to_rownames("Origin")  # Set Origin as row names
voter_matrix



############################
# Read the model from file
############################

# read table means_matrix from Wählerwanderung_SPD_CDU_AfD_Grüne_Linke_Sonstige_1000.txt
means_matrix <- read.table(file = "bund.txt", sep = "\t", row.names = 1, header = TRUE)

# Extract mean draws for voters transitioning to Grüne
mean_to_Grüne <- means_matrix[c("SPD_a21", "CDU_a21", "AfD_a21", "Linke_a21", "Sonstige_a21", "Nichtwähler_a21"), "Grüne_a25"]

# calculate totals for 2021
totals_2021 <- btw %>% 
  mutate(Sonstige_a21 = Gültig_a21 - SPD_a21 - CDU_a21 - Grüne_a21 - AfD_a21 - Linke_a21,
         Sonstige_a25 = Gültig_a25 - SPD_a25 - CDU_a25 - Grüne_a25 - AfD_a25 - Linke_a25,
         Nichtwähler_a21 = Gültig_a25 - Gültig_a21 + Sonstige_a21 - Sonstige_a25) %>%
  summarise(SPD_a21 = sum(SPD_a21),
            CDU_a21 = sum(CDU_a21),
            AfD_a21 = sum(AfD_a21),
            Linke_a21 = sum(Linke_a21),
            Sonstige_a21 = sum(Sonstige_a21),
            Nichtwähler_a21 = sum(Nichtwähler_a21)) %>%
  unlist()

# Multiply totals_2021 with mean_to_Grüne (element-wise multiplication)
voters_to_Grüne <- totals_2021 * mean_to_Grüne


# Extract posterior draws for voters transitioning from Grüne
mean_from_Grüne <- means_matrix["Grüne_a21", c("SPD_a25", "CDU_a25", "AfD_a25", "Linke_a25", "Sonstige_a25" ) ]

# calculate totals for 2021
Grüne_2021 <- btw %>% 
  summarise(Grüne_a21 = sum(Grüne_a21)) %>%
  pull()

# Multiply Grüne_2021 with mean_from_Grüne (element-wise multiplication)
voters_from_Grüne <- Grüne_2021 * mean_from_Grüne

movements = c(voters_to_Grüne["SPD_a21"] - voters_from_Grüne["SPD_a25"],
              voters_to_Grüne["CDU_a21"] - voters_from_Grüne["CDU_a25"],
              voters_to_Grüne["AfD_a21"] - voters_from_Grüne["AfD_a25"],
              voters_to_Grüne["Linke_a21"] - voters_from_Grüne["Linke_a25"],
              voters_to_Grüne["Sonstige_a21"] - voters_from_Grüne["Sonstige_a25"],
              voters_to_Grüne["Nichtwähler_a21"])
movements <- as.numeric(movements)

green_diff <- btw %>% 
  summarise(green_diff = sum(Grüne_a25 - Grüne_a21)) %>%
  pull()

# calculate the regression model
simulated <- sum(movements) 
correction_factor = green_diff / simulated
movements <- movements * correction_factor
movements <- round(movements,-2) 
sum(movements)


# Create the data frame
movements_df <- data.frame(
  party = c("SPD", "CDU", "AfD", "Linke", "Sonstige", "Nichtwähler"),
  voter = as.numeric(movements),
  row.names = NULL
)


# Plot the bar chart
movements_df %>% 
  ggplot(aes(x = reorder(party, -voter), y = voter, fill = party)) +  
  geom_bar(stat = "identity", color = "black", size=0.2) +  # Vertical bars
  geom_text(aes(label = scales::comma(voter)), vjust = ifelse(movements_df$voter < 0, 1.5, -0.5), size = 3) + # Add labels
  theme_minimal() +  # Minimal theme for clean look
  labs(y = "Stimmen", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  theme(legend.position = "none") +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "black", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="green", "FDP"="#FFCC00","Volt"="purple", "Sonstige"="darkgray",
                                "Nichtwähler" = "beige"))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))








