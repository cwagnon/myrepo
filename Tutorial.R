# tutorial from Non-metric Multidimensional Scaling (NMDS) in R
# https://cougrstats.netlify.app/post/2019-12-11-non-metric-multidimensional-scaling-nmds-in-r/

library(vegan)
library(tidyverse)
library(viridis)

orders <- read_csv("data/condensed_order.csv")

# A. RUNNING THE NMDS IN VEGAN #################################################
nmds_results <- metaMDS(comm = orders[ , 4:11],  # Define the community data
                        distance = "bray",       # Specify a bray-curtis distance
                        try = 100)               # Number of iterations

# B. PLOTTING THE NMDS #########################################################

# First create a data frame of the scores from the individual sites.
# This data frame will contain x and y values for where sites are located.
data_scores <- scores(nmds_results)

# Access site scores
site_scores <- as.data.frame(data_scores$sites)

# Now add the extra aquaticSiteType column
data_scores <- cbind(site_scores, orders[, 14])
colnames(data_scores)[3] <- "aquaticSiteType"

# Next, we can add the scores for species data
species_scores <- as.data.frame(scores(nmds_results, "species"))

# Add a column equivalent to the row name to create species labels
species_scores$species <- rownames(species_scores)

# Now we can build the plot!

ggplot() +
  geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
            alpha = 0.5, size = 10) +
  geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2,
                                     color = aquaticSiteType), size = 3) +
  scale_color_manual(values = inferno(15)[c(3, 8, 11)],
                     name = "Aquatic System Type") +
  annotate(geom = "label", x = -1, y = 1.25, size = 10,
           label = paste("Stress: ", round(nmds_results$stress, digits = 3))) +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(size = 24))
