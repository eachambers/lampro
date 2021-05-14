library(tidyverse)
library(stringr)
library(cowplot)
library(viridis)
setwd("~/Desktop/Structurama/")

### The following code will generate Structure plots for the FULL dataset (n=124) from raw
### output files from Structurama v.2. Apart from these raw output files, the only other 
### file required is a text file of identifiers to assign sample IDs to each individual.
### This identifiers file is contained within the Structurama directory. Minor tweaks in 
### this code (the identifiers file, the .out file, change in line 22) and commenting out
### certain lines (42-46,55,58-62,83-99,102) will allow one to make Structure plots for the
### other dataset we ran in Structurama (complex89).


# IMPORT AND TIDY THE DATA ------------------------------------------------

# Import sample IDs based on identifier in Structurama
identifiers <- read_tsv("complex89identifiers.txt", col_names=TRUE)

# Need to add an identifier (run #) to each output file
# Import output file, remove extra column (X93 for complex89 or X128 for all124); 
# Remove first 25% (2500 out of 10000) samples as burnin (we sampled every 100; ran
# for 1x10^6 generations)
output <- read_tsv("complex89_structurama_nomt_out.p", col_names=TRUE) %>%
  select(-X93) %>%
  slice(2501:n())

# Make it into a tidy dataset to make things easier
output <- output %>%
  gather(individual, population, -Cycle, -lnL, -`a1~Fixed(E(K)=5.000)`)


# DETERMINE PROPORTION OF SUPPORT -----------------------------------------
# Count up number of samples within each population, within each individual, and then 
# get a proportion of the total (7500) that are getting that population score; this is
# what we'll use for the actual plotting of the data

output %>% group_by(individual, population) %>% summarize(count=n()) %>%
  mutate(proportion = count/7500) %>%
  mutate(individual2 = str_replace_all(individual, "I", "")) %>%
  mutate(individual2 = str_replace_all(individual2, "[[:punct:]]", "")) %>%
  mutate(individual2 = as.integer(individual2)) %>%
  mutate(individual2 = as.numeric(individual2)) %>% 
  mutate(population = as.character(population)) -> output


# ADD SPECIES IDS ON TO DATA ----------------------------------------------
# The following will append identifiers onto your dataset and arrange the data
# by population so that the eventual figure will order it properly

# Only looking at complex89 individuals:
left_join(output, identifiers) %>% 
  arrange(population, species, desc(proportion)) -> finished

# To look at ALL samples included (all124):
# left_join(cleandata, identifiers) %>% 
#   arrange(population, species, desc(proportion)) -> finished_all

# If you only want to look at the complex89 individuals WITHIN an all124 run, you need 
# to parse out the relevant taxa:
# tokeep <- c("elapsoides","alterna","triangulum","gentilis","annulata","polyzona",
#             "abnorma","micropholis")
# left_join(output, identifiers) %>% 
#   arrange(population, species, desc(proportion)) %>% 
#   filter(species %in% tokeep) -> finished_subset


# CHANGE LEVELS OF DATA FOR PLOTTING --------------------------------------
# Change the levels so that the subsequent plot will order it properly (we want it to ignore the
# order of the individuals, as it would plot normally)

# Order the numbers assigned to individuals (col. individual2) numerically:
finished$individual2 <- factor(finished$individual2, levels=unique(finished$individual2))

# Order the species names according to how they'll be plotted:
finished$species <- factor(finished$species, levels=c("abnorma","micropholis","polyzona","alterna",
                                                              "annulata","gentilis","triangulum","elapsoides"))

# finished_all$species <- factor(finished_all$species, levels=c("abnorma","micropholis",  "polyzona","alterna",
#                                                                       "annulata","gentilis","triangulum","elapsoides",
#                                                                       "Aelegans","Ccoccinea","calligaster","ruthveni",
#                                                                       "knoblochi","mexicana","pyromelana","webbi",
#                                                                       "zonata","nigra","californiae","extenuata",
#                                                                       "splendida","getula","holbrooki"))

########################### MAKE PLOTS OF THE OUTPUT ######################################

# Plot for complex 89 individuals:
plot_complex89 <- ggplot(finished, aes(x=individual2, y=proportion, fill=population)) +
  geom_bar(stat="identity") + 
  panel_border() + 
  facet_grid(~species, space="free_x", scales="free") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c("firebrick1", "lightskyblue", "darkgrey", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4")) +
  theme_cowplot() %+replace% theme(axis.line=element_line(colour="black"),
                                   axis.text.x=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.ticks=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   legend.position="none",
                                   panel.border = element_rect(fill=NA, colour="black", linetype="solid",size=2.5),
                                   strip.text.y = element_text(size=30, face="bold"),
                                   strip.background = element_rect(colour="white", fill="white"),
                                   panel.spacing=unit(-0.1, "lines"))

# Plot for all 124 individuals
# plot_all124 <-ggplot(finished_pop_all, aes(x=individual2, y=proportion, fill=population)) + 
#   geom_bar(stat="identity") + 
#   panel_border() + 
#   facet_grid(~species, space="free_x", scales="free") +
#   scale_y_continuous(expand=c(0,0)) +
#   scale_fill_manual(values=c("lightsalmon", "blue", "lightcoral", "darkgrey", "firebrick1", "yellow", "mediumpurple1", "lightskyblue", "lightgreen", "lightseagreen")) +
#   theme_cowplot() %+replace% theme(axis.line=element_line(colour="black"),
#                                    axis.text.x=element_blank(),
#                                    axis.text.y=element_blank(),
#                                    axis.ticks=element_blank(),
#                                    axis.title.x=element_blank(),
#                                    axis.title.y=element_blank(),
#                                    legend.position="none",
#                                    panel.border = element_rect(fill=NA, colour="black", linetype="solid",size=1),
#                                    strip.text.y = element_text(size=30, face="bold"),
#                                    strip.background = element_rect(colour="white", fill="white"),
#                                    panel.spacing=unit(-0.1, "lines"))

save_plot("Complex89_plot.pdf", plot_complex89, base_height = 5, base_aspect_ratio = 4)
# save_plot("Run1_all124_124vis_figure.pdf", plot_all124, base_height = 5, base_aspect_ratio = 4)

############## COLORIZING FOR ALL 124 SPECIES (insert relevant line into above script and uncomment)
### RUN 1,2,5
# scale_fill_manual(values=c("lightsalmon", "blue", "lightcoral", "darkgrey", "firebrick1", "yellow", "mediumpurple1", "lightskyblue", "lightgreen", "lightseagreen")) +
### RUN 3,4,6-10
# scale_fill_manual(values=c("lightsalmon", "blue", "lightpink4", "lightcoral", "darkgrey", "firebrick1", "yellow", "mediumpurple1", "lightskyblue", "lightgreen", "lightseagreen")) +

############## COLORIZING FOR COMPLEX 89 (insert relevant line into above script and uncomment)
### RUN 2 (k=6)
# scale_fill_manual(values=c("firebrick1", "lightskyblue", "lightcoral", "darkgrey", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4", "blue")) +
### RUN 1, 3 (k=6)
# scale_fill_manual(values=c("yellow", "firebrick1", "lightskyblue", "darkgrey", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4")) +
### RUN 4 (k=4)
# scale_fill_manual(values=c("firebrick1", "lightskyblue", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4", "blue")) +
### RUN 5 (k=5)
# scale_fill_manual(values=c("yellow", "firebrick1", "lightskyblue", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4")) +
### RUN 6 (k=4)
# scale_fill_manual(values=c("firebrick1", "lightskyblue", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4")) +
### RUN 7 (k=5)
# scale_fill_manual(values=c("firebrick1", "lightskyblue", "darkgrey", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4", "blue")) +
### RUN 8 (k=6)
# scale_fill_manual(values=c("yellow", "firebrick1", "lightskyblue", "darkgrey", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4")) +
### RUN 9 (k=5)
# scale_fill_manual(values=c("firebrick1", "lightskyblue", "darkgrey", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4")) +
### RUN 10 (k=7)
# scale_fill_manual(values=c("yellow", "firebrick1", "lightskyblue", "lightcoral", "darkgrey", "mediumpurple1", "lightseagreen", "lightsalmon", "lightpink4")) +