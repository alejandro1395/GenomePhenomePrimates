############################
############################
#READ THE TABLE FROM YEARLY PAD#
###########################


library(ggplot2)
library(ggsignif)

PAD_dataset <- read.csv("../data/Phenome/subjects_measurements_yearly_50th_percentile.csv", header = T, sep = "\t")
summary(PAD_dataset)

species_names <- read.csv("../data/Phenome/PAD_species.csv", header = T, sep = "\t")
summary(species_names)


#DESCRIPTIVE PART

#Measurements counts for species
total_species <- as.data.frame(aggregate(PAD_dataset$measurements,
                by = list(PAD_dataset$species),
                FUN = sum))
colnames(total_species) <- c("species", "measurements")

ggplot(data=total_species, 
       aes(x=reorder(species, -as.numeric(as.character(measurements))), 
           y=as.numeric(as.character(measurements)))) + 
  geom_bar(stat = "identity", fill="#FFCC33", col="black") + xlab("Species") +
  ylab("Measurements count") +
  theme_light() + theme(axis.text.x = element_text(angle = 75, vjust = 0.53, hjust = 0.5))


#Sex bias measurement counts
total_sex_species <- as.data.frame(aggregate(PAD_dataset$measurements,
                                         by = list(PAD_dataset$species, PAD_dataset$sex),
                                         FUN = sum))
colnames(total_sex_species) <- c("species", "sex", "measurements")

ggplot(data=total_sex_species, 
       aes(x=reorder(species, -as.numeric(as.character(measurements))), 
           y=as.numeric(as.character(measurements)),
           fill=sex)) + 
  geom_bar(stat = "identity", col="black", position = "dodge") + xlab("Species") +
  ylab("Measurements count") +
  theme_light() + theme(axis.text.x = element_text(angle = 75, vjust = 0.53, hjust = 0.5)) +
  scale_fill_manual(values = c("#FFCC33", "#996633"))

#Measurements counts for each measure type

total_measurements <- as.data.frame(aggregate(PAD_dataset$measurements,
                                         by = list(PAD_dataset$measurement),
                                         FUN = sum))
colnames(total_measurements) <- c("measure", "measurements")

ggplot(data=total_measurements, 
       aes(x=reorder(measure, -as.numeric(as.character(measurements))), 
           y=as.numeric(as.character(measurements)))) + 
  geom_bar(stat = "identity", fill="#FFCC66", col="black") + xlab("Species") +
  ylab("Measurements count") +
  theme_light() + theme(axis.text.x = element_text(angle = 75, vjust = 0.53, hjust = 0.5))
