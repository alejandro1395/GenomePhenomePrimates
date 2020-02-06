################################
################################
#READ THE TABLE FROM YEARLY PAD#
################################


library(ggplot2)
library(ggsignif)

PAD_dataset <- read.csv("../data/Phenome/subjects_measurements_yearly_50th_percentile.csv", header = T, sep = "\t")
summary(PAD_dataset)

PAD_species_names <- read.csv("../data/Phenome/PAD_species.csv", header = T, sep = "\t")
summary(PAD_species_names)
View(PAD_species_names)
PAD_species_names$Species.name <- gsub(' ', '_', PAD_species_names$Species.name)
colnames(PAD_species_names) <- c("SpeciesBROAD", "Common.name", "Sequenced")

Consesus_species_names <- read.csv("../data/Phenome/PrimateGenomePhenomeSpeciesList.csv", header = T, sep = "\t")
summary(Consesus_species_names)



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
    geom_bar(stat = "identity", fill="#FFCC66", col="black") + xlab("Measure") +
  ylab("Measurements count") +
  theme_light() + theme(axis.text.x = element_text(angle = 75, vjust = 0.53, hjust = 0.5))

#Transpose year varlues for each measure

measurement_subset <- PAD_dataset[,c("measurement","y0","y1","y2","y3","y4","y5","y6","y7",
                                 "y8","y9","y10","y11","y12","y13","y14","y15","y16","y17",
                                     "y18","y19","y20","y21","y22","y23","y24","y25","y26","y27",
                                     "y28","y29","y30","y31","y32","y33","y34","y35","y36","y37",
                                      "y38","y39","y40","y41","y42","y43","y44","y45","y46","y47",
                                     "y48","y49","y50","y51","y52","y53","y54","y55","y56","y57",
                                     "y58","y59","y60","y61","y62","y63","y64","y65","y66","y67",
                                     "y68","y69","y70")]

rownames(measurement_subset) <- paste(as.character(PAD_dataset$measurement), as.character(PAD_dataset$species), 
          as.character(PAD_dataset$subject), sep = "_")
transposed_measurement_subset <- as.data.frame(t(measurement_subset))
View(transposed_measurement_subset)




#######################################
#######################################
######SUMMARIZE INFORMATION############ 
#######################################

#ROW1 OF TABLES
#First table that summarizes N of the study
library(dplyr)
library(tidyr)
diplr_primates_Ntotal <- PAD_dataset %>%
  group_by(species) %>% 
  summarise_each(funs(n_distinct(.)))  

diplr_primates_Ntotal <- subset(diplr_primates_Ntotal, select = c("species", "subject", "measurement"))
colnames(diplr_primates_Ntotal) <- c("species", "subjects", "measurement_type")
write.table(diplr_primates_Ntotal, file = "../results/Ntotal_primates.csv", sep="\t",
            row.names = FALSE, quote = FALSE)

#CREATE FUNCTION TO OUTPUT IN A CSV



#ROW2 OF TABLES --> (discrete variables)
discrete_subset <- subset(PAD_dataset, select = c("species", "sex", "social_environment", 
                                                  "housing", "diet", "measurement"))

Discrete_categories_N <- discrete_subset %>%
  group_by_all(.)%>%
  count() %>%
  pivot_wider(names_from = c("sex", "social_environment", "housing", "diet", "measurement"),
              values_from = n,
              values_fill = list(n = 0)) %>%
  write.table(., sep="\t", row.names = FALSE, quote = FALSE,file = "../results/PAD_discrete_summary.csv")
View(Discrete_categories)


#ROWS OF TABLES --> (continous variables) 
#Here I compute the sum and mean for all years of each primate in the dataset
measures_dataset  <- subset(PAD_dataset, select = c("species", "measurement", "y0", "y1", "y2", "y3", "y4", "y5",
                                                    "y6", "y7", "y8", "y9", "y10", "y11", "y12", "y13",
                                                    "y14", "y15", "y16", "y17", "y18", "y19", "y20",
                                                    "y21", "y22", "y23", "y24", "y25", "y26", "y27", "y28",
                                                    "y29", "y30", "y31", "y32", "y33", "y34", "y35", "y36",
                                                    "y37", "y38", "y39", "y40", "y41", "y42", "y43", "y44",
                                                    "y45", "y46", "y47", "y48", "y49", "y50", "y51", "y52",
                                                    "y53", "y54", "y55", "y56", "y57", "y58", "y59", "y60",
                                                    "y61", "y62", "y63", "y64", "y65", "y66", "y67", "y68",
                                                    "y69", "y70"))

measures_dataset <- measures_dataset[, colSums(is.na(measures_dataset)) != nrow(measures_dataset)]

mean_group <- measures_dataset %>%
  select(contains("y")) %>%
  rowMeans(na.rm = TRUE)

measures_dataset <- bind_cols(measures_dataset, mean_group = mean_group)
drop <- c("y0", "y1", "y2", "y3", "y4", "y5",
            "y6", "y7", "y8", "y9", "y10", "y11", "y12", "y13",
            "y14", "y15", "y16", "y17", "y18", "y19", "y20",
            "y21", "y22", "y23", "y24", "y25", "y26", "y27", "y28",
            "y29", "y30", "y31", "y32", "y33", "y34", "y35", "y36",
            "y37", "y38", "y39", "y40", "y41", "y42", "y43", "y44",
            "y45", "y46", "y47", "y48", "y49", "y50", "y51", "y52",
            "y53", "y54", "y55", "y56", "y57", "y58", "y59")
                                                                     
Measurements <- measures_dataset %>%
  group_by(species, measurement) %>% 
  summarise_all(funs(mean), na.rm=TRUE) %>%
  select(-drop) %>%
  pivot_wider(names_from = c("measurement"),
              values_from = c("mean_group"),
              values_fill = list(n = 0)) %>% 
  mutate_if(is.numeric, list(~na_if(., "NaN")))

#Let's join the 2 Macaque information into one

Rhesus <- Measurements %>%
  filter(grepl("Rhesus", species)) 
View(Rhesus)

means_rhesus <- colMeans(Rhesus[,2:ncol(Rhesus)], na.rm = TRUE)

#remove previous existing rhesus rows and adding the consensus mean
Measurements_reshus <- Measurements[-grep("Rhesus",Measurements$species),]
View(Measurements_reshus)


NEW_ROW <- data.frame(t(unlist(c("Rhesus Macaque", as.numeric(as.character(means_rhesus))))))
colnames(NEW_ROW) <- colnames(Measurements_reshus)

library(plyr)
all_data <- rbind(Measurements_reshus,NEW_ROW)
View(all_data)

Measurements_reshus <- rbind(Measurements_reshus, c("Rhesus Macaque", as.numeric(as.character(means_rhesus))))
  #write.table(., sep="\t", row.names = FALSE, quote = FALSE,file = "../results/PAD_measurements.csv")



#MEASUREMENTS info for each year/for each measurement
#FILL dataframe in loop


colnames(Measurements_reshus)[1] <- "Common.name"
all_species <- full_join(PAD_species_names, Consesus_species_names)
all_species_sorted <- all_species[order(all_species$SpeciesBROAD),]
all_measurements <- full_join(all_species_sorted, Measurements_reshus)
all_measurements_sorted <- all_measurements[order(all_measurements$SpeciesBROAD),]
View(all_measurements_sorted)
write.table(all_measurements_sorted[], sep="\t", row.names = FALSE, quote = FALSE,file = "../results/PAD_measurements.csv")


