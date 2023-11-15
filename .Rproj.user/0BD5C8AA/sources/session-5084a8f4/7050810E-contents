# Compute LD between SNPs

# Clear workspace
rm(list = ls())
# Set working directory
setwd("d:/analysis/2020_GenomicsBootCamp_Demo/")
#load packages
library(tidyverse)

# data source website: http://widde.toulouse.inra.fr/widde/
# data in this example: Hereford cattle
# publication: Matukumalli et al, PLoS One, 2009 (https://doi.org/10.1371/journal.pone.0005350)

# quality control
# select one chromosome
system("plink --file hereford --cow --chr 1 --mind 0.1 --geno 0.1 --maf 0.05 --hwe 0.0000001 --make-bed --out afterQC")

########################################
# Options to compute LD with PLINK
########################################
# limited window - just nearby SNPs - default behaviour
system("plink --bfile afterQC --cow --r2 --out resultLD1")

# all pairs below LD 0.2 (default threshold)
system("plink --bfile afterQC --cow --r2  --ld-window-r2 0 --out resultLD2")

# adjust the number of SNPs and inter-SNP distances for which you want to compute LD
system("plink --bfile afterQC --cow --r2 --ld-window-r2 0 --ld-window 100 --ld-window-kb 2000 --out resultLD3")

# all pairs you specify the threshold - WARNING: file size could be LARGE! 
#    Add "gz" to create packed files 
system("plink --bfile afterQC --cow --r2 gz --ld-window-r2 0 --out resultLD4")

# LD saved in a matrix of numbers
system("plink --bfile afterQC --cow --r2 square --out resultLD5")

# compute the D' measure of LD
system("plink --bfile afterQC --cow --r2 dprime --out resultLD6")



########################################
# visualize LD decay
########################################

# example run 
system("plink --bfile afterQC --cow --r2 --ld-window 1000 --ld-window-kb 1000  --ld-window-r2 0 --out LdExample")


# read in LD results file
LdValues <- read_table("LdExample.ld")

# calculate LD in 20 kb bins to display the trendline
averageLD <- LdValues %>%
  mutate(markerDistance = abs(BP_B - BP_A)/1000) %>%
  dplyr::filter(markerDistance < 5000) %>%
  mutate(intervals = cut_width(markerDistance, 20, boundary = 0)) %>%
  group_by(intervals) %>%
  summarise_at(vars(R2),funs(mean(., na.rm=TRUE))) %>%
  rename(averageR2=R2)

# calculate inter marker distances
fullLD <- LdValues %>%
  mutate(markerDistance = abs(BP_B - BP_A)/1000) %>%
  dplyr::filter(markerDistance < 5000) %>%
  mutate(intervals = cut_width(markerDistance, 20, boundary = 0))

#merge the two data sets (full LD info and average per bin)
mergedLD <- full_join(fullLD,averageLD, by = "intervals")

# visualize LD decay
ggplot(mergedLD) +
  geom_point(aes(x=markerDistance, y=R2)) +
  geom_line(aes(x=markerDistance, y=averageR2), color="red", size=2)


########################################

# LD pruning -  remove SNPs with high LD with each other (removes one from each pair)

# replace --nonfounders with --make-founders!
system("plink --bfile afterQC --cow --make-founders --indep-pairwise 50 5 0.7 --out afterQC")
# keep only selected markers for your data
system("plink --bfile afterQC --cow --exclude afterQC.prune.out --make-bed --out prunedSet")

