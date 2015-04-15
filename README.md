# DIPnet
These are scripts I have written as part of a collaborative NEScent Project "Diversity in the Indo-Pacific"

They include scripts for manipulating data files containing population diversity statsitics from the database.

The eventual aim will be ti build these scripts into an R package to access the database to pull down
data for species and/or regions.

list2df.R : This is a script to convert statistic result lists into a single data frame with species, gene and species_gene columns, then create a list of all species and statistics in each populatio location (fnID)

coverageStat.R : this is a script to summarized the coverage statistics

dip_spp.R : this is a script to extract species names and query the names against a fish species list. needs a lot of work to be useful.
