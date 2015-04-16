###################################
# Script to map dip species on to a large fish tree
###################################
library(ape)
library(geiger)
library(BioGeoBEARS)

# read in trees
rb.tree = read.tree("trees//rb_gas.tre")
reef.tree = read.tree("trees//Reef_tree.tre")
head(rb.tree$tip.label)
head(reef.tree$tip.label)
