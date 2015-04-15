################################
## Script for DIPnet work
###############################
library(ape)
library(taxize)
library(rfishbase)
library(reshape)
library(stringr)
library(plyr)
library(dplyr)
library(rMaps)
library(maptools)
library(maps)
library(RgoogleMaps)
library(ggmap)
library(ggplot2)
library(mapdata)
library(mapproj)
library(sp)
# library(rgeos)
# library(rgdal)

`%ni%` <- Negate(`%in%`)
###############################
## load data
load("DIPnet_stats_032415_ABGD_EEZ_country.Rdata")
# spp = as.character(names(divstats))
# write(spp, "DIP_spp.txt")
dip.spp = read.csv("DIPnetspecies.csv")
dip.spp = as.character(dip.spp[,1])
dip.spp = gsub(pattern = ".", replacement = " ", fixed = TRUE, dip.spp)
### load tree
tree = ladderize(read.tree("Reef_tree.tre"))
tree$tip.label = gsub("_", " ", tree$tip.label)
## subset spp to fish only
data(all_taxa)
all.fish = all_taxa[,c(2,3,10)]
spp.name = as.character(unlist(str_join(all.fish$Genus, all.fish$Species, sep = " ")))
all.fish = data.frame(spp.name, all.fish$Family)
names(all.fish) = c("species", "family")
###############################
# sub set fish spp
dip.fish = dip.spp[dip.spp %in% all.fish$species]

length(dip.fish[dip.fish %in% tree$tip.label])

length(dip.fish[dip.fish %ni% tree$tip.label])

length(dip.fish[dip.fish %ni% all.fish$Species])

l######################
## Refuge dataset from Pellisier et al Science

refuge = read.delim("refuge.txt", header = TRUE, sep = "\t")
refuge = refuge[,c(2:4,6:9)]
names(refuge) = c("longitude", "latitude", "current.iso.km", "current.area.km2", "past.iso.km", "past.area.km2", "total.rich")
write.csv(refuge, "RichnessFish.csv")


load("rspb20130818supp2.RData")
names(CoWReefs)
unique(CoWReefs$REGION)
################################

################################
# load stat file of interest
load("~//Google Drive//DIPnet_WG4/statistics/By_ESU/fn500id/DIPnet_stats_032415_ABGD_fn500id.Rdata")
#load("~//Google Drive//DIPnet_WG4/statistics/By_ESU/fn500id/")
data = divstats
fishdiv = read.csv("~//Google Drive//DIPnet_WG4/Spatial/RichnessFish_fn500km.csv")
# remove entries wth fewer than 3 populations
data = data[data != "Fewer than 3 sampled populations after filtering. No stats calculated"]
# add spp and gene name column to each list entry
divstat.list = c()
for (i in 1:length(data)){
  df = data.frame(data[[i]])
  sp = names(data[i])
  gene = str_split(sp, "_")[[1]][3]
  pop.number = length(df$popname)
  df$species = rep(sp, times = pop.number)
  df$gene = rep(gene, times = pop.number)
  divstat.list[[i]] = df
}
names(divstat.list) = names(data)
# combine list of data frames into single data frame
divstat.df = do.call("rbind", divstat.list)

# rename popname to fn500id
names(divstat.df) = c("fn500id","sampleN","UniqHapNum", "HaploDiv", "SWdiversity", "EffNumHaplos", "localFST", "NucDivSite", "NucDivLocus", "ThetaS", "TajD", "species", "gene")
divstat.df[,1] = as.character(divstat.df[,1])
fishdiv[,1] = as.character(fishdiv[,1])
divstat.df.rich = left_join(divstat.df, fishdiv, by = "fn500id")
write.csv(divstat.df.rich, "ABGD_fn500id_richness.csv")

no.info = divstat.df.rich$fn500id[is.na(divstat.df.rich$total_rich)]
write(no.info, "no_info.txt")


fn500id = unique(divstat.df$fn500id)

length(fishdiv$fn500id[fishdiv$fn500id %ni% unique(divstat.df$fn500id)])
length(fn500id[fn500id %ni% fishdiv$fn500id])



