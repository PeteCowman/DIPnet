########################################
# Script to parse life history data from fisbase for DIPnet taxa
########################################
library(rfishbase)
library(taxize)
library(plyr)
library(dplyr)
library(stringr)
library(stringi)

`%ni%` <- Negate(`%in%`)

# load fishbase species names
data(all_taxa)
all.fish = all_taxa[,c(2,3,10)]
spp.name = as.character(unlist(str_join(all.fish$Genus, all.fish$Species, sep = " ")))
all.fish = data.frame(spp.name, all.fish$Family)
all.gen = as.character(unique(all_taxa$Genus))
names(all.fish) = c("species", "family")
##########
# import species list - new list from a stat list
spp = read.csv("DIPnetspecies.csv", header = F)
names(spp) = "species"
spp$species = gsub(".", " ", spp$species, fixed = TRUE)
spp$species[grep("Acanthaster", spp$species)] = "Acanthaster planci"
spp = unique(spp)
genera = unlist(stri_split_fixed(spp$species, " ",  n=1, tokens_only=TRUE))
dip.fish = spp$species[genera %in% all.gen]
dip.invert = spp$species[genera %ni% all.gen]
##########
# check synonyms
syn.fish = dip.fish[dip.fish %ni% all.fish$species]
syn.table = tnrs( query = syn.fish, source = "NCBI")
syn.table = resolve(query = syn.fish, db = "gnr")
## synonyms/mistakes in species names found
## Centropyge loricula -> Centropyge loriculus (FB)
## Myripristis berntdi -> Myripristis berndti (spelling)
## Pomacentrus micronesicus -> new species
## Doryrhamphus excisus -> Doryrhamphus excisus excisus
## Mulloidichthys pflugeri -> Mulloidichthys pfluegeri (spelling)
## Auxis thazard -> Auxis thazard thazard (subspecies)
## Auxis rochei -> Auxis rochei rochei (subspecies)
## Himantura tutul -> new species
## Scomberomorus sp -> no idea what species this is. need to find original paper
## Acanthurus nigros -> new Hw endemic that has be (re)split from A nigrorois. not in fish base 

#########
# get info tables
dip.fish.info = species_info(dip.fish) # takes a long time to run...
length(dip.fish.info$SpecCode)
dip.fish.info$Genus_species = str_join(dip.fish.info$Genus, dip.fish.info$Species, sep = " ")
write.csv(dip.fish.info, "FishbaseTraits.csv")
fb.depth = dip.fish.info[,c(1:3, 97, 22, 25:26, 28:29)]
fb.pld = dip.fish.info[,c(1:3, 97, 90)]
#####

all_taxa[grep("Himantura", all_taxa$Genus),]

spp[grep("Himantura", spp)]
spp[grep("Centropyge", spp)]
spp[grep("Acanthurus", spp)]
###########################
# sub set data from Selkoe et al
selkoe = read.csv("~//Google Drive//DIPnet_WG4/PCM for Genetic Diversity/Life history Traits/Selkoe et al_speciestraits.csv")
head(selkoe)
## trim white space
# selkoe$Species = str_trim(selkoe$Species, side = "both")
# selkoe$Species[grep("Epinephelus/Hyporthodus quernus", selkoe$Species)] = "Epinephelus quernus"
# write.csv(selkoe, "~//Google Drive//DIPnet_WG4/PCM for Genetic Diversity/Life history Traits/Selkoe et al_speciestraits.csv")
### sub set tabel to dip.fish spp
selkoe_fish = selkoe[selkoe$Species %in% dip.fish,]
selkoe_invert = selkoe[selkoe$Species %in% dip.invert,]
write.csv(selkoe_fish, "~//Google Drive//DIPnet_WG4/PCM for Genetic Diversity/Life history Traits/Selkoe_DIP_fish.csv")
write.csv(selkoe_invert, "~//Google Drive//DIPnet_WG4/PCM for Genetic Diversity/Life history Traits/Selkoe_DIP_inverts.csv")
###########################
## import Cynthias data
# reef fam data
cr.traits = read.csv("traits/Reeffams_diversity.csv")
species = unlist(stri_split_fixed(cr.traits$Phyl_name, "_",  n=2, tokens_only=FALSE))
species = species[grep("_", species)]
cr.traits$species = species
cr.traits$species = gsub("_", " ", cr.traits$species)
DIPcr_traits = cr.traits[cr.traits$species %in% dip.fish,]
#length(DIPcr_traits$species[DIPcr_traits$species %ni% selkoe_fish$Species])
# Specis plds...need more work
# pld = read.csv("traits//Species_plds_Rawinfo.csv")
# pld$Species = str_trim(pld$Species, side = "both")
# pld$Species = gsub("   ", " ", pld$Species)
# names(pld)
# DIP_pld = pld[pld$Species %in% spp$species,]
# length(DIP_pld$Species)
###########################
### import Marie_fish_traits
mb_traits = read.csv("traits/Maria_fishtraits_DIP.csv")
names(mb_traits) = c("species", "Family", "HomeRangeLKm", "MaxLength", "MaxAge", "PLD_Mean", "TrophicLevel", "Food", "TrophicGroup", "Aggregation", "Position", "SpawnMode", "ParentalMode", "Active")
DIP_mb_fish = mb_traits[mb_traits$species %in% dip.fish,]
write.csv(DIP_mb_fish, "DIP_mb_FishTraits.csv")
write.csv(DIP_mb_fish, "~//Google Drive//DIPnet_WG4/PCM for Genetic Diversity/Life history Traits/DIP_mb_FishTraits.csv")
##########################
# import traits from Rocha paper
luiz_traits = read.csv("~//Google Drive//DIPnet_WG4/PCM for Genetic Diversity/Life history Traits/Luiz_2013_PNASSupplement_FishFunctions_sd01.csv")
luiz_traits = luiz_traits[,1:13]
luiz_traits$Genus_species = str_join(luiz_traits$Genus, luiz_traits$Species, sep = " ")
DIP_luiz_traits = luiz_traits[luiz_traits$Genus_species %in% spp$species,]
length(DIP_luiz_traits$Genus_species)
write.csv(DIP_luiz_traits, "~//Google Drive//DIPnet_WG4/PCM for Genetic Diversity/Life history Traits/luiz_dip_traits.csv")
