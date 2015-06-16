########################################
# Script to parse life history data from fisbase for DIPnet taxa
########################################
library(rfishbase)
library(taxize)
library(plyr)
library(dplyr)
library(stringr)
library(stringi)
###############################
#remove previous variable assignments
rm(list=ls())
##############################
options(stringsAsFactors = FALSE)
###############################
`%ni%` <- Negate(`%in%`)
###############################

# load fishbase species names
data(all_taxa)
all.fish = all_taxa[,c(2,3,10)]
spp.name = as.character(unlist(str_c(all.fish$Genus, all.fish$Species, sep = " ")))
all.fish = data.frame(spp.name, all.fish$Family)
all.gen = as.character(unique(all_taxa$Genus))
names(all.fish) = c("species", "family")
##########
# import species list - new list from a stat list
spp = read.delim("DIPnet_species.txt", header = T, sep = ",")
spp = as.character(spp[,2])
spp = gsub("_", " ", spp)
genera = unlist(stri_extract_first_words(spp))
species = unlist(stri_split_fixed(spp, " ",  n=2, tokens_only=TRUE))
species = species[species %ni% genera]
dipspp = data.frame(genera, species, stringsAsFactors = FALSE)
dipspp$genus_species = str_c(dipspp$genera, dipspp$species, sep = " ")
# fix Acanthaster
dipspp$genus_species[grep("Acanthaster", dipspp$genus_species)] = "Acanthaster planci"
spp = unique(dipspp$genus_species)
dip.fish = dipspp$genus_species[dipspp$genera %in% all.gen]
dip.invert = dipspp$genus_species[dipspp$genera %ni% all.gen]
##########

#########
## Get FIshBase Traits
## validate names
fish = validate_names(dip.fish)
## general traits
fish.traits = species(fish, fields=c("SpecCode", "FamCode", "DepthRangeShallow", "DepthRangeDeep", "Weight", "Length"))
fish.traits = cbind(species = speciesnames(fish.traits$SpecCode), fish.traits)
## get family names
family = c()
for (i in 1:length(fish.traits$FamCode)) {
  code = fish.traits$FamCode[i]
  fam = all_taxa$Family[all_taxa$FamCode == code]
  family[i] = unique(fam)
}
fish.traits$family = family
## fecundity
# fish.fecund = fecundity(fish, fields = c("SpecCode", "FecundityMin", "FecundityMax", "FecComment"))
# fish.fecund = cbind(species = speciesnames(fish.fecund$SpecCode), fish.fecund)
# #fish.fecund = fish.fecund[unique(fish.fecund$species),]
# ## ecology
# fish.eco = ecology(fish, fields=c("SpecCode", "FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph"))
# fish.eco = cbind(species = speciesnames(fish.eco$SpecCode), fish.eco)
# #fish.eco = fish.eco[unique(fish.eco$species),]
# ## fooditems
# fish.food = fooditems(fish, fields = c("SpecCode", "FoodI", "FoodII", "FoodIII", "Foodgroup"))
# fish.food = cbind(species = speciesnames(fish.food$SpecCode), fish.food)
# #fish.food = fish.food[unique(fish.food$species),]
##
###############################
# Join all fishbase traits
# fish.traits.comp = full_join(fish.traits, fish.fecund, by = "species")
# fish.traits.comp = full_join(fish.traits.comp, fish.eco, by = "species")
# fish.traits.comp = full_join(fish.traits.comp, fish.food, by = "species")
# fish.traits = unique(fish.traits.comp)
# fish.traits[duplicated(fish.traits$species),]
###########################
# sub set data from Selkoe et al
selkoe = read.csv("~//Google Drive//DIPnet_work//DIPnet_WG4//PCM for Genetic Diversity//Life history Traits//Raw_data//Selkoe et al_speciestraits.csv")
head(selkoe)
## trim white space
# selkoe$Species = str_trim(selkoe$Species, side = "both")
# selkoe$Species[grep("Epinephelus/Hyporthodus quernus", selkoe$Species)] = "Epinephelus quernus"
# write.csv(selkoe, "~//Google Drive//DIPnet_WG4/PCM for Genetic Diversity/Life history Traits/Selkoe et al_speciestraits.csv")
### sub set tabel to dip.fish spp
selkoe_fish = selkoe[selkoe$Fish == 1,]
selkoe_invert = selkoe[selkoe$Fish == 0,]
selkoe_dip = selkoe[selkoe$Species %in% spp,]
selkoe.dip.fish = selkoe_fish[selkoe_fish$Species %in% spp,]
write.csv(selkoe_fish, "~//Google Drive//DIPnet_work//DIPnet_WG4//PCM for Genetic Diversity//Life history Traits//Processed_data/Selkoe_DIP_fish.csv")
write.csv(selkoe_invert, "~//Google Drive//DIPnet_work//DIPnet_WG4//PCM for Genetic Diversity//Life history Traits//Processed_data/Selkoe_DIP_inverts.csv")
###########################
## import Cynthias data
# reef fam data
# cr.traits = read.csv("traits/Reeffams_diversity.csv")
# species = unlist(stri_split_fixed(cr.traits$Phyl_name, "_",  n=2, tokens_only=FALSE))
# species = species[grep("_", species)]
# cr.traits$species = species
# cr.traits$species = gsub("_", " ", cr.traits$species)
# DIPcr_traits = cr.traits[cr.traits$species %in% dip.fish,]
# length(DIPcr_traits$species[DIPcr_traits$species %ni% selkoe_fish$Species])
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
mb_fish = mb_traits[mb_traits$species %in% spp,]
write.csv(mb_fish, "DIP_mb_FishTraits.csv")
write.csv(mb_fish, "~//Google Drive//DIPnet_work//DIPnet_WG4//PCM for Genetic Diversity//Life history Traits//Processed_data/Maria_fishtraits_DIP.csv")
##########################
# import traits from Rocha paper
luiz_traits = read.csv("~//Google Drive//DIPnet_work//DIPnet_WG4//PCM for Genetic Diversity//Life history Traits//Raw_data/Luiz_2013_PNASSupplement_FishFunctions_sd01.csv")
luiz_traits = data.frame(species = str_c(luiz_traits$Genus, luiz_traits$Species, sep = " "), luiz_traits[,4:13])
luiz_traits = luiz_traits[luiz_traits$species %in% spp,]
length(luiz_traits$species)
write.csv(luiz_traits, "~//Google Drive//DIPnet_work//DIPnet_WG4//PCM for Genetic Diversity//Life history Traits//Processed_data/luiz_dip_traits.csv")

#######################
# Combin data sets
# validate names
# rename columns
names(selkoe_fish)[-25] = paste0("SK.", names(selkoe_fish))
selkoe_fish$species = selkoe_fish$SK.Species
names(mb_fish)[-1] = paste0("MB.", names(mb_fish)[-1])
names(luiz_traits)[-1] = paste0("LZ.", names(luiz_traits)[-1])
# Combine datasets using full.join
final.fish = full_join(fish.traits, selkoe_fish, by = "species")
final.fish = full_join(final.fish, mb_traits, by = "species")
final.fish = full_join(final.fish, luiz_traits, by = "species")
# get family
family = c()
for (i in 1:length(final.fish$species)) {
  spp = final.fish$species[i]
  fam = all.fish$family[all.fish$species == spp]
  family[i] = unique(fam)
}


# write output to gdrive
write.csv(final.fish, "~//Google Drive//DIPnet_work//DIPnet_WG4//PCM for Genetic Diversity//Life history Traits//Processed_data/DIPfishTraits.csv")

##########
# make individual trait datasets
# Depth
depth.heads = names(final.fish)[grep("depth", tolower(names(final.fish)))]
depth = data.frame(final.fish$family, final.fish$species, c(final.fish[,depth.heads]))
names(depth) = c("family", "species", depth.heads)
###############################################################################
###############################################################################
###############################################################################
# Invert Traits

