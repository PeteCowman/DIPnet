#####################################################################
## Script to add species, gene names and combine into one data frame
## First loop adds species, gene and species_gene names to each df in list
## then uses do.call("rbind, list) to combine all dfs in to single df
## Second loop changes df back into list where each list entry is a population name/fnID
## df with stats on each species_gene in that population name/fnID
## Third loop takes each fnID in list and output a df with summary stats for that fnID
## The thrid loop could be modified in future to calculate the "statistic" for each fnID
## that could be then compared to species richness for that fnID
####################################################################
## Written by Peter Cowman April 15th 2015 at DIPnet workshop
## Used with DIPnet Rdata files in goolge drive statistics folder
####################################################################
library(stringr)
library(plyr)
library(dply)


# load stat file of interest from statistics folder
#load("~//Google Drive//DIPnet_WG4/statistics/By_ESU/fn500id/DIPnet_stats_032415_ABGD_fn500id.Rdata")
load("DIPnet_stats_ABGD_032315.Rdata")
data = divstats

# remove entries wth fewer than 3 populations
# data = data[data != "Fewer than 3 sampled populations after filtering. No stats calculated"]
data = data[data != "NULL"]

# add spp, gene, spp_gene column to each list entry
stat.list = c()
for (i in 1:length(data)){
  df = data.frame(data[[i]])
  spg = names(data[i]) #species gene name
  gene = str_split(spg, "_")[[1]][3] # gene name
  spp = str_join(str_split(spg, "_")[[1]][-3], collapse = "_")
  pop.number = length(df$popname) #number of rows
  df$species = rep(spp, times = pop.number)
  df$species_gene = rep(spg, times = pop.number)
  df$gene = rep(gene, times = pop.number)
  stat.list[[i]] = df
}

# combine list of data frames into single data frame
stat.df = do.call("rbind", stat.list)
# write output file as .csv with appropriate file name
write.csv(stat.df, "DIPnet_stats_ABGD_032315.csv")
###############################
###############################
## script to loop through df and make a list of stats by fnid
data = stat.df
cell_ids = unique(data$popname)
stat_id_list = c()
for (i in 1:length(cell_ids)){
  cell = cell_ids[i]
  cellstat = data[data$popname == cell,]
  rownames(cellstat) = 1:length(cellstat$popname)
  stat_id_list[[i]] = cellstat  
}
## add popname/fnID to list entry names
names(stat_id_list) = cell_ids
# write output file as needed with appropriate file name
# this list could then be used to write a loop to get a standized measure for desired stat
##############################
##############################
## script to get spp number etc for each fn500id - could be used to compile location estimates for any statistic
data = stat_id_list
fID.summ = data.frame(fnID=as.character(), Species=as.numeric(), Genes=as.numeric(), Species_gene=as.numeric(), stringsAsFactors = FALSE) # make data frame, add new columns as need for stat estimates
for (i in 1:length(data)){
  fnid = names(data[i])
  stat = data[[i]]
  spp.no = length(unique(stat$species))
  gene.no = length(unique(stat$gene))
  spp.gene = length(unique(stat$species_gene))
  fID.summ[i,] = c(fnid, spp.no, gene.no, spp.gene)
  # could add any stat estimate for fnid/popname
  # also could add in species richness number here
}
#############################
# write output file as .csv with appropriate file name
write.csv(fID.summ, "DIPnet_stats_pop_summary.csv")


