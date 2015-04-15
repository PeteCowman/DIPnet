######################################
## Script for plotting distribution of coverage values of whole data set
######################################
library(ggplot2)


coverage = stat.df$Coverage
bins = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
hist(coverage, breaks = bins)
bin1 = coverage[coverage <= 0.1]
bin4 = coverage[coverage <= 0.4]
bin5 = coverage[coverage >= 0.5]
bin9 = coverage[coverage >= 0.9]

percent1 = length(bin1)*100/length(coverage)
percent4 = length(bin4)*100/length(coverage)
percent5 = length(bin5)*100/length(coverage)
percent9 = length(bin9)*100/length(coverage)
cat("Less than 10% Coverage = ", percent1, "\n", "Less than 40% Coverage = ", percent4, "\n","Greater than 50% Coverage = ", percent5, "\n","Greater than 90% Coverage = ", percent9, "\n")

genes = unique(stat.df$gene)
### CYB cover
CYB_cover = stat.df$Coverage[stat.df$gene == "CYB"]
hist(CYB_cover, breaks = bins)
bin1 = CYB_cover[CYB_cover <= 0.1]
bin4 = CYB_cover[CYB_cover <= 0.4]
bin5 = CYB_cover[CYB_cover >= 0.5]
bin9 = CYB_cover[CYB_cover >= 0.9]
percent1 = length(bin1)*100/length(CYB_cover)
percent4 = length(bin4)*100/length(CYB_cover)
percent5 = length(bin5)*100/length(CYB_cover)
percent9 = length(bin9)*100/length(CYB_cover)
cat("Less than 10% CYB_cover = ", percent1, "\n", "Less than 40% CYB_cover = ", percent4, "\n","Greater than 50% CYB_cover = ", percent5, "\n","Greater than 90% CYB_cover = ", percent9, "\n",sep = "")
### CO1 cover
CO1_cover = stat.df$Coverage[stat.df$gene == "CO1"]
hist(CO1_cover, breaks = bins)
bin1 = CO1_cover[CO1_cover <= 0.1]
bin4 = CO1_cover[CO1_cover <= 0.4]
bin5 = CO1_cover[CO1_cover >= 0.5]
bin9 = CO1_cover[CO1_cover >= 0.9]
percent1 = length(bin1)*100/length(CO1_cover)
percent4 = length(bin4)*100/length(CO1_cover)
percent5 = length(bin5)*100/length(CO1_cover)
percent9 = length(bin9)*100/length(CO1_cover)
cat("Less than 10% CO1_cover = ", percent1, "\n", "Less than 40% CO1_cover = ", percent4, "\n","Greater than 50% CO1_cover = ", percent5, "\n","Greater than 90% CO1_cover = ", percent9, "\n",sep = "")
### A68 cover
A68_cover = stat.df$Coverage[stat.df$gene == "A68"]
hist(A68_cover, breaks = bins)
bin1 = A68_cover[A68_cover <= 0.1]
bin4 = A68_cover[A68_cover <= 0.4]
bin5 = A68_cover[A68_cover >= 0.5]
bin9 = A68_cover[A68_cover >= 0.9]
percent1 = round(length(bin1)*100/length(A68_cover), 2)
percent4 = round(length(bin4)*100/length(A68_cover), 2)
percent5 = round(length(bin5)*100/length(A68_cover), 2)
percent9 = round(length(bin9)*100/length(A68_cover), 2)
cat("Less than 10% A68_cover = ", percent1, "\n", "Less than 40% A68_cover = ", percent4, "\n","Greater than 50% A68_cover = ", percent5, "\n","Greater than 90% A68_cover = ", percent9, "\n",sep = "")

### CR cover
CR_cover = stat.df$Coverage[stat.df$gene == "CR"]
hist(CR_cover, breaks = bins)
bin1 = CR_cover[CR_cover <= 0.1]
bin4 = CR_cover[CR_cover <= 0.4]
bin5 = CR_cover[CR_cover >= 0.5]
bin9 = CR_cover[CR_cover >= 0.9]
percent1 = length(bin1)*100/length(CR_cover)
percent4 = length(bin4)*100/length(CR_cover)
percent5 = length(bin5)*100/length(CR_cover)
percent9 = length(bin9)*100/length(CR_cover)
cat("Less than 10% CR_cover = ", percent1, "\n", "Less than 40% CR_cover = ", percent4, "\n","Greater than 50% CR_cover = ", percent5, "\n","Greater than 90% CR_cover = ", percent9, "\n",sep = "")
### ND1 cover
ND1_cover = stat.df$Coverage[stat.df$gene == "ND1"]
hist(ND1_cover, breaks = bins)
bin1 = ND1_cover[ND1_cover <= 0.1]
bin4 = ND1_cover[ND1_cover <= 0.4]
bin5 = ND1_cover[ND1_cover >= 0.5]
bin9 = ND1_cover[ND1_cover >= 0.9]
percent1 = length(bin1)*100/length(ND1_cover)
percent4 = length(bin4)*100/length(ND1_cover)
percent5 = length(bin5)*100/length(ND1_cover)
percent9 = length(bin9)*100/length(ND1_cover)
cat("Less than 10% ND1_cover = ", percent1, "\n", "Less than 40% ND1_cover = ", percent4, "\n","Greater than 50% ND1_cover = ", percent5, "\n","Greater than 90% ND1_cover = ", percent9, "\n",sep = "")

### CO2 cover
CO2_cover = stat.df$Coverage[stat.df$gene == "CO2"]
hist(CO2_cover, breaks = bins)
bin1 = CO2_cover[CO2_cover <= 0.1]
bin4 = CO2_cover[CO2_cover <= 0.4]
bin5 = CO2_cover[CO2_cover >= 0.5]
bin9 = CO2_cover[CO2_cover >= 0.9]
percent1 = length(bin1)*100/length(CO2_cover)
percent4 = length(bin4)*100/length(CO2_cover)
percent5 = length(bin5)*100/length(CO2_cover)
percent9 = length(bin9)*100/length(CO2_cover)
cat("Less than 10% CO2_cover = ", percent1, "\n", "Less than 40% CO2_cover = ", percent4, "\n","Greater than 50% CO2_cover = ", percent5, "\n","Greater than 90% CO2_cover = ", percent9, "\n",sep = "")

######################
# cat("Less than 10% Coverage = ", percent1, "\n", "Less than 40% Coverage = ", percent4, "\n","Greater than 50% Coverage = ", percent5, "\n","Greater than 90% Coverage = ", percent9, "\n")
# 
# cat("Less than 10% Coverage = ", percent1, "\n", "Less than 40% Coverage = ", percent4, "\n","Greater than 50% Coverage = ", percent5, "\n","Greater than 90% Coverage = ", percent9, "\n")
# 
# cat("Less than 10% CYB_cover = ", percent1, "\n", "Less than 40% CYB_cover = ", percent4, "\n","Greater than 50% CYB_cover = ", percent5, "\n","Greater than 90% CYB_cover = ", percent9, "\n",sep = "")
# 
# cat("Less than 10% CO1_cover = ", percent1, "\n", "Less than 40% CO1_cover = ", percent4, "\n","Greater than 50% CO1_cover = ", percent5, "\n","Greater than 90% CO1_cover = ", percent9, "\n",sep = "")
# 
# cat("Less than 10% A68_cover = ", percent1, "\n", "Less than 40% A68_cover = ", percent4, "\n","Greater than 50% A68_cover = ", percent5, "\n","Greater than 90% A68_cover = ", percent9, "\n",sep = "")
# 
# cat("Less than 10% CR_cover = ", percent1, "\n", "Less than 40% CR_cover = ", percent4, "\n","Greater than 50% CR_cover = ", percent5, "\n","Greater than 90% CR_cover = ", percent9, "\n",sep = "")
# 
# cat("Less than 10% ND1_cover = ", percent1, "\n", "Less than 40% ND1_cover = ", percent4, "\n","Greater than 50% ND1_cover = ", percent5, "\n","Greater than 90% ND1_cover = ", percent9, "\n",sep = "")
# 
# cat("Less than 10% CO2_cover = ", percent1, "\n", "Less than 40% CO2_cover = ", percent4, "\n","Greater than 50% CO2_cover = ", percent5, "\n","Greater than 90% CO2_cover = ", percent9, "\n",sep = "")
