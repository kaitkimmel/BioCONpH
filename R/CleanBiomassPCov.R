### Cleaning percent cover and biomass for final analyses
library(tidyr)
library(here)
percov <- read.delim(here("data", "txt files", "PerCov_Dwnld2020.txt")) 
# dataset online at https://www.cedarcreek.umn.edu/research/data/dataset?pce141
tbiomass <- read.delim(here("data", "txt files", "AbvGrndBiomass_2017.txt"))
# dataset online at https://www.cedarcreek.umn.edu/research/data/dataset?ple141
ph <- read.csv(here('data', "ph.csv"),stringsAsFactors=FALSE)
# dataset currently through 2016 online at https://www.cedarcreek.umn.edu/research/data/dataset?sphe141
ph2017 <- read.csv(here("data", "2017ph.csv"))
# 2017 pH data not currently available online, but will be updated soon!
ph2017 <- ph2017[,-4] # get rid of notes column
ph <- ph[,c(1,3,9)] # get columns from rest of pH data
ph <- rbind(ph, ph2017) # add 2017 data

percov$Species<- gsub(pattern = "schizachyrium scoparium", replacement = "Schizachyrium scoparium", percov$Species)
percov$Species<- gsub(pattern = "amorpha canescens", replacement = "Amorpha canescens", percov$Species)
percov$Species<- gsub(pattern = "Poa Pratensis", replacement = "Poa pratensis", percov$Species)
percov$Species<- gsub(pattern = "Poa pratensis ", replacement = "Poa pratensis", percov$Species)
percov$Species<- gsub(pattern = "Tragopogon dubius (major)", replacement = "Tragopogon dubius", percov$Species)
percov$Monospecies<- gsub(pattern = "LupinusPerennis", replacement = "Lupinus perennis",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "BoutelouaGracilis", replacement = "Bouteloua gracilis",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "AsclepiasTuberosa", replacement = "Asclepias tuberosa",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "SchizachyriumScoparium", replacement = "Schizachyrium scoparium",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "BromusInermis", replacement = "Bromus inermis",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "AmorphaCanescens", replacement = "Amorpha canescens",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "AgropyronRepens", replacement = "Agropyron repens",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "LespedezaCapitata", replacement = "Lespedeza capitata",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "PetalostemumVillosum", replacement = "Petalostemum villosum",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "PoaPratensis", replacement = "Poa pratensis",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "SolidagoRigida", replacement = "Solidago rigida",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "AnemoneCylindrica", replacement = "Anemone cylindrica",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "KoeleriaCristata", replacement = "Koeleria cristata",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "AchilleaMillefolium", replacement = "Achillea millefolium",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "AndropogonGerardi", replacement = "Andropogon gerardi",percov$Monospecies)
percov$Monospecies<- gsub(pattern = "SorghastrumNutans", replacement = "Sorghastrum nutans",percov$Monospecies)

# get only 16 planted species
sp <- unique(percov$Monospecies)
sp <- sp[-1]
percov <- percov[percov$Species %in% sp,]
percov <- percov[-which(is.na(percov$Percent.cover)),] # get rid of plots with no data
percov <- percov[percov$Season == "August",] # only want August data
percov$present <- 0 # create a column for if the species was present or not
for (i in 1:nrow(percov)){
  if(percov$Percent.cover[i] > 0){
    percov$present[i] <- 1 # change to 1 if species present
  }
}

percov <- percov[,c(3,4,15,17)]
## THESE ROWS HAVE DISCREPENCIES - 2 are eCO2 so dont matter, 1 is ambient. 
## Putting all to having species present because plot 309 has Koleria every other year.
##* 18477, 18478
##* 17930, 17931
##* 21138, 21139
ps <- c(18477, 18478, 17930, 17931, 21138, 21139)
percov[ps,"present"] <- 1
percov <- unique(percov)
percov <- spread(percov, Species, present)
percov[is.na(percov)] <- 0
percov$pSR <- rowSums(percov[,3:18])

tbiomass$Date <- as.Date(tbiomass$Date,"%m/%d/%Y")
tbiomass$Month <- as.numeric(format(tbiomass$Date, format = "%m"))
tbiomass$Year <- as.numeric(format(tbiomass$Date, format = "%Y"))
tbiomass <- tbiomass[tbiomass$Month == 8,]

tbiomass$Species<- gsub(pattern = "Achillea millefolium ", replacement = "Achillea millefolium", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Bouteloua gracilis ", replacement = "Bouteloua gracilis", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Asclepias tuberosa ", replacement = "Asclepias tuberosa", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Schizachyrium scoparium ", replacement = "Schizachyrium scoparium", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Amorpha canescens ", replacement = "Amorpha canescens", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Bromus inermis ", replacement = "Bromus inermis", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Agropyron repens ", replacement = "Agropyron repens", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Lespedeza capitata ", replacement = "Lespedeza capitata", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Petalostemum villosum ", replacement = "Petalostemum villosum", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Poa pratensis ", replacement = "Poa pratensis", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "poa pratensis", replacement = "Poa pratensis", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Solidago rigida ", replacement = "Solidago rigida", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Koeleria cristata ", replacement = "Koeleria cristata", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Lupinus perennis ", replacement = "Lupinus perennis", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Andropogon gerardi ", replacement = "Andropogon gerardi", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Sorghastrum nutans ", replacement = "Sorghastrum nutans", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "Anemone cylindrica ", replacement = "Anemone cylindrica", tbiomass$Species)
tbiomass$Species<- gsub(pattern = "bromus inermis", replacement = "Bromus inermis", tbiomass$Species)
tbiomass$monospecies<- gsub(pattern = "AchilleaMillefolium", replacement = "Achillea millefolium", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "BoutelouaGracilis", replacement = "Bouteloua gracilis", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "AsclepiasTuberosa", replacement = "Asclepias tuberosa", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "SchizachyriumScoparium", replacement = "Schizachyrium scoparium", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "Amorpha  canescens", replacement = "Amorpha canescens", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "AmorphaCanescens", replacement = "Amorpha canescens", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "BromusInermis", replacement = "Bromus inermis", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "AgropyronRepens", replacement = "Agropyron repens", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "LespedezaCapitata", replacement = "Lespedeza capitata", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "PetalostemumVillosum", replacement = "Petalostemum villosum", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "PoaPratensis", replacement = "Poa pratensis", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "Solidago sigida", replacement = "Solidago rigida", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "SolidagoRigida", replacement = "Solidago rigida", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "KoeleriaCristata", replacement = "Koeleria cristata", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "LupinusPerennis", replacement = "Lupinus perennis", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "AndropogonGerardi", replacement = "Andropogon gerardi", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "SorghastrumNutans", replacement = "Sorghastrum nutans", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "AnemoneCylindrica", replacement = "Anemone cylindrica", tbiomass$monospecies)
tbiomass$monospecies<- gsub(pattern = "bromusInermis", replacement = "Bromus inermis", tbiomass$monospecies)
# Change Cenriched to Cenrich
tbiomass$CO2.Treatment <- gsub(pattern = "Cenriched", replacement = "Cenrich", tbiomass$CO2.Treatment)
tbiomass$CO2.Treatment <- gsub(pattern = "Cenrich ", replacement = "Cenrich", tbiomass$CO2.Treatment)

sp1 <- c(sp, 'Unsorted Biomass')

tbiomass <- tbiomass[tbiomass$Species %in% sp1, ]
plts <- unique(tbiomass[tbiomass$Temp.Treatment == "HTelv","Plot"])
plts1 <- unique(tbiomass[tbiomass$Water.Treatment == "H2Oneg", "Plot"])
plts2 <- unique(c(plts, plts1))
tbiomass <- tbiomass[-which(tbiomass$Plot %in% plts2),]

tbiomass <- tbiomass[,c(3,4,5,6,7,14,15,17)]
names(tbiomass) <- c("Plot", "Ring", "CO2", "N", "SR", "Species", "Biomass", "Year")
tbiomass <- unique(tbiomass)

tbiomass <- spread(tbiomass, Species, Biomass)
tbiomass[is.na(tbiomass)] <- 0
tbiomass$TotalBiomass <- rowSums(tbiomass[,c(7:23)])
tbiomass <- tbiomass[,c(1:6,24)]
percov <- percov[,c(1,2,19)]

bio_mat <- merge(tbiomass, percov, by = c("Year", "Plot"))


findat <- merge(bio_mat, ph, by = c("Year", "Plot"))
findat$ExpYear <- findat$Year - 1997
findat$l.year <- log(findat$ExpYear)
findat$l.SR <- log(findat$SR)
findat$l.biomass <- log(findat$TotalBiomass)
findat$l.RR <- log(findat$pSR+0.01)
findat$Trt <- paste(findat$N, findat$CO2, sep = ".")

dat <- findat[-which(is.na(findat$pH)),]
dat <- dat[dat$Trt %in% c("Namb.Camb", "Nenrich.Camb"), ]
dat <- dat[dat$TotalBiomass > 0,]

for (i in 1:nrow(dat)){
  if (dat$SR[i] == 1){
    dat$pSR[i] = 1
  }
}
write.csv(dat, here("data", "paper_data.csv"))

