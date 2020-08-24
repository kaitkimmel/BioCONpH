#### Code to analyze BioCON data #####
#### Kailtin Kimmel ####
#### June 23, 2020 ####

# load libraries
library(here)
library(ggplot2)
library(ggpubr)
library(nlme)
library(AICcmodavg)
library(dplyr)
library(tidyr)

# Color palettes for graphing
sr.pal <- c("#000000","#A2AF9F","#00A4D4","#5AA150")
funcolpal <- c("#AB1003", "#107502", "#0882A6", "#7D0CB1") #C3, C4, Forb, Legume
trt.pal <- c("#5c5b57","#03805C")

# set.seed
set.seed(4234)

# data
dat <- read.csv(here("data", "paper_data.csv"), row.names = 1)

########################################
#### BEF Model without pH included ####
#######################################

BEFmod <- lme(l.biomass ~ N*l.SR*l.year, random = ~1|Ring/Plot,
              correlation = corCompSymm(form = ~ 1|Ring/Plot), 
              method = "ML", data = dat)
BEFmod1 <- lme(l.biomass ~ N + l.SR + l.year + N:l.SR + N:l.year + l.SR:l.year,
               random = ~1|Ring/Plot, correlation = corCompSymm(form = ~ 1|Ring/Plot), 
               method = "ML", data = dat)
anova(BEFmod, BEFmod1) # BEF mod1 lower by ~ 2 AIC points

###################
#### Figure 2 ####
#################
df <- expand.grid(l.SR = log(c(1,4,9,16)), N = c("Namb", "Nenrich"), l.year = log(seq(2,20, by = 1)))
p <- predictSE.lme(BEFmod1, df)
df <- cbind(p, df)
df$ExpYear <- exp(df$l.year)
df$SR <- exp(df$l.SR)
df$efit <- exp(df$fit)
df$lwr <- exp(df$fit-df$se.fit)
df$upr <- exp(df$fit + df$se.fit)



bioavg <- dat[,c(5,6,10,13)]%>% group_by(ExpYear, N,SR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))
bioavg$eBio <- exp(bioavg$l.biomass)
grdat <- merge(bioavg, df, by = c("ExpYear", "SR", "N"))


png(here("Figures", "Fig2.png"), height = 1250, width = 1750, res = 300)
ggplot(aes(x = ExpYear, y = efit), data = grdat) +
  geom_point(aes(x = ExpYear, y = eBio, color = N), data = grdat) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = N), alpha = 0.5) +
  geom_line(aes(color = N), lwd = 1) + 
  ylim(c(0,700)) +
  scale_color_manual(values = trt.pal)+
  scale_fill_manual(values = trt.pal) + 
  facet_grid(~SR) + 
  theme_classic() +
  guides(fill = FALSE) + 
  labs(x = "Experiment Year", y = bquote("Aboveground Biomass" ~(g/m^2))) +
  theme(text = element_text(size = 14))
dev.off()


#########################################################
#### comparison of 1st three years and last 3 years ####
#######################################################
first_three <- as.data.frame(dat[dat$ExpYear %in% c(2,3,4), c(2,3,5:7,9)]%>% group_by(Plot, Ring,N,SR) %>% 
  summarise(biomass = mean(TotalBiomass)))
first_three$l.biomass <- log(first_three$biomass)
first_three$l.SR <- log(first_three$SR)

BEFmod2 <- lme(l.biomass ~ N + l.SR + N:l.SR, random = ~1|Ring/Plot, data = first_three) # significant effect of Nenrich

last_three <- as.data.frame(dat[dat$ExpYear %in% c(18,19,20), c(2,3,5:7,9)]%>% group_by(Plot, Ring,N,SR) %>% 
                               summarise(biomass = mean(TotalBiomass)))
last_three$l.biomass <- log(last_three$biomass)
last_three$l.SR <- log(last_three$SR)

BEFmod3 <- lme(l.biomass ~ N + l.SR + N:l.SR, random = ~1|Ring/Plot, data = last_three) # non-significant differnce between N threaments

###########################################
#### Model selection with pH included ####
#########################################

# Full model (4-way interaction) with planted SR
mod <- lme(l.biomass ~ N*l.SR*pH*ExpYear,
           random = ~1|Ring/Plot, correlation = corCompSymm(form = ~ 1|Ring/Plot), 
           method = "ML", data = dat)
mod1 <- lme(l.biomass ~ N*l.SR*pH*l.year,
            random = ~1|Ring/Plot, correlation = corCompSymm(form = ~ 1|Ring/Plot), 
            method = "ML", data = dat)
anova(mod, mod1) # log-year fits better
# Model with no 4-way interactions
mod2 <- lme(l.biomass ~ N + l.SR + pH +l.year + N:l.SR + N:pH + N:l.year +
              l.SR:pH + l.SR:l.year + pH:l.year + N:l.SR:pH + N:l.SR:l.year + 
              N:pH:l.year + l.SR:pH:l.year,
            random = ~1|Ring/Plot, correlation = corCompSymm(form = ~ 1|Ring/Plot), 
            method = "ML", data = dat)
anova(mod1, mod2) # model without 4-way interaction better by ~2 AIC points
#Take out 3-way interactions
mod3 <- lme(l.biomass ~ N + l.SR + pH + l.year + N:l.year + N:l.SR + N:pH + l.SR:pH + l.SR:l.year + pH:l.year , 
            random = ~1|Ring/Plot, correlation = corCompSymm(form = ~ 1|Ring/Plot), 
            method = "ML", data = dat)
anova(mod2, mod3, BEFmod1) #mod3 best fitting model

###################
#### Figure 4 ####
#################
df1 <- expand.grid(l.SR = log(seq(1,16, by = 0.1)), l.year = log(seq(2,20, by = 1)), N = c("Namb","Nenrich"), pH = seq(4,8, by = .5))
p1 <- predictSE.lme(mod3, df1)
df1 <- cbind(p1, df1)
df1$RR <- exp(df1$l.SR)
df1$ExpYear <- as.factor(exp(df1$l.year))
df1$efit <- exp(df1$fit)
df1$lwr <- exp(df1$fit - df1$se.fit)
df1$upr <- exp(df1$fit + df1$se.fit)
df1sum <- aggregate(df1[,c("efit", "lwr", "upr")], by = list(SR = df1$RR, N = df1$N), FUN = "mean")

gr1 <- ggplot(aes(x = SR, y = efit), data = df1sum[df1sum$N == "Namb",]) + 
  geom_point(aes(x = SR, y = TotalBiomass), data = dat[dat$N == "Namb",], alpha = 0.7)+
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "red",alpha = 0.5) + 
  geom_line(color = "red", lwd = 1) +
  ylim(0,1800) + 
  theme_classic() + 
  scale_x_continuous(breaks = c(1,4,9,16)) + 
  labs(x = "Planted Species Richness", y = bquote("Aboveground Biomass" ~(g/m^2))) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))


df3 <- expand.grid(pH = seq(4,8, by = 0.1), l.SR = log(1), l.year = log(seq(2,20, by = 2)), N = c("Namb", "Nenrich"))
p3 <- predictSE.lme(mod3, df3)
df3 <- cbind(p3, df3)
df3$SR<- as.factor(exp(df3$l.SR))
df3$ExpYear <- exp(df3$l.year)
df3$efit <- exp(df3$fit) 
df3$lwr <- exp(df3$fit - df3$se.fit)
df3$upr <- exp(df3$fit + df3$se.fit)
df3 <- aggregate(df3[,c("efit", "upr", "lwr")], by = list(pH = df3$pH, N = df3$N), FUN = "mean")

gr3 <- ggplot(aes(x = pH, y = efit), data = df3[df3$N == "Nenrich",]) + 
  geom_point(aes(x = pH, y = TotalBiomass), alpha = 0.7, data = dat[dat$SR == 1 & dat$N == "Namb",])+
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = "red") + 
  geom_line(color = "red", lwd = 1) +
  ylim(0,1800) +
  theme_classic() + 
  labs(x = "pH", y = bquote("Aboveground Biomass" ~(g/m^2)), color = "Planted Richness") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))
pdf(here("Figures", "Prod.pdf"), width = 10, height = 5.5, onefile = FALSE)
ggarrange(plotlist = list(gr1, gr3), ncol = 2, nrow = 1, common.legend = FALSE, labels = c("(a)", "(b)"))
dev.off()

png(here("Figures", "Fig4.png"), width = 2500, height = 1250, res = 300)
ggarrange(plotlist = list(gr1, gr3), ncol = 2, nrow = 1, common.legend = FALSE, labels = c("(a)", "(b)"))
dev.off()

#########################################
#### N impact  on observed richness ####
########################################
dat1 <- dat[dat$SR != 1,] # Not modeling richness in monocultures
md1 <- lme(pSR ~ N*SR*ExpYear, random = ~1|Ring/Plot, 
           correlation = corCompSymm(form = ~ 1|Ring/Plot), 
           method = "ML", data = dat1)
md1.l <- lme(pSR ~ N*SR*l.year, random = ~1|Ring/Plot, 
             correlation = corCompSymm(form = ~ 1|Ring/Plot), 
             method = "ML", data = dat1)
md1.m <- lme(pSR ~ N*l.SR*ExpYear, random = ~1|Ring/Plot, 
             correlation = corCompSymm(form = ~ 1|Ring/Plot), 
             method = "ML", data = dat1)
md1.n <- lme(pSR ~ N*l.SR*l.year, random = ~1|Ring/Plot, 
             correlation = corCompSymm(form = ~ 1|Ring/Plot), 
             method = "ML", data = dat1)
anova(md1, md1.l ,md1.m, md1.n) # log SR and log year best fit

#########################
#### N impact on pH ####
#######################
md2 <- lme(pH ~ N*SR*ExpYear, random = ~1|Ring/Plot, 
           correlation = corCompSymm(form = ~ 1|Ring/Plot), 
           method = "ML", data = dat)
md2.l <- lme(pH~ N*SR*l.year, random = ~1|Ring/Plot, 
             correlation = corCompSymm(form = ~ 1|Ring/Plot), 
             method = "ML", data = dat)
md2.m <- lme(pH~ N*l.SR*ExpYear, random = ~1|Ring/Plot, 
             correlation = corCompSymm(form = ~ 1|Ring/Plot), 
             method = "ML", data = dat)
md2.n <- lme(pH~ N*l.SR*l.year, random = ~1|Ring/Plot, 
             correlation = corCompSymm(form = ~ 1|Ring/Plot), 
             method = "ML", data = dat)
anova(md2, md2.l, md2.m, md2.n) # log SR and linear year best fit

##################
#### Figure 3 ####
##################
averages <- dat[,c(5,6,8,9,10)]%>% group_by(ExpYear, N,SR) %>% summarise_all(funs(mean(., na.rm = TRUE)))
newdf4 <- expand.grid(N = c("Namb", "Nenrich"), SR = c(1,4,9,16), ExpYear = seq(2,20, by = 1))
newdf4$l.SR <- log(newdf4$SR)
newdf4$l.year <- log(newdf4$ExpYear)
p4 <- predictSE.lme(md1.n, newdf4)
p4 <- cbind(p4, newdf4)
p5 <- predictSE.lme(md2.m, newdf4)
p5 <- cbind(p5, newdf4)

pdf(here("Figures", "FactorResponse.pdf"), height = 5, width = 7, onefile = FALSE)
g1 <- ggplot(aes(x = ExpYear, y = fit), data = p4[p4$SR != 1, ]) +
  geom_ribbon(aes(ymin = fit -se.fit, ymax = fit+se.fit, fill = N), alpha = 0.5) + 
  geom_line(aes(color = N), lwd = .5) + 
  geom_point(aes(x = ExpYear, y = pSR, color = N), data = averages) + 
  facet_grid(~SR) +
  scale_fill_manual(values = trt.pal) + 
  scale_color_manual(values= trt.pal) +
  scale_y_continuous(breaks = c(1,3,5,7,9,11,13))+
  labs (x = "Experiment Year", y = "Observed Richness") + 
  theme_classic() + 
  theme(text = element_text(size = 14))

g2 <- ggplot(aes(x = ExpYear, y = fit), data = p5) +
  geom_ribbon(aes(ymin = fit -se.fit, ymax = fit+se.fit, fill = N), alpha = 0.5) + 
  geom_line(aes(color = N), lwd = 1) + 
  geom_point(aes(x = ExpYear, y = pH, color = N), data = averages) +
  facet_grid(~SR) +
  scale_fill_manual(values = trt.pal) + 
  scale_color_manual(values= trt.pal) +
  labs (x = "Experiment Year", y = "pH") + 
  theme_classic() + 
  theme(text = element_text(size = 14))
ggarrange(plotlist = list(g1,g2), common.legend = TRUE, nrow = 2, ncol = 1, labels = c("A", "B"))
dev.off()

png(here("Figures", "Fig3.png"), height = 1750, width = 2500, res = 300)
ggarrange(plotlist = list(g1,g2), common.legend = TRUE, nrow = 2, ncol = 1, labels = c("(a)", "(b)"))
dev.off()

#################################
#### Predictions for Fig 5. ####
################################

# set p4 predictions of monoculture SR to 1
p4$fit[p4$SR == 1] <- 1
# Set up dataset based on predicted outputs from Fig 4 models
PlusN <- data.frame(SR = p4$SR[p4$N == "Nenrich"], ExpYear = p4$ExpYear[p4$N == "Nenrich"], 
                    pSR = p4$fit[p4$N == "Nenrich"], pH = p5$fit[p5$N == "Nenrich"])
PlusN$l.SR <- log(PlusN$pSR)
PlusN$N <- "Nenrich"
PlusN$Scenario <- "PlusN"
# Richness of ambient plots
SRloss <- PlusN
SRloss$pSR <- p4[p4$N == "Namb", "fit"]
SRloss$l.SR <- log(SRloss$pSR)
SRloss$Scenario <- "Richness"
# pH of ambient plots
pHChange <- PlusN
pHChange$pH <- p5[p5$N == "Namb", "fit"]
pHChange$Scenario <- "pH"
# Richness and pH of ambient plots
AllChange<- data.frame(SR = p4$SR[p4$N == "Namb"],  ExpYear = p4$ExpYear[p4$N == "Namb"], 
                       pSR = p4$fit[p4$N == "Namb"],  pH = p5$fit[p5$N == "Namb"])
AllChange$l.SR <- log(AllChange$pSR)
AllChange$N <- "Nenrich"
AllChange$Scenario <- "Richness and pH"
# Ambient 
Ambient <- AllChange
Ambient$N <- "Namb"
Ambient$Scenario <- "Ambient"
#combine datasets
df4 <- rbind(PlusN, SRloss, pHChange, AllChange, Ambient)
df4$l.year <- log(df4$ExpYear)
pred <- predictSE.lme(mod3, df4)
df4 <- cbind(pred, df4)
df4$efit <- exp(df4$fit) #back transform
Nadd <- df4[df4$Scenario == "PlusN",c(11,3,4)] # Pull out for supplemental figure
names(Nadd)[1] <-"Nfit"

avgdiff <- aggregate(df4$efit, by = list(SR = df4$SR, Scenario = df4$Scenario), FUN = "mean")
Amb <- avgdiff[avgdiff$Scenario == "Ambient",]
names(Amb)[3] <- "amb.fit"
PlusN <- merge(avgdiff, Amb, by = "SR")
PlusN <- PlusN[PlusN$Scenario.x == "PlusN",]
PlusN$diff <- PlusN$x - PlusN$amb.fit
PlusN <- PlusN[,c(1,2,3,6)]
names(PlusN) <- c("SR", "Source", "Biomass", "Diff")
PlusN$Source <- "Net N effect"
pH <- merge(avgdiff, PlusN, by = "SR")
pH <- pH[pH$Scenario == "pH",]
pH$diff <- pH$x - pH$Biomass
pH <- pH[,c(1,2,7)]
names(pH) <- c("SR", "Source", "Diff")
Richness <- merge(avgdiff, PlusN[,c(1:3)], by = "SR")
Richness <- Richness[Richness$Scenario == "Richness",]
Richness$diff <- Richness$x - Richness$Biomass
Richness<- Richness[,c(1,2,6)]
names(Richness) <- c("SR", "Source", "Diff")
PlusN <- PlusN[,-3]
dat2 <- rbind(PlusN, Richness, pH)
dat2$SR <- as.factor(dat2$SR)
dat2$Source <- factor(dat2$Source, levels = c("pH", "Richness", "Net N effect"))

png (here("Figures", "Fig5.png"), height = 2700, width = 3600, res = 600)
ggplot(aes(x = SR, y = Diff), data = dat2) + 
  geom_bar(aes(fill = Source, lty= Source), stat = "identity", color = "black") +
  scale_fill_manual(values = c("navy", "goldenrod", "seagreen")) +
  scale_linetype_manual(values= (c("dotted", "dotted", "solid"))) + 
  labs (x = "Planted Richness", y = bquote(atop("Biomass"~ (g/m^2) ~"above ambient", "or potential gains without constraints"))) + 
  theme_classic()+ 
  theme(text = element_text(size = 14))
dev.off()

df5 <- merge(df4, Nadd, by = c("SR", "ExpYear"))
df5$diff <- (df5$efit-df5$Nfit)
df5$Scenario[df5$Scenario == "Richness and pH"] <- "Total"
df5$Scenario <- factor(df5$Scenario, levels = c("pH", "Richness", "Total"))

pdf(here("Figures", "supp1.pdf"), height = 4, width = 5)
ggplot(aes(x= ExpYear, y = diff), data = df5[-which(is.na(df5$Scenario)),]) + 
  geom_line(aes(color = Scenario), lwd = 1) +
  facet_grid(~SR) + 
  geom_hline(yintercept =  0) + 
  scale_color_manual(values = c("navy", "goldenrod", "red")) +
  theme_classic() + 
  labs(x = "Experiment Year", y = "Absolute biomass change \nover N enriched conditions",
       color = "Factor Shifted") +
  theme(text = element_text(size = 14), legend.position = "top")
dev.off()
