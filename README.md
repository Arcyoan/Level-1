# Level-1
## Q1, variance
hydro <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/hydrothermal.csv")
parallel <- subset(hydro, (flow_direction == "parallel"))
var(parallel$flow_rate)
perpendicular <- subset(hydro, (flow_direction == "perpendicular"))
var(perpendicular$flow_rate)
## Variance is greater in parallel. Variance of parallel = 12.23064. Variance of perpendicular is 6.423745
##Q2, over or under representation
tropics <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/foram.csv")
trop <- subset(tropics,lat >= -23 & lat <= +23)
length(trop$lat)
length(tropics$lat)
## Over-represented   data 50%>40%. Length of lat between -23 and 23 is 26. Total length of the lat is 42.
## Q3 differ signifantly as a function of sediment type, anov?

Arctic <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/lakemethane.csv")
Arctic_anova <- aov(methane_flux ~ sed_type, data = Arctic)
summary(Arctic_anova)
## Yes it differ significantly, Pr(>F) = 4.27e-08

##Q4 landslide frequency 
binom.test(5,10, 0.8) ## 1st slope
binom.test(1, 10, 0.8) ## 2nd slope
## yes it differs between two sets Probablity of success of 1st slope is 0.5, Propbablity of success of 2nd slope is 0.1

##Q5 Warmer ocean water cause significant greater rate of glacier melting?
green <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/greenland.csv")
melt_rate <- green$melt_rate
ocean_temp <- green$ocean_temp
ks.test(melt_rate, ocean_temp) 
## weak evidence therefore  Does not significantly greater rate of glacier melting. p-value = 0.1545

##Q6 Signifcant changes in crustal thickness in Himalayas in Pala to Neog periods. T-test?
Him1 <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/himalaya.csv")
pala <- Him1$La_Yb[Him1$time == "Paleogene"]
neo <- Him1$La_Yb[Him1$time == "Neogene"]
t.test(pala,neo)
## No there is not a significant change in crustal thickness in Himalayas. The p-value is 3.119e-11, so there is not a significant change in crustal thickness

##Q7 Does the size of pristine craters (modification state "p") differ significantly between type S (structureless floor) and type P (central peak) crater types?
## Calculate mean of P,S
venus <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/venuscrater.csv")
subset_modp <- subset(venus, modification.state == "p") 
subset_craterS <- subset(subset_modp, crater.type == "S")
subset_cratersP <- subset(subset_modp, crater.type == "P")
mean(subset_cratersP$diameter)
mean(subset_craterS$diameter)
23.9/6.596
## Yes, the size of pristine crater differs significantly between type S and type P. 
##The mean of S is 6.596 and the mean of P is 23.9. On average, a crater with type P is 3.6 times greater than crater type S

##Q8 Does the grain size distribution differ significantly between the known salmon spawning site and the alternate site?
## Not ks.test
salmon <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/salmon_river.csv")
spawning <- subset(salmon,site == "spawning")
hist(spawning$grain_size)
alternative <-subset(salmon, site == "alternate")
hist(alternative$grain_size)
ks.test(spawning1,alternative1)
