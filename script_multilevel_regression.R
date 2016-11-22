library(tidyverse)

# Multi-Level DATA  ----------------------------------------------------
indiv.data <- read_csv("bh1996.csv") #this is individual data
# Grp = group level, hrs = hours, wbeing = wellbeing, and lead= leadership scores

indiv.grouped <- group_by(indiv.data, GRP) #group by GRP variable; gets group-level data
group.data <- indiv.grouped %>% summarise(HRS.GRP=mean(HRS, na.rm=TRUE), N.GRP=n())
## na.rm = TRUE a.k.a. if there are missing values (na), do i remove it (rm), yes = TRUE 
## N.GRP= n(), gives you the number of values within that group 
## should you run correlational analysis at this point, you would have correlations at a group level 

multilevel.data <- full_join(indiv.data, group.data, by="GRP") 
# View(multilevel.data) shows some columns are individual, others are at group level



# Multi-Level ANALYSIS --------------------------------------------------
library(nlme)

intercept.model.ignoring.groups <- gls(WBEING ~1, data=multilevel.data)
# one overall mean for the groups  (grand mean)

intercept.model.with.groups <- lme(WBEING~1, random=~1|GRP, data=multilevel.data) 
# give each group its own mean. you will still have a grand mean for the groups and there will be sep means for each group

anova(intercept.model.ignoring.groups,intercept.model.with.groups)
# testing if they are different.. 
# results: no CI so basing on p-value, it is significant so there is sig difference 
# results: AIC, BIC, logLik since the with-groups model approaches 0 compared to other model and pvalue is sig then...
# interpret: it means if you're looking at WBEING as DV, with-groups model is better 
# but if it wasn't significant, DV you're looking at isn't related to the groups.. so no need for multi-level analysis 

VarCorr(intercept.model.with.groups)
#intercept: variance=.0358, stdDev=.1892
#residual: variance=.7894, stdDev=.8885
#effect/ (effect+error) where effect = intercept-variance; error = residual-variance 
#result = .0433 
#interpret: 4% of the DV accounted for by group inclusion 
#--------------------------------------------------------------------------------------



#FIXED vs. RANDOM effects --------------------------------------------------------------

Model.1 <- lme(WBEING ~ HRS+HRS.GRP, random=~1|GRP, data=multilevel.data)

#fixed effects
summary(Model.1)
#HRS + HRS.GRP = ....

#random effects 
