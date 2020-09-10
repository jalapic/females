
### Hormone Analysis
library(tidyverse)

# Import data
horm <- readr::read_csv('raw_data/hormones.csv')

horm %>% separate(Mouse, c("cohortx","Mouse.ID"), "-") -> horm
horm$Mouse.ID  <- as.numeric(horm$Mouse.ID)
horm$cohortx <- LETTERS[as.numeric(horm$cohortx)-87+1]
head(horm)



# ISI ranks

rankdf <- do.call('rbind',Map(cbind, lapply(m.isi,cbind), cohort=LETTERS[1:8]))
colnames(rankdf)<-c("Mouse.ID","cohortx")
rankdf <- as.data.frame(rankdf)
rankdf$rank <- 1:12
rankdf$Mouse.ID <- as.numeric(as.character(rankdf$Mouse.ID))

head(rankdf)


# David's Scores 

dsdf <- do.call('rbind',Map(cbind, lapply(m.d,cbind), cohort=LETTERS[1:8]))
dsdf <- cbind(dsdf, rownames(dsdf))
dsdf <- as.data.frame(dsdf)
colnames(dsdf)<-c("ds","cohortx","Mouse.ID")
dsdf$Mouse.ID <- as.numeric(as.character(dsdf$Mouse.ID))

horm %>% left_join(rankdf) %>%  left_join(dsdf) %>% as.data.frame() -> hormdf


table(hormdf$rank)


#Glickoscores
gldf <- do.call('rbind',Map(cbind, lapply(df.glickos, function(x)  x$ratings[,1]), cohort=LETTERS[1:8]))
gldf <- as.data.frame(gldf)
colnames(gldf)<-c("Mouse.ID","cohortx")
gldf$glrank <-1:12
gldf$Mouse.ID <- as.numeric(as.character(gldf$Mouse.ID))

hormdf %>% left_join(gldf)  -> hormdf



#Estrus state

#es from estrus carpentry file
#day 15 estrus col
#day 15 / day14 if no day15.

es15 <- es %>% filter(day==15) %>% select(cohortx,State,Mouse.ID) 
colnames(es15)[2]<-"State15"

hormdf %>% left_join(es15) -> hormdf

es %>% filter(day>=14) %>% select(cohortx,State,day,Mouse.ID) %>% group_by(cohortx,Mouse.ID) %>% filter(row_number()==max(row_number())) ->es14

colnames(es14)[2]<-"State1415"
es14$day<-NULL

hormdf %>% left_join(es14) -> hormdf


## Add 'status' category
hormdf$status <- ifelse(hormdf$glrank>7, "sub","dom")


# Hormones Plot
pcort <- ggplot(hormdf,aes(status,CORT,color=status,fill=status))+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  scale_color_manual(values = c("purple4", "orange"))+
  scale_fill_manual(values = c("purple4", "orange"))+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Dominant","Subordinate"))+
  labs(x="Social Status",
       y="Corticosterone Level")+
  newggtheme

pest <- ggplot(hormdf,aes(status,E,color=status,fill=status))+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  scale_color_manual(values = c("purple4", "orange"))+
  scale_fill_manual(values = c("purple4", "orange"))+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Dominant","Subordinate"))+
  labs(x="Social Status",
       y="Estradiol Level")+
  newggtheme

library(gridExtra)
phorm <-grid.arrange(pcort,pest,nrow=1)

ggsave("img/hormones.png", phorm, width=9, height=6)



## Gene Expression Data.
ge <- readr::read_csv("raw_data/geneexpr.csv")
head(ge)

ge %>% separate(Mouse, c("cohort","Mouse.ID"),"-") %>%
  mutate(cohortx = LETTERS[as.numeric(cohort)-87+1]) %>%
  select(-cohort) -> ge

ge$Mouse.ID <- as.numeric(as.character(ge$Mouse.ID))


## Outlier detection
ge[,2:12] <- apply(ge[,2:12],2, outlierdetection)


## Merge
hormdf %>% left_join(ge) -> hormdf 


# Write CSV
write.csv(hormdf, "data/hormones_genes.csv", row.names=F)
  



hgdf<-hormdf %>% 
  mutate(subjectid=paste(cohortx,Mouse.ID,sep="-")) %>% 
  select(-Mouse.ID) %>% select(-Cohort) %>% select(-State15)




# mod.domsub.cort<-brm(CORT~status+(1|cohortx),
#                      data=hgdf,
#                      family='gaussian',
#                      control = list(adapt_delta=0.95),
#                      chains = 4,iter = 10000,warmup = 4000, thin = 1)
# mod.domsub.E<-brm(E~status+(1|cohortx),
#                      data=hgdf,
#                      family='gaussian',
#                      control = list(adapt_delta=0.95),
#                      chains = 4,iter = 10000,warmup = 4000, thin = 1)
# 
# saveRDS(mod.domsub.cort,"results/brms_results/mod.domsub.cort.RDS")
# saveRDS(mod.domsub.E,"results/brms_results/mod.domsub.E.RDS")


mod.domsub.cort<-readRDS("results/brms_results/mod.domsub.cort.RDS")
mod.domsub.E<-readRDS("results/brms_results/mod.domsub.E.RDS")

# mod.domsub.cort_state<-brm(CORT~status+State1415+(1|cohortx),
#                      data=hgdf,
#                      family='gaussian',
#                      control = list(adapt_delta=0.95),
#                      chains = 4,iter = 10000,warmup = 4000, thin = 1)
# mod.domsub.E_state<-brm(E~status+State1415+(1|cohortx),
#                      data=hgdf,
#                      family='gaussian',
#                      control = list(adapt_delta=0.95),
#                      chains = 4,iter = 10000,warmup = 4000, thin = 1)
# 
# mod.domsub.E_state2<-brm(E~State1415+(1|cohortx),
#                      data=hgdf,
#                      family='gaussian',
#                      control = list(adapt_delta=0.95),
#                      chains = 4,iter = 10000,warmup = 4000, thin = 1)
# 
# 
# saveRDS(mod.domsub.cort_state,"results/brms_results/mod.domsub.cort_state.RDS")
# saveRDS(mod.domsub.E_state,"results/brms_results/mod.domsub.E_state.RDS")
# saveRDS(mod.domsub.E_state,"results/brms_results/mod.domsub.E_state2.RDS")

mod.domsub.cort_state<-readRDS("results/brms_results/mod.domsub.cort_state.RDS")
mod.domsub.E_state<-readRDS("results/brms_results/mod.domsub.E_state.RDS")
mod.domsub.E_state2<-readRDS("results/brms_results/mod.domsub.E_state2.RDS")

cort_weights<-model_weights(mod.domsub.cort,mod.domsub.cort_state,weights = "loo")
estradiol_weights<-model_weights(mod.domsub.E,mod.domsub.E_state,mod.domsub.E_state2, weights = "loo")

cort_weights

estradiol_weights

## to get comparison between each pair of estrus state
coefs <- as.matrix(mod.domsub.E_state2)[, 1:4]
newdata <- data.frame(State1415 = levels(as.factor(hgdf$State1415)))
tuk.mat <- multcomp::contrMat(n = table(newdata$State1415), type = "Tukey")
Xmat <- model.matrix(~State1415, data = newdata)
pairwise.mat <- tuk.mat %*% Xmat

my_posterior<-  coefs %*% t(pairwise.mat) %>% as.data.frame() %>% gather(comps,value,1:4)

comps = broom::tidyMCMC(coefs %*% t(pairwise.mat), conf.int = TRUE, conf.method = "HPDinterval") %>% mutate(Sig=ifelse(conf.low*conf.high>0,"Sig","-")) %>% 
  mutate(poster=paste(round(estimate,1),
                      paste("[",round(conf.low,1),", ",round(conf.high,1),"]",sep="")))
comps


