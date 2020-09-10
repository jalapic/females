### Comparison of individual behaviors

# Get matrices of fighting, chasing, mounting for each cohort----
mats <- lapply(df.l, behav_matrices)

# makes lists of each type of behavioral matrix
mats.fi <- sapply(mats,`[`,1)
mats.ch <- sapply(mats,`[`,2)
mats.mo <- sapply(mats,`[`,3)


## DC test of each matrix----
mats.fi.dcs <- lapply(mats.fi, dc_test)
mats.ch.dcs <- lapply(mats.ch, dc_test)
mats.mo.dcs <- lapply(mats.mo, dc_test)

data.frame(
cohort=LETTERS[1:8],
fight.dc = lapply(mats.fi.dcs, function(x) x$DC) %>% unlist,
fight.pval = lapply(mats.fi.dcs, function(x) x$`DC.pvalue`) %>% unlist,
chase.dc = lapply(mats.ch.dcs, function(x) x$DC) %>% unlist,
chase.pval = lapply(mats.ch.dcs, function(x) x$`DC.pvalue`) %>% unlist,
mount.dc = lapply(mats.mo.dcs, function(x) x$DC) %>% unlist,
mount.pval = lapply(mats.mo.dcs, function(x) x$`DC.pvalue`) %>% unlist
) -> dcs.df

#fighting
median(dcs.df$fight.dc)
quantile(dcs.df$fight.dc,probs=c(.25,.75))

#chasing
median(dcs.df$chase.dc)
quantile(dcs.df$chase.dc,probs=c(.25,.75))

#mounting
median(dcs.df$mount.dc)
quantile(dcs.df$mount.dc,probs=c(.25,.75))

#long df
dcs.df[c(1,2,4,6)] %>%
  gather(behav,value,2:4) -> dcs.df.long 

dcs.df.long$behav <- factor(dcs.df.long$behav, levels=c("fight.dc", "chase.dc","mount.dc"))

#plot
pd <- ggplot(dcs.df.long,aes(behav,value,color=behav))+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Fight","Chase","Mount"))+
  labs(x="Behavior",
       y="Directional\nConsistency")+
  newggtheme

ggsave("img/dcplot.png", pd, width=5, height=6)

## Friedman test of DCs with post-hoc tests
friedman.test(as.matrix(dcs.df[c(2,4,6)]))

## Post-hoc test
PMCMR::posthoc.friedman.nemenyi.test(as.matrix(dcs.df[c(2,4,6)]))






### Do matrices of each behavior correlate ?----

# detach igraph
detach("package:igraph", unload=TRUE)
library(sna)

fich.qap <- mapply(QAPtest, mats.fi, mats.ch)
fimo.qap <- mapply(QAPtest, mats.fi, mats.mo)
chmo.qap <- mapply(QAPtest, mats.ch, mats.mo)

fich.qap
fimo.qap
chmo.qap

median(nth_element(unlist(fich.qap),2))
quantile(nth_element(unlist(fich.qap),2), c(.25,.75))

median(nth_element(unlist(fimo.qap),2))
quantile(nth_element(unlist(fimo.qap),2), c(.25,.75))

median(nth_element(unlist(chmo.qap),2))
quantile(nth_element(unlist(chmo.qap),2), c(.25,.75))


### Supplemental Figure - 3 matrices of each behavior....

m.isi # list of best orders.


pfi<-pch<-pmo<-vector('list',8)
for(i in 1:8){
  mm <- mats.fi[[i]][m.isi[[i]],m.isi[[i]]]
  rownames(mm)<-colnames(mm)<-1:12
  pfi[[i]] <- matrixplot(mm)+ ggtitle(paste(LETTERS[i], "Fighting"))  +
    ylab("Winner's Rank") +
    xlab("Loser's Rank")
}

for(i in 1:8){
  mm <- mats.ch[[i]][m.isi[[i]],m.isi[[i]]]
  rownames(mm)<-colnames(mm)<-1:12
  pch[[i]] <- matrixplot(mm) + ggtitle(paste(LETTERS[i], "Chasing"))  +
    ylab("Winner's Rank") +
    xlab("Loser's Rank")
}

for(i in 1:8){
  mm <- mats.mo[[i]][m.isi[[i]],m.isi[[i]]]
  rownames(mm)<-colnames(mm)<-1:12
  pmo[[i]] <- matrixplot(mm) + ggtitle(paste(LETTERS[i], "Mounting"))  +
    ylab("Winner's Rank") +
    xlab("Loser's Rank")
}

px=grid.arrange(pfi[[1]],pfi[[2]],pfi[[3]],pfi[[4]],pfi[[5]],pfi[[6]],pfi[[7]],pfi[[8]],
                pch[[1]],pch[[2]],pch[[3]],pch[[4]],pch[[5]],pch[[6]],pch[[7]],pch[[8]],
                pmo[[1]],pmo[[2]],pmo[[3]],pmo[[4]],pmo[[5]],pmo[[6]],pmo[[7]],pmo[[8]],
                nrow=3)

ggsave("img/matricesbehavs.png", px, width=25,height=9)





### Gini-Coefficient of Inequities ----

# Gini-coefficient of giving/receiving fighting, chasing, mounting

gi.fi.out <- lapply(mats.fi, function(x) ineq::Gini(rowSums(x))) %>% unlist
gi.ch.out <- lapply(mats.ch, function(x) ineq::Gini(rowSums(x))) %>% unlist
gi.mo.out <- lapply(mats.mo, function(x) ineq::Gini(rowSums(x))) %>% unlist

gi.fi.in <- lapply(mats.fi, function(x) ineq::Gini(colSums(x))) %>% unlist
gi.ch.in <- lapply(mats.ch, function(x) ineq::Gini(colSums(x))) %>% unlist
gi.mo.in <- lapply(mats.mo, function(x) ineq::Gini(colSums(x))) %>% unlist


data.frame(
  cohort=LETTERS[1:8],
  direction =  rep(c("out", "in"),each=24),
  behavior =  rep(c("fighting","chasing","mounting"),each=8),
  value = c(gi.fi.out,gi.ch.out,gi.mo.out,gi.fi.in,gi.ch.in,gi.mo.in)
) -> df.gini

df.gini


# plot
df.gini$behavior<-factor(df.gini$behavior, levels = c("fighting",'chasing','mounting'))
df.gini$direction<-factor(df.gini$direction, levels = c("out",'in'))

pgini <- ggplot(df.gini,aes(behavior,value,color=behavior))+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Fight","Chase","Mount"))+
  labs(x="Behavior",
       y="Gini\nCoefficient")+
  newggtheme+
  facet_wrap(~direction)

ggsave("img/giniplot.png", pgini, width=9, height=6)



## Friedman test of Ginis with post-hoc tests

#make 3 col matrix for giving/receiving:
out.gini <- matrix(c(gi.fi.out,gi.ch.out,gi.mo.out),ncol=3)
colnames(out.gini)<-c("Fight","Chase","Mount")

in.gini <- matrix(c(gi.fi.in,gi.ch.in,gi.mo.in),ncol=3)
colnames(in.gini)<-c("Fight","Chase","Mount")

friedman.test(out.gini)
PMCMR::posthoc.friedman.nemenyi.test(out.gini)## Post-hoc test

friedman.test(in.gini)
PMCMR::posthoc.friedman.nemenyi.test(in.gini)## Post-hoc test




# rank  and proportion of each behavior ----
mo.df<-vector('list',length=8)
for(i in 1:8){
data.frame(
 mounts_out = rowSums(mats.mo[[i]]),
 mounts_in = colSums(mats.mo[[i]]),
 rank = order(as.numeric(m.isi[[i]]))
) -> mo.df[[i]]
}

mo.df.l <- do.call('rbind',Map(cbind,mo.df,cohort=LETTERS[1:8]))
mo.df.l <- mo.df.l %>% gather(key,value,1:2)

mo.df.l$key <- factor(mo.df.l$key, levels = c("mounts_out", "mounts_in"))

pmo <- ggplot(mo.df.l,aes(factor(rank),value))+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  theme(legend.position = "none")+
scale_x_discrete(labels=1:12)+
  labs(x="Rank",
       y="Total Mounts")+
  newggtheme+
  facet_wrap(~key)

pmo

ggsave("img/mountplot.png", pmo, width=15, height=5)



# rank and proportion of each behavior ----

rbind(
reshape2::melt(mats.fi) %>% mutate(behav = "fighting"),
reshape2::melt(mats.ch) %>% mutate(behav = "chasing"),
reshape2::melt(mats.mo) %>% mutate(behav = "mounting")
) %>% 
  mutate(cohort = substr(L1,1,1)) -> behavsdf

colnames(behavsdf)[1:2]<-c("Actor","Recipient")


behavsdf$behav<-factor(behavsdf$behav,levels=c("fighting","chasing","mounting"))


head(behavsdf)

behavsdf %>%
  group_by(Actor,cohort,behav) %>%
  summarize(total = sum(value)) %>%
  mutate(prop = total/sum(total)) -> behavdf.actor

behavsdf %>%
  group_by(Recipient,cohort,behav) %>%
  summarize(total = sum(value)) %>%
  mutate(prop = total/sum(total)) -> behavdf.recipient


## add in ranks
rankdf.recipient<-rankdf.actor<-rankdf
colnames(rankdf.actor)<-c("Actor","cohort","rank")
colnames(rankdf.recipient)<-c("Recipient","cohort","rank")
behavdf.actor %>% full_join(rankdf.actor) -> behavdf.actor
behavdf.recipient %>% full_join(rankdf.recipient) -> behavdf.recipient

ggplot(behavdf.actor,aes(factor(rank),prop,color=behav))+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  theme(legend.position = "none")+
  labs(x="Rank",
       y="Proportion")+
  newggtheme+
  facet_wrap(~behav, scales = "free_y") -> p.prop.actor

ggplot(behavdf.recipient,aes(factor(rank),prop,color=behav))+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  theme(legend.position = "none")+
  labs(x="Rank",
       y="Proportion")+
  newggtheme+
  facet_wrap(~behav, scales = "free_y") -> p.prop.recipient



### Plot of proportion of each behavior by rank
head(behavdf.actor)

behavdf.actor$behav<-factor(behavdf.actor$behav, levels=c("mounting","chasing","fighting"))
behavdf.recipient$behav<-factor(behavdf.recipient$behav, levels=c("mounting","chasing","fighting"))

behavdf.actor %>% group_by(behav,rank)%>%
  summarize(meanprop = mean(prop)) %>%
  ggplot(aes(x=rank,y=meanprop,fill=behav))+geom_col()+
  ylab("Proportion of \nAgonistic Interactions")+
  xlab("Rank")+
  scale_x_continuous(breaks=1:12)+
  newggtheme+
  theme(legend.position = "right")+
  scale_fill_manual(values=c("palegreen3", "gold", "darkorchid3")) +
  ggtitle("Attacks Given")-> pwinprop

behavdf.recipient %>% group_by(behav,rank)%>%
  summarize(meanprop = mean(prop)) %>%
  ggplot(aes(x=rank,y=meanprop,fill=behav))+geom_col()+
  ylab("Proportion of \nAgonistic Interactions")+
  xlab("Rank")+
  scale_x_continuous(breaks=1:12)+
  newggtheme+
  theme(legend.position = "right") +
  scale_fill_manual(values=c("palegreen3", "gold", "darkorchid3"))+
  ggtitle("Attacks Recieved")-> ploseprop

library(gridExtra)
pprops<-grid.arrange(pwinprop,ploseprop,nrow=1)
ggsave("img/behavprops.png", pprops,width=20,height=10)


## write csvs
write.csv(behavdf.actor,"data/proportion_given.csv",row.names = F)
write.csv(behavdf.recipient,"data/proportion_received.csv",row.names = F)

