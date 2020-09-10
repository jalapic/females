### Start-End Analysis
library(tidyverse)

dfx.l # from 04_startends  - a list of clean time-ordered dataframes


# add day time hour columns and sort by time
dfx.lz <- lapply(dfx.l, get_ztime)

# split each dataframe in list into  observations
dfx.lz.obs <- lapply(dfx.lz, get_obs)

#check obs  times
lapply(dfx.lz.obs, function(x) lapply(x, function(u) max(u$ztime_secs)-min(u$ztime_secs))) %>% unlist() -> times
times[order(times)]

#add observation duration
dfx.lz.obs <- lapply(dfx.lz.obs, function(x) lapply(x, function(u) u %>% mutate(obstime = max(ztime_secs)-min(ztime_secs))))

# expand behavioral dataframes
dfx.lz.all <- lapply(dfx.lz.obs, function(x) lapply(x, behavdf_all))




### All  animals together across days ----
dfx.lz.alldays <- lapply(dfx.lz.all,get_perhour_eachday)

dfx.lz.alldays <- Map(cbind, lapply(dfx.lz.alldays,as.data.frame), cohort=LETTERS[1:8])

dfx.lz.alldays.df <- do.call('rbind',dfx.lz.alldays)


## plot
head(dfx.lz.alldays.df)

dfx.lz.alldays.df$Behavior<-factor(dfx.lz.alldays.df$Behavior,levels=c("Fighting","Chasing","Mounting"))

ggplot(dfx.lz.alldays.df,aes(factor(day),propn,color=Behavior))+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  theme(legend.position = "none")+
  labs(x="Day",
       y="Total Events per \n Hour of Observation")+
  newggtheme+
  ggtitle("Rate of Agonistic Behaviors Across Days")+
  facet_wrap(~Behavior) -> p.rates

ggsave("img/rates.png", p.rates,width=18,height=6)




## Datasets for Model Analysis:

# Summary dataset 1:-----
write.csv(dfx.lz.alldays.df, "data/ratebehavior_day.csv",row.names=F)



# Summary dataset 2:-----
# MODEL: include every observation period? ; random factors-  length of obs, etc.

# Make dataframe of observations

dfx.lz.all <-  lapply(dfx.lz.all, function(x) Map(cbind,x,obs=1:length(x))) #add obs number

do.call('rbind',
Map(cbind, lapply(dfx.lz.all, function(x) do.call('rbind', x)), cohortx=LETTERS[1:8])
) -> obs.df 

obs.df %>% filter(Actor!="Start",Actor!="End") -> obs.df

rankdf.actor <- rankdf
colnames(rankdf.actor)[1]<-"Actor"
colnames(rankdf.actor)[3]<-"rank.Actor"
rankdf.actor$Actor<-as.character(rankdf.actor$Actor)

rankdf.recipient <- rankdf
colnames(rankdf.recipient)[1]<-"Recipient"
colnames(rankdf.recipient)[3]<-"rank.Recipient"
rankdf.recipient$Recipient<-as.character(rankdf.recipient$Recipient)


obs.df %>% full_join(rankdf.actor) %>% full_join(rankdf.recipient) -> obs.df

obs.df <- obs.df %>% mutate(unique_obs = paste0(cohortx,obs)) 

write.csv(obs.df, "data/obsdf.csv",row.names=F)


head(obs.df)


#summarise (code by JC then slightly modified by WL)
aggr<-c("Fighting","Mounting","Chasing")

obs.df$rank.Actor<-factor(obs.df$rank.Actor)
obs.df$rank.Recipient<-factor(obs.df$rank.Recipient)
obs.df$day<-factor(obs.df$day)
obs.df$unique_obs<-factor(obs.df$unique_obs)
obs.df$cohortx<-factor(obs.df$cohortx)
obs.df$Behavior<-factor(obs.df$Behavior, levels=c("Fighting","Chasing","Mounting"))

obs.df %>% select(unique_obs,obs,day,obstime,cohortx) %>% unique -> obs.ids

obs.df %>%
  group_by(Behavior,rank.Actor,unique_obs) %>%
  tally() %>%
  complete(Behavior,rank.Actor,unique_obs, fill=list(n=0))  %>%
  arrange(unique_obs,Behavior,rank.Actor) %>%
  unique -> obs.df.Actor.summary

obs.df %>%
  group_by(Behavior,rank.Recipient,unique_obs) %>%
  tally() %>%
  complete(Behavior,rank.Recipient,unique_obs, fill=list(n=0))  %>%
  arrange(unique_obs,Behavior,rank.Recipient) %>%
  unique -> obs.df.Recipient.summary


obs.df.Actor.summary %>% full_join(obs.ids) %>% ungroup() %>% arrange( unique_obs,Behavior, cohortx,rank.Actor) %>%
mutate(proportion = (n/obstime)*60*60)  -> obs.df.Actor.summary

obs.df.Recipient.summary %>% full_join(obs.ids) %>% ungroup() %>% arrange( unique_obs,Behavior, cohortx,rank.Recipient) %>%
  mutate(proportion = (n/obstime)*60*60)  -> obs.df.Recipient.summary

write.csv(obs.df.Actor.summary, "data/ratebehavior_rank.Actor_obs.csv",row.names=F)
write.csv(obs.df.Recipient.summary, "data/ratebehavior_rank.Recipient_obs.csv",row.names=F)

# (WL) Glicko ranking and ratings for each individual upto each group housing day
glick_by_day<-lapply(df.l.day.split, 
                     function (x) lapply(x,
                                         function(y) expandrows(y[c(2,4,3)]) %>% 
                                           get_glickos)) #8*14 lists

temp<-list()
temp2<-list()
for(i in 1:8){
  for(j in 1:14){
    temp[[j]]<-glick_by_day[[i]][[j]]$ratings %>%
      mutate(glick_rank_by_day=row.names(.),
             cohortx=LETTERS[i],
             day=as.numeric(j),
             Mouse.ID=Player) %>% 
      select(cohortx,Mouse.ID,day,glick_rank_by_day,Rating)
  }
  temp2[[i]]<-rbindlist(temp)
}


glick_day_df<-rbindlist(temp2) %>% 
  as.data.frame() %>% 
  mutate(glick_rank_by_day=as.numeric(glick_rank_by_day))


# (WL) Add in estrus state of each day 
head(rankestrus)
str(glick_day_df)

glick_rank_by_day_estrus<-full_join(glick_day_df,rankestrus)

write.csv(glick_rank_by_day_estrus,"data/glick_rank_by_day_estrus.csv",row.names = FALSE)
