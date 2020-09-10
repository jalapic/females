### Estrus Data Carpentry

# import raw data
es <- read.csv("raw_data/estrus_87-94.csv",stringsAsFactors = F)
head(es)

# total estrus state values
unique(es$Estrus.State)
table(es$Estrus.State)

es[es$Estrus.State=="No smear/Not clear (elaborate in notes)",6]

# add  NA to those that are not clear
es$State <- ifelse(grepl("No", es$Estrus.State), NA, es$Estrus.State)
table(es$State)
head(es)
str(es)


# make date column
es$Date <- as.Date(es$Date.that.smears.were.taken, format="%m/%d/%Y")

# make cohort column
es$cohortx <-  LETTERS[es$Cohort-87+1]



## Add in missing data
esex <- read.csv("raw_data/estrus_oldextra.csv", stringsAsFactors = F)

# add  NA to those that are not clear
esex$State <- ifelse(grepl("No", esex$Estrus.State), NA, esex$Estrus.State)


# make date column
esex$Date <- as.Date(esex$Date.that.smears.were.taken, format="%m/%d/%Y")

# make cohort column
esex$cohortx <-  LETTERS[esex$Cohort-87+1]

# Fix some 'esex' issues:
esex <- esex[-5,]
esex[50,3]<-2
esex[61:64,3]<-5:8
esex[66:69,3]<-1:4
esex<-esex[-65,]
esex[67,2]<-88
esex[67,9]<-"B"



# bind together
esex <- esex[,colnames(es)] #put into same order
es<-rbind(es,esex)
#

# make day column
es %>% group_by(cohortx) %>%
  mutate(day = lubridate::yday(Date) - min(lubridate::yday(Date)) + 1) -> es


#Fix Errors
es[508,10] <- 14     # which(es$Timestamp=="7/12/2017 13:55")
es[830,10] <- 6     # which(es$Timestamp=="7/18/2017 12:20")
es[543,10] <- 13     # which(es$Timestamp=="7/12/2017 14:37")- first one

#Check
table(es$day,es$cohortx)


## 
head(es)
es1 <- es %>% filter(day<15)

### Quick visualization

library(tidyverse)
ggplot(es1, aes(x=day,y=Mouse.ID,fill=State))+facet_wrap(~cohortx, nrow=2)+
  geom_tile(colour="black", size=.25)+
  scale_fill_manual(values=c("red","blue","mistyrose","dodgerblue","ghostwhite"))+
  newggtheme+
  scale_y_continuous(breaks=1:12)+
  theme(legend.position = "top") +
  scale_x_continuous(breaks=1:14)->pestrus

ggsave("img/estrus.png",pestrus,height=10,width=20)


### Add in rank ?
rankdf <- do.call('rbind',Map(cbind, lapply(m.isi,cbind), cohort=LETTERS[1:8]))
colnames(rankdf)<-c("Mouse.ID","cohortx")
rankdf <- as.data.frame(rankdf)
rankdf$rank <- 1:12

rankdf$Mouse.ID <- as.numeric(as.character(rankdf$Mouse.ID))

es %>% full_join(rankdf) -> es.rank

es.rank$rank <- as.numeric(as.character(es.rank$rank))

ggplot(es.rank %>% filter(day<15), aes(x=day,y=rank,fill=State))+facet_wrap(~cohortx, nrow=2)+
  geom_tile(colour="black", size=.25)+
  scale_fill_manual(values=c("red","blue","pink","dodgerblue","ghostwhite"))+
  newggtheme+
  scale_y_reverse(breaks=1:12)+
  theme(legend.position = "top") +
  scale_x_continuous(breaks=1:14) ->pestrus.rank

ggsave("img/estrusrank.png",pestrus.rank,height=10,width=20)



### Proportion of time in each state by rank
head(es.rank)

es.rank %>%
  filter(State!="NA") %>%
  group_by(cohortx,rank,State) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  complete(cohortx,rank,State, fill = list(total = 0)) %>%
  group_by(cohortx,rank) %>%
  mutate(proportion = total/sum(total))  -> es.rank.sum

es.rank.sum$State<-factor(es.rank.sum$State, levels=c("Diestrus","Metestrus","Proestrus","Estrus"))

ggplot(es.rank.sum, aes(x=rank,y=proportion,fill=State))+
  geom_col()+
  facet_wrap(~cohortx, nrow=2) +
  newggtheme+
  theme(legend.position = 'top')+
  ylab("Proportion of days")+
  xlab("Rank")+
  scale_x_continuous(breaks=1:12)+
  scale_fill_manual(values=c("red","pink","dodgerblue","blue")) -> p.esprop

ggsave("img/estrusprop.png",p.esprop,height=10,width=20)

#write.csv(es.rank.sum,"data/estrusprops.csv",row.names=F)

es.rank.sum %>%
  group_by(State) %>%
  summarise(median = median(proportion), 
            lqr=quantile(proportion,.25),
            uqr=quantile(proportion,.75))



#Simplified dataframe
rankestrus <- es.rank %>% select(cohortx,Mouse.ID,rank,day,State)
write.csv(rankestrus, "data/rankestrus.csv", row.names=F)



