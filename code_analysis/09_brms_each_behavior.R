library(brms)
library(viridis)
rate_day<-readr::read_csv("data/ratebehavior_day.csv")

rate_rank_Actor<-readr::read_csv("data/ratebehavior_rank.Actor_obs.csv") %>% 
  group_by(Behavior,rank.Actor,day,cohortx) %>% 
  summarize(total_n=sum(n),
            total_obstime=sum(obstime)) %>% 
  mutate(rate=(total_n/total_obstime)*60*60) %>% 
  mutate(subjectID=paste(cohortx,rank.Actor,sep=""))


rate_rank_Recipient<-readr::read_csv("data/ratebehavior_rank.Recipient_obs.csv") %>% 
  group_by(Behavior,rank.Recipient,day,cohortx) %>% 
  summarize(total_n=sum(n),
            total_obstime=sum(obstime)) %>% 
  mutate(rate=(total_n/total_obstime)*60*60) %>% 
  mutate(subjectID=paste(cohortx,rank.Recipient,sep=""))



### hourly occurrence rate of each behavior across group housing day
day_fight<-rate_day %>% filter(Behavior=="Fighting")
day_chase<-rate_day %>% filter(Behavior=="Chasing")
day_mount<-rate_day %>% filter(Behavior=="Mounting")


# mod.day.fight<-brm(propn~day+(1|cohort),day_fight,family='gaussian',control = list(adapt_delta=0.95))
# mod.day.chase<-brm(propn~day+(1|cohort),day_chase,family='gaussian',control = list(adapt_delta=0.95))
# mod.day.mount<-brm(propn~day+(1|cohort),day_mount,family='gaussian',control = list(adapt_delta=0.95))

# saveRDS(mod.day.fight,"results/brms_results/mod.day.fight")
# saveRDS(mod.day.chase,"results/brms_results/mod.day.chase")
# saveRDS(mod.day.mount,"results/brms_results/mod.day.mount")

mod.day.fight<-readRDS("results/brms_results/mod.day.fight")
mod.day.chase<-readRDS("results/brms_results/mod.day.chase")
mod.day.mount<-readRDS("results/brms_results/mod.day.mount")



### hourly occurrence rate of each behavior given/received across final social rank 
rank_Actor_fight<-rate_rank_Actor %>% filter(Behavior=="Fighting")
rank_Actor_chase<-rate_rank_Actor %>% filter(Behavior=="Chasing")
rank_Actor_mount<-rate_rank_Actor %>% filter(Behavior=="Mounting")

# mod.rank.fight_hg_given<-brm(bf(formula= rate ~ mo(rank.Actor)+(1|day)+(1|cohortx)+(1|subjectID),
#                           hu~mo(rank.Actor)+(1|day)+(1|cohortx)+(1|subjectID),
#                           nl=FALSE),
#                     data=rank_Actor_fight,
#                     family=hurdle_gamma(link = "log"),
#                     control = list(adapt_delta=0.95)
#                     )
# 
# 
# mod.rank.chase_hg_given<-brm(bf(formula= rate ~ mo(rank.Actor)+(1|day)+(1|cohortx)+(1|subjectID),
#                           hu~mo(rank.Actor)+(1|day)+(1|cohortx)+(1|subjectID),
#                           nl=FALSE),
#                     data=rank_Actor_chase,
#                     family=hurdle_gamma(link = "log"),
#                     control = list(adapt_delta=0.95)
#                     )
# 
# 
# mod.rank.mount_hg_given<-brm(bf(formula= rate ~ mo(rank.Actor)+(1|day)+(1|cohortx)+(1|subjectID),
#                           hu~mo(rank.Actor)+(1|day)+(1|cohortx)+(1|subjectID),
#                           nl=FALSE),
#                     data=rank_Actor_mount,
#                     family=hurdle_gamma(link = "log"),
#                     control = list(adapt_delta=0.95)
#                     )
# 
# saveRDS(mod.rank.fight_hg_given,"results/brms_results/mod.rank.fight_hg_given.RDS")
# saveRDS(mod.rank.chase_hg_given,"results/brms_results/mod.rank.chase_hg_given.RDS")
# saveRDS(mod.rank.mount_hg_given,"results/brms_results/mod.rank.mount_hg_given.RDS")

mod.rank.fight_hg_given<-readRDS("results/brms_results/mod.rank.fight_hg_given.RDS")
mod.rank.chase_hg_given<-readRDS("results/brms_results/mod.rank.chase_hg_given.RDS")
mod.rank.mount_hg_given<-readRDS("results/brms_results/mod.rank.mount_hg_given.RDS")



rank_Recipient_fight<-rate_rank_Recipient %>% filter(Behavior=="Fighting")
rank_Recipient_chase<-rate_rank_Recipient %>% filter(Behavior=="Chasing")
rank_Recipient_mount<-rate_rank_Recipient %>% filter(Behavior=="Mounting")

# mod.rank.fight_hg_received<-brm(bf(formula= rate ~ mo(rank.Recipient)+(1|day)+(1|cohortx)+(1|subjectID),
#                           hu~mo(rank.Recipient)+(1|day)+(1|cohortx)+(1|subjectID),
#                           nl=FALSE),
#                     data=rank_Recipient_fight,
#                     family=hurdle_gamma(link = "log"),
#                     control = list(adapt_delta=0.95)
#                     )
# 
# 
# mod.rank.chase_hg_received<-brm(bf(formula= rate ~ mo(rank.Recipient)+(1|day)+(1|cohortx)+(1|subjectID),
#                           hu~mo(rank.Recipient)+(1|day)+(1|cohortx)+(1|subjectID),
#                           nl=FALSE),
#                     data=rank_Recipient_chase,
#                     family=hurdle_gamma(link = "log"),
#                     control = list(adapt_delta=0.95)
#                     )
# 
# 
# mod.rank.mount_hg_received<-brm(bf(formula= rate ~ mo(rank.Recipient)+(1|day)+(1|cohortx)+(1|subjectID),
#                           hu~mo(rank.Recipient)+(1|day)+(1|cohortx)+(1|subjectID),
#                           nl=FALSE),
#                     data=rank_Recipient_mount,
#                     family=hurdle_gamma(link = "log"),
#                     control = list(adapt_delta=0.95)
#                     )
# 
# 
# saveRDS(mod.rank.fight_hg_received,"results/brms_results/mod.rank.fight_hg_received.RDS")
# saveRDS(mod.rank.chase_hg_received,"results/brms_results/mod.rank.chase_hg_received.RDS")
# saveRDS(mod.rank.mount_hg_received,"results/brms_results/mod.rank.mount_hg_received.RDS")

mod.rank.fight_hg_received<-readRDS("results/brms_results/mod.rank.fight_hg_received.RDS")
mod.rank.chase_hg_received<-readRDS("results/brms_results/mod.rank.chase_hg_received.RDS")
mod.rank.mount_hg_received<-readRDS("results/brms_results/mod.rank.mount_hg_received.RDS")



#### Summary figure - Marginal effect plot======================
dat_given_fight<-marginal_effects(mod.rank.fight_hg_given)$rank.Actor %>%
  mutate(xvar=rank.Actor,yvar=estimate__,lower=lower__,upper=upper__,se=se__) %>% 
  select(xvar,yvar,lower,upper,se) %>% 
  mutate(Behavior="Fight") %>% 
  as.data.frame()

dat_given_chase<-marginal_effects(mod.rank.chase_hg_given)$rank.Actor %>%
  mutate(xvar=rank.Actor,yvar=estimate__,lower=lower__,upper=upper__,se=se__) %>% 
  select(xvar,yvar,lower,upper,se) %>% 
  mutate(Behavior="Chase") %>% 
  as.data.frame()

dat_given_mount<-marginal_effects(mod.rank.mount_hg_given)$rank.Actor %>%
  mutate(xvar=rank.Actor,yvar=estimate__,lower=lower__,upper=upper__,se=se__) %>% 
  select(xvar,yvar,lower,upper,se) %>% 
  mutate(Behavior="Mount") %>% 
  as.data.frame()

dat_given<-rbind(dat_given_fight,dat_given_chase,dat_given_mount) %>% 
  mutate(Behavior=factor(Behavior,levels = c("Fight","Chase","Mount")))

dat_received_fight<-marginal_effects(mod.rank.fight_hg_received)$rank.Recipient %>%
  mutate(xvar=rank.Recipient,yvar=estimate__,lower=lower__,upper=upper__,se=se__) %>% 
  select(xvar,yvar,lower,upper,se) %>% 
  mutate(Behavior="Fight") %>% 
  as.data.frame()

dat_received_chase<-marginal_effects(mod.rank.chase_hg_received)$rank.Recipient %>%
  mutate(xvar=rank.Recipient,yvar=estimate__,lower=lower__,upper=upper__,se=se__) %>% 
  select(xvar,yvar,lower,upper,se) %>% 
  mutate(Behavior="Chase") %>% 
  as.data.frame()

dat_received_mount<-marginal_effects(mod.rank.mount_hg_received)$rank.Recipient %>%
  mutate(xvar=rank.Recipient,yvar=estimate__,lower=lower__,upper=upper__,se=se__) %>% 
  select(xvar,yvar,lower,upper,se) %>% 
  mutate(Behavior="Mount") %>% 
  as.data.frame()

dat_received<-rbind(dat_received_fight,dat_received_chase,dat_received_mount) %>% 
  mutate(Behavior=factor(Behavior,levels = c("Fight","Chase","Mount")))

dat_each_behav<-rbind(dat_given %>% mutate(direction="Given"),
                      dat_received %>% mutate(direction="Received"))


each_behav_rank<-ggplot(data=dat_each_behav ,aes(xvar,exp(yvar)))+
  geom_line(aes(color=Behavior,group=Behavior),size=1.5,alpha=0.5)+
  newggtheme_with_legend+
  xlab("Final Social Rank")+
  ylab("Hourly Rate of Each Behavior")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  #scale_y_continuous(breaks = c(0,0.5,1.0,1.5,2,2.5),limits = c(0,2.5))+
  geom_ribbon(aes(xvar,exp(yvar),ymax=exp(yvar+se),ymin=exp(yvar-se),group=Behavior,fill=Behavior),alpha=0.4)+
  scale_color_viridis(discrete=TRUE)+
  scale_fill_viridis(discrete=TRUE)+
  facet_wrap(~direction,scales = "free_y")

print(each_behav_rank)
