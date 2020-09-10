library(brms)
### Does the rank affect how much likely a mouse is in a certain estrus state?
# Multinomial model with monotonic effect function in the model 
rankestrus<-readr::read_csv("data/rankestrus.csv")
levels(as.factor(rankestrus$State))
rankestrus_a<-rankestrus %>% mutate(State=factor(State))
rankestrus_b<-rankestrus %>% mutate(State=factor(State,level=c("Estrus","Metestrus","Proestrus","Diestrus")))
rankestrus_c<-rankestrus %>% mutate(State=factor(State,level=c("Metestrus","Proestrus","Diestrus","Estrus")))
rankestrus_d<-rankestrus %>% mutate(State=factor(State,level=c("Proestrus","Diestrus","Estrus","Metestrus")))

# multinom.mod2_a<-brm(State~mo(rank)+(1|cohortx)+(1|subjectID),data=rankestrus_a,
#                    family = categorical(link = "logit"),control = list(adapt_delta=0.95))
# 
# multinom.mod2_b<-brm(State~mo(rank)+(1|cohortx)+(1|subjectID),data=rankestrus_b,
#                    family = categorical(link = "logit"),control = list(adapt_delta=0.95))
# 
# multinom.mod2_c<-brm(State~mo(rank)+(1|cohortx)+(1|subjectID),data=rankestrus_c,
#                    family = categorical(link = "logit"),control = list(adapt_delta=0.95))
# 
# multinom.mod2_d<-brm(State~mo(rank)+(1|cohortx)+(1|subjectID),data=rankestrus_d,
#                    family = categorical(link = "logit"),control = list(adapt_delta=0.95))

# saveRDS(multinom.mod_a,"results/brms_results/multinom.mod_a.RDS")
# saveRDS(multinom.mod_b,"results/brms_results/multinom.mod_b.RDS")
# saveRDS(multinom.mod_c,"results/brms_results/multinom.mod_c.RDS")
# saveRDS(multinom.mod_d,"results/brms_results/multinom.mod_d.RDS")

multinom.mod_a<-readRDS("results/brms_results/multinom.mod_a.RDS")
multinom.mod_b<-readRDS("results/brms_results/multinom.mod_b.RDS")
multinom.mod_c<-readRDS("results/brms_results/multinom.mod_c.RDS")
multinom.mod_d<-readRDS("results/brms_results/multinom.mod_d.RDS")

summary(multinom.mod_a)
summary(multinom.mod_b)
summary(multinom.mod_c)
summary(multinom.mod_d)

### 
totalrate_rank_Actor<-readr::read_csv("data/ratebehavior_rank.Actor_obs.csv") %>% 
  group_by(rank.Actor,day,cohortx) %>% 
  summarize(total_n=sum(n),
            total_obstime=sum(obstime)) %>% 
  mutate(rate=(total_n/total_obstime)*60*60)

totalrate_rank_Recipient<-readr::read_csv("data/ratebehavior_rank.Recipient_obs.csv") %>% 
  group_by(rank.Recipient,day,cohortx) %>% 
  summarize(total_n=sum(n),
            total_obstime=sum(obstime)) %>% 
  mutate(rate=(total_n/total_obstime)*60*60)


rate_rank_Actor<-readr::read_csv("data/ratebehavior_rank.Actor_obs.csv") %>% 
  group_by(rank.Actor,day,cohortx,Behavior) %>% 
  summarize(total_n=sum(n),
            total_obstime=sum(obstime)) %>% 
  mutate(rate=(total_n/total_obstime)*60*60)

rate_rank_Recipient<-readr::read_csv("data/ratebehavior_rank.Recipient_obs.csv") %>% 
  group_by(rank.Recipient,day,cohortx,Behavior) %>% 
  summarize(total_n=sum(n),
            total_obstime=sum(obstime)) %>% 
  mutate(rate=(total_n/total_obstime)*60*60)


glick_rank_by_day_estrus<-readr::read_csv("data/glick_rank_by_day_estrus.csv")

glick_rank_by_day_estrus<-glick_rank_by_day_estrus %>% 
  mutate(subjectID=paste(cohortx,Mouse.ID,sep="-"))

totalrate_Actor_estrus<-full_join(totalrate_rank_Actor,
                                  glick_rank_by_day_estrus %>% 
                                    mutate(rank.Actor=rank) %>% 
                                    select(-rank))

totalrate_Recipient_estrus<-full_join(totalrate_rank_Recipient,
                                      glick_rank_by_day_estrus %>% 
                                        mutate(rank.Recipient=rank) %>% 
                                        select(-rank))

rate_Actor_estrus<-left_join(rate_rank_Actor,
                             glick_rank_by_day_estrus %>% 
                               mutate(rank.Actor=rank) %>% 
                               select(-rank))

rate_Recipient_estrus<-full_join(rate_rank_Recipient,
                                 glick_rank_by_day_estrus %>% 
                                   mutate(rank.Recipient=rank) %>% 
                                   select(-rank))


# totalrate_Actor_estrus<-totalrate_Actor_estrus %>% 
#   filter(!is.na(State)) %>% 
#   filter(!is.na(rank.Actor)) %>% 
#   filter(!is.na(glick_rank_by_day))
# totalrate_Recipient_estrus<-totalrate_Recipient_estrus %>% 
#   filter(!is.na(State)) %>% 
#   filter(!is.na(rank.Recipient)) %>% 
#   filter(!is.na(glick_rank_by_day))


# mod.Actor_estrus_hg1x<-brm(bf(formula= rate ~ State+(1|day)+(1|cohortx)+(1|subjectID),
#                              hu~mo(glick_rank_by_day)+(1|day)+(1|cohortx)+(1|subjectID),
#                              nl=FALSE),
#                           data=totalrate_Actor_estrus,
#                           family=hurdle_gamma(link = "log"),
#                           control = list(adapt_delta=0.95)
# )
#
# mod.Recipient_estrus_hg1x<-brm(bf(formula= rate ~ State+(1|day)+(1|cohortx)+(1|subjectID),
#                                  hu~mo(glick_rank_by_day)+(1|day)+(1|cohortx)+(1|subjectID),
#                                  nl=FALSE),
#                               data=totalrate_Recipient_estrus,
#                               family=hurdle_gamma(link = "log"),
#                               control = list(adapt_delta=0.95)
# )
# 
# saveRDS(mod.Actor_estrus_hg1x,"results/brms_results/mod.Actor_estrus_hg1x.RDS")
# saveRDS(mod.Recipient_estrus_hg1x,"results/brms_results/mod.Recipient_estrus_hg1x.RDS")
mod.Actor_estrus_hg1x <- readRDS("results/brms_results/mod.Actor_estrus_hg1x.RDS")
mod.Recipient_estrus_hg1x <- readRDS("results/brms_results/mod.Recipient_estrus_hg1x.RDS")

estrus_comparison(mod.Actor_estrus_hg1x,totalrate_Actor_estrus)
estrus_comparison(mod.Recipient_estrus_hg1x,totalrate_Recipient_estrus)


## each behavior 

# rate_Actor_estrus<-rate_Actor_estrus %>%
#   filter(!is.na(State)) %>%
#   filter(!is.na(rank.Actor)) %>%
#   filter(!is.na(glick_rank_by_day))
# rate_Recipient_estrus<-rate_Recipient_estrus %>%
#   filter(!is.na(State)) %>%
#   filter(!is.na(rank.Recipient)) %>%
#   filter(!is.na(glick_rank_by_day))
# 
# 
# mod.Actor_estrus_hg_fight<-brm(bf(formula= rate ~ State+(1|day)+(1|cohortx)+(1|subjectID),
#                                   hu~mo(glick_rank_by_day)+(1|day)+(1|cohortx)+(1|subjectID),
#                                   nl=FALSE),
#                                data=rate_Actor_estrus %>% filter(Behavior=="Fighting"),
#                                family=hurdle_gamma(link = "log"),
#                                control = list(adapt_delta=0.95)
# )
# 
# mod.Actor_estrus_hg_chase<-brm(bf(formula= rate ~ State+(1|day)+(1|cohortx)+(1|subjectID),
#                                   hu~mo(glick_rank_by_day)+(1|day)+(1|cohortx)+(1|subjectID),
#                                   nl=FALSE),
#                                data=rate_Actor_estrus %>% filter(Behavior=="Chasing"),
#                                family=hurdle_gamma(link = "log"),
#                                control = list(adapt_delta=0.95)
# )
# 
# mod.Actor_estrus_hg_mount<-brm(bf(formula= rate ~ State+(1|day)+(1|cohortx)+(1|subjectID),
#                                   hu~mo(glick_rank_by_day)+(1|day)+(1|cohortx)+(1|subjectID),
#                                   nl=FALSE),
#                                data=rate_Actor_estrus %>% filter(Behavior=="Mounting"),
#                                family=hurdle_gamma(link = "log"),
#                                control = list(adapt_delta=0.95)
# )
# 
# 
# 
# 
# mod.Recipient_estrus_hg_fight<-brm(bf(formula= rate ~ State+(1|day)+(1|cohortx)+(1|subjectID),
#                                   hu~mo(glick_rank_by_day)+(1|day)+(1|cohortx)+(1|subjectID),
#                                   nl=FALSE),
#                                data=rate_Recipient_estrus %>% filter(Behavior=="Fighting"),
#                                family=hurdle_gamma(link = "log"),
#                                control = list(adapt_delta=0.95)
# )
# 
# mod.Recipient_estrus_hg_chase<-brm(bf(formula= rate ~ State+(1|day)+(1|cohortx)+(1|subjectID),
#                                   hu~mo(glick_rank_by_day)+(1|day)+(1|cohortx)+(1|subjectID),
#                                   nl=FALSE),
#                                data=rate_Recipient_estrus %>% filter(Behavior=="Chasing"),
#                                family=hurdle_gamma(link = "log"),
#                                control = list(adapt_delta=0.95)
# )
# 
# mod.Recipient_estrus_hg_mount<-brm(bf(formula= rate ~ State+(1|day)+(1|cohortx)+(1|subjectID),
#                                   hu~mo(glick_rank_by_day)+(1|day)+(1|cohortx)+(1|subjectID),
#                                   nl=FALSE),
#                                data=rate_Recipient_estrus %>% filter(Behavior=="Mounting"),
#                                family=hurdle_gamma(link = "log"),
#                                control = list(adapt_delta=0.95)
# )
# 
# 
# saveRDS(mod.Actor_estrus_hg_fight,"results/brms_results/mod.Actor_estrus_hg_fight.RDS")
# saveRDS(mod.Actor_estrus_hg_chase,"results/brms_results/mod.Actor_estrus_hg_chase.RDS")
# saveRDS(mod.Actor_estrus_hg_mount,"results/brms_results/mod.Actor_estrus_hg_mount.RDS")
# 
# saveRDS(mod.Recipient_estrus_hg_fight,"results/brms_results/mod.Recipient_estrus_hg_fight.RDS")
# saveRDS(mod.Recipient_estrus_hg_chase,"results/brms_results/mod.Recipient_estrus_hg_chase.RDS")
# saveRDS(mod.Recipient_estrus_hg_mount,"results/brms_results/mod.Recipient_estrus_hg_mount.RDS")

mod.Actor_estrus_hg_fight <- readRDS("results/brms_results/mod.Actor_estrus_hg_fight.RDS")
mod.Actor_estrus_hg_chase <- readRDS("results/brms_results/mod.Actor_estrus_hg_chase.RDS")
mod.Actor_estrus_hg_mount <- readRDS("results/brms_results/mod.Actor_estrus_hg_mount.RDS")

mod.Recipient_estrus_hg_fight <- readRDS("results/brms_results/mod.Recipient_estrus_hg_fight.RDS")
mod.Recipient_estrus_hg_chase <- readRDS("results/brms_results/mod.Recipient_estrus_hg_chase.RDS")
mod.Recipient_estrus_hg_mount <- readRDS("results/brms_results/mod.Recipient_estrus_hg_mount.RDS")

estrus_comparison(mod.Actor_estrus_hg_fight,rate_Actor_estrus %>% filter(Behavior=="Fighting"))

estrus_comparison(mod.Actor_estrus_hg_chase,rate_Actor_estrus %>% filter(Behavior=="Chasing"))

estrus_comparison(mod.Actor_estrus_hg_mount,rate_Actor_estrus %>% filter(Behavior=="Mounting"))

estrus_comparison(mod.Recipient_estrus_hg_fight,rate_Recipient_estrus %>% filter(Behavior=="Fighting"))

estrus_comparison(mod.Recipient_estrus_hg_chase,rate_Recipient_estrus %>% filter(Behavior=="Chasing"))

estrus_comparison(mod.Recipient_estrus_hg_mount,rate_Recipient_estrus %>% filter(Behavior=="Mounting"))

