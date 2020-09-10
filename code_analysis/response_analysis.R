library(tidyverse)

jackknife_dci<-read.csv("data/jackknife_dci.csv") %>% select(-X)
jackknife_ttri<-read.csv("data/jackknife_ttri.csv") %>% select(-X)
df.wl_boot_dci<-read.csv("data/df.wl_boot_dci.csv") %>% select(-X)
df.wl_boot_ttri<-read.csv("data/df.wl_boot_ttri.csv") %>% select(-X)
observer_correlation_spearman<-read.csv("data/observer_correlation.csv") %>% select(-X)
observer_correlation_pearson<-read.csv("data/observer_correlation_pearson.csv") %>% select(-X)



df.wl <- lapply(df.l, function(x) expandrows.observer(x))


dci_df<-df.wl %>% 
  map(~filter(.,score==1)) %>% 
  map(~select(.,Actor,Recipient)) %>% 
  map(~get_wl_matrix(.)) %>% 
  map(~dci(.)) %>%
  map(~as.data.frame(.)) %>% 
  map2_df(.,names(.),~mutate(.x,cohort=.y))
colnames(dci_df)[1]<-"dci"


ttri_df<-df.wl %>% 
  map(~filter(.,score==1)) %>% 
  map(~select(.,Actor,Recipient)) %>% 
  map(~get_wl_matrix(.)) %>% 
  map(~ttri(.)) %>%
  map(~.$ttri) %>% 
  map(~as.data.frame(.)) %>% 
  map2_df(.,names(.),~mutate(.x,cohort=.y))
colnames(ttri_df)[1]<-"ttri"



jackknife_dci %>%
  ggplot(.,aes(dci,color=as.factor(jack)))+
  geom_freqpoly(binwidth=0.005)+
  geom_vline(data=dci_df,aes(xintercept= dci), color="black", lty=2)+
  facet_wrap(~cohort,scales="free_x",ncol=2)+
  theme_minimal()


jackknife_ttri %>%
  ggplot(.,aes(ttri,color=as.factor(jack)))+
  geom_freqpoly(binwidth=0.005)+
  geom_vline(data=ttri_df,aes(xintercept= ttri), color="black", lty=2)+
  facet_wrap(~cohort,scales="free_y",ncol=2)+
  theme_minimal()



summary(observer_correlation_spearman)
summary(observer_correlation_pearson)
