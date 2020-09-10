library(tidyverse)
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

#1. Jackknifing 
## DC
jack_dci<-function(df,jackknife=0.75){
  N<-nrow(df)
  jack<-list()
  for(i in 1:10000){
    jack[i]<- df %>%
      sample_n(N*jackknife) %>%
      filter(score==1.0) %>%
      select(Actor, Recipient) %>% 
      mutate(Actor=as.character(Actor),
             Recipient=as.character(Recipient)) %>%
      get_wl_matrix() %>%
      dci()
  }
  jack<-rbindlist(jack %>% map(~as.data.frame(.))) %>% 
    as.data.frame() 
  colnames(jack)<-"dci"
  return(jack)
}

set.seed(302)

df.wl_jackknife75<-df.wl %>% map(~jack_dci(.,jackknife = 0.75)) %>% map2(.,names(.),~mutate(.x,cohort=.y,jack="75")) %>% bind_rows()
df.wl_jackknife50<-df.wl %>% map(~jack_dci(.,jackknife = 0.50)) %>% map2(.,names(.),~mutate(.x,cohort=.y,jack="50")) %>% bind_rows()
df.wl_jackknife40<-df.wl %>% map(~jack_dci(.,jackknife = 0.40)) %>% map2(.,names(.),~mutate(.x,cohort=.y,jack="40")) %>% bind_rows()

jackknife_dci<-rbind(df.wl_jackknife75,df.wl_jackknife50,df.wl_jackknife40)


##Triangle Transitivity
jack_ttri<-function(df,jackknife=0.75){
  N<-nrow(df)
  jack<-list()
  for(i in 1:10000){
    jack[i]<- df %>%
      sample_n(N*jackknife) %>%
      filter(score==1.0) %>%
      select(Actor, Recipient) %>% 
      mutate(Actor=as.character(Actor),
             Recipient=as.character(Recipient)) %>%
      get_wl_matrix() %>%
      ttri() %>% 
      .$ttri
  }
  jack<-rbindlist(jack %>% map(~as.data.frame(.))) %>% 
    as.data.frame() 
  colnames(jack)<-"ttri"
  return(jack)
}

set.seed(302)

df.wl_jackknife75_ttri<-df.wl %>% map(~jack_ttri(.,jackknife = 0.75)) %>% map2(.,names(.),~mutate(.x,cohort=.y,jack="75")) %>% bind_rows()
df.wl_jackknife50_ttri<-df.wl %>% map(~jack_ttri(.,jackknife = 0.50)) %>% map2(.,names(.),~mutate(.x,cohort=.y,jack="50")) %>% bind_rows()
df.wl_jackknife40_ttri<-df.wl %>% map(~jack_ttri(.,jackknife = 0.40)) %>% map2(.,names(.),~mutate(.x,cohort=.y,jack="40")) %>% bind_rows()

jackknife_ttri<-rbind(df.wl_jackknife75_ttri,df.wl_jackknife50_ttri,df.wl_jackknife40_ttri)




##Landau's h


jack_devries<-function(df,jackknife=0.75){
  N<-nrow(df)
  jack<-list()
  for(i in 1:1000){
    jack[[i]]<- df %>%
      sample_n(N*jackknife) %>%
      filter(score==1.0) %>%
      select(Actor, Recipient) %>%
      mutate(Actor=as.character(Actor),
             Recipient=as.character(Recipient)) %>%
      get_wl_matrix() %>%
      devries() %>%
      bind_rows() %>%
      as.data.frame
  }

  jack<-rbindlist(jack %>% map(~as.data.frame(.))) %>%
    as.data.frame()
  colnames(jack)<-c("landau_h","pvalue")
  return(jack)
}

df.wl_jackknife75_devries<-df.wl %>% map(~jack_devries(.,jackknife = 0.75)) %>%
  map2(.,names(.),~mutate(.x,cohort=.y,jack="75%")) %>% bind_rows()

df.wl_jackknife50_devries<-df.wl %>% map(~jack_devries(.,jackknife = 0.50)) %>%
  map2(.,names(.),~mutate(.x,cohort=.y,jack="50%")) %>% bind_rows()

df.wl_jackknife40_devries<-df.wl %>% map(~jack_devries(.,jackknife = 0.40)) %>%
  map2(.,names(.),~mutate(.x,cohort=.y,jack="40%")) %>% bind_rows()

jackknife_devries<-rbind(df.wl_jackknife75_devries,df.wl_jackknife50_devries,df.wl_jackknife40_devries)
write.csv(jackknife_devries,"data/jackknife_devries.csv")

write.csv(jackknife_dci,"data/jackknife_dci.csv")
write.csv(jackknife_ttri,"data/jackknife_ttri.csv")




#2. Bootstrapping

boots_dci<-function(df,n=10000){
  df<-df %>% filter(score==1) %>% 
    select(Actor,Recipient,score)
  boots.results<-NULL
  for(i in 1:n){
    tmp<-df[sample(nrow(df),nrow(df),replace=T), ]
    tmp<-tmp %>% 
      mutate(Actor=as.character(Actor),
             Recipient=as.character(Recipient)) %>% 
      select(Actor,Recipient) %>% 
      get_wl_matrix() %>% 
      dci()
    boots.results[[i]]<-tmp
  }
  boots<-boots.results %>% as.data.frame()
  colnames(boots)<-"dci"
  return(boots)
}


boots_ttri<-function(df,n=10000){
  df<-df %>% filter(score==1) %>% 
    select(Actor,Recipient,score)
  boots.results<-NULL
  for(i in 1:n){
    tmp<-df[sample(nrow(df),nrow(df),replace=T), ]
    tmp<-tmp %>% 
      mutate(Actor=as.character(Actor),
             Recipient=as.character(Recipient)) %>% 
      select(Actor,Recipient) %>% 
      get_wl_matrix() %>% 
      ttri() %>% 
      .$ttri
    boots.results[[i]]<-tmp
  }
  boots<-boots.results %>% as.data.frame()
  colnames(boots)<-"ttri"
  return(boots)
}


df.wl_boot_dci<-df.wl %>% map(~boots_dci(.)) %>% map2_df(.,names(.),~mutate(.x,cohort=.y))

df.wl_boot_ttri<-df.wl %>% map(~boots_ttri(.)) %>% map2_df(.,names(.),~mutate(.x,cohort=.y))

write.csv(df.wl_boot_dci,"data/df.wl_boot_dci.csv")
write.csv(df.wl_boot_ttri,"data/df.wl_boot_ttri.csv")


#3. Between observer comparison

df.wl %>% map(~table(.$Observer))
# Cohort B - WL, CW, AD
# Cohort C - SB, CW, RH, TN, AD
# Cohort D - WL, CN, EY
# Cohort E - MB, IK, CW, EY
# Cohort F - WL, CW, CN, RH, AD
# Cohort G - EY, CW, IK, RH
# cohort H - RH, MB, WL, CW

# how many hours each obsever observe throughout entire female study?
observation_count<-df.wl %>% 
  map(~group_by(.,Observer)) %>%
  map(~tally(.)) %>% 
  map2_df(.,names(.),~mutate(.x,cohort=.y))

observation_count %>% 
  group_by(Observer) %>% 
  tally(.) %>% 
  arrange(n)



observer_compare<-function(df, initial="CW",method="pearson"){
  
  dfx<- df %>% filter(Observer==initial) %>% select(Actor,Recipient)
  dfy<- df %>% filter(Observer!=initial) %>% select(Actor,Recipient)
  observed_pct<-nrow(dfx)/nrow(df)*100
  observer1<-dfx %>% 
    get_wl_matrix() %>% 
    ds() 
  others<-dfy %>% 
    get_wl_matrix() %>% 
    ds() 
  
  temp<-cbind(observer1,others) %>% 
    as.data.frame() %>% 
    mutate(mouseID=rownames(.))
            
  temp_result<-cor.test(temp$observer1,temp$others,paired=T,method=method)
  result<-data.frame(rho=NA,p_value=NA,initial=NA,observed_pct=NA)
  result$rho<-temp_result$estimate
  result$p_value<-temp_result$p.value
  result$initial<-initial
  result$observed_pct<-observed_pct
  
  print<-list()
  print$result<-result
  print$observer1<-observer1
  print$others<-others
  return(print)
}

observer_correlation<-data.frame()

observer_correlation<-df.wl$B %>% observer_compare(.,initial="AD") %>% .$result %>%  mutate(cohort="B")
observer_correlation[2,]<-df.wl$B %>% observer_compare(.,initial="CN") %>% .$result %>% mutate(cohort="B")
observer_correlation[3,]<-df.wl$B %>% observer_compare(.,initial="CW") %>% .$result %>% mutate(cohort="B")
observer_correlation[4,]<-df.wl$B %>% observer_compare(.,initial="TN") %>% .$result %>% mutate(cohort="B")
observer_correlation[5,]<-df.wl$B %>% observer_compare(.,initial="WL") %>% .$result %>% mutate(cohort="B")

observer_correlation[6,]<-df.wl$C %>% observer_compare(.,initial="AD") %>% .$result %>% mutate(cohort="C")
observer_correlation[7,]<-df.wl$C %>% observer_compare(.,initial="CW") %>% .$result %>% mutate(cohort="C")
observer_correlation[8,]<-df.wl$C %>% observer_compare(.,initial="EY") %>% .$result %>% mutate(cohort="C")
observer_correlation[9,]<-df.wl$C %>% observer_compare(.,initial="IK") %>% .$result %>% mutate(cohort="C")
observer_correlation[10,]<-df.wl$C %>% observer_compare(.,initial="RH") %>% .$result %>% mutate(cohort="C")
observer_correlation[11,]<-df.wl$C %>% observer_compare(.,initial="SB") %>% .$result %>% mutate(cohort="C")
observer_correlation[12,]<-df.wl$C %>% observer_compare(.,initial="TN") %>% .$result %>% mutate(cohort="C")

observer_correlation[13,]<-df.wl$D %>% observer_compare(.,initial="AD") %>% .$result %>% mutate(cohort="D")
observer_correlation[14,]<-df.wl$D %>% observer_compare(.,initial="CN") %>% .$result %>% mutate(cohort="D")
observer_correlation[15,]<-df.wl$D %>% observer_compare(.,initial="EY") %>% .$result %>% mutate(cohort="D")
observer_correlation[16,]<-df.wl$D %>% observer_compare(.,initial="KB") %>% .$result %>% mutate(cohort="D")
observer_correlation[17,]<-df.wl$D %>% observer_compare(.,initial="RH") %>% .$result %>% mutate(cohort="D")
observer_correlation[18,]<-df.wl$D %>% observer_compare(.,initial="SB") %>% .$result %>% mutate(cohort="D")
observer_correlation[19,]<-df.wl$D %>% observer_compare(.,initial="WL") %>% .$result %>% mutate(cohort="D")

observer_correlation[20,]<-df.wl$E %>% observer_compare(.,initial="CN") %>% .$result %>% mutate(cohort="E")
observer_correlation[21,]<-df.wl$E %>% observer_compare(.,initial="CW") %>% .$result %>% mutate(cohort="E")
observer_correlation[22,]<-df.wl$E %>% observer_compare(.,initial="EY") %>% .$result %>% mutate(cohort="E")
observer_correlation[23,]<-df.wl$E %>% observer_compare(.,initial="IK") %>% .$result %>% mutate(cohort="E")
observer_correlation[24,]<-df.wl$E %>% observer_compare(.,initial="MB") %>% .$result %>% mutate(cohort="E")
observer_correlation[25,]<-df.wl$E %>% observer_compare(.,initial="SB") %>% .$result %>% mutate(cohort="E")

observer_correlation[26,]<-df.wl$F %>% observer_compare(.,initial="AD") %>% .$result %>% mutate(cohort="F")
observer_correlation[27,]<-df.wl$F %>% observer_compare(.,initial="CN") %>% .$result %>% mutate(cohort="F")
observer_correlation[28,]<-df.wl$F %>% observer_compare(.,initial="CW") %>% .$result %>% mutate(cohort="F")
observer_correlation[29,]<-df.wl$F %>% observer_compare(.,initial="EY") %>% .$result %>% mutate(cohort="F")
observer_correlation[30,]<-df.wl$F %>% observer_compare(.,initial="RH") %>% .$result %>% mutate(cohort="F")
observer_correlation[31,]<-df.wl$F %>% observer_compare(.,initial="TN") %>% .$result %>% mutate(cohort="F")

observer_correlation[32,]<-df.wl$G %>% observer_compare(.,initial="AL") %>% .$result %>% mutate(cohort="G")
observer_correlation[33,]<-df.wl$G %>% observer_compare(.,initial="CN") %>% .$result %>% mutate(cohort="G")
observer_correlation[34,]<-df.wl$G %>% observer_compare(.,initial="CW") %>% .$result %>% mutate(cohort="G")
observer_correlation[35,]<-df.wl$G %>% observer_compare(.,initial="EY") %>% .$result %>% mutate(cohort="G")
observer_correlation[36,]<-df.wl$G %>% observer_compare(.,initial="RH") %>% .$result %>% mutate(cohort="G")

observer_correlation[37,]<-df.wl$H %>% observer_compare(.,initial="AD") %>% .$result %>% mutate(cohort="H")
observer_correlation[38,]<-df.wl$H %>% observer_compare(.,initial="EY") %>% .$result %>% mutate(cohort="H")
observer_correlation[39,]<-df.wl$H %>% observer_compare(.,initial="MB") %>% .$result %>% mutate(cohort="H")
observer_correlation[40,]<-df.wl$H %>% observer_compare(.,initial="RH") %>% .$result %>% mutate(cohort="H")
observer_correlation[41,]<-df.wl$H %>% observer_compare(.,initial="SB") %>% .$result %>% mutate(cohort="H")
observer_correlation[42,]<-df.wl$H %>% observer_compare(.,initial="WL") %>% .$result %>% mutate(cohort="H")

write.csv(observer_correlation,"data/observer_correlation.csv")
write.csv(observer_correlation,"data/observer_correlation_pearson.csv")
