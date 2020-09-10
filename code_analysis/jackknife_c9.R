### Bootstrapping -  c9

c9
N<-nrow(c9)

boot40<-list()
for(i in 1:1000){
boot40[i]<- c9 %>%
  sample_n(N*.4) %>%
  select(Actorx, Recipientx, score) %>% 
  filter(score==1.0) %>%
  map_if(is.integer, as.character) %>%
  get_wl_matrix() %>%
  dci
}

boot50<-list()
for(i in 1:1000){
  boot50[i]<- c9 %>%
    sample_n(N*.5) %>%
    select(Actorx, Recipientx, score) %>% 
    filter(score==1.0) %>%
    map_if(is.integer, as.character) %>%
    get_wl_matrix() %>%
    dci
}

boot75<-list()
for(i in 1:1000){
  boot75[i]<- c9 %>%
    sample_n(N*.75) %>%
    select(Actorx, Recipientx, score) %>% 
    filter(score==1.0) %>%
    map_if(is.integer, as.character) %>%
    get_wl_matrix() %>%
    dci
}


#obs 
c9 %>%
  select(Actorx, Recipientx, score) %>% 
  filter(score==1.0) %>%
  map_if(is.integer, as.character) %>%
  get_wl_matrix() %>%
  dci

#0.8697674

boots<-data.frame(boot40=unlist(boot40),boot50=unlist(boot50),boot75=unlist(boot75))
boot.melt <-  boots %>% reshape2::melt()
head(boot.melt)
summary(boots)


ggplot(boot.melt, aes(value, colour = variable)) +geom_freqpoly(binwidth = .005) + theme_minimal() +geom_vline(xintercept=0.8697674, color="black", lty=2)
ggplot(boot.melt, aes(value, ..density.., colour = variable)) +geom_freqpoly(binwidth = .005) + theme_minimal() +geom_vline(xintercept=0.8697674, color="black", lty=2)




######

### Bootstrapping -  c9

c9
N<-nrow(c9)

boot40<-list()
for(i in 1:1000){
  boot40[i]<- c9 %>%
    sample_n(N*.4) %>%
    select(Actorx, Recipientx, score) %>% 
    filter(score==1.0) %>%
    map_if(is.integer, as.character) %>%
    get_wl_matrix() %>%
    rowSums() %>%
    ineq::Gini()
}

boot50<-list()
for(i in 1:1000){
  boot50[i]<- c9 %>%
    sample_n(N*.5) %>%
    select(Actorx, Recipientx, score) %>% 
    filter(score==1.0) %>%
    map_if(is.integer, as.character) %>%
    get_wl_matrix() %>%
    rowSums() %>%
    ineq::Gini()
}

boot75<-list()
for(i in 1:1000){
  boot75[i]<- c9 %>%
    sample_n(N*.75) %>%
    select(Actorx, Recipientx, score) %>% 
    filter(score==1.0) %>%
    map_if(is.integer, as.character) %>%
    get_wl_matrix() %>%
    rowSums() %>%
    ineq::Gini()
}


#obs 
c9 %>%
  select(Actorx, Recipientx, score) %>% 
  filter(score==1.0) %>%
  map_if(is.integer, as.character) %>%
  get_wl_matrix() %>%
  rowSums() %>%
  ineq::Gini()

#0.6247287

boots<-data.frame(boot40=unlist(boot40),boot50=unlist(boot50),boot75=unlist(boot75))
boot.melt <-  boots %>% reshape2::melt()
head(boot.melt)
summary(boots)


ggplot(boot.melt, aes(value, colour = variable)) +geom_freqpoly(binwidth = .005) + theme_minimal() +geom_vline(xintercept=.6247287, color="black", lty=2)
ggplot(boot.melt, aes(value, ..density.., colour = variable)) +geom_freqpoly(binwidth = .005) + theme_minimal() +geom_vline(xintercept=.6247287, color="black", lty=2)



####
### Bootstrapping -  c9

c9
N<-nrow(c9)

boot40<-list()
for(i in 1:1000){
  boot40[i]<- c9 %>%
    sample_n(N*.4) %>%
    select(Actorx, Recipientx, score) %>% 
    filter(score==1.0) %>%
    map_if(is.integer, as.character) %>%
    get_wl_matrix() %>%
    rowSums() %>%
    ineq::Gini()
}

boot50<-list()
for(i in 1:1000){
  boot50[i]<- c9 %>%
    sample_n(N*.5) %>%
    select(Actorx, Recipientx, score) %>% 
    filter(score==1.0) %>%
    map_if(is.integer, as.character) %>%
    get_wl_matrix() %>%
    rowSums() %>%
    ineq::Gini()
}

boot75<-list()
for(i in 1:1000){
  boot75[i]<- c9 %>%
    sample_n(N*.75) %>%
    select(Actorx, Recipientx, score) %>% 
    filter(score==1.0) %>%
    map_if(is.integer, as.character) %>%
    get_wl_matrix() %>%
    rowSums() %>%
    ineq::Gini()
}


#obs 
c9 %>%
  select(Actorx, Recipientx, score) %>% 
  filter(score==1.0) %>%
  map_if(is.integer, as.character) %>%
  get_wl_matrix() %>%
  rowSums() %>%
  ineq::Gini()

#0.6247287

boots<-data.frame(boot40=unlist(boot40),boot50=unlist(boot50),boot75=unlist(boot75))
boot.melt <-  boots %>% reshape2::melt()
head(boot.melt)
summary(boots)


ggplot(boot.melt, aes(value, colour = variable)) +geom_freqpoly(binwidth = .005) + theme_minimal() +geom_vline(xintercept=.6247287, color="black", lty=2)
ggplot(boot.melt, aes(value, ..density.., colour = variable)) +geom_freqpoly(binwidth = .005) + theme_minimal() +geom_vline(xintercept=.6247287, color="black", lty=2)
