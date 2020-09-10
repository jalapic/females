library(MCMC.qpcr)

pcrdf<-readr::read_csv("raw_data/qPCR_rawCTvalues.csv") %>% 
  filter(sample!="water") %>% 
  mutate(sample=as.integer(sample)) %>% 
  arrange(sample) %>% 
  mutate(dupl_id=rep(c("A","B"),419))

sampledf<-readr::read_csv("raw_data/qPCR_sampleID.csv")

#input efficiency info
eff<-pcrdf %>% mutate(region.gene=paste(region,gene,sep=".")) %>% 
  select(region.gene) %>% 
  mutate(efficiency=2) %>% 
  unique()

df<-left_join(pcrdf,sampledf) %>% 
  mutate(cohortx=LETTERS[as.numeric(str_sub(subjectid,1,2))-86],
         Mouse.ID=as.numeric(str_sub(subjectid,4))) 


# I jus don't want to run all analysis R script every single time 

glicko<-df.glickos %>% lapply(., function(x) x$ratings) %>%
  map(~mutate(.,glrank=row.names(.))) %>% 
  map2_df(.,names(.),~mutate(.x,cohortx=.y)) %>% 
  mutate(Mouse.ID=Player) %>% 
  select(cohortx,Mouse.ID,glrank)


df<-left_join(df,glicko) %>% 
  filter(subjectid!="90-5") %>%  
  mutate(status=ifelse(as.numeric(glrank)<7,"Dom","Sub")) %>% 
  mutate(uniqueid=paste(subjectid,dupl_id))

df_list<-df %>% mutate(region.gene=paste(region,gene,sep = ".")) %>% 
  split(.$region.gene) 


for(i in 1:13){df_list[[i]][1,12]->colnames(df_list[[i]])[2]}
df_list[[1]]
df_list <- df_list %>% map(~ select(.,-gene) %>% select(.,-region.gene) %>% select(.,-region)) %>% 
  map(~arrange(.,sample))


qpcr_df<-full_join(df_list[[1]][,c(2,9)],df_list[[2]][,c(2,9)]) %>% 
  full_join(.,df_list[[3]][,c(2,9)]) %>% 
  full_join(.,df_list[[4]][,c(2,9)]) %>% 
  full_join(.,df_list[[5]][,c(2,9)]) %>% 
  full_join(.,df_list[[6]][,c(2,9)]) %>% 
  full_join(.,df_list[[7]][,c(2,9)]) %>% 
  full_join(.,df_list[[8]][,c(2,9)]) %>% 
  full_join(.,df_list[[9]][,c(2,9)]) %>% 
  full_join(.,df_list[[10]][,c(2,9)]) %>% 
  full_join(.,df_list[[11]][,c(2,9)]) %>% 
  full_join(.,df_list[[12]][,c(2,9)]) %>% 
  full_join(.,df_list[[13]][,c(2,9)]) %>% 
  full_join(.,df_list[[1]][,c(4:9)]) %>% 
  mutate(mPOA.cypha=as.numeric(mPOA.cypha),
         mPOA.eralpha=as.numeric(mPOA.eralpha),
         mPOA.erbeta=as.numeric(mPOA.erbeta),
         mPOA.gnrh=as.numeric(mPOA.gnrh),
         mPOA.oprm1=as.numeric(mPOA.oprm1),
         mPOA.otr=as.numeric(mPOA.otr),
         mPOA.pr=as.numeric(mPOA.pr),
         VMH.eralpha=as.numeric(VMH.eralpha),
         VMH.erbeta=as.numeric(VMH.erbeta),
         VMH.gapdh=as.numeric(VMH.gapdh),
         VMH.oprm1=as.numeric(VMH.oprm1),
         VMH.otr=as.numeric(VMH.otr),
         VMH.pr =as.numeric(VMH.pr)
  )

colnames(qpcr_df)[15]<-"sample" #this is really important - I spend 20 min figuring why the mcmc.qpcr function didn't work


qpcr_df <-qpcr_df %>% 
  mutate(glrank=as.integer(glrank)) %>% 
  left_join(.,hgdf %>% select(cohortx,glrank,State1415))


colnames(qpcr_df)
eff2=cbind(as.data.frame(names(qpcr_df)[c(1,3:14)]),efficiency=as.numeric(2))
#make sure all CT values are numeric, unless it won't work!
qs=cq2counts(data=qpcr_df,effic=eff2,genecols = c(1,3:14),condcols = c(2,15:20))
qs$status=relevel(qs$status,ref="Dom")


set.seed(1126)
# naive=mcmc.qpcr(data=qs,fixed="status",random = "cohortx")
# saveRDS(naive,"results/brms_results/naive.RDS")

naive<-readRDS("results/brms_results/naive.RDS")

# summary(naive) # DIC: 8228.303 

s0=HPDsummary(model=naive,data=qs,relative = TRUE)
s0$summary 

outs=outlierSamples(naive,qs)
outs # No outliers

naive_sum<-s0$summary %>% 
  filter(gene!="mPOA.cypha") %>% 
  filter(gene!="VMH.gapdh") %>% 
  as.data.frame() %>% 
  mutate(region=sub("\\..*","",gene)) %>% 
  mutate(gene=sub(".*\\.","",gene)) %>% 
  mutate(sig=ifelse(lower*upper>0,"sig","-")) %>% 
  mutate(text=paste(round(mean,2),sep=" [",paste(round(lower,2),sep=", ",paste(round(upper,2),"]",sep=""))))


genexp<-ggplot()+
  geom_point(data=naive_sum,aes(y=gene,x=mean))+
  geom_errorbarh(data=naive_sum,aes(y=gene,xmin=lower,xmax=upper),height=0.5)+
  geom_vline(xintercept = 0,linetype="dashed",color="grey")+
  geom_text(data=naive_sum %>% filter(sig=="sig"),
            aes(label=text,x=-Inf,gene),hjust="inward",fontface="bold",vjust=-0.0001)+
  geom_text(data=naive_sum %>% filter(sig!="sig"),aes(label=text,x=-Inf,y=gene),hjust="inward",vjust=-0.0001)+
  scale_color_viridis(discrete = T,option="D")+
  scale_fill_viridis(discrete = T,option="D")+
  ylab("")+
  theme(legend.position = "top",
        legend.text = element_text(family="Helvetica",size=10,hjust=-2),
        strip.text.y = element_text(family="Helvetica",size=15,angle=180),
        strip.placement = "outside",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family="Helvetica"),
        axis.title.y=element_blank(),
        axis.title.x=element_text(family="Helvetica",size=12),
        panel.spacing.y=unit(0, "lines"),
        panel.background = element_rect(fill=NULL))+
  labs(fill="",color="")+
  xlab("Parameter estimate (mean ± 95% CI)")+
  newggtheme+
  facet_wrap(~region,strip.position = "left",scales="free_y",ncol=1)+
  scale_x_continuous(breaks=c(-2,-1,0,1,2),limits = c(-2.5,2.5))

print(genexp)
