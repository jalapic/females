library(tidyverse)
library(gridExtra)
library(viridis)

##' Figure 1=========================="analysis_code/03_matrix_plot.R"
figure1=grid.arrange(socio[[1]],socio[[2]],socio[[3]],socio[[4]],socio[[5]],socio[[6]],socio[[7]],socio[[8]],
                bimat[[1]],bimat[[2]],bimat[[3]],bimat[[4]],bimat[[5]],bimat[[6]],bimat[[7]],bimat[[8]],
                pglickos[[1]],pglickos[[2]],pglickos[[3]],pglickos[[4]],pglickos[[5]],pglickos[[6]],pglickos[[7]],pglickos[[8]],
                nrow=)

ggsave("img/Fig1_matrices_glicko.png", figure1, width=25,height=9)



figure1_rev=grid.arrange(socio[[1]],socio[[2]],socio[[3]],socio[[4]],socio[[5]],socio[[6]],socio[[7]],socio[[8]],
                     
                     pglickos[[1]],pglickos[[2]],pglickos[[3]],pglickos[[4]],pglickos[[5]],pglickos[[6]],pglickos[[7]],pglickos[[8]],
                     nrow=2)

ggsave("img/Fig1_matrices_glicko_rev.png", figure1_rev, width=25,height=6)


figure1_revsupp=grid.arrange(bimat[[1]],bimat[[2]],bimat[[3]],bimat[[4]],bimat[[5]],bimat[[6]],bimat[[7]],bimat[[8]],
                     nrow=1)

ggsave("img/Fig1_matrices_glicko_revsupp.png", figure1_revsupp, width=25,height=3)


##' Figure 2=========================="analysis_code/04_dominance_overall_by_day.R"
#' Red lines indicate triangle transitivity values that were significantly above chance; 
#' Black lines indicate triangle transitivity values that were not significantly above chance.
pttri=ggplot(day.ttri.df, aes(x=day,y=ttri,color=color,group=cohort))+
  geom_line(size=1.2) +
  newggtheme+
  scale_color_manual(values=c("black","red"))+
  scale_x_continuous(breaks=1:14)+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1))+
  ylab("Triangle Transitivity")+
  xlab("Day")

phval=ggplot(day.hval.df, aes(x=day,y=hval,color=color,group=cohort))+
  geom_line(size=1.2) +
  newggtheme+
  scale_color_manual(values=c("black","red"))+
  scale_x_continuous(breaks=1:14)+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1),limits = c(0,1))+
  ylab("Landau's Modified h'")+
  xlab("Day")

hierday=grid.arrange(pttri,phval,nrow=1)

ggsave("img/Fig2_hierarchyday.png", hierday, width=13, height=6)


##' Figure 3=========================="data_carpentry/05_male_v_female.R"
label_names <- list(
  'hval'="Landau's h'",
  'tt'="Triangle\nTransitivity",
  'st'="Steepness",
  'dc'="Directional\nConsistency",
  'desp'="Despotism",
  'gcw'="GC Wins",
  'gcl'="GC Losses"
)

my_labeller <- function(variable,value){
  return(label_names[value])
}


p.malefemale<-malefemale.long %>% 
  mutate(measure=factor(measure,level=c("hval", "tt",  "st", "dc", "desp", "gcw", "gcl"))) %>% 
  ggplot(., aes(sex, value, color = sex))+   
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(aes(fill = sex), alpha=0.2,outlier.colour = NA)+
  facet_grid(.~measure, labeller=my_labeller) +
  #geom_boxplot(size = .8, alpha = .1)+
  scale_color_manual(values = c("purple4", "orange"))+
  scale_fill_manual(values = c("purple4", "orange"))+
  ylab("Value") +
  xlab("")+
  theme_bw()+
  newggtheme +
  theme(legend.position="right")+
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1),limits = c(0,1))
#  ggtitle("Comparison of Summary Statistics of Male & Female Hierarchies\n")+
#  theme(plot.title = element_text(hjust = 0))

p.malefemale

ggsave("img/Fig3_malefemale.png",p.malefemale,width=18,height=5)


##' Figure 4=========================="analysis_code/09_brms_each_behavior.R"
# dat_each_behav<-readRDS("img/img_RDS/dat_each_behav.RDS")

each_behav_rank<-dat_each_behav %>% 
  mutate(Behavior=ifelse(Behavior=="Fight","Fighting",ifelse(Behavior=="Chase","Chasing","Mounting"))) %>% 
  mutate(Behavior=factor(Behavior,level=c("Fighting","Chasing","Mounting"))) %>% 
  ggplot(. ,aes(xvar,exp(yvar)))+
  geom_line(aes(color=Behavior,group=Behavior),size=1.5,alpha=0.5)+
  newggtheme_with_legend+
  xlab("Final Social Rank")+
  ylab("Hourly Rate of Each Behavior")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_continuous(breaks = scales::pretty_breaks(4))+
  geom_ribbon(aes(xvar,exp(yvar),ymax=exp(yvar+se),ymin=exp(yvar-se),group=Behavior,fill=Behavior),alpha=0.4)+
  scale_color_viridis(discrete=TRUE)+
  scale_fill_viridis(discrete=TRUE)+
  facet_wrap(~direction,scales = "free_y")

each_behav_rank

ggsave("img/Fig4_bhav_rank_marginal.png",each_behav_rank,width=12,height=6)

##' Figure 5=========================="data_carpentry/03_estrus_states.R"

# c("#000004FF", "#721F81FF", "#F1605DFF", "#FCFDBFFF","ghostwhite") # viridis option A = B 
# c("#0D0887FF", "#9C179EFF", "#ED7953FF", "#F0F921FF", "ghostwhite") #viridis option C. Option D is used for 'each behavior data'

p.esprop<-ggplot(es.rank.sum, aes(x=rank,y=proportion,fill=State))+
  geom_col()+
  facet_wrap(~cohortx, nrow=1) +
  newggtheme+
  #theme(legend.position = 'top')+
  theme(axis.text.y = element_blank() ,
        axis.title.y = element_blank())+
  #ylab("Proportion of Days")+
  xlab("Rank")+
  scale_x_continuous(breaks=c(1,3,5,7,9,11))+
  scale_fill_manual(values=c("#000004FF", "#721F81FF", "#F1605DFF", "#FCFDBFFF")) 

str(es.rank)
levels(factor(es.rank$State))

pestrus.rank<-ggplot(es.rank %>% 
                       filter(day<15) %>% 
                       mutate(State=factor(State,levels=c( "Diestrus" ,  "Metestrus", "Proestrus",  "Estrus"  ))), 
                     aes(x=day,y=rank,fill=State))+facet_wrap(~cohortx, nrow=1)+
  geom_tile(colour="black", size=.25)+
  scale_fill_manual(values=c("#000004FF", "#721F81FF", "#F1605DFF", "#FCFDBFFF", "ghostwhite"))+
  newggtheme+
  scale_y_reverse(breaks=1:12)+
  #theme(legend.position = "top") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  xlab("Day")+
  #ylab("Rank")+
  scale_x_continuous(breaks=c(1,5,10,14))


figure5=grid.arrange(p.esprop,pestrus.rank)

ggsave("img/Fig5_estrus.png",figure5,height=7.5,width=25)




p.espropx<-ggplot(es.rank.sum, aes(x=rank,y=proportion,fill=State))+
  geom_col()+
  facet_wrap(~cohortx, nrow=1) +
  newggtheme+
  theme(legend.position = 'right',
        axis.title.y = element_text(size = rel(2), vjust = 1))+
  #theme(axis.text.y = element_blank() ,
  #      axis.title.y = element_blank())+
  ylab("Proportion of Days")+
  xlab("Rank")+
  scale_x_continuous(breaks=c(1,3,5,7,9,11))+
  scale_y_continuous(breaks = scales::pretty_breaks(3))+
  scale_fill_manual(values=c("#000004FF", "#721F81FF", "#F1605DFF", "#FCFDBFFF")) 

pestrus.rankx<-ggplot(es.rank %>% filter(day<15), aes(x=day,y=rank,fill=State))+facet_wrap(~cohortx, nrow=1)+
  geom_tile(colour="black", size=.25)+
  scale_fill_manual(values=c("#000004FF", "#721F81FF", "#F1605DFF", "#FCFDBFFF", "ghostwhite"))+
  newggtheme+
  scale_y_reverse(breaks=1:12)+
  theme(axis.title.y = element_text(size = rel(2), vjust = 1)) +
  #theme(axis.text.y = element_blank(),
  #      axis.title.y = element_blank())+
  xlab("Day")+
  ylab("Rank")+
  scale_x_continuous(breaks=c(1,5,10,14))

figure5_lab=grid.arrange(p.espropx,pestrus.rankx)

ggsave("img/Fig5_estrus_lab.png",figure5_lab,height=7.5,width=25)

p.espropxx<-ggplot(es.rank.sum, aes(x=rank,y=proportion,fill=State))+
  geom_col()+
  facet_wrap(~cohortx, nrow=1) +
  newggtheme+
  theme(legend.position = 'top',
        axis.title.y = element_text(size = rel(2), vjust = 1))+
  #theme(axis.text.y = element_blank() ,
  #      axis.title.y = element_blank())+
  ylab("Proportion of Days")+
  xlab("Rank")+
  scale_x_continuous(breaks=c(1,3,5,7,9,11))+
  scale_y_continuous(breaks = scales::pretty_breaks(3))+
  scale_fill_manual(values=c("#000004FF", "#721F81FF", "#F1605DFF", "#FCFDBFFF")) 

ggsave("img/Fig5_estrus_for_legend.png",p.espropxx,height=7.5,width=25)

##' Figure 6=========================="analysis_code/11_hormones.R"

t=28
u=20
i=420
temp1<- data.frame(a = c(1,1,2,2), b = c(i, i+t,i+t, i))
hormdf <- hormdf %>% mutate(status=ifelse(glrank>7,"Subordinate","Dominant"))
pcort<-ggplot(hormdf,aes(as.factor(status),CORT))+
  geom_jitter(aes(color=status),position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(aes(color=status,fill=status),alpha=0.2,outlier.colour = NA)+
  scale_color_manual(values = c("purple4", "orange"))+
  scale_fill_manual(values = c("purple4", "orange"))+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Dominant","Subordinate"))+
  labs(x="",
       y="Corticosterone (ng/ml)")+
  newggtheme+
  geom_line(data = temp1, aes(x = a, y = b),color="red") + 
  annotate("text", x = 1.5, y = i+t+u, label = "153.9 ng/ul [107.6, 200.0]", size = 4.5,color="red")+
  ggtitle("(A)")+
  theme(plot.title = element_text(hjust = -0.18, vjust=2.12))+
  scale_y_continuous(breaks = scales::pretty_breaks(5))

tx=3
ux=2
ix=53
temp2<- data.frame(a = c(1,1,2,2), b = c(ix, ix+tx,ix+tx, ix))

pest <- ggplot(hormdf,aes(as.factor(status),E))+
  geom_jitter(aes(color=status),position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(aes(color=status,fill=status),alpha=0.2,outlier.colour = NA)+
  scale_color_manual(values = c("purple4", "orange"))+
  scale_fill_manual(values = c("purple4", "orange"))+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Dominant","Subordinate"))+
  labs(x="",
       y="Estradiol (pg/ml)")+
  newggtheme+
  geom_line(data = temp2, aes(x = a, y = b)) + 
  annotate("text", x = 1.5, y = ix+tx+ux, label = "-2.97 pg/ul [-8.03, 1.99]", size = 4.5)+
  ggtitle("(B)")+
  theme(plot.title = element_text(hjust = -0.15, vjust=2.12))+
  scale_y_continuous(breaks = c(10,20,30,40,50))


phorm <-grid.arrange(pcort,pest,nrow=1)

ggsave("img/Fig6_hormones.png", phorm, width=10, height=6)


##' Figure 7=========================="analysis_code/12_mcmc.qpcr_gene_expression.R"
# naive_sum<-readRDS("img/img_RDS/naive_sum.RDS")

gene_info<-data.frame(gene=levels(as.factor(naive_sum$gene)),
                      genex=c("ERα","ERβ","GnRH","OPRM1","OTR","PR"))

gene_info<-gene_info %>% mutate(genex=factor(genex,level=rev(c("ERα","ERβ","PR","OTR","OPRM1","GnRH"))))

naive_sum<-left_join(naive_sum %>% mutate(gene=factor(gene)),gene_info)
                     

genexp<-ggplot()+
  geom_point(data=naive_sum,aes(y=genex,x=mean,color=sig))+
  geom_errorbarh(data=naive_sum,aes(y=genex,xmin=lower,xmax=upper,color=sig),height=0.5)+
  geom_vline(xintercept = 0,linetype="dashed",color="grey")+
  geom_text(data=naive_sum %>% filter(sig=="sig"),
            aes(label=text,x=-Inf,genex),hjust="inward",fontface="bold",vjust=0.2,color="red")+
  geom_text(data=naive_sum %>% filter(sig!="sig"),aes(label=text,x=-Inf,y=genex),hjust="inward",vjust=0.2)+
  scale_color_manual(values=c("black","red"))+
  ylab("")+
  labs(fill="",color="")+
  xlab("Log2(Fold Change) (mean ± 95% CI)")+
  newggtheme+
  facet_grid(region~.,scales="free_y",space="free_y",switch = "y")+
  scale_x_continuous(breaks=c(-2,-1,0,1,2),limits = c(-2.5,2.5))+
  theme(axis.text.y = element_text(hjust = 0,size=12),
        axis.title.x = element_text(size=15),
        strip.text.y = element_text(size=15,angle=180),
        strip.placement = "outside")

genexp

ggsave("img/Fig7_genes.png", genexp, width=8, height=6)

##' Supp Figure S1=========================="analysis_code/07_startends.R"
library(scales)
q_colors =  3
v_colors =  viridis(q_colors, option = "D")

p.rates <- ggplot(dfx.lz.alldays.df,aes(factor(day),propn,color=Behavior))+
  geom_jitter(position = position_nudge(x=0.14),shape=21,size=1.8)+
  geom_boxplot2(aes(fill=Behavior),alpha=0.4,outlier.colour = NA)+
  theme(legend.position = "none")+
  labs(x="Day",
       y="Total Events per \n Hour of Observation")+
  newggtheme+
  ggtitle("Rate of Agonistic Behaviors Across Days")+
  facet_wrap(~Behavior) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)

p.rates

ggsave("img/Supp_FigS1_rates_by_day.png", p.rates,width=18,height=6)

##' Supp Figure S2=========================="analysis_code/05_dominance_by_behavior.R"

pmat_behav=grid.arrange(pfi[[1]],pfi[[2]],pfi[[3]],pfi[[4]],pfi[[5]],pfi[[6]],pfi[[7]],pfi[[8]],
                        pch[[1]],pch[[2]],pch[[3]],pch[[4]],pch[[5]],pch[[6]],pch[[7]],pch[[8]],
                        pmo[[1]],pmo[[2]],pmo[[3]],pmo[[4]],pmo[[5]],pmo[[6]],pmo[[7]],pmo[[8]],
                        nrow=3)

ggsave("img/Supp_FigS2_matricesbehavs.png", pmat_behav, width=25,height=9)


##' Supp Figure S3=========================="analysis_code/05_dominance_by_behavior.R"
dat.figs3<-dcs.df.long %>% 
  mutate(direction="DC",
         behavior=ifelse(behav=="fight.dc","fighting",ifelse(behav=="chase.dc","chasing","mounting"))) %>%
  mutate(behavior=factor(behavior,level=c("fighting","chasing","mounting"))) %>% 
  select(cohort,direction,behavior,value) %>% 
  rbind(.,df.gini %>% 
          mutate(direction=ifelse(direction=="in","GC Losses","GC Wins"))) %>% 
  mutate(direction=factor(direction,level=c("DC","GC Wins","GC Losses")))

eachbehav<-ggplot(dat.figs3,aes(behavior,value,color=behavior))+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  geom_boxplot2(aes(fill=behavior),alpha=0.4,outlier.colour = NA)+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Fighting","Chasing","Mounting"))+
  labs(x="Behavior",
       y="Value")+
  newggtheme+
  facet_wrap(~direction)+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  scale_y_continuous(breaks = c(0.25,0.5,0.75,1),limits = c(0,1.1))

eachbehav

ggsave("img/Supp_FigS3_eachbehav.png", eachbehav, width=12, height=5)

##' Supp Figure S4=========================="analysis_code/05_dominance_by_behavior.R"
dat.figs4<-rbind(behavdf.actor %>% ungroup() %>% select(-Actor) %>% mutate(direction="Given"),
      behavdf.recipient %>% ungroup() %>% select(-Recipient) %>% mutate(direction="Received"))

pprops<-dat.figs4 %>% group_by(behav,rank,direction)%>%
  summarize(meanprop = mean(prop)) %>%
  ungroup() %>% 
  mutate(behav1=ifelse(behav=="fighting","Fighting",ifelse(behav=="chasing","Chasing","Mounting"))) %>% 
  mutate(behav1=factor(behav1,level=c("Mounting","Chasing","Fighting"))) %>% 
  ggplot(aes(x=rank,y=meanprop,fill=behav1))+geom_col(alpha=0.6)+
  ylab("Proportion of \nAgonistic Interactions")+
  xlab("Rank")+
  scale_x_continuous(breaks=1:12)+
  newggtheme+
  theme(legend.position = "right") +
  scale_fill_manual(values=c( "#FDE725FF", "#21908CFF", "#440154FF"),breaks=c("Fighting","Chasing","Mounting"))+
  facet_wrap(~direction)

ggsave("img/Supp_FigS4_behavprops.png", pprops,width=20,height=10)
