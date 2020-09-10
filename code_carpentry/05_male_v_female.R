library(tidyverse)

# Load male files
temp <- list.files(pattern="*.csv",path="raw_data/male/")

males<-vector('list',length(temp))
for(i in 1:length(temp)){
  males[[i]]<-readr::read_csv(paste0("raw_data/male/",temp[i]))
}


# get 14 days worth of winner-loser data:

males.dfs <- lapply(males, male_dfs)
males.dfs

# get sociomatrices
males.wl <- lapply(males.dfs, function(x) get_wl_matrix(as.matrix(x[,1:2])))
males.di <- lapply(males.wl, get_di_matrix)


# get male values

# Calculate modifed h'
male.dv <- lapply(males.wl,devries)
male.dv.p <- lapply(male.dv, function(x) x$`h-modified`) %>% unlist
male.dv.pval <- lapply(male.dv, function(x) x$`p-value`) %>% unlist

#  Calculate directional consistency
male.dc <- lapply(males.wl,dc_test)
male.dc.p <- lapply(male.dc, function(x) x$DC) %>% unlist
male.dc.pval <- lapply(male.dc, function(x) x$`DC.pvalue`) %>% unlist


#  Steepness
male.st.p <- lapply(males.wl, steepness::getStp) %>% unlist
male.st.pval <- lapply(males.wl, getStp.pval) %>% unlist


#  Triangle transitivity
male.tt <- lapply(males.di,ttri_test)
male.tt.p <- lapply(male.tt, function(x) x$ttri) %>% unlist
male.tt.pval <- lapply(male.tt, function(x) x$pval) %>% unlist



#  Despotism
male.d <- lapply(males.wl,despotism)
male.d.val <- lapply(male.d, function(x) x[[1]]) %>% unlist
male.d.val


# Gini-coefficients 
male.gcw <- lapply(males.wl, function(x) ineq::Gini(rowSums(x))) %>% unlist() #GC Wins
male.gcl <- lapply(males.wl, function(x) ineq::Gini(colSums(x))) %>% unlist() #GC Losses


#### Compare to Females ----

# dataframe
data.frame(hval=c(male.dv.p,m.dv.p),
           dc=c(male.dc.p,m.dc.p),
           st=c(male.st.p,m.st.p),
           tt=c(male.tt.p,m.tt.p),
           desp=c(male.d.val/100,m.d.val/100),
           gcw=c(male.gcw,gcw),
           gcl=c(male.gcl,gcl),
           cohort = c(letters[1:10],LETTERS[1:8]),
           sex = c(rep("Male",10),rep("Female",8))
)-> malefemale

head(malefemale)

# "Landau's h'","Directional\nConsistency","Despotism","Triangle\nTransitivity","Steepness","GC Wins","GC Losses"

# longform
malefemale %>% gather(measure,value,1:7) -> malefemale.long

malefemale.long$sex <- factor(malefemale.long$sex, levels=c("Male","Female"))

malefemale.long$measure <- factor(malefemale.long$measure,
                                  levels=c("hval","tt","dc","st","desp","gcw","gcl"))


## Plot

# for the labels
label_names <- list(
  'hval'="Landau's h'",
  'tt'="Triangle\nTransitivity",
  'dc'="Directional\nConsistency",
  'st'="Steepness",
  'desp'="Despotism",
  'gcw'="GC Wins",
  'gcl'="GC Losses"
)

my_labeller <- function(variable,value){
    return(label_names[value])
}

p.malefemale<-
  ggplot(malefemale.long, aes(sex, value, color = sex, fill = sex))+   
  #facet_grid(.~measure) + 
  facet_grid(.~measure, labeller=my_labeller) +
  geom_boxplot(size = .8, alpha = .1)+
  scale_color_manual(values = c("purple4", "orange"))+
  scale_fill_manual(values = c("purple4", "orange"))+
  ylab("Value") +
  xlab("")+
  theme_bw()+
  newggtheme +
  theme(legend.position="right")+
  scale_x_discrete(labels=NULL) +
  ylim(0,1) #+
#  ggtitle("Comparison of Summary Statistics of Male & Female Hierarchies\n")+
#  theme(plot.title = element_text(hjust = 0))

saveRDS(p.malefemale,"img/img_RDS/Fig3_malefemale.RDS")

ggsave("img/malefemale.png",p.malefemale,width=18,height=6)







## statistics
head(malefemale)

apply(malefemale[,1:7],2,function(x) wilcox.test(x ~ malefemale$sex, exact=F))



