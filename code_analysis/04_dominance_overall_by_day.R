
### Day By Day Emergence of Hierarchies

library(tidyverse)

# add day to raw dataframes
df.l.day <- lapply(df.l, add_day)

# maximum day per cohort
lapply(df.l.day, function(x) max(x$day)) %>% unlist # all 14


# split dataframes by day in consecutive order
df.l.day.split <- lapply(df.l.day, split_by_day) #nested list 8x14


# get ttri by day:
df.day.ttris <- lapply(df.l.day.split, ttri_by_day)

day.ttris <- lapply(df.day.ttris, function(x) lapply(x, function(y) y$ttri)) %>% unlist
day.ttris.pval <- lapply(df.day.ttris, function(x) lapply(x, function(y) y$pval)) %>% unlist

data.frame(ttri = day.ttris,
           pvals = day.ttris.pval,
           cohort = rep(LETTERS[1:8],each=14),
           day=1:14
           ) -> day.ttri.df

day.ttri.df$color <- ifelse(day.ttri.df$pvals<0.05, "sig","ns")

p4=ggplot(day.ttri.df, aes(x=day,y=ttri,color=color,group=cohort))+
  geom_line() +
  theme_minimal()+
  scale_color_manual(values=c("dodgerblue","red"))+
  scale_x_continuous(breaks=1:14)



## Devries over time:
df.day.behavdf <- lapply(df.l.day.split, function(x) lapply(x, behavdf))
df.day.expandrows <- lapply(df.day.behavdf, function(x) lapply(x, expandrows))
wl.mat.day <- lapply(df.day.expandrows, function(x) lapply(x, wlmatrix))
wl.mat.devries <-  lapply(wl.mat.day, function(x) lapply(x, devries))


day.hvals <- lapply(wl.mat.devries, function(x) lapply(x, function(y) y$`h-modified`)) %>% unlist

day.h.pvals <- lapply(wl.mat.devries, function(x) lapply(x, function(y) y$`p-value`)) %>% unlist

data.frame(hval = day.hvals,
           pvals = day.h.pvals,
           cohort = rep(LETTERS[1:8],each=14),
           day=1:14
) -> day.hval.df

day.hval.df$color <- ifelse(day.hval.df$pvals<0.05, "sig","ns")

p5=ggplot(day.hval.df, aes(x=day,y=hval,color=color,group=cohort))+
  geom_line() +
  theme_minimal()+
  scale_color_manual(values=c("dodgerblue","red"))+
  scale_x_continuous(breaks=1:14)


# add cohort IDs to lines...


# save figure
library(gridExtra)
p6=grid.arrange(p4,p5,nrow=1)
ggsave("img/hierarchyday.png", p6, width=10, height=5)




# which days significant first...
day.hval.df %>% 
  group_by(cohort) %>%
  filter(pvals<.05) %>%
  filter(row_number()==1)




