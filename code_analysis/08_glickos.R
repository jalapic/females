
# Get glicko scores
df.glickos <-  lapply(df.wl, get_glickos, cval=3)


# Make glicko plot
pglickos<-NULL
for(i in 1:length(df.glickos)){
  pglickos[[i]]<- plotglicko(df.glickos[[i]], ylim1=1500,ylim2=3000)+
    ggtitle(LETTERS[[i]])+theme(plot.title = element_text(hjust = 0))
  
}


library(gridExtra)
plotglickos <- grid.arrange(pglickos[[1]],pglickos[[2]],pglickos[[3]],pglickos[[4]],
             pglickos[[5]],pglickos[[6]],pglickos[[7]],pglickos[[8]],nrow=2 )

ggsave("img/glickos.png",plotglickos,width=20,height=9)

# saveRDS(pglickos,"imag/img_RDS/pglickos.RDS")
