### Matrix Plot.

# Organize matrices by David's Scores
m.wlds <- lapply(m.wl, compete::org_matrix, method="ds")


# Organize matrices by ISI
m.wlds <- lapply(m.wl, compete::org_matrix, method="ds")
m.dids <- lapply(m.di, compete::org_matrix, method="ds")
m.isi <- lapply(m.wlds, function(x) compete::isi98(x)$best_order)


bimat<-socio<-vector('list',8)
for(i in 1:8){
  mm <- m.wlds[[i]][m.isi[[i]],m.isi[[i]]]
  rownames(mm)<-colnames(mm)<-1:12
  socio[[i]] <- matrixplot(mm)+ ggtitle(LETTERS[i])  +
    ylab("Winner's Rank") +
    xlab("Loser's Rank")
}

for(i in 1:8){
  mm <- m.wlds[[i]][m.isi[[i]],m.isi[[i]]]
  rownames(mm)<-colnames(mm)<-1:12
  bimat[[i]] <- matrixplot0(mm) + ggtitle(LETTERS[i])  +
  ylab("Winner's Rank") +
  xlab("Loser's Rank")
}


library(gridExtra)
p4=grid.arrange(socio[[1]],socio[[2]],socio[[3]],socio[[4]],socio[[5]],socio[[6]],socio[[7]],socio[[8]],
             bimat[[1]],bimat[[2]],bimat[[3]],bimat[[4]],bimat[[5]],bimat[[6]],bimat[[7]],bimat[[8]],
             nrow=2)

ggsave("img/matrices.png", p4, width=25,height=6)

# saveRDS(socio,"img/img_RDS/socio.RDS")
# saveRDS(bimat,"img/img_RDS/bimat.RDS")
