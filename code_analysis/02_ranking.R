## Ranking Individuals

library(ggplot2)

# Rank Matrices according to David's Scores

dss <- lapply(m.wl, compete::ds) # each cohort's David's Scores
dss.dfs <- lapply(dss, get_dsdf) # put into dataframe
dss.df.all <- do.call('rbind', Map(cbind, dss.dfs, cohort = LETTERS[1:8])) #put into long df

# Plot

p1 <- ggplot(dss.df.all, aes(x=rank, y=ds, group=cohort)) + geom_line() +
  geom_hline(yintercept = 0, col='red', lty=2) +
  xlab("David's Score") +
  ylab("Rank") +
  theme_minimal() +
  scale_x_continuous(breaks=1:12)

ggsave("img/davidscores.png", p1, width=4,height=4)
