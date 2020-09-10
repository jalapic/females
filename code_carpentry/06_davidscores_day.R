## Assessment of Estrus State vs Behavior----


# Per day David's Scores:

df.l.days <- lapply(df.l, add_day)
df.l.days.consec <- lapply(df.l.days, consec_days)
df.l.days.consec.wl <- lapply(df.l.days.consec, function(y) lapply(y, expandrows2))
df.l.days.consec.ds <-   lapply(df.l.days.consec.wl, ds_day)

# Per day Estrus State:
rankestrus


## Add this in to data frames of per day win loss ?



