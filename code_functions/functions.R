

#' Expand rows of google forms
#'
#' @param df A dataframe with three columns "Actor" "Recipient","Behavior"
#' @return The dataframe with expanded rows, as well as a row-identifier
#'  and score variable.
#' @examples
#' expandrows(df)
#' @section Further details:
#' Expand rows that have a variable where 2 or more factors 
#' are split by commas into separate rows with
#' all other variables duplicated.
#' Make sure columns/variables are named "Actor", "Recipient"
#' The score variable will be '1' for Actors that are clear winners
#' and a '0.5' for ties between Actors and Recipients.
#' @export

expandrows <- function(df){
  
  library(splitstackshape)  
  library(data.table)  
  
  
  temp <- cSplit(cSplit(cbind(id = 1:nrow(df), df),
                        "Actor", ",", "long"), 
                 "Recipient", ",", "long")
  
  
  ## Convert "Actor" and "Recipient" to numeric
  SD <- c("Actor", "Recipient")
  temp[, (SD) := lapply(.SD, as.numeric), .SDcols = SD]
  
  
  ## Sort Actors and Recipients, and check for duplicates and any points where Actors equal Recipients  
  temp[, toDrop := duplicated(
    paste(pmin(Actor, Recipient), pmax(Actor, Recipient))) |
      Actor == Recipient, by = id]
  
  ## Create your "score" column
  temp[, score := ifelse(any(toDrop), 0.5, 1), by = id]
  
  ## Subset and drop the irrelevant columns
  
  out <-  temp[temp$toDrop!=T,]
  out <- out[,c(2,3,4,6)]
  return(out)
}


expandrows.observer <- function(df){
  
  library(splitstackshape)  
  library(data.table)  
  
  temp <- cSplit(cSplit(cbind(id = 1:nrow(df), df),
                        "Actor", ",", "long"), 
                 "Recipient", ",", "long")
  
  
  ## Convert "Actor" and "Recipient" to numeric
  SD <- c("Actor", "Recipient")
  temp[, (SD) := lapply(.SD, as.numeric), .SDcols = SD]
  
  
  ## Sort Actors and Recipients, and check for duplicates and any points where Actors equal Recipients  
  temp[, toDrop := duplicated(
    paste(pmin(Actor, Recipient), pmax(Actor, Recipient))) |
      Actor == Recipient, by = id]
  
  ## Create your "score" column
  temp[, score := ifelse(any(toDrop), 0.5, 1), by = id]
  
  ## Subset and drop the irrelevant columns
  
  out <-  temp[temp$toDrop!=T,]
  out <- out %>% select(Actor,Recipient,score,Observer)
  return(out)
}


expandrows.male <- function(df){
  
  library(splitstackshape)  
  library(data.table)  
  
  
  temp <- cSplit(cSplit(cbind(id = 1:nrow(df), df),
                        "Actor", ",", "long"), 
                 "Recipient", ",", "long")
  
  
  ## Convert "Actor" and "Recipient" to numeric
  SD <- c("Actor", "Recipient")
  temp[, (SD) := lapply(.SD, as.numeric), .SDcols = SD]
  
  
  ## Sort Actors and Recipients, and check for duplicates and any points where Actors equal Recipients  
  temp[, toDrop := duplicated(
    paste(pmin(Actor, Recipient), pmax(Actor, Recipient))) |
      Actor == Recipient, by = id]
  
  ## Create your "score" column
  temp[, score := ifelse(any(toDrop), 0.5, 1), by = id]
  
  ## Subset and drop the irrelevant columns
  
  out <-  temp[temp$toDrop!=T,]
  out <- out[,c(2,3,5)]
  return(out)
}


## Another version for the days analysis
# these only vary in which columns are returned.
expandrows2 <- function(df){
  
  library(splitstackshape)  
  library(data.table)  
  
  
  temp <- cSplit(cSplit(cbind(id = 1:nrow(df), df),
                        "Actor", ",", "long"), 
                 "Recipient", ",", "long")
  
  
  ## Convert "Actor" and "Recipient" to numeric
  SD <- c("Actor", "Recipient")
  temp[, (SD) := lapply(.SD, as.numeric), .SDcols = SD]
  
  
  ## Sort Actors and Recipients, and check for duplicates and any points where Actors equal Recipients  
  temp[, toDrop := duplicated(
    paste(pmin(Actor, Recipient), pmax(Actor, Recipient))) |
      Actor == Recipient, by = id]
  
  ## Create your "score" column
  temp[, score := ifelse(any(toDrop), 0.5, 1), by = id]
  
  ## Subset and drop the irrelevant columns
  
  out <-  temp[temp$toDrop!=T,]
  out <- out[,c(2:5)]
  return(out)
}




# Makes a three column dataframe - with Actor, Recipient, Behavior
# Removes ties
# Only keep one row per behavioral event (e.g.Fighting,Chasing is one event)

behavdf <- function(df){
  dfx <- df[c(2,4,3)] #keeps three columns,actor,recipient,behavior
  
  dfx$Behavior<-as.character(dfx$Behavior)
  
  dfx <- do.call(rbind, 
                 with(dfx, Map(expand.grid, 
                               Actor = Actor,
                               Recipient = Recipient,
                               Behavior = strsplit(Behavior, ", ")
                 )))
  
  return(dfx)
}

# Create behavior dataframe including time of observations
# Makes a three column dataframe - with Actor, Recipient, Behavior
# Removes ties
# Only keep one row per behavioral event (e.g.Fighting,Chasing is one event)

behavdf1 <- function(df){
  
  dfx <- df %>% #filter(Actor!='Start', Actor!="End") %>%
    select(Actor,Recipient,day,obstime,Behavior,ztime_secs)
  
  dfx$Behavior<-as.character(dfx$Behavior)
  
  dfx <- do.call(rbind, 
                 with(dfx, Map(expand.grid, 
                               Actor = Actor,
                               Recipient = Recipient,
                               day = day,
                               obstime = obstime,
                               ztime_secs = ztime_secs,
                               Behavior = strsplit(Behavior, ", ")
                 )))
  
  return(dfx)
}

## gets all behaviors by all individuals keeping times
behavdf_all <- function(df){
  x <- behavdf1(df)
  x <- separate_rows(separate_rows(x, Actor),Recipient) 
  
  x1 <- x %>% filter(Actor!=Recipient)
  x2 <- x %>% filter(Actor=='Start' | Actor=="End")
  
  x3 <- rbind(x1,x2) %>% arrange(ztime_secs,Actor,Recipient,Behavior)
  
  return(x3)
}




#Gets win-loss matrix  from expandrows output


wlmatrix <- function(df){
  df<-df[df$score==1][,c(1,2)]
  return(get_wl_matrix(as.matrix(df)))
}

#Gets binary matrix  from expandrows output
dimatrix <- function(df){
  df<-df[df$score==1][,c(1,2)]
  wlm <- get_wl_matrix(as.matrix(df))
  return(get_di_matrix(wlm))
}


# Calculate despotism
despotism <- function(x) {
  rev(sort(round(100*(rowSums(x)/sum(x)),2)))
}


# Calculate p-value from steepness test
getStp.pval <- function(x){
  a <- steepness::steeptest(x,rep=1000)
  return( sum(a$Stpsim>=a$Stp)/1000 )
}


# Make dataframe out of David's Scores
get_dsdf <- function(d){
  d.df <- data.frame(rank=1:12, ds = rev(d[order(d)]))
  return(d.df)
}


### Making Matrix Plots

# takes wl matrix and order of id's and creates ggplot matrix.


matrixplot <- function(m, mylevs=NULL, lowcolor="white",highcolor="red1"){
  
  library(ggplot2)
  
  #make the df we will use for plotting
  m.dat <- reshape2::melt(m)
  m.dat <- data.frame(m.dat)
  m.dat <- m.dat[complete.cases(m.dat),] #removing NAs
  colnames(m.dat)<-c("Actor","Recipient","value")
  
  if(is.null(mylevs)) { mylevs = rownames(m)}
  
  #reorder the levels of the y-axis so plots properly
  m.dat$Recipient <- factor(m.dat$Recipient, levels=mylevs)
  m.dat$Actor <- factor(m.dat$Actor, levels = rev(mylevs))
  m.dat[m.dat == 0] <- NA
  
  
  #plot
  p1<-ggplot(m.dat, aes(Recipient, Actor, fill = value)) + 
    geom_tile(colour="black", 
              size=0.5, stat="identity") + 
    geom_text(data=m.dat, aes(Recipient, Actor, label = value), color="black", size=rel(3.5)) +
    scale_fill_gradient(low = lowcolor, high = highcolor, space = "Lab", na.value = "white", guide = "colourbar") +
    scale_x_discrete(expand = c(0, 0), position = "top") +
    scale_y_discrete(expand = c(0, 0)) +
    xlab("Loser") + 
    ylab("Winner") +
    theme(axis.text.x = element_text(vjust = 1),
          axis.text.y = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"),
          axis.text = element_text(color="#3C3C3C", size=rel(1.1)),
          legend.position = "none"        
    ) 
  return(p1)
}


# Dichotomized Matrix

matrixplot0 <- function(m,mylevs=NULL,lowcolor="white",highcolor="firebrick3"){
  
  dcs <- m / (m + t(m))
  m.dat_dc <- reshape2::melt(dcs)
  colnames(m.dat_dc)[3]<-"DC"
  
  m1 <- get_di_matrix(m)
  m.dat1 <- reshape2::melt(m1)
  m.dat1 <- data.frame(m.dat1)
  m.dat1 <- merge(m.dat1, m.dat_dc)
  m.dat1$value <- ifelse(is.na(m.dat1$DC), NA, m.dat1$value)
  
  
  #reorder the levels of the y-axis so plots properly
  if(is.null(mylevs)){mylevs = rownames(m)}
  m.dat1$Recipient <- factor(m.dat1$Recipient, levels=mylevs)
  m.dat1$Actor <- factor(m.dat1$Actor, levels = rev(mylevs))
  
  m.dat1$DC[m.dat1$value == 0] <- NA
  m.dat1$value[m.dat1$value == 0] <- NA
  
  
  p2<-ggplot(m.dat1, aes(Recipient, Actor)) + 
    geom_tile(colour="black", aes(fill=DC),
              size=0.5, stat="identity") + 
    geom_text(data=m.dat1, aes(Recipient, Actor, label = value), color="black", size=rel(3.5)) +
    scale_fill_gradient(low = lowcolor, high = highcolor, space = "Lab", na.value = "white", guide = "colourbar") +
    scale_x_discrete(expand = c(0, 0), position='top') +
    scale_y_discrete(expand = c(0, 0)) +
    xlab("Loser") + 
    ylab("Winner") +
    theme(axis.text.x = element_text(vjust = 1),
          axis.text.y = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"),
          axis.text = element_text(color="#3C3C3C", size=rel(1.1)),
          legend.position = "none"        
    ) 
  return(p2)
}


# add day column to each wl raw dataframe
add_day <- function(xx){
  xx$ts <- as.POSIXct(strptime(as.character(xx$Timestamp),'%m/%d/%Y %H:%M:%S'))
  xx <- xx %>% mutate(day = lubridate::yday(xx$ts) - min(lubridate::yday(xx$ts))+1)  
  return(xx)
}


# get times into dataframe and arrange.
get_ztime <- function(xx){
  xx$ts <- as.POSIXct(strptime(as.character(xx$Timestamp),'%m/%d/%Y %H:%M:%S'))
  xx <- xx %>% mutate(day = lubridate::yday(xx$ts) - min(lubridate::yday(xx$ts))+1)  
  
  xx$hour <- lubridate::hour(xx$ts)-12
  xx$minute <- lubridate::minute(xx$ts)
  xx$seconds <-  lubridate::second(xx$ts)
  xx$ztime_secs  <- (xx$hour*60*60) + (xx$minute*60) + xx$seconds
  
  xx <- xx %>% arrange(day,ztime_secs)
  xx <- xx %>% select(-Timestamp)
  if(!is.null(xx$Notes))
    xx<-xx %>% select(-Notes)
  if(!is.null(xx$Outside.Normal.Observation.))
    xx<-xx %>% select(-Outside.Normal.Observation.)
  return(xx)
}

# split each dataframe in list into  observations
get_obs <- function(xx){
  v <- which(xx$Actor=="Start")
  return(split(xx, findInterval(1:nrow(xx), v)))
}


#split dataframes by successive days
split_by_day <- function(df){
  out<-vector('list',14)
  for(i in 1:14){ out[[i]] <-  df[df$day<=i,] }
  return(out)
}


# Function to get ttri by day
ttri_by_day <- function(l){
  q <- lapply(l, function(x) expandrows(x[c(2,4,3)]))
  q.di <- lapply(q, dimatrix) 
  q.tt <- lapply(q.di,ttri_test)
  return(q.tt)
}


## Function to ensure square 12x12 matrix for all groups
square_matrix <- function(mat){
  matx<-matrix(0,12,12)
  rownames(matx)<-colnames(matx)<-as.character(1:12)
  matx.melt <- reshape2::melt(matx)
  mat.melt <- reshape2::melt(mat)
  colnames(matx.melt)<-colnames(mat.melt)
  maty.melt<-rbind(matx.melt,mat.melt)
  maty.melt  <- maty.melt %>% group_by(Actor,Recipient) %>% summarise(total = sum(value))
  mat.out <- reshape2::acast(maty.melt, Actor~Recipient, value.var="total")
  return(mat.out)
}


# Get matrices of fighting, chasing, mounting for each cohort
behav_matrices <- function(a){
  aa<-behavdf(a)
  b<-expandrows(aa)
  
  bf <- b[b$Behavior=="Fighting",] #fighting matrix
  bc <- b[b$Behavior=="Chasing",] #chasing matrix
  bm <- b[b$Behavior=="Mounting",]#mounting matrix
  
  bf.m <- wlmatrix(bf)
  bc.m <- wlmatrix(bc)
  bm.m <- wlmatrix(bm)
  
  bf.m <- square_matrix(bf.m)
  bc.m <- square_matrix(bc.m)
  bm.m <- square_matrix(bm.m)
  
  return(list('fighting' = bf.m, 'chasing' = bc.m, 'mounting' = bm.m))
}


### Making half box/half point plot 

# somewhat hackish solution to:
# https://twitter.com/EamonCaddigan/status/646759751242620928
# based mostly on copy/pasting from ggplot2 geom_violin source:
# https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r

library(ggplot2)
library(dplyr)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )





######################################################
#source("https://raw.githubusercontent.com/tidyverse/ggplot2/2f3fef72e140d34210daa9d95917c77b19e89669/R/geom-boxplot.r")
library(grid)

ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

geom_boxplot2 <- function(mapping = NULL, data = NULL,
                          stat = "boxplot", position = "dodge",
                          ...,
                          outlier.colour = NULL,
                          outlier.color = NULL,
                          outlier.fill = NULL,
                          outlier.shape = 19,
                          outlier.size = 1.5,
                          outlier.stroke = 0.5,
                          outlier.alpha = NULL,
                          notch = FALSE,
                          notchwidth = 0.5,
                          varwidth = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBoxplot2 <- ggproto("GeomBoxplot2", Geom,
                        setup_data = function(data, params) {
                          data$width <- data$width %||%
                            params$width %||% (resolution(data$x, FALSE) * 0.9)
                          
                          if (!is.null(data$outliers)) {
                            suppressWarnings({
                              out_min <- vapply(data$outliers, min, numeric(1))
                              out_max <- vapply(data$outliers, max, numeric(1))
                            })
                            
                            data$ymin_final <- pmin(out_min, data$ymin)
                            data$ymax_final <- pmax(out_max, data$ymax)
                          }
                          
                          # if `varwidth` not requested or not available, don't use it
                          if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
                            data$xmin <- data$x - data$width / 2
                            data$xmin2 <- data$x - data$width / 4
                            data$xmax <- data$x + data$width / 2
                          } else {
                            # make `relvarwidth` relative to the size of the largest group
                            data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
                            data$xmin <- data$x - data$relvarwidth * data$width / 2
                            data$xmin2 <- data$x - data$relvarwidth * data$width / 4
                            data$xmax <- data$x + data$relvarwidth * data$width / 2
                          }
                          data$width <- NULL
                          if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL
                          
                          data
                        },
                        
                        draw_group = function(data, panel_params, coord, fatten = 2,
                                              outlier.colour = NULL, outlier.fill = NULL,
                                              outlier.shape = 19,
                                              outlier.size = 1.5, outlier.stroke = 0.5,
                                              outlier.alpha = NULL,
                                              notch = FALSE, notchwidth = 0.5, varwidth = FALSE) {
                          
                          common <- data.frame(
                            colour = data$colour,
                            size = data$size,
                            linetype = data$linetype,
                            fill = alpha(data$fill, data$alpha),
                            group = data$group,
                            stringsAsFactors = FALSE
                          )
                          
                          whiskers <- data.frame(
                            x = c(data$x,data$x,data$xmin2,data$xmin2),
                            xend = c(data$x,data$x,data$x,data$x),
                            y = c(data$upper, data$lower,data$ymax,data$ymin),
                            yend = c(data$ymax, data$ymin,data$ymax,data$ymin),
                            alpha = NA,
                            common,
                            stringsAsFactors = FALSE
                          )
                          
                          
                          box <- data.frame(
                            xmin = data$xmin,
                            xmax = data$x,
                            ymin = data$lower,
                            y = data$middle,
                            ymax = data$upper,
                            ynotchlower = ifelse(notch, data$notchlower, NA),
                            ynotchupper = ifelse(notch, data$notchupper, NA),
                            notchwidth = notchwidth,
                            alpha = data$alpha,
                            common,
                            stringsAsFactors = FALSE
                          )
                          
                          if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
                            outliers <- data.frame(
                              y = data$outliers[[1]],
                              x = data$x[1],
                              colour = outlier.colour %||% data$colour[1],
                              fill = outlier.fill %||% data$fill[1],
                              shape = outlier.shape %||% data$shape[1],
                              size = outlier.size %||% data$size[1],
                              stroke = outlier.stroke %||% data$stroke[1],
                              fill = NA,
                              alpha = outlier.alpha %||% data$alpha[1],
                              stringsAsFactors = FALSE
                            )
                            outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
                          } else {
                            outliers_grob <- NULL
                          }
                          
                          ggname("geom_boxplot2", grobTree(
                            outliers_grob,
                            GeomSegment$draw_panel(whiskers, panel_params, coord),
                            GeomCrossbar$draw_panel(box, fatten = fatten, panel_params, coord)
                          ))
                        },
                        
                        draw_key = draw_key_boxplot,
                        
                        default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                          alpha = NA, shape = 19, linetype = "solid"),
                        
                        required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax")
)

#################################################################
stat_boxplot <- function(mapping = NULL, data = NULL,
                         geom = "boxplot", position = "dodge",
                         ...,
                         coef = 1.5,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplot,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      coef = coef,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBoxplot <- ggproto("StatBoxplot", Stat,
                       required_aes = c("x", "y"),
                       non_missing_aes = "weight",
                       
                       setup_params = function(data, params) {
                         params$width <- params$width %||% (resolution(data$x) * 0.75)
                         
                         if (is.double(data$x) && !has_groups(data) && any(data$x != data$x[1L])) {
                           warning(
                             "Continuous x aesthetic -- did you forget aes(group=...)?",
                             call. = FALSE)
                         }
                         
                         params
                       },
                       
                       compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5) {
                         qs <- c(0, 0.25, 0.5, 0.75, 1)
                         
                         if (!is.null(data$weight)) {
                           mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                           stats <- as.numeric(stats::coef(mod))
                         } else {
                           stats <- as.numeric(stats::quantile(data$y, qs))
                         }
                         names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                         iqr <- diff(stats[c(2, 4)])
                         
                         outliers <- data$y < (stats[2] - coef * iqr) | data$y > (stats[4] + coef * iqr)
                         if (any(outliers)) {
                           stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
                         }
                         
                         if (length(unique(data$x)) > 1)
                           width <- diff(range(data$x)) * 0.9
                         
                         df <- as.data.frame(as.list(stats))
                         df$outliers <- list(data$y[outliers])
                         
                         if (is.null(data$weight)) {
                           n <- sum(!is.na(data$y))
                         } else {
                           # Sum up weights for non-NA positions of y and weight
                           n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                         }
                         
                         df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                         df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
                         
                         df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                         df$width <- width
                         df$relvarwidth <- sqrt(n)
                         df
                       }
)


### newggtheme
library(ggplot2)

newggtheme <- theme(
  plot.title = element_text(hjust =.5, vjust = 1, size = rel(2.3)), 
  panel.background = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  plot.background = element_blank(), 
  text = element_text(color = "gray20", size = 10), 
  axis.text = element_text(size = rel(1)), 
  axis.text.x = element_text(color = "gray20", size = rel(1.8)), 
  axis.text.y = element_text(color = "gray20", size = rel(1.8)), 
  axis.title.x = element_text(size = rel(2.5), vjust = 0), 
  axis.title.y = element_text(size = rel(2.5), vjust = 1), 
  axis.ticks.y = element_blank(), 
  axis.ticks.x = element_blank(), 
  strip.text.x = element_text(size = rel(2.3)),
  legend.position = "none",
  legend.key=element_rect(fill=NA),
  legend.title = element_blank(),
  legend.text=element_text(size=rel(1.7))
)

newggtheme_with_legend <- theme(
  plot.title = element_text(hjust =.5, vjust = 1, size = rel(2.3)), 
  panel.background = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  plot.background = element_blank(), 
  text = element_text(color = "gray20", size = 10), 
  axis.text = element_text(size = rel(1)), 
  axis.text.x = element_text(color = "gray20", size = rel(1.8)), 
  axis.text.y = element_text(color = "gray20", size = rel(1.8)), 
  axis.title.x = element_text(size = rel(2.5), vjust = 0), 
  axis.title.y = element_text(size = rel(2.5), vjust = 1), 
  axis.ticks.y = element_blank(), 
  axis.ticks.x = element_blank(), 
  strip.text.x = element_text(size = rel(2.3)),
  #legend.position = "none",
  legend.key=element_rect(fill=NA),
  legend.title = element_blank(),
  legend.text=element_text(size=rel(1.7))
)



## QAP test
QAPtest <- function(mat1,mat2){
  diag(mat1)<-NA
  diag(mat2)<-NA
  qap.fit <- sna::qaptest(list(mat1, mat2), gcor, g1=1, g2=2, reps = 10000)
  return(list('r'=qap.fit$testval,'pval'=qap.fit$pgreq))
}


## Mantel Test
MANTEL <- function(x, y, ...) {
  diag(mat1)<-NA
  diag(mat2)<-NA
  out<-  vegan::mantel(x,y, ...)
  return(list('r'=out$statistic,'pval'=out$signif))
}

#get every nth element of vector
nth_element <- function(a,N){ a[seq(1, length(a), N)]}




### get total per day frequencies of behavior
get_perhour <- function(aadf){
  
  day<-unique(aadf$day)
  aggr<-c("Fighting","Mounting","Chasing")
  a1<-aadf %>% group_by(day) %>% summarise(totaltime = sum(obstime[Recipient=="Start"]))
  aadf$Behavior<-factor(aadf$Behavior, levels=c("Start","End","Fighting","Mounting","Chasing","Induce-flee","Subordinate"))
  
  if(nrow(aadf)>2){
    a2<-aadf %>% filter(Actor!="Start",Actor!="End") %>% group_by(day,Behavior)  %>% tally() %>% 
      complete(day,Behavior,fill=list(n=0))  %>% filter(Behavior %in% aggr)
    
    perhour <- a2 %>% full_join(a1) %>% mutate(prop = (n/totaltime)*60*60)
    return(perhour)
  } else {
    
    a3<-data.frame(day=day,Behavior=c("Fighting","Mounting","Chasing"),n=0)
    perhour <- a3 %>% full_join(a1) %>% mutate(prop = (n/totaltime)*60*60)
    return(perhour)
  }
}

## do the per hour collapsing across days
get_perhour_eachday<-function(aa){
  do.call('rbind',lapply(lapply(aa, get_perhour),as.data.frame))%>%
    group_by(day,Behavior) %>% 
    summarise(totaln = sum(n), totaltime = sum(totaltime)) %>%
    mutate(propn = (totaln/totaltime)*60*60)
}


## Get 14 days worth of male dfs
male_dfs <- function(xx){
  # get day  # keep first 14 days
  # remove start/ends
  add_day(xx) %>% 
    filter(day<=14, Actor!="Start", Actor!="End") %>%
    select(Actor,Recipient) %>%
    mutate_if(is.character, as.factor) -> xxx
  
  # expand actor/recipient columns
  xxxx<-expandrows.male(xxx)
  return(xxxx)
}

## Function to get glicko scores from wl dataframes
get_glickos <- function(aa, cval=3){
  PlayerRatings::glicko(
    as.data.frame(aa[,c(1,2,4)]) %>%
      mutate(event=row_number()) %>% 
      select(event,Actor,Recipient,score),
    history=T,plot=F,cval=cval)
}


## Glickoplot

plotglicko <- function(robj, mycolors=c("black", "grey", "orange", "red"), 
                       ltypes=c(1,2,3,1,2,3,1,2,3,1,2,3), thetitle="",  linewd=1, ylim1=1000,ylim2=3250,
                       ndays=1){
  
  x<-as.data.frame(unlist(robj$history))  
  n <- length(x)/4
  
  x.ratings<-x[,1:n]
  x.deviations<-x[,(1+n):(n+n)]
  
  #longform the data
  x.ratingsmelt<-reshape2::melt(x.ratings)
  
  ids<-rownames(x.ratings)       #to make id column
  x.ratingsmelt$ids<-rep(ids, n)  #making id column
  
  l.ids<-length(ids)
  x.ratingsmelt$event<-rep(1:n, each=l.ids) 
  
  
  #add ranks
  xrn<-as.data.frame(x.ratings[n])
  colnames(xrn)<-c("finalrating")
  
  x.ratingsmelt$rank<-rank(-xrn$finalrating, ties.method="random")
  
  #make ids1 a factor with levels defined by rank
  x.ratingsmelt1 <- data.frame(ids=unique(x.ratingsmelt$ids),rank=unique(x.ratingsmelt$rank))
  x.ratingsmelt1 <- x.ratingsmelt1[order(x.ratingsmelt1$rank),]
  x.ratingsmelt$ids1 <- factor(x.ratingsmelt$ids,levels=x.ratingsmelt1$ids)
  
  #define color palette, multiple options (see below)
  colourCount <-length(unique(x.ratingsmelt$ids))
  getPalette = colorRampPalette(mycolors)
  
  
  ### now plot using ids1 instead of ids.
  p1<-ggplot(x.ratingsmelt, aes(x = event, y = value, col=ids1, linetype=ids1)) +
    scale_colour_manual(values = getPalette(colourCount)) +
    scale_linetype_manual(values=ltypes) +
    ylab("Glicko Rating") +
    xlab("Event") +
    ggtitle(thetitle)+
    ylim(ylim1,ylim2)+
    geom_line(lwd = linewd) +
    theme(plot.title = element_text(hjust = 0, vjust = 1, size = rel(1.7)), 
          panel.background = element_blank(), 
          plot.background = element_blank(), 
          panel.grid.major.y = element_line(color = "gray75",linetype = 'dotted'), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor = element_blank(), 
          strip.background  = element_blank(),
          strip.text = element_text(size=rel(1.1)),
          text = element_text(color="gray20", size=10),
          axis.text = element_text(size=rel(1.0)),
          axis.text.x = element_text(color="gray20", size=rel(1.0)),
          axis.text.y = element_text(color="gray20", size=rel(1.0)),
          axis.title.x = element_text(size=rel(1.0), vjust=0),
          axis.title.y = element_text(size=rel(1.0), vjust=1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  
  
  return(p1)
}


### Outlier detection.

library(outliers)

grubbs.flag <- function(x) {
  outliers <- NULL
  test <- x
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}


# iterative outlier detection

outlierdetection <- function(aa){
  aa[which(grubbs.flag(aa)$Outlier)]<-NA
  return(aa)
}


## get consecutive days dataframes
consec_days <- function(x){
  maxday <- max(x$day)
  xi<-vector('list',maxday)
  for(i in 1:maxday){
    xi[[i]] <- x[x$day<=i,]
  }
  return(xi)    
}


## Get David Scores each day  
ds_day<-function(X){
  wlmats <- lapply(X, function(y) get_wl_matrix(y[,c(2,4)]))
  allds <- lapply(wlmats, compete::ds)
  
  #to make sure all vectors are equal length
  helper1<-function(z){       
    v1<-rep(0,12)
    names(v1)<-as.character(1:12)
    v3 <- c(v1, z)
    v4<-tapply(v3, names(v3), sum)
    return(v4)
  }
  
  allds <- lapply(allds, helper1)
  xxx <- reshape2::melt(do.call(cbind,allds))
  xxx<-as.data.frame(xxx)
  colnames(xxx)<-c("Mouse.ID","Day","DavidS")
  return(xxx)
}


## get group comparison from brms result for estrus state data

estrus_comparison<- function(model,mydata){
  coefs <- as.matrix(model)[, c(1,3,4,5)]
  newdata <- data.frame(State = levels(as.factor(mydata$State)))
  # A Tukeys contrast matrix
  
  # table(newdata$x) - gets the number of replicates of each level
  tuk.mat <- multcomp::contrMat(n = table(newdata$State), type = "Tukey")
  Xmat <- model.matrix(~State, data = newdata)
  pairwise.mat <- tuk.mat %*% Xmat
  
  
  my_posterior<-  coefs %*% t(pairwise.mat) %>% as.data.frame() %>% gather(comps,value,1:4)
  
  comps = broom::tidyMCMC(coefs %*% t(pairwise.mat), conf.int = TRUE, conf.method = "HPDinterval") %>% mutate(Sig=ifelse(conf.low*conf.high>0,"Sig","-")) %>% 
    mutate(poster=paste(round(estimate,2),
                        paste("[",round(conf.low,2),", ",round(conf.high,2),"]",sep="")))
  colnames(comps)<-c("term" , "Estimate","std.error", "Lower CI",  "High CI" ,"Sig"    ,   "poster"   )
  return(comps)
}
