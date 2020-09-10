## start/ends

#### Fixing Errors in Raw Data Files

## Import Data
cohort87x <- read.csv("raw_data/cohort87_raw.csv", stringsAsFactors = F)
cohort88x <- read.csv("raw_data/cohort88_raw.csv", stringsAsFactors = F)
cohort89x <- read.csv("raw_data/cohort89_raw.csv", stringsAsFactors = F)
cohort90x <- read.csv("raw_data/cohort90_raw.csv", stringsAsFactors = F)
cohort91x <- read.csv("raw_data/cohort91_raw.csv", stringsAsFactors = F)
cohort92x <- read.csv("raw_data/cohort92_raw.csv", stringsAsFactors = F)
cohort93x <- read.csv("raw_data/cohort93_raw.csv", stringsAsFactors = F)
cohort94x <- read.csv("raw_data/cohort94_raw.csv", stringsAsFactors = F)

### Start/End errors...
#rule is to add 2 mins to beginning or 2 mins to end of observation periods


## Cohort 94 fix errors/issues 
cohort94x <- cohort94x[-137, ]  # 5 vs 5 error

cohort94x <-  cohort94x[-105,] #remove false start

tmp <- cohort94x[57,] # add start
tmp[1] <- "3/21/2017 14:08:03"
tmp[2:5] <- "Start"
cohort94x <- rbind(cohort94x,tmp)



## Cohort 93 fix errors/issues 

tmp <- cohort93x[229,] # add end
tmp[1] <- "3/23/2017 18:10:38"
tmp[2:5] <- "End"
cohort93x <- rbind(cohort93x,tmp)



## Cohort 92 fix errors/issues 

tmp <- cohort92x[13,] # add end
tmp[1] <- "3/20/2017 14:00:04"
tmp[2:5] <- "End"
cohort92x <- rbind(cohort92x,tmp)

cohort92x <- cohort92x[-30,] # remove false end

tmp <- cohort92x[298,] # add end
tmp[1] <- "3/27/2017 14:02:36"
tmp[2:5] <- "End"
cohort92x <- rbind(cohort92x,tmp)

cohort92x <- cohort92x[-447,] # remove false end

cohort92x[290:310,1:6]




## Cohort 91 fix errors/issues

cohort91x <- cohort91x[-400,]
cohort91x <- cohort91x[-134, ] # 2 vs 2 error
cohort91x <- cohort91x[-327, ] # Mistaken ID

tmp <- cohort91x[145,] # add end
tmp[1] <- "3/21/2017 18:01:00"
tmp[2:5] <- "End"
cohort91x <- rbind(cohort91x,tmp)

tmp <- cohort91x[239,] # add end
tmp[1] <- "3/23/2017 15:51:56"
tmp[2:5] <- "End"
cohort91x <- rbind(cohort91x,tmp)

tmp <- cohort91x[416,] # add end
tmp[1] <- "3/30/2017 15:54:15"
tmp[2:5] <- "End"
cohort91x <- rbind(cohort91x,tmp)

cohort91x[327,1]<-"3/27/2017 17:15:00" #wrong end time


## Cohort 90 fix errors/issues 

cohort90x <- cohort90x[-315, ] # wrong form
cohort90x <- cohort90x[-314, ] # wrong form
cohort90x <- cohort90x[-313, ] # wrong form

cohort90x <- cohort90x[-464,] #remove end
cohort90x <- cohort90x[-569,] # remove end



## Cohort 89 fix errors/issues 
# cohort89 <- cohort89[-490, ] # Cait has these in, but I don't see why?
# cohort89 <- cohort89[-248, ] # Cait has these in, but I don't see why?

tmp <- cohort89x[321,] # add end
tmp[1] <- "2/24/2017 14:06:44"
tmp[2:5] <- "End"
cohort89x <- rbind(cohort89x,tmp)

tmp <- cohort89x[568,] # add end
tmp[1] <- "3/6/2017 14:36:22"
tmp[2:5] <- "End"
cohort89x <- rbind(cohort89x,tmp)



## Cohort 88 fix errors/issues 

cohort88x <- cohort88x[-490, ]
cohort88x <- cohort88x[-248, ]

tmp <- cohort88x[500,] # add end
tmp[1] <- "3/2/2017 15:41:23"
tmp[2:5] <- "End"
cohort88x <- rbind(cohort88x,tmp)

cohort88x <- cohort88x[-462,] # remove end

tmp <- cohort88x[393,] # add end
tmp[1] <- "2/26/2017 16:07:42"
tmp[2:5] <- "End"
cohort88x <- rbind(cohort88x,tmp)

cohort88x <- cohort88x[-278,]  #remove start

tmp <- cohort88x[254,] # add end
tmp[1] <- "2/22/2017 17:16:36"
tmp[2:5] <- "End"
cohort88x <- rbind(cohort88x,tmp)

cohort88x <- cohort88x[-c(255:258),] # out of observations

cohort88x <- cohort88x[-133,] #mis-id

tmp <- cohort88x[152,] # add end
tmp[1] <- "2/20/2017 17:32:06"
tmp[2:5] <- "End"
cohort88x <- rbind(cohort88x,tmp)


tmp <- cohort88x[131,] # add start
tmp[1] <- "2/20/2017 16:03:07"
tmp[2:5] <- "Start"
cohort88x <- rbind(cohort88x,tmp)

tmp <- cohort88x[244,] # add end
tmp[1] <- "2/22/2017 13:33:22"
tmp[2:5] <- "End"
cohort88x <- rbind(cohort88x,tmp)

tmp <- cohort88x[245,] # add start
tmp[1] <- "2/22/2017 16:20:12"
tmp[2:5] <- "Start"
cohort88x <- rbind(cohort88x,tmp)



## Cohort 87 fix errors/issues 
cohort87x <- cohort87x[-362, ] # wrong cohort
cohort87x <- cohort87x[-361, ] # wrong cohort
cohort87x[105, 4] <- 5  # wrong id


tmp <- cohort87x[84,] # add end
tmp[1] <- "2/21/2017 15:59:15"
tmp[2:5] <- "End"
cohort87x <- rbind(cohort87x,tmp)

tmp <- cohort87x[198,] # add end
tmp[1] <- "2/23/2017 17:59:05"
tmp[2:5] <- "End"
cohort87x <- rbind(cohort87x,tmp)

cohort87x <- cohort87x[-361,] #false end

tmp <- cohort87x[360,] # add end
tmp[1] <- "3/2/2017 14:01:11"
tmp[2:5] <- "End"
cohort87x <- rbind(cohort87x,tmp)

tmp <- cohort87x[332,] # add start
tmp[1] <- "3/2/2017 12:12:19"
tmp[2:5] <- "Start"
cohort87x <- rbind(cohort87x,tmp)

tmp <- cohort87x[167,] # add start
tmp[1] <- "2/23/2017 16:03:28"
tmp[2:5] <- "Start"
cohort87x <- rbind(cohort87x,tmp)

cohort87x[355:375,1:6]




## put into list

dfx.l <- 
  list('A'=cohort87x, 
     'B'=cohort88x,
     'C'=cohort89x,
     'D'=cohort90x,
     'E'=cohort91x,
     'F'=cohort92x,
     'G'=cohort93x,
     'H'=cohort94x
     )

