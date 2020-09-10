#### Fixing Errors in Raw Data Files

## Import Data
cohort87 <- read.csv("raw_data/cohort87_raw.csv", stringsAsFactors = F)
cohort88 <- read.csv("raw_data/cohort88_raw.csv", stringsAsFactors = F)
cohort89 <- read.csv("raw_data/cohort89_raw.csv", stringsAsFactors = F)
cohort90 <- read.csv("raw_data/cohort90_raw.csv", stringsAsFactors = F)
cohort91 <- read.csv("raw_data/cohort91_raw.csv", stringsAsFactors = F)
cohort92 <- read.csv("raw_data/cohort92_raw.csv", stringsAsFactors = F)
cohort93 <- read.csv("raw_data/cohort93_raw.csv", stringsAsFactors = F)
cohort94 <- read.csv("raw_data/cohort94_raw.csv", stringsAsFactors = F)

## Cohort 94 fix errors/issues 
cohort94 <- cohort94[-137, ]  # 5 vs 5 error
cohort94 <- cohort94[cohort94$Actor!="Start",] #remove starts
cohort94 <- cohort94[cohort94$Actor!="End",] #remove ends

## Cohort 93 fix errors/issues 
cohort93 <- cohort93[cohort93$Actor!="Start",]
cohort93 <- cohort93[cohort93$Actor!="End",]

## Cohort 92 fix errors/issues 
cohort92 <- cohort92[cohort92$Actor!="Start",]
cohort92 <- cohort92[cohort92$Actor!="End",]

## Cohort 91 fix errors/issues 
cohort91 <- cohort91[-400,]
cohort91 <- cohort91[-134, ] # 2 vs 2 error
cohort91 <- cohort91[-327, ] # Mistaken ID
cohort91 <- cohort91[cohort91$Actor!="Start",]
cohort91 <- cohort91[cohort91$Actor!="End",]

## Cohort 90 fix errors/issues 
cohort90 <- cohort90[-315, ] # wrong form
cohort90 <- cohort90[-314, ] # wrong form
cohort90 <- cohort90[-313, ] # wrong form
cohort90 <- cohort90[cohort90$Actor!="Start",]
cohort90 <- cohort90[cohort90$Actor!="End",]


## Cohort 89 fix errors/issues 
# cohort89 <- cohort89[-490, ] # Cait has these in, but I don't see why?
# cohort89 <- cohort89[-248, ] # Cait has these in, but I don't see why?
cohort89 <- cohort89[cohort89$Actor!="Start",]
cohort89 <- cohort89[cohort89$Actor!="End",]
cohort89[210:211,2]<-c("2","2")

## Cohort 88 fix errors/issues 
cohort88 <- cohort88[-490, ]
cohort88 <- cohort88[-248, ]
cohort88 <- cohort88[cohort88$Actor!="Start",]
cohort88 <- cohort88[cohort88$Actor!="End",]
cohort88 <- cohort88[cohort88$Recipient!= "Start", ] 


## Cohort 87 fix errors/issues 
cohort87 <- cohort87[-362, ] # wrong cohort
cohort87 <- cohort87[-361, ] # wrong cohort
cohort87[105, 4] <- 5  # wrong id
cohort87 <- cohort87[cohort87$Actor!="Start",]
cohort87 <- cohort87[cohort87$Actor!="End",]



## Import male behavioral data
cohort09 <- read.csv("raw_data/male/cohort009_jc_startend.csv", stringsAsFactors = F)
cohort10 <- read.csv("raw_data/male/cohort010_jc_startend.csv", stringsAsFactors = F)
cohort12 <- read.csv("raw_data/male/cohort012_jc_startend.csv", stringsAsFactors = F)
cohort15 <- read.csv("raw_data/male/cohort015_jc_startend.csv", stringsAsFactors = F)
cohort16 <- read.csv("raw_data/male/cohort016_jc_startend.csv", stringsAsFactors = F)
cohort17 <- read.csv("raw_data/male/cohort017_wl_startend.csv", stringsAsFactors = F)
cohort18 <- read.csv("raw_data/male/cohort018_wl_startend.csv", stringsAsFactors = F)
cohort37 <- read.csv("raw_data/male/cohort037_jc_startend.csv", stringsAsFactors = F)
cohort38 <- read.csv("raw_data/male/cohort038_jc_startend.csv", stringsAsFactors = F)
cohort45 <- read.csv("raw_data/male/cohort045_jc_startend.csv", stringsAsFactors = F)


