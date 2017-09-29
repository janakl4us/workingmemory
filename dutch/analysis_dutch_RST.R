rm(list=ls())
setwd("___")
options(scipen=999)
library("plyr")
library("dplyr")
library("pastecs") #descriptive statistics

#-------------------------------------------------------------------#
#-----------------structure of script-------------------------------#
#--- 1. read in data------------------------------------------------#
#--- 2. analyze error rates in processing task----------------------#
#--- 3. analyze reaction times in processing task-------------------#
#--- 4. analyze performance in memory recall task-------------------#
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
#--- 1. read in data -----------------------------------------------#
#-------------------------------------------------------------------#
#read in result file
raw <- read.csv("___.csv", header=T, na.strings=c("NA","NaN", " "), strip.white = T)
raw <- raw[raw$Q0 != "Q0",]

#create subject list - second value of rep needs to be adjusted according to number of participants
for (i in length(raw$trial_type)){
  subj <- cbind(subj = (rep(1:___, each=150)))
}

#combine subject list and proc file
raw <- cbind(subj,raw)

#define factor structure
str(raw)
raw$subj <- as.factor(raw$subj)
raw$rt <- as.numeric(as.character(raw$rt))
raw$trial_index <- as.numeric(as.character(raw$trial_index))

#---------------------------------------------#
#----------analysis processing task-----------#
#---------------------------------------------#
proc <- subset(raw, raw$phase == "exp")

#code errors
proc$err = 0
proc$err[proc$key_press == "-1"] = 1 #error if no key response
proc$rt[proc$key_press == "-1"] = NA #remove RT if no key response
proc$key_press[proc$rt==0] = NA #remove key response if no RT
proc$err[(proc$key_press=="39" & proc$response=="37") | 
           (proc$key_press=="37" & proc$response=="39")] <- 1 #code errors
proc$rt[proc$rt==0] = NA #remove RT if no RT
proc$rt[proc$err==1] = NA #remove RT if error in key response

#-------------------------------------------------------------------#
#--- 2. analyze error rates in processing task----------------------#
#-------------------------------------------------------------------#
#look at error distribution (if more than 9 errors == more than 15%)
round(tapply(proc$err,INDEX=proc$subj,FUN=sum, na.rm=T))

err_mean <- ddply(proc, .(subj), summarize, merr=mean(err))
plot(err_mean$subj, err_mean$merr)

#remove participants with more than 9 errors
drop.subj = err_mean$subj[err_mean$merr>0.15]
proc = proc[!(proc$subj %in% drop.subj),]

#create data frame with subject number and error rate
res_err <- ddply(proc, .(subj), summarize,  merr=mean(err))
res_err$merr <- res_err$merr * 100 #convert to percent
hist(res_err$merr)
plot(res_err$merr)

#-------------------------------------------------------------------#
#--- 3. analyze reaction times in processing task-------------------#
#-------------------------------------------------------------------#
proc_rt = proc[!(is.na(proc$rt)),]
hist(proc_rt$rt)
plot(proc_rt$subj, proc_rt$rt)
plot(proc_rt$rt)

#------Outliers------#
#aggregate per subject
subj.M.SD = ddply(proc_rt,.(subj), summarize, subj.M=mean(rt, na.rm=T), subj.SD=sd(rt, na.rm=T))
proc_rt = merge(proc_rt, subj.M.SD, by=c("subj"))

#compute upper und lower outlier limits per subject
proc_rt$subj.min = (proc_rt$subj.M - 3*(proc_rt$subj.SD))
proc_rt$subj.max = (proc_rt$subj.M + 3*(proc_rt$subj.SD))

#mark outliers
proc_rt$outlier = 0
proc_rt$outlier[(proc_rt$rt < proc_rt$subj.min)] = 1
proc_rt$outlier[(proc_rt$rt > proc_rt$subj.max)] = 1

#remove outlier RTs
proc_rt$rt[proc_rt$outlier == 1] = NA
table(proc_rt$outlier)
proc_rt = proc_rt[!(is.na(proc_rt$rt)),]
hist(proc_rt$rt)

#remove participants with more than 9 errors
proc_rt = proc_rt[!(proc_rt$subj %in% drop.subj),]

#create data frame with subject number and mean RT
res_rt <- ddply(proc_rt, .(subj), summarize,  mrt=mean(rt))

#combine results of processing task
res <- cbind.data.frame(res_rt, res_err$merr)

#-------------------------------------------------------------------#
#--- 4. analyze performance in memory recall task-------------------#
#-------------------------------------------------------------------#

mem <- subset(raw, raw$trial_type=="survey-text" & raw$trial_index > 16)
mem <- subset(mem, select = c(subj, Q0, Q1, Q2, Q3, Q4, Q5, trial_index))

#remove participants with more than 9 errors
mem = mem[!(mem$subj %in% drop.subj),]

#convert all responses to lowercase
mem <- mutate_each(mem, funs(tolower))

#BLOCK 1
bl1 <- subset(mem, trial_index==23)

bl1$item1 <- 0
bl1$item2 <- 0
bl1$item3 <- 0

bl1$item1[grep("systeem", bl1$Q0)] <- 1
bl1$item1[grep("systeem", bl1$Q1)] <- 1
bl1$item1[grep("systeem", bl1$Q2)] <- 1
bl1$item1[grep("systeem", bl1$Q3)] <- 1
bl1$item1[grep("systeem", bl1$Q4)] <- 1
bl1$item1[grep("systeem", bl1$Q5)] <- 1

bl1$item2[grep("jurk", bl1$Q0)] <- 1
bl1$item2[grep("jurk", bl1$Q1)] <- 1
bl1$item2[grep("jurk", bl1$Q2)] <- 1
bl1$item2[grep("jurk", bl1$Q3)] <- 1
bl1$item2[grep("jurk", bl1$Q4)] <- 1
bl1$item2[grep("jurk", bl1$Q5)] <- 1

bl1$item3[grep("tunnel", bl1$Q0)] <- 1
bl1$item3[grep("tunnel", bl1$Q1)] <- 1
bl1$item3[grep("tunnel", bl1$Q2)] <- 1
bl1$item3[grep("tunnel", bl1$Q3)] <- 1
bl1$item3[grep("tunnel", bl1$Q4)] <- 1
bl1$item3[grep("tunnel", bl1$Q5)] <- 1

bl1$correct <- (bl1$item1 + bl1$item2 + bl1$item3)/3

#BLOCK 2
bl2 <- subset(mem, trial_index==34)

bl2$item1 <- 0
bl2$item2 <- 0
bl2$item3 <- 0
bl2$item4 <- 0
bl2$item5 <- 0

bl2$item1[grep("keuken", bl2$Q0)] <- 1
bl2$item1[grep("keuken", bl2$Q1)] <- 1
bl2$item1[grep("keuken", bl2$Q2)] <- 1
bl2$item1[grep("keuken", bl2$Q3)] <- 1
bl2$item1[grep("keuken", bl2$Q4)] <- 1
bl2$item1[grep("keuken", bl2$Q5)] <- 1

bl2$item2[grep("gras", bl2$Q0)] <- 1
bl2$item2[grep("gras", bl2$Q1)] <- 1
bl2$item2[grep("gras", bl2$Q2)] <- 1
bl2$item2[grep("gras", bl2$Q3)] <- 1
bl2$item2[grep("gras", bl2$Q4)] <- 1
bl2$item2[grep("gras", bl2$Q5)] <- 1

bl2$item3[grep("duivel", bl2$Q0)] <- 1
bl2$item3[grep("duivel", bl2$Q1)] <- 1
bl2$item3[grep("duivel", bl2$Q2)] <- 1
bl2$item3[grep("duivel", bl2$Q3)] <- 1
bl2$item3[grep("duivel", bl2$Q4)] <- 1
bl2$item3[grep("duivel", bl2$Q5)] <- 1

bl2$item4[grep("arm", bl2$Q0)] <- 1
bl2$item4[grep("arm", bl2$Q1)] <- 1
bl2$item4[grep("arm", bl2$Q2)] <- 1
bl2$item4[grep("arm", bl2$Q3)] <- 1
bl2$item4[grep("arm", bl2$Q4)] <- 1
bl2$item4[grep("arm", bl2$Q5)] <- 1

bl2$item5[grep("schuld", bl2$Q0)] <- 1
bl2$item5[grep("schuld", bl2$Q1)] <- 1
bl2$item5[grep("schuld", bl2$Q2)] <- 1
bl2$item5[grep("schuld", bl2$Q3)] <- 1
bl2$item5[grep("schuld", bl2$Q4)] <- 1
bl2$item5[grep("schuld", bl2$Q5)] <- 1

bl2$correct <- (bl2$item1 + bl2$item2 + bl2$item3 + bl2$item4 + bl2$item5)/5

#BLOCK 3
bl3 <- subset(mem, trial_index==41)

bl3$item1 <- 0
bl3$item2 <- 0
bl3$item3 <- 0

bl3$item1[grep("project", bl3$Q0)] <- 1
bl3$item1[grep("project", bl3$Q1)] <- 1
bl3$item1[grep("project", bl3$Q2)] <- 1
bl3$item1[grep("project", bl3$Q3)] <- 1
bl3$item1[grep("project", bl3$Q4)] <- 1
bl3$item1[grep("project", bl3$Q5)] <- 1

bl3$item2[grep("thee", bl3$Q0)] <- 1
bl3$item2[grep("thee", bl3$Q1)] <- 1
bl3$item2[grep("thee", bl3$Q2)] <- 1
bl3$item2[grep("thee", bl3$Q3)] <- 1
bl3$item2[grep("thee", bl3$Q4)] <- 1
bl3$item2[grep("thee", bl3$Q5)] <- 1

bl3$item3[grep("geluk", bl3$Q0)] <- 1
bl3$item3[grep("geluk", bl3$Q1)] <- 1
bl3$item3[grep("geluk", bl3$Q2)] <- 1
bl3$item3[grep("geluk", bl3$Q3)] <- 1
bl3$item3[grep("geluk", bl3$Q4)] <- 1
bl3$item3[grep("geluk", bl3$Q5)] <- 1

bl3$correct <- (bl3$item1 + bl3$item2 + bl3$item3)/3

#BLOCK 4
bl4 <- subset(mem, trial_index==54)

bl4$item1 <- 0
bl4$item2 <- 0
bl4$item3 <- 0
bl4$item4 <- 0
bl4$item5 <- 0
bl4$item6 <- 0

bl4$item1[grep("toekomst", bl4$Q0)] <- 1
bl4$item1[grep("toekomst", bl4$Q1)] <- 1
bl4$item1[grep("toekomst", bl4$Q2)] <- 1
bl4$item1[grep("toekomst", bl4$Q3)] <- 1
bl4$item1[grep("toekomst", bl4$Q4)] <- 1
bl4$item1[grep("toekomst", bl4$Q5)] <- 1

bl4$item2[grep("vork", bl4$Q0)] <- 1
bl4$item2[grep("vork", bl4$Q1)] <- 1
bl4$item2[grep("vork", bl4$Q2)] <- 1
bl4$item2[grep("vork", bl4$Q3)] <- 1
bl4$item2[grep("vork", bl4$Q4)] <- 1
bl4$item2[grep("vork", bl4$Q5)] <- 1

bl4$item3[grep("soldaat", bl4$Q0)] <- 1
bl4$item3[grep("soldaat", bl4$Q1)] <- 1
bl4$item3[grep("soldaat", bl4$Q2)] <- 1
bl4$item3[grep("soldaat", bl4$Q3)] <- 1
bl4$item3[grep("soldaat", bl4$Q4)] <- 1
bl4$item3[grep("soldaat", bl4$Q5)] <- 1

bl4$item4[grep("natuur", bl4$Q0)] <- 1
bl4$item4[grep("natuur", bl4$Q1)] <- 1
bl4$item4[grep("natuur", bl4$Q2)] <- 1
bl4$item4[grep("natuur", bl4$Q3)] <- 1
bl4$item4[grep("natuur", bl4$Q4)] <- 1
bl4$item4[grep("natuur", bl4$Q5)] <- 1

bl4$item5[grep("wolf", bl4$Q0)] <- 1
bl4$item5[grep("wolf", bl4$Q1)] <- 1
bl4$item5[grep("wolf", bl4$Q2)] <- 1
bl4$item5[grep("wolf", bl4$Q3)] <- 1
bl4$item5[grep("wolf", bl4$Q4)] <- 1
bl4$item5[grep("wolf", bl4$Q5)] <- 1

bl4$item6[grep("juweel", bl4$Q0)] <- 1
bl4$item6[grep("juweel", bl4$Q1)] <- 1
bl4$item6[grep("juweel", bl4$Q2)] <- 1
bl4$item6[grep("juweel", bl4$Q3)] <- 1
bl4$item6[grep("juweel", bl4$Q4)] <- 1
bl4$item6[grep("juweel", bl4$Q5)] <- 1

bl4$correct <- (bl4$item1 + bl4$item2 + bl4$item3 + bl4$item4 + bl4$item5 + bl4$item6)/6

#BLOCK 5
bl5 <- subset(mem, trial_index==59)

bl5$item1 <- 0
bl5$item2 <- 0
bl5$item3 <- 0
bl5$item4 <- 0
bl5$item5 <- 0
bl5$item6 <- 0

bl5$item1[grep("test", bl5$Q0)] <- 1
bl5$item1[grep("test", bl5$Q1)] <- 1
bl5$item1[grep("test", bl5$Q2)] <- 1
bl5$item1[grep("test", bl5$Q3)] <- 1
bl5$item1[grep("test", bl5$Q4)] <- 1
bl5$item1[grep("test", bl5$Q5)] <- 1

bl5$item2[grep("hamer", bl5$Q0)] <- 1
bl5$item2[grep("hamer", bl5$Q1)] <- 1
bl5$item2[grep("hamer", bl5$Q2)] <- 1
bl5$item2[grep("hamer", bl5$Q3)] <- 1
bl5$item2[grep("hamer", bl5$Q4)] <- 1
bl5$item2[grep("hamer", bl5$Q5)] <- 1

bl5$correct <- (bl5$item1 + bl5$item2)/2

#BLOCK 6
bl6 <- subset(mem, trial_index==70)

bl6$item1 <- 0
bl6$item2 <- 0
bl6$item3 <- 0
bl6$item4 <- 0
bl6$item5 <- 0

bl6$item1[grep("kast", bl6$Q0)] <- 1
bl6$item1[grep("kast", bl6$Q1)] <- 1
bl6$item1[grep("kast", bl6$Q2)] <- 1
bl6$item1[grep("kast", bl6$Q3)] <- 1
bl6$item1[grep("kast", bl6$Q4)] <- 1
bl6$item1[grep("kast", bl6$Q5)] <- 1

bl6$item2[grep("helm", bl6$Q0)] <- 1
bl6$item2[grep("helm", bl6$Q1)] <- 1
bl6$item2[grep("helm", bl6$Q2)] <- 1
bl6$item2[grep("helm", bl6$Q3)] <- 1
bl6$item2[grep("helm", bl6$Q4)] <- 1
bl6$item2[grep("helm", bl6$Q5)] <- 1

bl6$item3[grep("probleem", bl6$Q0)] <- 1
bl6$item3[grep("probleem", bl6$Q1)] <- 1
bl6$item3[grep("probleem", bl6$Q2)] <- 1
bl6$item3[grep("probleem", bl6$Q3)] <- 1
bl6$item3[grep("probleem", bl6$Q4)] <- 1
bl6$item3[grep("probleem", bl6$Q5)] <- 1

bl6$item4[grep("tempo", bl6$Q0)] <- 1
bl6$item4[grep("tempo", bl6$Q1)] <- 1
bl6$item4[grep("tempo", bl6$Q2)] <- 1
bl6$item4[grep("tempo", bl6$Q3)] <- 1
bl6$item4[grep("tempo", bl6$Q4)] <- 1
bl6$item4[grep("tempo", bl6$Q5)] <- 1

bl6$item5[grep("ijs", bl6$Q0)] <- 1
bl6$item5[grep("ijs", bl6$Q1)] <- 1
bl6$item5[grep("ijs", bl6$Q2)] <- 1
bl6$item5[grep("ijs", bl6$Q3)] <- 1
bl6$item5[grep("ijs", bl6$Q4)] <- 1
bl6$item5[grep("ijs", bl6$Q5)] <- 1

bl6$correct <- (bl6$item1 + bl6$item2 + bl6$item3 + bl6$item4 + bl6$item5)/5

#BLOCK 7
bl7 <- subset(mem, trial_index==79)

bl7$item1 <- 0
bl7$item2 <- 0
bl7$item3 <- 0
bl7$item4 <- 0

bl7$item1[grep("nest", bl7$Q0)] <- 1
bl7$item1[grep("nest", bl7$Q1)] <- 1
bl7$item1[grep("nest", bl7$Q2)] <- 1
bl7$item1[grep("nest", bl7$Q3)] <- 1
bl7$item1[grep("nest", bl7$Q4)] <- 1
bl7$item1[grep("nest", bl7$Q5)] <- 1

bl7$item2[grep("rat", bl7$Q0)] <- 1
bl7$item2[grep("rat", bl7$Q1)] <- 1
bl7$item2[grep("rat", bl7$Q2)] <- 1
bl7$item2[grep("rat", bl7$Q3)] <- 1
bl7$item2[grep("rat", bl7$Q4)] <- 1
bl7$item2[grep("rat", bl7$Q5)] <- 1

bl7$item3[grep("titel", bl7$Q0)] <- 1
bl7$item3[grep("titel", bl7$Q1)] <- 1
bl7$item3[grep("titel", bl7$Q2)] <- 1
bl7$item3[grep("titel", bl7$Q3)] <- 1
bl7$item3[grep("titel", bl7$Q4)] <- 1
bl7$item3[grep("titel", bl7$Q5)] <- 1

bl7$item4[grep("hitte", bl7$Q0)] <- 1
bl7$item4[grep("hitte", bl7$Q1)] <- 1
bl7$item4[grep("hitte", bl7$Q2)] <- 1
bl7$item4[grep("hitte", bl7$Q3)] <- 1
bl7$item4[grep("hitte", bl7$Q4)] <- 1
bl7$item4[grep("hitte", bl7$Q5)] <- 1

bl7$correct <- (bl7$item1 + bl7$item2 + bl7$item3 + bl7$item4)/4

#BLOCK 8
bl8 <- subset(mem, trial_index==84)

bl8$item1 <- 0
bl8$item2 <- 0

bl8$item1[grep("tafel", bl8$Q0)] <- 1
bl8$item1[grep("tafel", bl8$Q1)] <- 1
bl8$item1[grep("tafel", bl8$Q2)] <- 1
bl8$item1[grep("tafel", bl8$Q3)] <- 1
bl8$item1[grep("tafel", bl8$Q4)] <- 1
bl8$item1[grep("tafel", bl8$Q5)] <- 1

bl8$item2[grep("kwart", bl8$Q0)] <- 1
bl8$item2[grep("kwart", bl8$Q1)] <- 1
bl8$item2[grep("kwart", bl8$Q2)] <- 1
bl8$item2[grep("kwart", bl8$Q3)] <- 1
bl8$item2[grep("kwart", bl8$Q4)] <- 1
bl8$item2[grep("kwart", bl8$Q5)] <- 1

bl8$correct <- (bl8$item1 + bl8$item2)/2

#BLOCK 9
bl9 <- subset(mem, trial_index==97)

bl9$item1 <- 0
bl9$item2 <- 0
bl9$item3 <- 0
bl9$item4 <- 0
bl9$item5 <- 0
bl9$item6 <- 0

bl9$item1[grep("prijs", bl9$Q0)] <- 1
bl9$item1[grep("prijs", bl9$Q1)] <- 1
bl9$item1[grep("prijs", bl9$Q2)] <- 1
bl9$item1[grep("prijs", bl9$Q3)] <- 1
bl9$item1[grep("prijs", bl9$Q4)] <- 1
bl9$item1[grep("prijs", bl9$Q5)] <- 1

bl9$item2[grep("raam", bl9$Q0)] <- 1
bl9$item2[grep("raam", bl9$Q1)] <- 1
bl9$item2[grep("raam", bl9$Q2)] <- 1
bl9$item2[grep("raam", bl9$Q3)] <- 1
bl9$item2[grep("raam", bl9$Q4)] <- 1
bl9$item2[grep("raam", bl9$Q5)] <- 1

bl9$item3[grep("brein", bl9$Q0)] <- 1
bl9$item3[grep("brein", bl9$Q1)] <- 1
bl9$item3[grep("brein", bl9$Q2)] <- 1
bl9$item3[grep("brein", bl9$Q3)] <- 1
bl9$item3[grep("brein", bl9$Q4)] <- 1
bl9$item3[grep("brein", bl9$Q5)] <- 1

bl9$item4[grep("appel", bl9$Q0)] <- 1
bl9$item4[grep("appel", bl9$Q1)] <- 1
bl9$item4[grep("appel", bl9$Q2)] <- 1
bl9$item4[grep("appel", bl9$Q3)] <- 1
bl9$item4[grep("appel", bl9$Q4)] <- 1
bl9$item4[grep("appel", bl9$Q5)] <- 1

bl9$item5[grep("twijfel", bl9$Q0)] <- 1
bl9$item5[grep("twijfel", bl9$Q1)] <- 1
bl9$item5[grep("twijfel", bl9$Q2)] <- 1
bl9$item5[grep("twijfel", bl9$Q3)] <- 1
bl9$item5[grep("twijfel", bl9$Q4)] <- 1
bl9$item5[grep("twijfel", bl9$Q5)] <- 1

bl9$item6[grep("bed", bl9$Q0)] <- 1
bl9$item6[grep("bed", bl9$Q1)] <- 1
bl9$item6[grep("bed", bl9$Q2)] <- 1
bl9$item6[grep("bed", bl9$Q3)] <- 1
bl9$item6[grep("bed", bl9$Q4)] <- 1
bl9$item6[grep("bed", bl9$Q5)] <- 1

bl9$correct <- (bl9$item1 + bl9$item2 + bl9$item3 + bl9$item4 + bl9$item5 + bl9$item6)/6

#BLOCK 10
bl10 <- subset(mem, trial_index==106)

bl10$item1 <- 0
bl10$item2 <- 0
bl10$item3 <- 0
bl10$item4 <- 0

bl10$item1[grep("wiel", bl10$Q0)] <- 1
bl10$item1[grep("wiel", bl10$Q1)] <- 1
bl10$item1[grep("wiel", bl10$Q2)] <- 1
bl10$item1[grep("wiel", bl10$Q3)] <- 1
bl10$item1[grep("wiel", bl10$Q4)] <- 1
bl10$item1[grep("wiel", bl10$Q5)] <- 1

bl10$item2[grep("varken", bl10$Q0)] <- 1
bl10$item2[grep("varken", bl10$Q1)] <- 1
bl10$item2[grep("varken", bl10$Q2)] <- 1
bl10$item2[grep("varken", bl10$Q3)] <- 1
bl10$item2[grep("varken", bl10$Q4)] <- 1
bl10$item2[grep("varken", bl10$Q5)] <- 1

bl10$item3[grep("droom", bl10$Q0)] <- 1
bl10$item3[grep("droom", bl10$Q1)] <- 1
bl10$item3[grep("droom", bl10$Q2)] <- 1
bl10$item3[grep("droom", bl10$Q3)] <- 1
bl10$item3[grep("droom", bl10$Q4)] <- 1
bl10$item3[grep("droom", bl10$Q5)] <- 1

bl10$item4[grep("roos", bl10$Q0)] <- 1
bl10$item4[grep("roos", bl10$Q1)] <- 1
bl10$item4[grep("roos", bl10$Q2)] <- 1
bl10$item4[grep("roos", bl10$Q3)] <- 1
bl10$item4[grep("roos", bl10$Q4)] <- 1
bl10$item4[grep("roos", bl10$Q5)] <- 1

bl10$correct <- (bl10$item1 + bl10$item2 + bl10$item3 + bl10$item4)/4

#BLOCK 11
bl11 <- subset(mem, trial_index==113)

bl11$item1 <- 0
bl11$item2 <- 0
bl11$item3 <- 0
bl11$item4 <- 0

bl11$item1[grep("geest", bl11$Q0)] <- 1
bl11$item1[grep("geest", bl11$Q1)] <- 1
bl11$item1[grep("geest", bl11$Q2)] <- 1
bl11$item1[grep("geest", bl11$Q3)] <- 1
bl11$item1[grep("geest", bl11$Q4)] <- 1
bl11$item1[grep("geest", bl11$Q5)] <- 1

bl11$item2[grep("bos", bl11$Q0)] <- 1
bl11$item2[grep("bos", bl11$Q1)] <- 1
bl11$item2[grep("bos", bl11$Q2)] <- 1
bl11$item2[grep("bos", bl11$Q3)] <- 1
bl11$item2[grep("bos", bl11$Q4)] <- 1
bl11$item2[grep("bos", bl11$Q5)] <- 1

bl11$item3[grep("cultuur", bl11$Q0)] <- 1
bl11$item3[grep("cultuur", bl11$Q1)] <- 1
bl11$item3[grep("cultuur", bl11$Q2)] <- 1
bl11$item3[grep("cultuur", bl11$Q3)] <- 1
bl11$item3[grep("cultuur", bl11$Q4)] <- 1
bl11$item3[grep("cultuur", bl11$Q5)] <- 1

bl11$correct <- (bl11$item1 + bl11$item2 + bl11$item3)/3
  
#BLOCK 12
bl12 <- subset(mem, trial_index==124)

bl12$item1 <- 0
bl12$item2 <- 0
bl12$item3 <- 0
bl12$item4 <- 0
bl12$item5 <- 0

bl12$item1[grep("zomer", bl12$Q0)] <- 1
bl12$item1[grep("zomer", bl12$Q1)] <- 1
bl12$item1[grep("zomer", bl12$Q2)] <- 1
bl12$item1[grep("zomer", bl12$Q3)] <- 1
bl12$item1[grep("zomer", bl12$Q4)] <- 1
bl12$item1[grep("zomer", bl12$Q5)] <- 1

bl12$item2[grep("concept", bl12$Q0)] <- 1
bl12$item2[grep("concept", bl12$Q1)] <- 1
bl12$item2[grep("concept", bl12$Q2)] <- 1
bl12$item2[grep("concept", bl12$Q3)] <- 1
bl12$item2[grep("concept", bl12$Q4)] <- 1
bl12$item2[grep("concept", bl12$Q5)] <- 1

bl12$item3[grep("land", bl12$Q0)] <- 1
bl12$item3[grep("land", bl12$Q1)] <- 1
bl12$item3[grep("land", bl12$Q2)] <- 1
bl12$item3[grep("land", bl12$Q3)] <- 1
bl12$item3[grep("land", bl12$Q4)] <- 1
bl12$item3[grep("land", bl12$Q5)] <- 1

bl12$item4[grep("steen", bl12$Q0)] <- 1
bl12$item4[grep("steen", bl12$Q1)] <- 1
bl12$item4[grep("steen", bl12$Q2)] <- 1
bl12$item4[grep("steen", bl12$Q3)] <- 1
bl12$item4[grep("steen", bl12$Q4)] <- 1
bl12$item4[grep("steen", bl12$Q5)] <- 1

bl12$item5[grep("tempel", bl12$Q0)] <- 1
bl12$item5[grep("tempel", bl12$Q1)] <- 1
bl12$item5[grep("tempel", bl12$Q2)] <- 1
bl12$item5[grep("tempel", bl12$Q3)] <- 1
bl12$item5[grep("tempel", bl12$Q4)] <- 1
bl12$item5[grep("tempel", bl12$Q5)] <- 1

bl12$correct <- (bl12$item1 + bl12$item2 + bl12$item3 + bl12$item4 + bl12$item5)/5

#BLOCK 13
bl13 <- subset(mem, trial_index==137)

bl13$item1 <- 0
bl13$item2 <- 0
bl13$item3 <- 0
bl13$item4 <- 0
bl13$item5 <- 0
bl13$item6 <- 0

bl13$item1[grep("tuin", bl13$Q0)] <- 1
bl13$item1[grep("tuin", bl13$Q1)] <- 1
bl13$item1[grep("tuin", bl13$Q2)] <- 1
bl13$item1[grep("tuin", bl13$Q3)] <- 1
bl13$item1[grep("tuin", bl13$Q4)] <- 1
bl13$item1[grep("tuin", bl13$Q5)] <- 1

bl13$item2[grep("jeugd", bl13$Q0)] <- 1
bl13$item2[grep("jeugd", bl13$Q1)] <- 1
bl13$item2[grep("jeugd", bl13$Q2)] <- 1
bl13$item2[grep("jeugd", bl13$Q3)] <- 1
bl13$item2[grep("jeugd", bl13$Q4)] <- 1
bl13$item2[grep("jeugd", bl13$Q5)] <- 1

bl13$item3[grep("talent", bl13$Q0)] <- 1
bl13$item3[grep("talent", bl13$Q1)] <- 1
bl13$item3[grep("talent", bl13$Q2)] <- 1
bl13$item3[grep("talent", bl13$Q3)] <- 1
bl13$item3[grep("talent", bl13$Q4)] <- 1
bl13$item3[grep("talent", bl13$Q5)] <- 1

bl13$item4[grep("boter", bl13$Q0)] <- 1
bl13$item4[grep("boter", bl13$Q1)] <- 1
bl13$item4[grep("boter", bl13$Q2)] <- 1
bl13$item4[grep("boter", bl13$Q3)] <- 1
bl13$item4[grep("boter", bl13$Q4)] <- 1
bl13$item4[grep("boter", bl13$Q5)] <- 1

bl13$item5[grep("waanzin", bl13$Q0)] <- 1
bl13$item5[grep("waanzin", bl13$Q1)] <- 1
bl13$item5[grep("waanzin", bl13$Q2)] <- 1
bl13$item5[grep("waanzin", bl13$Q3)] <- 1
bl13$item5[grep("waanzin", bl13$Q4)] <- 1
bl13$item5[grep("waanzin", bl13$Q5)] <- 1

bl13$item6[grep("schaap", bl13$Q0)] <- 1
bl13$item6[grep("schaap", bl13$Q1)] <- 1
bl13$item6[grep("schaap", bl13$Q2)] <- 1
bl13$item6[grep("schaap", bl13$Q3)] <- 1
bl13$item6[grep("schaap", bl13$Q4)] <- 1
bl13$item6[grep("schaap", bl13$Q5)] <- 1

bl13$correct <- (bl13$item1 + bl13$item2 + bl13$item3 + bl13$item4 + bl13$item5 + bl13$item6)/6

#BLOCK 14
bl14 <- subset(mem, trial_index==142)

bl14$item1 <- 0
bl14$item2 <- 0

bl14$item1[grep("gewicht", bl14$Q0)] <- 1
bl14$item1[grep("gewicht", bl14$Q1)] <- 1
bl14$item1[grep("gewicht", bl14$Q2)] <- 1
bl14$item1[grep("gewicht", bl14$Q3)] <- 1
bl14$item1[grep("gewicht", bl14$Q4)] <- 1
bl14$item1[grep("gewicht", bl14$Q5)] <- 1

bl14$item2[grep("vijver", bl14$Q0)] <- 1
bl14$item2[grep("vijver", bl14$Q1)] <- 1
bl14$item2[grep("vijver", bl14$Q2)] <- 1
bl14$item2[grep("vijver", bl14$Q3)] <- 1
bl14$item2[grep("vijver", bl14$Q4)] <- 1
bl14$item2[grep("vijver", bl14$Q5)] <- 1

bl14$correct <- (bl14$item1 + bl14$item2)/2

#BLOCK 15
bl15 <- subset(mem, trial_index==151)

bl15$item1 <- 0
bl15$item2 <- 0
bl15$item3 <- 0
bl15$item4 <- 0

bl15$item1[grep("kunst", bl15$Q0)] <- 1
bl15$item1[grep("kunst", bl15$Q1)] <- 1
bl15$item1[grep("kunst", bl15$Q2)] <- 1
bl15$item1[grep("kunst", bl15$Q3)] <- 1
bl15$item1[grep("kunst", bl15$Q4)] <- 1
bl15$item1[grep("kunst", bl15$Q5)] <- 1

bl15$item2[grep("broek", bl15$Q0)] <- 1
bl15$item2[grep("broek", bl15$Q1)] <- 1
bl15$item2[grep("broek", bl15$Q2)] <- 1
bl15$item2[grep("broek", bl15$Q3)] <- 1
bl15$item2[grep("broek", bl15$Q4)] <- 1
bl15$item2[grep("broek", bl15$Q5)] <- 1

bl15$item3[grep("troost", bl15$Q0)] <- 1
bl15$item3[grep("troost", bl15$Q1)] <- 1
bl15$item3[grep("troost", bl15$Q2)] <- 1
bl15$item3[grep("troost", bl15$Q3)] <- 1
bl15$item3[grep("troost", bl15$Q4)] <- 1
bl15$item3[grep("troost", bl15$Q5)] <- 1

bl15$item4[grep("winter", bl15$Q0)] <- 1
bl15$item4[grep("winter", bl15$Q1)] <- 1
bl15$item4[grep("winter", bl15$Q2)] <- 1
bl15$item4[grep("winter", bl15$Q3)] <- 1
bl15$item4[grep("winter", bl15$Q4)] <- 1
bl15$item4[grep("winter", bl15$Q5)] <- 1

bl15$correct <- (bl15$item1 + bl15$item2 + bl15$item3 + bl15$item4)/4

scores <- cbind.data.frame(bl1$subj, bl1$correct, bl2$correct, bl3$correct, bl4$correct, bl5$correct,
                bl6$correct, bl7$correct, bl8$correct, bl9$correct, bl10$correct,
                bl11$correct, bl12$correct, bl13$correct, bl14$correct, bl15$correct)
colnames(scores)[1] <- "subj"

scores$pcs <- rowSums(scores[,c(2:16)], na.rm=T)/15

res <- cbind(res_rt, res_err$merr, scores$pcs)
colnames(res)[3] <- "merr"
colnames(res)[4] <- "pcs"

#DESCRIPTIVE STATISTICS OF SAMPLE#
stat.desc(res, norm = T)

write.csv(res, file = "results_online_reading_span.csv")
