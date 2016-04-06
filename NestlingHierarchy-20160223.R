# Isabel Winney
# 15th October 2015

# making a publication standard script for the analysis
# of whether the nestling mass hierarchy or whether the
# natal or social brood affects nestling activity.
# (some other covariates relating to condition and maternal
# effects are also considered)

# this is a shortened version of script makingdataset-mar2015.R
# combined with addingend-mar2015-correctedfactorlevels-apr2015.R

################################################################

# read in the data:

rm(list=ls())
setwd("C:/Users/Issie/SkyDrive/PhD/masterdatasheets")

# nestling mass and tarsus data:
nestmass <- read.table("nestling-d2d12May2014-plustarsusJan2015.txt",
                       header=T)

head(nestmass)
str(nestmass)

# the age here is worked out as the capture date minus the
# hatch date, so the nestlings that are measured on days 2
# and 12 have 'ages' in this data set of 1 and 11.

# a small number of nestlings were not measured for mass:

which(is.na(nestmass$mass))
length(which(is.na(nestmass$mass)))
nestmass[which(is.na(nestmass$mass)),]
# 17 cases where mass is missing.


# create full subsets for each age, 
# but MINUS THOSE WITHOUT MASS so that these individuals
# are not considered in calculations of the mean brood
# mass
nestmass$one <- 1
head(nestmass)

nestmassNA <- nestmass[-which(is.na(nestmass$mass)),]
which(is.na(nestmassNA$mass))
summary(nestmassNA)

massd2 <- subset(nestmassNA, nestmassNA$age==1)

massd12 <- subset(nestmassNA, nestmassNA$age==11)

head(massd2)
head(massd12)


# are there any repeat measures?
table(table(massd2$birdid))
table(table(massd12$birdid))
# nope

# how many tarsus measurements am I missing?
length(which(is.na(massd12$tarsus)))
# 23. Quite a few more but not terrible. I was
# expecting far more to be missing...



# are any nestlings dead when measured?

which(massd2$capturedate==massd2$deathdate)
which(massd12$capturedate==massd12$deathdate)

# three day 2s. Look in more detail:

massd2[which(massd2$capturedate==massd2$deathdate),]

# looking at the database, all three of these nestlings died
# due to accidents at measuring.
# now do I exclude these nestlings or not?
# go to the database. All of these nestlings died
# on the day of capture due to observer accidents.
# Therefore, they are relevant to the natal hierarchy
# on day 2. They  aren't relevant to the social hierarchy 
# on day 2.

# make a subset that excludes them for the calculation of the
# mean social brood mass, but use the original data set for the
# calculation of the natal brood mass:

massd2minusdead <- massd2[-which(massd2$capturedate==massd2$deathdate),]
summary(massd2minusdead)
summary(massd2)
length(massd2minusdead[,1])
length(massd2[,1])
which(massd2minusdead$birdid==1427)
which(massd2minusdead$birdid==1439)
which(massd2minusdead$birdid==1595)
# ok, so I have removed three birds, the maxima
# and minima for mass are the same, and the three
# birds I wanted removed are not in the data set :)


# now I need the mean mass
# first on day 2:

# mass of the natal brood
natald2 <- tapply(massd2$mass, INDEX=massd2$natal, FUN=mean)
head(natald2)

nam <- as.data.frame(names(natald2))
d2natal <- cbind(nam, natald2)
head(d2natal)

names(d2natal) <- c("natal", "meand2")
head(d2natal)

# mass of the social brood
# update 14th March 2015 - this uses the social
# brood masses minus the nestlings that died during
# measurements
sociald2 <- tapply(massd2minusdead$mass, INDEX=massd2minusdead$social, FUN=mean)
head(sociald2)

nam <- as.data.frame(names(sociald2))
d2social <- cbind(nam, sociald2)
head(d2social)

names(d2social) <- c("social", "meand2")
head(d2social)

# then day 12
natald12 <- tapply(massd12$mass, INDEX=massd12$natal, FUN=mean)
head(natald12)

nam <- as.data.frame(names(natald12))
d12natal <- cbind(nam, natald12)
head(d12natal)

names(d12natal) <- c("natal", "meand12")
head(d12natal)

# social d12
sociald12 <- tapply(massd12$mass, INDEX=massd12$social, FUN=mean)
head(sociald12)

nam <- as.data.frame(names(sociald12))
d12social <- cbind(nam, sociald12)
head(d12social)

names(d12social) <- c("social", "meand12")
head(d12social)


# I want to merge these masses with my arena data
# I have added all 'to edge' measures for day 12 
# arena tests because I want to look at the from
# edge measures in the analysis
# ar <- read.table("AR-jan2015.txt", header=T)

# due to corrections for the mass of five nestlings on
# 16th October 2015 (see notes below) the data set was
# updated:
ar <- read.table("AR-oct2015-d12masscorrected-wdordercorrected.txt", header=T)

head(ar)
str(ar)

ar$d2mass <- massd2$mass[match(ar$birdid, massd2$birdid)]

summary(ar$d2mass)

# it seems there are many individuals where I 
# do not have mass on d2. Never mind.

ar$d2natalmean <- d2natal$meand2[match(ar$natal, d2natal$natal)]
ar$d2socialmean <- d2social$meand2[match(ar$social, d2social$social)]
ar$d12natalmean <- d12natal$meand12[match(ar$natal, d12natal$natal)]
ar$d12socialmean <- d12social$meand12[match(ar$social, d12social$social)]

summary(ar)

# why so many where the day two mass is missing?
head(ar[which(is.na(ar$d2mass)),])
tail(ar[which(is.na(ar$d2mass)),])

# so these nestlings are a mixture. Some were not hatched by the
# time of day two measurements, though some of the early ones were
# estimated to be a specific age early on, and I measured them
# for activity anyway. Later in my time I would have ruled 
# against the latter ones being measured but there you go.

ar$d2deltanat <- ar$d2mass - ar$d2natalmean
ar$d2deltasoc <- ar$d2mass - ar$d2socialmean
ar$d12deltanat <- ar$mass - ar$d12natalmean
ar$d12deltasoc <- ar$mass - ar$d12socialmean



# I want to know the cross-foster status of my nestlings

# 22nd October 2015
# I am now doing this direct from the nestlings alive in
# each year from 2011-2015 that were of stage 2 or 3 when
# last seen i.e. no chigg or egg:

nestlings2011to15 <- read.table("nestlings2011-15-lastseenstage2or3-20151022.txt", 
                   header=T)

head(nestlings2011to15)
summary(nestlings2011to15)
# some broods do not have a hatch date. These broods were
# first found with well developed nestlings.

# remove nestlings that died on day 2:
which(nestlings2011to15$AgeAtDeath==0)

nestlingsalive <- nestlings2011to15[-which(nestlings2011to15$AgeAtDeath==0),]
summary(nestlingsalive)
length(nestlingsalive[,1])
length(nestlings2011to15[,1])

# so those five nestlings have been removed.

# now, who was cross-fostered?

nestlingsalive$fostered <- nestlingsalive$BroodRef!=nestlingsalive$RearingBrood
head(nestlingsalive)
table(nestlingsalive$fostered)

# all those with FALSE for this should have 
# a foster brood of NA
table(nestlingsalive$fostered[which(is.na(nestlingsalive$FosterBrood))])

# and all those with TRUE should have a brood
table(nestlingsalive$fostered[which(nestlingsalive$FosterBrood>0)])

length(nestlingsalive[,1])
1165+1174
# excellent. All present and accounted for.

# so, all those with TRUE are fostered and all those with
# FALSE are not fostered:

nestlingsalive$fos <- ifelse(nestlingsalive$fostered==T,1,0)
table(nestlingsalive$fos, nestlingsalive$fostered)

# and now I can ask what proportion of nestlings in 
# a REARING brood were cross-fostered:

fostd <- aggregate(nestlingsalive$fos, 
                   list(nestlingsalive$RearingBrood),
                   FUN=mean)

head(fostd)
summary(fostd)

# any brood with a zero is all non-fostered:
fostd$FN <- ifelse(fostd$x==0, "N", "F")


# and I can add this to my data frame:
ar$FN <- fostd$FN[match(ar$social, fostd$Group.1)]
summary(ar$FN)
str(ar$FN)
table(ar$FN)

# now, is this the same as the data frame I have
# made in the past of FPLN?

FPLN <- read.table("FPLN-May2014.txt", header=T)

head(FPLN)
str(FPLN)

ar$FPLN <- FPLN$FPLN[match(ar$birdid, FPLN$BirdID)]

table(ar$FN, ar$FPLN)

# wow. Yes, it is. So I can use either :D
# duh, of course it is even for that one nestling
# that has the same foster as rearing because that
# nestling has been found and measured in its
# rearing brood despite being suspected of being
# cross-fostered it is not possible to say which
# nestling it was...

table(ar$FPLN)
table(ar$FPLN, ar$cf)
table(ar$FN, ar$cf)

# who are the three that don't match the pattern?

ar$cf[which(ar$FPLN=="F")] # so it is the 43:45 matches
which(ar$FPLN=="F")

ar[246:252,]

# er, what? 
# oh. Oh dear. Well, this is difficult. This individual
# is a result of a fieldwork mix-up where we cross-fostered
# two nests but cut all the wrong nails, and in the end
# do not know who we cross-fostered where. In effect, I
# have no clear idea where this nestling was from, but
# we meant to cross-foster the broods. I am still unclear
# which nestling came from where --> maybe try removing
# broods 1366 and 1368 from the final analysis because
# of uncertainty in who is who?
# Same applies to a brood in 2013 where we mixed them 
# all up (brood 1522). I can't remember others off the 
# top of my head.
# ok, so I went through all the notes of nestling captures
# from 2011-2013 and wrote down a full list of nestlings
# where the notes said to check the nestling's true
# identity. Most of these should be ok but I could
# run an analysis that does not have them to check.
# these nestlings are:

# 5342, 5344, 6283, 6284, 6285, 6286, 6287, 6288, 6289,
# 6279, 6280, 6281, 6282, 6869, 6937, 6979, 6986.
# on top of this, broods L009, L011, and M064, which
# includes brood ids 1366, 1368, and 1522.
# and I have also gone through all the brood forms to
# view any general notes and found the following extra
# nestlings that could be mixed up:
# 6300, 6874.

# so, checked in three ways. One, broods that I remembered
# as mix-ups. Two, going through all nestling capture notes
# from 2011-2013. Three, going through all offspring notes
# for broods from 2011-2013 by going to broods --> offspring
# in the data base.



# get myself a minedge. This is how far nestlings move
# once they have hit the edge of the arena

ar$minedge <- ar$total - ar$edge

# I only want to look at day 12 arenas. This
# will simplify my analysis and interpretation.

ar12 <- subset(ar, ar$age==12)



# are the masses in the database and the masses in my data
# sheet the same?

head(massd12)

ar12$d12masscheck <- massd12$mass[match(ar12$birdid, massd12$birdid)]

# 16th October 2015 - this should now come out with no
# differences because I corrected the data set as per
# notes below:

summary(ar12$mass - ar12$d12masscheck)
# some are different. Which?

length(which(ar12$mass - ar12$d12masscheck != 0))
ar12[which(ar12$mass - ar12$d12masscheck !=0),]
# five birds. CM0022, CM0050, CM0380, CM0382, and
# CM0526. All these birds had a mass that was out
# by a little (up to 3.5 g).
# So on 16th October 2015 these masses were corrected
# along with the order for the first day 12 test for
# CM0526, which was changed from 1 to 2.



### ### ###


# and I want a mix of cross-fostering classifications:
ar12$CF <- ifelse(ar12$FPLN=="N", "N", ifelse(ar12$FPLN=="F", "N", "Y"))

table(ar12$cf, ar12$CF)
# that is interesting...
cbind(ar12$cf, ar12$CF, ar12$natal, ar12$social)



ar12$crossF <- ar12$natal==ar12$social
table(ar12$cf, ar12$crossF)

# right. So the number assigned as cross-fostered based
# on their brood ids is the same as the number marked
# as cross-fostered. Does it segregate with FPLN?

table(ar12$cf, ar12$FPLN)
# good.


# add tarsus measurements:

ar12$tarsus <- massd12$tarsus[match(ar12$birdid, massd12$birdid)]
summary(ar12$tarsus)

# in this subset, how many am I missing tarsus for?
length(unique(ar12$birdid[which(is.na(ar12$tarsus))]))


# how many individuals are missing one or both mass measurements?
length(unique(ar12$birdid[which(is.na(ar12$mass))]))
length(unique(ar12$birdid[which(is.na(ar12$d2mass))]))



# from this data set, I remove nestlings for which I do not 
# know mass on day two:
ar123 <- subset(ar12, ar12$d2mass!="NA")
summary(ar123)

# and make sure I can match up my data later
ar123$datalabel <- seq(1,length(ar123[,1]),1)
ar123$datalabel

# however, this reveals the interesting problem that I 
# have different subsets of individuals for which I don't
# know mass on day 2 compared to those where I don't know
# mass on day 12. I can do separate hierarchy calculations
# for each set.

# or their total activity:
ar123 <- subset(ar123, ar123$total!="NA")
summary(ar123)

# and have a separate subset where I also don't know
# mass on day 12:
ar123m12 <- subset(ar123, ar123$mass!="NA")
summary(ar123m12)
str(ar123m12)


# are the social and natal broods in my arena data set the
# same as the ones in the mass data set?
head(nestmass)

ar$natal2 <- nestmass$natal[match(ar$birdid, nestmass$birdid)]
summary(ar$natal2-ar$natal)

ar$social2 <- nestmass$social[match(ar$birdid, nestmass$birdid)]
summary(ar$social2-ar$social)

# well, that is one relief. Those brood IDs are right.

summary(ar)
# though six nestlings (or one nestling and its mass) are missing
# from the nestmass data set.

#######################################################################


# take linear residuals of the relationship between
# tarsus and mass, as a measure of condition.

# therefore, do residual mass from single measures of
# the individuals tested in the arena test for which
# we have both mass and tarsus measurements:

ar123m12tarsus <- subset(ar123m12, ar123m12$tarsus!="NA")
summary(ar123m12tarsus)
summary(ar123m12$tarsus)
ar123m12tarsus$birddupl <- duplicated(ar123m12tarsus$birdid)

table(ar123m12tarsus$birddupl)
head(ar123m12tarsus)

singletarsusmass <- subset(ar123m12tarsus, ar123m12tarsus$birddupl==F)
summary(singletarsusmass)
table(singletarsusmass$birddupl)

plot(singletarsusmass$mass, singletarsusmass$tarsus)

# now take the residuals:
lmt <- lm(mass~tarsus, data=singletarsusmass)
plot(lmt)





summary(lmt)

singletarsusmass$residmass2 <- residuals(lmt)
plot(singletarsusmass$mass, singletarsusmass$residmass2)
singletarsusmass[which(singletarsusmass$residmass>6),]
# the original data book says this nestling's tarsus
# and mass are true. The nestling's leg was too thick
# to ring so perhaps something was wrong.

# add to the original data frame:

ar123m12tarsus$residmass2 <- singletarsusmass$residmass2[
  match(ar123m12tarsus$birdid,singletarsusmass$birdid)]

which(ar123m12tarsus$residmass2>6)
ar123m12[which(ar123m12tarsus$residmass2>6),]
massd12[which(massd12$social==1389),]

plot(ar123m12tarsus$residmass2, log(ar123m12tarsus$total+0.5))
plot(ar123m12tarsus$mass, log(ar123m12tarsus$total+0.5))
plot(ar123m12tarsus$tarsus, log(ar123m12tarsus$total+0.5))

# it looks like the mass relationship is really a being
# bigger relationship.



###################################################################

###################################################################
# I need to know certain numbers for writing in my
# paper.
# How many nestlings am I missing day 2 mass for?


ardu <- subset(ar123m12tarsus, ar123m12tarsus$birddupl==F)
summary(ardu$d2mass)
summary(ardu$mass)
summary(ardu$tarsus)
summary(ardu)
# interestingly, wd1to3 are all ones :) nice check that this
# works
table(ar123m12tarsus$wd1to3)
table(ardu$wd1to3)

ardu$deld2 <- ardu$d2deltasoc - ardu$d2deltanat

# make CF4. The purpose of this is to separate those non-
# fostered nestlings in non-fostered nests from the rest
# who were affected by the manipulation in some way, and 
# then amongst those who were affected, sort out which
# of them moved place in the competitive hierarchy:

table(ardu$crossF, ardu$FPLN)

ardu$CFN <- ifelse(ardu$FPLN=="N",
                       "N", ifelse(ardu$deld2==0,
                                   "cfN", "cfY"))

table(ardu$CFN)


plot(ardu$tarsus, ardu$total, log="y")
plot(ardu$mass, ardu$total, log="y")
plot(ardu$residmass, ardu$total, log="y")

summary(ardu$d2mass + ardu$mass)
table(ardu$CF4, ardu$FPLN)


# I want to account for the nestlings moving further to
# reach a favoured square to finish on:

end1 <- table(ar123m12tarsus$end)
end1

end2 <- end1/length(ar123m12tarsus$end)
end2

end2 <- t(end2)
end2 <- data.frame(end2)
end2

# now let's add this covariate to the data frame...
ar123m12tarsus$endprob <- end2$Freq[match(ar123m12tarsus$end, end2$Var2)]
table(ar123m12tarsus$endprob)
head(ar123m12tarsus)

plot(jitter(ar123m12tarsus$endprob, amount=0.005), 
     ar123m12tarsus$total, log="y")

# interesting. So those nestlings that do not move are
# more likely to be facing unpopular squares, but nestlings
# that end on popular squares make sure they move. Uh, well
# that makes sense both if they aim for the popular squares
# and if unpopular squares are more likely to be faced by
# only nestlings that fail to move...

str(ar123m12tarsus)
# make the hierarchies in to simple numeric rather than
# double numeric values:
summary(ar123m12tarsus$d2deltanat)
ar123m12tarsus$d2deltanat <- as.numeric(ar123m12tarsus$d2deltanat)
summary(ar123m12tarsus$d2deltanat)

summary(ar123m12tarsus$d12deltanat)
ar123m12tarsus$d12deltanat <- as.numeric(ar123m12tarsus$d12deltanat)
summary(ar123m12tarsus$d12deltanat)

summary(ar123m12tarsus$d2deltasoc)
ar123m12tarsus$d2deltasoc <- as.numeric(ar123m12tarsus$d2deltasoc)
summary(ar123m12tarsus$d2deltasoc)

summary(ar123m12tarsus$d12deltasoc)
ar123m12tarsus$d12deltasoc <- as.numeric(ar123m12tarsus$d12deltasoc)
summary(ar123m12tarsus$d12deltasoc)


# some of the fixed effects need to be sorted:

# I need to extract the cohort of each individual
# from the date on which the test was carried out:

ar123m12tarsus$cohort <- as.POSIXlt(as.Date(ar123m12tarsus$date, "%d/%m/%Y"))$year+1900
table(ar123m12tarsus$cohort)

# Also, in preliminary analyses, I found no difference between
# observers EH and O, and  observer O also had a lower sample
# size than would be ideal for a factor. Therefore, observers
# EH and O are amalgamated:
table(ar123m12tarsus$releaser)

ar123m12tarsus$releaser2 <- ifelse(ar123m12tarsus$releaser=="EH",
                            "EHO",
                            ifelse(ar123m12tarsus$releaser=="O",
                                   "EHO", ar123m12tarsus$releaser))
str(ar123m12tarsus$releaser2)
table(ar123m12tarsus$releaser2)


# I need to group the noise categories, because the
# 'changing' noise category is too small. In the supplement
# for Winney et al, this category is amalgamated with
# no noise because they are similar:

table(ar123m12tarsus$noise)
ar123m12tarsus$noise2 <- ifelse(ar123m12tarsus$noise=="c", "2", ar123m12tarsus$noise)
table(ar123m12tarsus$noise2)

# lastly, I need to group my test locations in to tests
# in Steve's shed and tests in the lambing shed:

table(ar123m12tarsus$where)

ar123m12tarsus$location <- ifelse(ar123m12tarsus$where=="Lmbar", "LM",
                            "SS")

table(ar123m12tarsus$location)

###################################################################
###################################################################


# now I need to split my data in to nestlings that
# reach the arena edge and nestlings that do not,
# based on whether they have a value for 'edge' 
# (movement up to reaching the edge of the arena,
# can be equal to 'total' i.e. total movement, has
# a minimum value of 1 since minimum activity is 1)
# or not (never reached the edge, so no value kept
# for reaching the edge)
summary(ar123m12tarsus$edge)
head(ar123m12tarsus$edge)

# want to make sure I can cut out duplicates
ar123m12tarsus$dupl <- duplicated(ar123m12tarsus$birdid)

# birds that don't reach the edge have NA for edge

edgey <- ar123m12tarsus[-which(is.na(ar123m12tarsus$edge)),]
edgen <- ar123m12tarsus[which(is.na(ar123m12tarsus$edge)),]

head(edgey)
table(edgey$edge)
head(edgen)
table(edgen$edge)
summary(edgen$edge)


# those that don't reach the edge, I need their
# start value (0), end value, whether this is
# their true end/death (yes=1), and whether they
# hit the wall (no=0)

# 14th March 2015: changed residmass to residmass2
# and used the new social hierarchy on day 2 (see
# makingdataset-mar2015.R)

edge1 <- data.frame("birdid"=edgen$birdid,
               "social"=edgen$social,
               "natal"=edgen$natal,
               "start"=0,
               "end"=edgen$total,
               "finish"=1,
               "wall"=0,
               "d2deltanat"=edgen$d2deltanat,
               "d12deltanat"=edgen$d12deltanat,
               "d2deltasoc"=edgen$d2deltasoc,
               "d12deltasoc"=edgen$d12deltasoc,
               "FPLN"=edgen$FPLN,
               "d12mass"=edgen$mass,
               "dupl"=edgen$dupl,
               "endprob"=edgen$endprob,
               "d2mass"=edgen$d2mass,
               "tarsus"=edgen$tarsus,
               "residmass"=edgen$residmass2,
               "datalabel"=edgen$datalabel,
               "location"=edgen$location,
               "time"=edgen$time,
               "wd1to3"=edgen$wd1to3,
               "socbroodsz"=edgen$socbroodsz,
               "cohort"=edgen$cohort,
               "releaser"=edgen$releaser2,
               "noise"=edgen$noise2,
               "comp"=edgen$comp)

head(edge1)
tail(edge1)
edge1
str(edge1)
summary(edge1)

# those that reach the edge, I need two rows of
# data from each one. The first is what they do
# before they reach the edge, the second is what
# they do 

# for each before the wall, I need a start value (0), 
# end value, whether this is their true end/death
# (no, not for this part), and whether they hit the wall
# (still no for this first set of information)

# is this activity to the edge of the arena
# the end point for these nestlings? 1 = yes
#edgey$finish <- ifelse(edgey$minedge==0, 1, 0)
#table(edgey$minedge, edgey$finish)

# I don't think the value 'finish' is used for anything
# because all the ones that hit the wall are considered
# to have a record for after hitting the wall, which is
# their finish point. They could finish at the wall though
# but then they would be like the nestlings that never 
# touched the wall. Check this with Mirre...

edge2 <- data.frame("birdid"=edgey$birdid,
               "social"=edgey$social,
               "natal"=edgey$natal,
               "start"=0,
               "end"=edgey$edge,
               "finish"=0,
               "wall"=0,
               "d2deltanat"=edgey$d2deltanat,
               "d12deltanat"=edgey$d12deltanat,
               "d2deltasoc"=edgey$d2deltasoc,
               "d12deltasoc"=edgey$d12deltasoc,
               "FPLN"=edgey$FPLN,
               "d12mass"=edgey$mass,
               "dupl"=edgey$dupl,
               "endprob"=edgey$endprob,
               "d2mass"=edgey$d2mass,
               "tarsus"=edgey$tarsus,
               "residmass"=edgey$residmass2,
               "datalabel"=edgey$datalabel,
               "location"=edgey$location,
               "time"=edgey$time,
               "wd1to3"=edgey$wd1to3,
               "socbroodsz"=edgey$socbroodsz,
               "cohort"=edgey$cohort,
               "releaser"=edgey$releaser2,
               "noise"=edgey$noise2,
               "comp"=edgey$comp)

head(edge2)
tail(edge2)
str(edge2)
summary(edge2)

# now, for every nestling that hit the wall I need
# a second record for its values beyond hitting the wall.
# I need a start value (edge), end value (total+1), whether 
# this is their true end/death (yes=1), and whether they 
# hit the wall (yes)


edge3 <- data.frame("birdid"=edgey$birdid,
               "social"=edgey$social,
               "natal"=edgey$natal,
               "start"=edgey$edge,
               "end"=edgey$total+1,
               "finish"=1,
               "wall"=1,
               "d2deltanat"=edgey$d2deltanat,
               "d12deltanat"=edgey$d12deltanat,
               "d2deltasoc"=edgey$d2deltasoc,
               "d12deltasoc"=edgey$d12deltasoc,
               "FPLN"=edgey$FPLN,
               "d12mass"=edgey$mass,
               "dupl"=edgey$dupl,
               "endprob"=edgey$endprob,
               "d2mass"=edgey$d2mass,
               "tarsus"=edgey$tarsus,
               "residmass"=edgey$residmass2,
               "datalabel"=edgey$datalabel,
               "location"=edgey$location,
               "time"=edgey$time,
               "wd1to3"=edgey$wd1to3,
               "socbroodsz"=edgey$socbroodsz,
               "cohort"=edgey$cohort,
               "releaser"=edgey$releaser2,
               "noise"=edgey$noise2,
               "comp"=edgey$comp)
head(edge3)
tail(edge3)
str(edge3)
summary(edge3)

# now combine these in to a data frame:

names(edge1)==names(edge2)
names(edge2)==names(edge3)
names(edge1)==names(edge3)

edgeall <- rbind(edge1, edge2, edge3)

summary(edgeall)
str(edgeall)

# looking good! 

# I'd quite like something that represents the
# difference from natal to social hierarchies
# within days, but also the between age difference
# in social hierarchy would be nice:

edgeall$deld2 <- edgeall$d2deltasoc - edgeall$d2deltanat

edgeall$deld2d12 <- edgeall$d12deltasoc - edgeall$d2deltasoc

summary(edgeall)
str(edgeall)
head(edgeall)
edgeall[610:620,]

# Now, separate my nestlings in to those from non-
# fostered nests versus the rest, and amongst the
# rest, split them between those that change hierarchy
# place on day two and those that do not:
edgeall$CFN <- ifelse(edgeall$FPLN=="N",
                       "N", ifelse(edgeall$deld2==0,
                                   "cfN", "cfY"))

table(edgeall$CFN)
# I have called it CFN because I am referencing the N
# nestlings as the reference level --> make sure this 
# is so:
edgeall$CFN <- as.factor(edgeall$CFN)

contrasts(edgeall$CFN) 
contrasts(edgeall$CFN) <- contr.treatment(levels(edgeall$CFN), 3)
contrasts(edgeall$CFN) 

table(edgeall$CFN, edgeall$FPLN)

# good. The non-fostered brood nestlings are all N.

# based on preliminary data analyses (detailed in Winney
# et al heritability of personality paper supplement),
# there are fixed effects that could influence the
# activity of nestlings. I have already added many of
# these to the data frame, but some need changing.

# the fixed effects I need to consider are (from my
# supplement in the heritability paper):
# test location as factor
# environmental noise as factor
# time of day as continuous
# order of testing within a day as factor
# social brood size as continuous
# cohort as a factor
# releaser as a factor
# compromised as a factor
# likelihood of finishing at a given part of the arena
# edge as continuous.

# so, which of these is continuous, factor, etc in
# the data frame?

str(edgeall)

# make wd1to3 (within day test order) a factor:

edgeall$factorwd1to3 <- as.factor(edgeall$wd1to3)

# make cohort a factor:

edgeall$factorcohort <- as.factor(edgeall$cohort)

# make compromised a factor:

edgeall$factorcomp <- as.factor(edgeall$comp)


# of the factors, are the reference levels the levels
# with the largest sample size? / the one required for
# hypothesis testing (this applies for the cross-fostering
# category):
table(edgeall$location)
contrasts(edgeall$location)
contrasts(edgeall$location) <- contr.treatment(levels(edgeall$location),2)
contrasts(edgeall$location)


table(edgeall$noise)
contrasts(edgeall$noise)
contrasts(edgeall$noise) <- contr.treatment(levels(edgeall$noise),2)
contrasts(edgeall$noise)



table(edgeall$factorwd1to3)
contrasts(edgeall$factorwd1to3)
# this is as desired - first test as the reference
# level.

table(edgeall$factorcohort)
contrasts(edgeall$factorcohort)
contrasts(edgeall$factorcohort) <- contr.treatment(levels(edgeall$factorcohort),3)
contrasts(edgeall$factorcohort)



table(edgeall$releaser)
contrasts(edgeall$releaser)
contrasts(edgeall$releaser) <- contr.treatment(levels(edgeall$releaser),2)
contrasts(edgeall$releaser)


table(edgeall$factorcomp)
contrasts(edgeall$factorcomp)


# excellent.



######
######

# and the set with only cross-foster affected and moved
edgecfYe <- subset(edgeall, edgeall$CFN=="cfY")


edgecfYe$zd2deltasoc <- scale(edgecfYe$d2deltasoc)
edgecfYe$zd12deltasoc <- scale(edgecfYe$d12deltasoc)

edgecfYe$zdeld2 <- scale(edgecfYe$deld2)
edgecfYe$zdeld2d12 <- scale(edgecfYe$deld2d12)

edgecfYe$ztime <- scale(edgecfYe$time)
edgecfYe$zsocbroodsz <- scale(edgecfYe$socbroodsz)
edgecfYe$zendprob <- scale(edgecfYe$endprob)
edgecfYe$zd2mass <- scale(edgecfYe$d2mass)
edgecfYe$zd12mass <- scale(edgecfYe$d12mass)
edgecfYe$zresidmass <- scale(edgecfYe$residmass)

edgecfYe$factorbirdid <- as.factor(edgecfYe$birdid)
edgecfYe$factorsocial <- as.factor(edgecfYe$social)
edgecfYe$factornatal <- as.factor(edgecfYe$natal)


# and for a finishing part of my investigation, I wanted
# to know whether it is the social brood or the social
# mother that changes an offspring's behaviour. Therefore,
# add the social mother to the data set:


parents <- read.table("brood-parent-july2014-myDBparents.txt", header=T)
head(parents)
str(parents)
summary(parents)


# draw out female id for the NATAL brood:

edgeall$natalmother <- parents$SocialMumID[match(edgeall$natal, parents$BroodRef)]
head(edgeall$natalmother)
summary(edgeall$natalmother)
edgeall$natalmother

edgeall$factormum <- as.factor(edgeall$natalmother)
str(edgeall$factormum)
head(edgeall$factormum)

# !!! no missing values!!!

which(parents$BroodRef==1315)
parents[1311,]
edgeall$factormum[1]

# ok good. That matches.

# add this natal mother to the cross-fostered subset data:


edgecfYe$natalmother <- parents$SocialMumID[match(edgecfYe$natal, parents$BroodRef)]
head(edgecfYe$natalmother)
summary(edgecfYe$natalmother)
edgecfYe$natalmother
str(edgecfYe$natalmother)

edgecfYe$factormum <- as.factor(edgecfYe$natalmother)
str(edgecfYe$factormum)
head(edgecfYe$factormum)

# !!! no missing values!!!

which(parents$BroodRef==1359)
parents[1355,]
edgecfYe$socialmother[1]





###################################################################
###################################################################

# are any of my hierarchies better as factors, or
# as squared terms, wrt model fit?


# to do this properly, I will use a dataset made of
# only single observations per individual:
edgesingle <- subset(edgeall, edgeall$dupl==F)

head(edgesingle)



# I scale and centre each hierarchy:

edgesingle$zd2deltanat <- scale(edgesingle$d2deltanat)
edgesingle$zd12deltanat <- scale(edgesingle$d12deltanat)
edgesingle$zd2deltasoc <- scale(edgesingle$d2deltasoc)
edgesingle$zd12deltasoc <- scale(edgesingle$d12deltasoc)

summary(edgesingle)

# and for the delta hierarchies, make categories
# so that I can compare continuous with factor fits:

library(gtools)

# four
edgesingle$catnatd2 <- quantcut(edgesingle$d2deltanat, seq(0,1,1/4))
table(edgesingle$catnatd2)

str(edgesingle$catnatd2)


# five
edgesingle$catnatd2.2 <- quantcut(edgesingle$d2deltanat, seq(0,1,1/5))
table(edgesingle$catnatd2.2)

str(edgesingle$catnatd2.2)

###
edgesingle$catsocd2 <- quantcut(edgesingle$d2deltasoc, seq(0,1,1/4))
table(edgesingle$catsocd2)


edgesingle$catsocd2.2 <- quantcut(edgesingle$d2deltasoc, seq(0,1,1/5))
table(edgesingle$catsocd2.2)




### day 12's
edgesingle$catnatd12 <- quantcut(edgesingle$d12deltanat, seq(0,1,1/4))
table(edgesingle$catnatd12)


edgesingle$catnatd12.2 <- quantcut(edgesingle$d12deltanat, seq(0,1,1/5))
table(edgesingle$catnatd12.2)




edgesingle$catsocd12 <- quantcut(edgesingle$d12deltasoc, seq(0,1,1/4))
table(edgesingle$catsocd12)




edgesingle$catsocd12.2 <- quantcut(edgesingle$d12deltasoc, seq(0,1,1/5))
table(edgesingle$catsocd12.2)




# day two mass and residual mass:

edgesingle$zresidmass <- scale(edgesingle$residmass)
edgesingle$zd2mass <- scale(edgesingle$d2mass)

#################



# now the mass difference hierarchies. Have to test
# four models: keeping the hierarchy linear, quadratic,
# as a four-level factor, as a five-level factor

# I use a data set of single values so that the
# significance of the fixed effect is not confounded by
# other variables.

# this is done in coxph because there are no repeated
# measures - I am just focussing on fixed effects

library(coxme)

model1 <- coxph(Surv(start, end, finish)~catnatd2+
                   strata(wall), 
                 data=edgesingle)

model2 <- coxph(Surv(start, end, finish)~zd2deltanat+
                   strata(wall), 
                 data=edgesingle)

model3 <- coxph(Surv(start, end, finish)~zd2deltanat+
                   I((zd2deltanat)^2) + strata(wall), 
                 data=edgesingle)

model4 <- coxph(Surv(start, end, finish)~catnatd2.2+
                   strata(wall), 
                 data=edgesingle)

anova(model1, model2, model3, model4)
# All these models are equivalent: keep the linears



model5 <- coxph(Surv(start, end, finish)~catnatd12+
                   strata(wall), 
                 data=edgesingle)

model6 <- coxph(Surv(start, end, finish)~zd12deltanat+
                   strata(wall), 
                 data=edgesingle)

model7 <- coxph(Surv(start, end, finish)~zd12deltanat+
                   I((zd12deltanat)^2) + strata(wall), 
                 data=edgesingle)

model8 <- coxph(Surv(start, end, finish)~catnatd12.2+
                   strata(wall), 
                 data=edgesingle)

anova(model5, model6, model7, model8)
# linear
# though there is strictly no need to do this one because
# I don't use the day 12 natal hierarchy in an analysis.



model9 <- coxph(Surv(start, end, finish)~catsocd2+
                   strata(wall), 
                 data=edgesingle)

model10 <- coxph(Surv(start, end, finish)~zd2deltasoc+
                   strata(wall), 
                 data=edgesingle)

model11 <- coxph(Surv(start, end, finish)~zd2deltasoc+
                   I((zd2deltasoc)^2) + strata(wall), 
                 data=edgesingle)

model12 <- coxph(Surv(start, end, finish)~catsocd2.2+
                   strata(wall), 
                 data=edgesingle)

anova(model9, model10, model11, model12)

# so this implies that models 9 and 12 are a slight
# improvement on the linear or quadratic models.

anova(model10, model11, model12, model9)
# but are not different from each other.

# models 9 and 10 are about the most different
anova(model9, model10)

# why is this?
summary(model9)
summary(model10)
# perhaps because with a linear fit there is no
# difference between the nestlings, but with a 
# categorical fit there is a large difference 
# between the smallest and the rest. But this 
# might mean the only different ones are the
# smallest:
model9.1 <- coxph(Surv(start, end, finish)~relevel(catsocd2,2)+
                  strata(wall), 
                data=edgesingle)
summary(model9.1)

# so only the smallest are different. Maybe it would
# be good to run the main model with both the four-level
# factor and with the linear term to see what the 
# difference is.

#####


model13 <- coxph(Surv(start, end, finish)~catsocd12+
                   strata(wall), 
                 data=edgesingle)

model14 <- coxph(Surv(start, end, finish)~zd12deltasoc+
                   strata(wall), 
                 data=edgesingle)

model15 <- coxph(Surv(start, end, finish)~zd12deltasoc+
                   I((zd12deltasoc)^2) + strata(wall), 
                 data=edgesingle)

model16 <- coxph(Surv(start, end, finish)~catsocd12.2+
                   strata(wall), 
                 data=edgesingle)

anova(model13, model14, model15, model16)

# these all have quite similar likelihoods.


# thinking: what if delh2/d2 or delh2h12/d2d12
# are not linear?

# for this, I consider the subset where there is
# change in hierarchy
edgesinglecfY <- subset(edgesingle, edgesingle$CFN=="cfY")


edgesinglecfY$zdeld2 <- scale(edgesinglecfY$deld2)

edgesinglecfY$zdeld2d12 <- scale(edgesinglecfY$deld2d12)



model17 <- coxph(Surv(start, end, finish)~zdeld2+
                   I((zdeld2)^2) + strata(wall), 
                 data=edgesinglecfY)

model18 <- coxph(Surv(start, end, finish)~zdeld2+
                   strata(wall), 
                 data=edgesinglecfY)

anova(model17, model18)

# linear



model19 <- coxph(Surv(start, end, finish)~zdeld2d12+
                   I((zdeld2d12)^2) + strata(wall), 
                 data=edgesinglecfY)

model20 <- coxph(Surv(start, end, finish)~zdeld2d12+
                   strata(wall), 
                 data=edgesinglecfY)

anova(model19, model20)

# linear



# and what about day 2 mass?
model21 <- coxph(Surv(start, end, finish)~zd2mass+
                   I(zd2mass^2) + strata(wall), 
                 data=edgesingle)

model22 <- coxph(Surv(start, end, finish)~zd2mass+
                   strata(wall), 
                 data=edgesingle)

anova(model22, model21)


# or residual mass
model23 <- coxph(Surv(start, end, finish)~zresidmass+
                   I(zresidmass^2) + strata(wall), 
                 data=edgesingle)

model24 <- coxph(Surv(start, end, finish)~zresidmass+
                   strata(wall), 
                 data=edgesingle)

anova(model24, model23)


# hokies! It's all good to go with linear on almost
# everything. Just one case where it would be good
# to experiment with a factor



#############



####

#### SAMPLE SIZES ####

# these are all the summary statistics reported in results

# to look at my numbers of individuals and 
# broods and whatever else tested, I need 
# to use the unique values in ardu:


# number of individuals
length(unique(ardu$birdid))

# for the methods: number of cross-fostering affected
# nestlings that were actually cross-fostered (i.e.
# P plus F):
table(ardu$FPLN, ardu$CFN)
# versus the whole data set:
table(ardu$FPLN)

# number of assays
length(ar123m12tarsus$birdid)

# what is the data like after being split in to
# up to two nestlings per record?

length(edgeall$end)
table(edgeall$finish)
table(edgeall$finish,edgeall$wall)


# number of social and natal broods
length(unique(ardu$social))
length(unique(ardu$natal))

# overlap between the natal and social brood ids
# where zero implies they are not the same
ardu$samenatalsocial <- ifelse(ardu$natal==ardu$social, 1, 0)
ardu$natal[1:10]
ardu$social[1:10]
ardu$samenatalsocial[1:10]

summary(ardu$samenatalsocial)
table(ardu$samenatalsocial)


# how far do nestlings go?
summary(ardu$total)

# my cross-fostered nestlings should all
# have zeroes:
table(ardu$CFN, ardu$samenatalsocial)
# no...

table(ardu$FPLN, ardu$samenatalsocial)
# oh! CFN are just nestlings that are in a brood
# where cross-fostering occurred.



# difference in mass between nestling categories:
tapply(ardu$d2mass, INDEX=list(ardu$CFN), FUN=mean)
tapply(ardu$d2mass, INDEX=list(ardu$CFN), FUN=sd)

tapply(ardu$mass, INDEX=list(ardu$CFN), FUN=mean)
tapply(ardu$mass, INDEX=list(ardu$CFN), FUN=sd)

# does mass differ significantly between the categories?
ardu$CFN <- as.factor(ardu$CFN)

kruskal.test(ardu$d2mass, ardu$CFN)
kruskal.test(ardu$d2mass~ardu$CFN) # checking that this is identical to previous
kruskal.test(ardu$mass, ardu$CFN)

# does day 2 hierarchy differ significantly between
# the cross-fostering categories?
tapply(ardu$d2deltasoc, INDEX=list(ardu$CFN), FUN=mean)
tapply(ardu$d2deltasoc, INDEX=list(ardu$CFN), FUN=sd)

kruskal.test(ardu$d2deltasoc, ardu$CFN)

tapply(ardu$d12deltasoc, INDEX=list(ardu$CFN), FUN=mean)
tapply(ardu$d12deltasoc, INDEX=list(ardu$CFN), FUN=sd)

kruskal.test(ardu$d12deltasoc, ardu$CFN)


# since there is no difference, Julia says report the
# overall means and standard deviations
mean(ardu$d2mass)
sd(ardu$d2mass)

mean(ardu$mass)
sd(ardu$mass)

mean(ardu$d2deltasoc)
sd(ardu$d2deltasoc)

mean(ardu$d12deltasoc)
sd(ardu$d12deltasoc)


# are the day two and 12 hierarchies too similar to pull apart?
plot(ardu$d2deltasoc, ardu$d12deltasoc)
cor(ardu$d2deltasoc, ardu$d12deltasoc)

# very similar. This might make the interaction a bit hard to
# distinguish.


# is there an imbalance in the number of natal versus
# social broods in the CF subset?
arducfY <- subset(ardu, ardu$CFN=="cfY")
summary(arducfY)
table(arducfY$CFN)


length(unique(arducfY$natal))
length(unique(arducfY$social))

# how many tests of these individuals?
ar123m12tarsus$deld2 <- ar123m12tarsus$d2deltasoc - 
  ar123m12tarsus$d2deltanat

ar123m12tarsus$CFN <- ifelse(ar123m12tarsus$FPLN=="N",
                   "N", ifelse(ar123m12tarsus$deld2==0,
                               "cfN", "cfY"))

table(ar123m12tarsus$CFN)
# the cfY's are the sample size

# what is the data like after being split in to
# up to two nestlings per record in the data subset
# of nestlings affected by cross-fostering?
length(edgecfYe$end)
table(edgecfYe$finish)
table(edgecfYe$wall)
table(edgecfYe$finish,edgecfYe$wall) 
# nestlings that are 0 for wall and 1 for finish never
# reached the wall.


# how many nestlings are in CF affected broods or not?
table(arducfY$FPLN)

# how does this compare to the cfN group of the full data set?
table(ardu$FPLN, ardu$CFN)


length(unique(arducfY$social))
length(unique(arducfY$natal))
# not a lot of difference, in 209 individuals that's good.



# summary for nestling activity
summary(ar123m12tarsus$total)
length(ar123m12tarsus$total)

# how many of these nestlings were in what kind
# of foster brood?

table(ardu$CFN, ardu$FPLN)
# F = only cross-fostered nestlings. Part fostered nests = P+L

# this is something like checking assumptions to see
# whether summaries are better with a linear model or
# what. I'm not totally sure what it's about when I look
# at it today (24th March 2015):
day2 <- lm(d2mass~CFN, data=ardu)
plot(day2)

day12 <- lm(mass~CFN, data=ardu)
plot(day12)
# raw data doesn't seem great for the day 12
# analysis. Can transform, or use the KW test result.


day2 <- lm(d2deltasoc~CFN, data=ardu)
plot(day2)
summary(day2)
aov(d2deltasoc~CFN, data=ardu)

day12 <- lm(d12deltasoc~CFN, data=ardu)
plot(day12)
summary(day12)
aov(d12deltasoc~CFN, data=ardu)


# so these are not different by KW test or
# by linear model. The ranks (not normally
# distributed) are also not different by
# KW test. I have just put the KW tests in
# the paper, but am confident in the results
# of both tests.




tapply(edgeall$end, INDEX=list(quantcut(edgeall$d12mass)), FUN=mean)
tapply(edgeall$end, INDEX=list(quantcut(edgeall$d12mass)), FUN=sd)

# what should I fit? Mass? log(mass)?

plot(ardu$mass, log(ardu$total))
plot(log(ardu$mass), log(ardu$total))


# well neither looks good but normal mass looks
# better

plot(ardu$d2deltanat, log(ardu$total))
plot(ardu$d2deltasoc, log(ardu$total))
plot(ardu$mass, log(ardu$total))
plot(jitter(edgeall$wd1to3), log(edgeall$end))



####################
####################

# 12th Feb 2015
# check for collinearity
# I know cohort and releaser have some
# correlations that I cannot change
library(lattice)


# take the single measures data set with one
# line per offspring:
plotdataset <- ardu[c("d2deltanat", "d2deltasoc",
                       "mass","d2mass","residmass2",
                       "endprob","d12deltasoc",
                       "socbroodsz")]
head(plotdataset)
splom(plotdataset)

# natal and social day wo hierarchy are very
# colinear, and very similar to day two mass
# as well.

# though the day two hierarchy similarity is
# inflated by non-fostered individuals. See
# what happens without these:
plot(edgecfYe$d2deltanat, edgecfYe$d2deltasoc)
cor(edgecfYe$d2deltanat, edgecfYe$d2deltasoc)
# so the correlation is still quite high. This might
# make it hard to draw the two apart.

# day two mass is quite similar to the
# day two residual mass hierarchies
cor(ardu$d2deltanat, ardu$d2mass)
cor(ardu$d2deltasoc, ardu$d2mass)
cor(ardu$residmass, ardu$mass)
cor(ardu$d12deltasoc, ardu$mass)
cor(ardu$deld2, ardu$mass)
cor(ardu$deld2, ardu$d2mass)
#day two versus 12 mass
cor(ardu$mass, ardu$d2mass)

cor(edgecfYe$d12mass, edgecfYe$deld2d12)

plot(edgecfYe$d12mass, edgecfYe$deld2d12)


# will have to take care with all of these.

# how does activity change with a change in hierarchy place?
plot(edgecfYe$deld2, edgecfYe$end)
plot(edgecfYe$deld2d12, edgecfYe$end)

# looks like lower on d2 and higher on d12 is better, but these
# probably correlate to d12 mass:

plot(edgecfYe$deld2, edgecfYe$d12mass)
plot(edgecfYe$deld2d12, edgecfYe$d12mass)
# especially with the latter, it might be very tough to draw the
# two apart:
cor(edgecfYe$deld2d12, edgecfYe$d12mass)


# another way to explore colinearity is to calculate
# variance inflation factors, as per Zuur et al 2010 MEE
# paper. This can be done in package car 
# for linear models:

library(car)

vif(glm(end~wall+d2deltasoc*d12deltasoc + d12mass + residmass +
          d2mass + socbroodsz + noise + wd1to3 + endprob + time + I(time^2) +
          cohort + releaser, data=edgeall, family=poisson))
# so that suggests some vif's are high in this set of
# covariates but not so high that they would be excluded
# Time and time^2 obviously overlap. That is because they
# are related.


vif(glm(end~wall+d2deltasoc*d2deltanat  + d12mass + residmass +
          d2mass + socbroodsz + noise + wd1to3 + endprob +  time + I(time^2) +
          cohort + releaser, data=edgeall, family=poisson))
# but this shows having the day two natal and social
# hierarchies in the same model will be a problem
# with all data


vif(glm(end~wall+d2deltasoc*d2deltanat  + d12mass + residmass +
          d2mass + socbroodsz + noise + wd1to3 + endprob +  time + I(time^2) +
          cohort + releaser, data=edgecfYe, family=poisson))
# though less so in the subset that switches places...

##############################################################
##############################################################

# fixed effect selection?
str(edgeall)


# test location as factor
# environmental noise as factor
# time of day as continuous
# order of testing within a day as factor
# social brood size as continuous
# cohort as a factor
# releaser as a factor
# compromised as a factor
# likelihood of finishing at a given part of the arena
# edge as continuous.

edgeall$factorbirdid <- as.factor(edgeall$birdid)
edgeall$factorsocial <- as.factor(edgeall$social)
edgeall$factornatal <- as.factor(edgeall$natal)
edgeall$ztime <- scale(edgeall$time)
edgeall$zsocbroodsz <- scale(edgeall$socbroodsz)
edgeall$zendprob <- scale(edgeall$endprob)

# and my coefficients of interest:

edgeall$zd2deltasoc <- scale(edgeall$d2deltasoc)
edgeall$zd12deltasoc <- scale(edgeall$d12deltasoc)
edgeall$zd2mass <- scale(edgeall$d2mass)
edgeall$zd12mass <- scale(edgeall$d12mass)
edgeall$zresidmass <- scale(edgeall$residmass)



fixed1 <- coxme(Surv(start, end, finish)~ strata(wall)+
                  location + noise + factorwd1to3 +
                  factorcohort + releaser + factorcomp +
                  ztime + zsocbroodsz + zendprob + I(ztime^2) +
                  (1|factorbirdid) + (1|factorsocial),
                data=edgeall)

print(fixed1)

# do I really need all this? Time^2 is the highest order term:


fixed2 <- coxme(Surv(start, end, finish)~ strata(wall)+
                  location + noise + factorwd1to3 +
                  factorcohort + releaser + factorcomp +
                  ztime + zsocbroodsz + zendprob +
                  (1|factorbirdid) + (1|factorsocial),
                data=edgeall)

print(fixed2)
# super similar coefficients and variance components
anova(fixed1, fixed2)
# no big difference in model fit


# same for zsocbroodsz, which also has a small effect
# size?


fixed3 <- coxme(Surv(start, end, finish)~ strata(wall)+
                  location + noise + factorwd1to3 +
                  factorcohort + releaser + factorcomp +
                  ztime + zendprob +
                  (1|factorbirdid) + (1|factorsocial),
                data=edgeall)

print(fixed3)

# compromised

fixed4 <- coxme(Surv(start, end, finish)~ strata(wall)+
                  location + noise + factorwd1to3 +
                  factorcohort + releaser + 
                  ztime + zendprob +
                  (1|factorbirdid) + (1|factorsocial),
                data=edgeall)

print(fixed4)


# again some small changes, but actually nothing major
# for any fixed or random effect.

anova(fixed2, fixed3)
anova(fixed3, fixed4)

# and this suggests no big change to the fit of the
# model.

# I could now feasibly remove time. On the other hand,
# the two effects I have removed so far have effect
# sizes below 0.1 and standard errors larger than their
# effect sizes, and this is not true for time. Keep.

##############################################################
##############################################################

# for the first part of my investigation, 
# I want to know the effects of the social and natal
# hierarchies and of state on the activity of an
# individual nestling.

# I could therefore do a main model with just state
# at testing and hierarchy, and an exended model with
# residual mass and the other fixed effects added in.


# first, check residuals

check1.1 <- coxph(Surv(start, end, finish)~ strata(wall)+
                    zd2deltasoc*zd12deltasoc + CFN+ 
                    zd12mass,
                  data=edgeall)
check1.1z <- cox.zph(check1.1)
check1.1z

# amazingly good residuals

# now the full model

check1.2 <- coxph(Surv(start, end, finish)~ strata(wall)+
                    zd2deltasoc*zd12deltasoc + CFN+ 
                    zd12mass + zd2mass + zresidmass +
                    location + noise + factorwd1to3 +
                    factorcohort + releaser + ztime +
                    zendprob,
                  data=edgeall)
check1.2z <- cox.zph(check1.2)
check1.2z

# cohort is terrible, as are location and end position.
plot(check1.2z[8])
plot(check1.2z[12])
plot(check1.2z[17])

# but this is related to the fact that they are
# factors, and levels of the factor with small
# sample size as well.
plot(check1.2z[13])
# except for end position probability, where the
# nestlings that do not move are biased in where
# they stop moving.


# maybe the best strategy here is to acknowledge
# the bad Schoenfeld residuals, but also that these
# residuals are from a coxph (out of necessity - 
# there is no model checking procedure yet for 
# coxme) and also acknowledge the full model is 
# suboptimal because there is autocorrelation.

# is there any difference in the variance if I nest birds
# within social broods?:
coxme1.reduced.testnested <- coxme(Surv(start, end, finish)~ strata(wall)+
                          zd2deltasoc*zd12deltasoc + CFN+ 
                          zd12mass + 
                          (1|factorsocial/factorbirdid),
                        data=edgeall)

print(coxme1.reduced.testnested)

# now the main model:
coxme1.reduced <- coxme(Surv(start, end, finish)~ strata(wall)+
                          zd2deltasoc*zd12deltasoc + CFN+ 
                          zd12mass + 
                (1|factorbirdid) + (1|factorsocial),
              data=edgeall)

print(coxme1.reduced) 
# the variance components are the same magnitude however I express them.
anova(coxme1.reduced)
# anova interesting, but not useful for significnce
# report in this case.
# model chisq is 363.25

# how is the model fitting progressing:

coxme1.reduced.refine <- coxme(Surv(start, end, finish)~ strata(wall)+
                          zd2deltasoc*zd12deltasoc + CFN+ 
                          zd12mass + 
                          (1|factorbirdid) + (1|factorsocial),
                        data=edgeall,
                        refine.n=100)

coxme1.reduced.refine$refine
# so this is hard for the model to fit, but not
# impossible. More data points per individual
# would be much better (the Laplace approximation
# works best with many observations per individual).


# knock out each of the random effects in turn
# to check the significance:

# knock out bird id
coxme1.reduced.b <- coxme(Surv(start, end, finish)~ strata(wall)+
                          zd2deltasoc*zd12deltasoc + CFN+ 
                          zd12mass + 
                          (1|factorsocial),
                        data=edgeall)

print(coxme1.reduced.b)
# model chisq 247.28
# difference between this and full model:
363.25-247.28

# knock out social brood id
coxme1.reduced.s <- coxme(Surv(start, end, finish)~ strata(wall)+
                          zd2deltasoc*zd12deltasoc + CFN+ 
                          zd12mass + 
                          (1|factorbirdid),
                        data=edgeall)

print(coxme1.reduced.s)
# chisq 329.42
# difference between this and full model:
363.25-329.42

# or as an equation to calculate the chisq from the models:
2*(coxme1.reduced$loglik[1:2] - coxme1.reduced.b$loglik[1:2])
2*(coxme1.reduced$loglik[1:2] - coxme1.reduced.s$loglik[1:2])


# what is the profile likelihood for each random
# effect? To do this, use vfixed, which can fix
# the variance of one or more random effects when
# a list of those random effects is passed to the
# function (see the variance vignette at
# https://cran.r-project.org/web/packages/coxme/vignettes/variance.pdf):

# create a vector to store the log-likelihoods,
# and one that is of 'double precision' which
# means R will store twice as many of the decimal
# point values:

estvars <- seq(0.7,1.05,length=20)^2

coxme1.reduced.birdlogliks <- double(20)

# the second variance is set at the value of the
# social brood variance from the original model.

for(i in 1:length(coxme1.reduced.birdlogliks)){
  coxme1.reduced.birdlimits <- coxme(Surv(start, end, finish)~ strata(wall)+
                            zd2deltasoc*zd12deltasoc + CFN+ 
                            zd12mass + 
                            (1|factorbirdid) + (1|social),
                          data=edgeall,
                          vfixed=list(estvars[i],
                                      coxme1.reduced$vcoef$factorsocial[[1]]))
  
  coxme1.reduced.birdlogliks[i] <- 2*diff(coxme1.reduced.birdlimits$loglik)[1]
}

plot(sqrt(estvars), coxme1.reduced.birdlogliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxme1.reduced$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxme1.reduced$loglik)[1] - coxme1.reduced.birdlogliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y



# now for the social brood:

estvars <- seq(0.5,1,length=20)^2

coxme1.reduced.sociallogliks <- double(20)


for(i in 1:length(coxme1.reduced.sociallogliks)){
  coxme1.reduced.sociallimits <- coxme(Surv(start, end, finish)~ strata(wall)+
                                       zd2deltasoc*zd12deltasoc + CFN+ 
                                       zd12mass + 
                                       (1|factorbirdid) + (1|social),
                                     data=edgeall,
                                     vfixed=list(coxme1.reduced$vcoef$factorbirdid[[1]],
                                                 estvars[i]))
  
  coxme1.reduced.sociallogliks[i] <- 2*diff(coxme1.reduced.sociallimits$loglik)[1]
}

plot(sqrt(estvars), coxme1.reduced.sociallogliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxme1.reduced$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxme1.reduced$loglik)[1] - coxme1.reduced.sociallogliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y





# and now the full model:

coxme1.full <- coxme(Surv(start, end, finish)~ strata(wall)+
                          zd2deltasoc*zd12deltasoc + CFN+ 
                          zd12mass + zd2mass + zresidmass +
                       location + noise + factorwd1to3 +
                       factorcohort + releaser + ztime + zendprob +
                          (1|factorbirdid) + (1|factorsocial),
                        data=edgeall)

print(coxme1.full)
anova(coxme1.full)
# this anova is not useful for determining the significance
# of a single factor because it is  fitted via a
# sequential process.
# interestingly, in the full model, social brood
# effects are down by 0.1 variance, and bird id 
# effects are up by 0.1 variance
# chisq of this model is 489.11

# significance of bird id:


coxme1.full.b <- coxme(Surv(start, end, finish)~ strata(wall)+
                       zd2deltasoc*zd12deltasoc + CFN+ 
                       zd12mass + zd2mass + zresidmass +
                       location + noise + factorwd1to3 +
                         factorcohort + releaser + ztime +zendprob +
                       (1|factorsocial),
                     data=edgeall)

print(coxme1.full.b)
# chisq of this model is 349.30

# and significance of social brood id:

coxme1.full.s <- coxme(Surv(start, end, finish)~ strata(wall)+
                       zd2deltasoc*zd12deltasoc + CFN+ 
                       zd12mass + zd2mass + zresidmass +
                       location + noise + factorwd1to3 +
                         factorcohort + releaser + ztime +zendprob +
                       (1|factorbirdid),
                     data=edgeall)

print(coxme1.full.s)
# chisq of this model is 468.40

# chisq of birdid:
489.11-349.3
# significance of birdid:
1-pchisq(489.11-349.3, 1)

# chisq of social brood:
489.11-468.40
# significance of social brood:
1-pchisq(460.94-441.32, 1)



# distributions of the likelihoods for each random effect:

# bird id:

estvars <- seq(0.7,1.2,length=20)^2

coxme1.full.birdlogliks <- double(20)


for(i in 1:length(coxme1.full.birdlogliks)){
  coxme1.full.birdlimits <- coxme(Surv(start, end, finish)~ strata(wall)+
                                      zd2deltasoc*zd12deltasoc + CFN+ 
                                      zd12mass + zd2mass + zresidmass +
                                      location + noise + factorwd1to3 +
                                      factorcohort + releaser + ztime +zendprob +
                                      (1|factorbirdid) + (1|social),
                                    data=edgeall,
                                    vfixed=list(estvars[i],
                                                coxme1.full$vcoef$factorsocial[[1]]))
  
  coxme1.full.birdlogliks[i] <- 2*diff(coxme1.full.birdlimits$loglik)[1]
}

plot(sqrt(estvars), coxme1.full.birdlogliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxme1.full$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxme1.full$loglik)[1] - coxme1.full.birdlogliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y



# social brood id:
estvars <- seq(0.4,0.9,length=20)^2

coxme1.full.sociallogliks <- double(20)


for(i in 1:length(coxme1.full.sociallogliks)){
  coxme1.full.sociallimits <- coxme(Surv(start, end, finish)~ strata(wall)+
                                      zd2deltasoc*zd12deltasoc + CFN+ 
                                      zd12mass + zd2mass + zresidmass +
                                      location + noise + factorwd1to3 +
                                      factorcohort + releaser + ztime +zendprob +
                                         (1|factorbirdid) + (1|social),
                                       data=edgeall,
                                       vfixed=list(coxme1.full$vcoef$factorbirdid[[1]],
                                                   estvars[i]))
  
  coxme1.full.sociallogliks[i] <- 2*diff(coxme1.full.sociallimits$loglik)[1]
}

plot(sqrt(estvars), coxme1.full.sociallogliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxme1.full$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxme1.full$loglik)[1] - coxme1.full.sociallogliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y




# so there is some persistent effect where nestlings
# in cross-fostered broods that do not move in the
# hierarchy end up being less active. Is this a 
# significant comparison when comparing between the
# CF categories, rather than between N and the CFs


coxme1.CFasbase <- coxme(Surv(start, end, finish)~ strata(wall)+
                          zd2deltasoc*zd12deltasoc + relevel(CFN,2) + 
                          zd12mass + 
                          (1|factorbirdid) + (1|factorsocial),
                        data=edgeall)

print(coxme1.CFasbase)


# and does it withstand the removal of the other
# fixed effects:

coxme1.CFonly <- coxme(Surv(start, end, finish)~ strata(wall)+
                           relevel(CFN,2) + 
                           (1|factorbirdid) + (1|factorsocial),
                         data=edgeall)

print(coxme1.CFonly)

# a bit. The last check is whether the relationship
# withstands the removal of the nestlings where we
# are missing some day two masses from their brood
# or where there was a mix-up in nestling identity,
# but this happens later after the code to produce
# these data sets has been run.


# last part to this analysis: does the social brood
# effect come from a shared social mother?
# Add social mother to the analysis:

coxme1.maternal <- coxme(Surv(start, end, finish)~ strata(wall)+
                           zd2deltasoc*zd12deltasoc + relevel(CFN,2) + 
                           zd12mass + 
                           (1|factorbirdid) + (1|factorsocial) + (1|factormum),
                         data=edgeall)

print(coxme1.maternal)


# chisq of this model 363.25
print(coxme1.reduced)
# chisq of the model this is based on also 363.25



# distributions of the likelihoods for the maternal effect:

estvars <- seq(0.01,0.5,length=20)^2

coxme1.maternal.logliks <- double(20)


for(i in 1:length(coxme1.maternal.logliks)){
  coxme1.maternal.limits <- coxme(Surv(start, end, finish)~ strata(wall)+
                             zd2deltasoc*zd12deltasoc + relevel(CFN,2) + 
                             zd12mass + 
                             (1|factorbirdid) + (1|factorsocial) + (1|factormum),
                                  data=edgeall,
                                  vfixed=list(coxme1.maternal$vcoef$factorbird[[1]],
                                              coxme1.maternal$vcoef$factorsocial[[1]],
                                              estvars[i]))
  
  coxme1.maternal.logliks[i] <- 2*diff(coxme1.maternal.limits$loglik)[1]
}

plot(sqrt(estvars), coxme1.maternal.logliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxme1.maternal$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxme1.maternal$loglik)[1] - coxme1.maternal.logliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y
# so since this is effectively a zero, I think it is safe to
# assume the variance component lower bound is zero

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y




# one last thing. During testing for whether the hierarchies
# should be fitted as linear or quadratic or factors, there
# was one hierarchy that showed some benefit to being
# quadratic (or at least, not linear). This was zd2deltasoc.

# so, see whether the results are the same with a four-level
# factor:

edgeall$fourleveld2deltasoc <- quantcut(edgeall$d2deltasoc, seq(0,1,1/4))
table(edgeall$fourleveld2deltasoc)


coxme1.asfourd2delta <- coxme(Surv(start, end, finish)~ strata(wall)+
                          fourleveld2deltasoc*zd12deltasoc + CFN+ 
                          zd12mass + 
                          (1|factorbirdid) + (1|factorsocial),
                        data=edgeall)

print(coxme1.asfourd2delta)

# so in this model it does not make a difference.
print(coxme1.reduced)

# and the chisq of this model is a little higher --> there
# is no difference from this.

#########################################################
#########################################################


# so the next question was whether nestlings that change
# place in the hierarchy on day two or between days two
# and 12 have a different level of activity.

# natal brood ID is added, as is the difference in the
# hierarchies between day two, and day two to 12.

# there is no CFN in this model because only nestlings
# that changed place and were cross-fostered are included
# so that there is no overlap between the changing place
# terms and no overlap between the natal and social brood
# terms.

# check residuals

check2.1 <- coxph(Surv(start, end, finish)~ strata(wall)+
                    zdeld2 + zdeld2d12,
                  data=edgecfYe)
check2.1z <- cox.zph(check2.1)
check2.1z

# not great for the difference between days
# 2 and 12
plot(check2.1z[2])
# this suggests a bit of a curve. Well, does a
# curve fit better?


check2.2 <- coxph(Surv(start, end, finish)~ strata(wall)+
                    zdeld2 + zdeld2d12 + I(zdeld2d12^2),
                  data=edgecfYe)
check2.2z <- cox.zph(check2.2)
check2.2z
# yes...

# does it fit better if I do an anova?

compare1 <- coxph(Surv(start, end, finish)~ strata(wall)+
                    zdeld2 + zdeld2d12,
                  data=edgecfYe)

compare2 <- coxph(Surv(start, end, finish)~ strata(wall)+
                    zdeld2 + zdeld2d12 + I(zdeld2d12^2),
                  data=edgecfYe)

anova(compare2, compare1)
# no... so then I think, as before, do not worry
# re the residuals since their applicability to coxme
# is not known.

# now the full model

check2.3 <- coxph(Surv(start, end, finish)~ strata(wall)+
                    zdeld2 + zdeld2d12 +
                    location + noise + factorwd1to3 +
                    factorcohort + releaser + ztime +
                    zendprob,
                  data=edgecfYe)
check2.3z <- cox.zph(check2.3)
check2.3z

# well, better than last time.

# the reduced model:

coxmeCF.reduced <- coxme(Surv(start, end, finish)~ strata(wall)+
                        zdeld2 + zdeld2d12 +
                        (1|factorbirdid) + (1|factorsocial) + 
                        (1|factornatal),
                      data=edgecfYe)

print(coxmeCF.reduced)

# chisq 154.74

# ask how much the model random effects were
# ?corrected? during model fit:

coxmeCF.reduced.refine <- coxme(Surv(start, end, finish)~ strata(wall)+
                           zdeld2 + zdeld2d12 +
                           (1|factorbirdid) + (1|factorsocial) + 
                           (1|factornatal),
                         data=edgecfYe,
                         refine.n=100)

coxmeCF.reduced.refine$refine
# this suggests the correction is potentially substantial.
# but I already knew this because it is 605-1106 data points
# fitted on 155.13 degrees of freedom from the random effects,
# so it is tough for a hazards model.


# testing significances:

coxmeCF.reduced.b <- coxme(Surv(start, end, finish)~ strata(wall)+
                           zdeld2 + zdeld2d12 +
                           (1|factorsocial) + 
                           (1|factornatal),
                         data=edgecfYe)

print(coxmeCF.reduced.b)
# natal brood accounting for more now. Hierarchy
# difference matters a little, but this model does
# not have main random effect...
# model chisq 100.16
154.74-100.16


coxmeCF.reduced.s <- coxme(Surv(start, end, finish)~ strata(wall)+
                           zdeld2 + zdeld2d12 +
                           (1|factorbirdid) + (1|factornatal),
                         data=edgecfYe)

print(coxmeCF.reduced.s)
# natal and bird both take some of the
# variance that used to be in social. Day
# two change a little more important but
# still not major.
# model chisq 147.54
154.74-147.54
1-pchisq(7.2,1)


coxmeCF.reduced.n <- coxme(Surv(start, end, finish)~ strata(wall)+
                           zdeld2 + zdeld2d12 +
                           (1|factorbirdid) + (1|factorsocial),
                         data=edgecfYe)

print(coxmeCF.reduced.n)
# is like the full model
# model chisq 154.74. Full model 154.74
# so no change



# distributions of the likelihoods for birdid:

estvars <- seq(0.7,1.2,length=20)^2

coxmeCF.bird.logliks <- double(20)


for(i in 1:length(coxmeCF.bird.logliks)){
  coxmeCF.reduced.limits <- coxme(Surv(start, end, finish)~ strata(wall)+
                             zdeld2 + zdeld2d12 +
                             (1|factorbirdid) + (1|factorsocial) + 
                             (1|factornatal),
                           data=edgecfYe,
                           vfixed=list(estvars[i], 
                                       coxmeCF.reduced$vcoef$factorsocial[[1]],
                                       coxmeCF.reduced$vcoef$factornatal[[1]]))
  
  coxmeCF.bird.logliks[i] <- 2*diff(coxmeCF.reduced.limits$loglik)[1]
}

plot(sqrt(estvars), coxmeCF.bird.logliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxmeCF.reduced$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxmeCF.reduced$loglik)[1] - coxmeCF.bird.logliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y
# so since this is effectively a zero, I think it is safe to
# assume the variance component lower bound is zero

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y




# distributions of the likelihoods for social brood id:

estvars <- seq(0.2,1.0,length=20)^2

coxmeCF.social.logliks <- double(20)


for(i in 1:length(coxmeCF.social.logliks)){
  coxmeCF.reduced.limits <- coxme(Surv(start, end, finish)~ strata(wall)+
                                    zdeld2 + zdeld2d12 +
                                    (1|factorbirdid) + (1|factorsocial) + 
                                    (1|factornatal),
                                  data=edgecfYe,
                                  vfixed=list(coxmeCF.reduced$vcoef$factorbirdid[[1]],
                                              estvars[i], 
                                              coxmeCF.reduced$vcoef$factornatal[[1]]))
  
  coxmeCF.social.logliks[i] <- 2*diff(coxmeCF.reduced.limits$loglik)[1]
}

plot(sqrt(estvars), coxmeCF.social.logliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxmeCF.reduced$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxmeCF.reduced$loglik)[1] - coxmeCF.social.logliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y
# so since this is effectively a zero, I think it is safe to
# assume the variance component lower bound is zero

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y




# distributions of the likelihoods for natal brood id:

estvars <- seq(0.01,0.6,length=20)^2

coxmeCF.natal.logliks <- double(20)


for(i in 1:length(coxmeCF.natal.logliks)){
  coxmeCF.reduced.limits <- coxme(Surv(start, end, finish)~ strata(wall)+
                                    zdeld2 + zdeld2d12 +
                                    (1|factorbirdid) + (1|factorsocial) + 
                                    (1|factornatal),
                                  data=edgecfYe,
                                  vfixed=list(coxmeCF.reduced$vcoef$factorbirdid[[1]], 
                                              coxmeCF.reduced$vcoef$factorsocial[[1]],
                                              estvars[i]))
  
  coxmeCF.natal.logliks[i] <- 2*diff(coxmeCF.reduced.limits$loglik)[1]
}

plot(sqrt(estvars), coxmeCF.natal.logliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxmeCF.reduced$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxmeCF.reduced$loglik)[1] - coxmeCF.natal.logliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y
# so since this is effectively a zero, I think it is safe to
# assume the variance component lower bound is zero

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y



# the full model

coxmeCF.full <- coxme(Surv(start, end, finish)~ strata(wall)+
                        zdeld2 + zdeld2d12 +
                        location + noise + factorwd1to3 +
                        factorcohort + releaser + ztime +
                        zendprob +
                       (1|factorbirdid) + (1|factorsocial) + 
                        (1|factornatal),
                     data=edgecfYe)

print(coxmeCF.full)

# model chisq 231.23


# significances:

coxmeCF.full.b <- coxme(Surv(start, end, finish)~ strata(wall)+
                        zdeld2 + zdeld2d12 +
                        location + noise + factorwd1to3 +
                        factorcohort + releaser + ztime +
                          zendprob +
                        (1|factorsocial) + (1|factornatal),
                      data=edgecfYe)

print(coxmeCF.full.b)
# again, some of the variance is taken up
# by natal brood
# model chisq 166.04
231.23-166.04



coxmeCF.full.s <- coxme(Surv(start, end, finish)~ strata(wall)+
                        zdeld2 + zdeld2d12 +
                        location + noise + factorwd1to3 +
                        factorcohort + releaser + ztime +
                          zendprob +
                        (1|factorbirdid) + (1|factornatal),
                      data=edgecfYe)

print(coxmeCF.full.s)
# natal brood partially takes some social
# brood vaariance, so does bird id
# model chisq 228.79
231.23-228.79
1-pchisq(2.44,1)



coxmeCF.full.n <- coxme(Surv(start, end, finish)~ strata(wall)+
                        zdeld2 + zdeld2d12 +
                        location + noise + factorwd1to3 +
                        factorcohort + releaser + ztime +
                          zendprob +
                        (1|factorbirdid) + (1|factorsocial),
                      data=edgecfYe)

print(coxmeCF.full.n)
# model chisq 231.26
# suggesting this is *better* than the full model...




# distributions of the likelihoods for bird id:

estvars <- seq(0.7,1.3,length=20)^2

coxmeCFfull.bird.logliks <- double(20)


for(i in 1:length(coxmeCFfull.bird.logliks)){
  coxmeCF.full.limits <- coxme(Surv(start, end, finish)~ strata(wall)+
                                    zdeld2 + zdeld2d12 +
                                    location + noise + factorwd1to3 +
                                    factorcohort + releaser + ztime +
                                    zendprob +
                                    (1|factorbirdid) + (1|factorsocial) + 
                                    (1|factornatal),
                                  data=edgecfYe,
                                  vfixed=list(estvars[i], 
                                              coxmeCF.full$vcoef$factorsocial[[1]],
                                              coxmeCF.full$vcoef$factornatal[[1]]))
  
  coxmeCFfull.bird.logliks[i] <- 2*diff(coxmeCF.full.limits$loglik)[1]
}

plot(sqrt(estvars), coxmeCFfull.bird.logliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxmeCF.full$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxmeCF.full$loglik)[1] - coxmeCFfull.bird.logliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y
# so since this is effectively a zero, I think it is safe to
# assume the variance component lower bound is zero

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y



# distributions of the likelihoods for social brood id:

estvars <- seq(0.2,1,length=20)^2

coxmeCFfull.social.logliks <- double(20)


for(i in 1:length(coxmeCFfull.social.logliks)){
  coxmeCF.full.limits <- coxme(Surv(start, end, finish)~ strata(wall)+
                                 zdeld2 + zdeld2d12 +
                                 location + noise + factorwd1to3 +
                                 factorcohort + releaser + ztime +
                                 zendprob +
                                 (1|factorbirdid) + (1|factorsocial) + 
                                 (1|factornatal),
                               data=edgecfYe,
                               vfixed=list(coxmeCF.full$vcoef$factorbirdid[[1]],
                                           estvars[i], 
                                           coxmeCF.full$vcoef$factornatal[[1]]))
  
  coxmeCFfull.social.logliks[i] <- 2*diff(coxmeCF.full.limits$loglik)[1]
}

plot(sqrt(estvars), coxmeCFfull.social.logliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxmeCF.full$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxmeCF.full$loglik)[1] - coxmeCFfull.social.logliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y
# so since this is effectively a zero, I think it is safe to
# assume the variance component lower bound is zero

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y




# distributions of the likelihoods for natal brood id:

estvars <- seq(0.01,0.6,length=20)^2

coxmeCFfull.natal.logliks <- double(20)


for(i in 1:length(coxmeCFfull.natal.logliks)){
  coxmeCF.full.limits <- coxme(Surv(start, end, finish)~ strata(wall)+
                                 zdeld2 + zdeld2d12 +
                                 location + noise + factorwd1to3 +
                                 factorcohort + releaser + ztime +
                                 zendprob +
                                 (1|factorbirdid) + (1|factorsocial) + 
                                 (1|factornatal),
                               data=edgecfYe,
                               vfixed=list(coxmeCF.full$vcoef$factorbirdid[[1]],
                                           coxmeCF.full$vcoef$factorsocial[[1]],
                                           estvars[i]))
  
  coxmeCFfull.natal.logliks[i] <- 2*diff(coxmeCF.full.limits$loglik)[1]
}

plot(sqrt(estvars), coxmeCFfull.natal.logliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxmeCF.full$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxmeCF.full$loglik)[1] - coxmeCFfull.natal.logliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y
# so since this is effectively a zero, I think it is safe to
# assume the variance component lower bound is zero

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y





# what if I add the mother?

coxmeCF.maternal <- coxme(Surv(start, end, finish)~ strata(wall)+
                           zdeld2 + zdeld2d12 +
                           (1|factorbirdid) + (1|factorsocial) + 
                           (1|factornatal) + (1|factormum),
                         data=edgecfYe)

print(coxmeCF.maternal)
#chisq of this model 154.74
print(coxmeCF.reduced)
# chisq of the original model 154.74



# distributions of the likelihood for maternal id:

estvars <- seq(0.01,0.7,length=20)^2

coxmeCF.mother.logliks <- double(20)


for(i in 1:length(coxmeCF.mother.logliks)){
  coxmeCF.maternal.limits <- coxme(Surv(start, end, finish)~ strata(wall)+
                              zdeld2 + zdeld2d12 +
                              (1|factorbirdid) + (1|factorsocial) + 
                              (1|factornatal) + (1|factormum),
                            data=edgecfYe,
                            vfixed=list(coxmeCF.maternal$vcoef$factorbirdid[[1]], 
                                        coxmeCF.maternal$vcoef$factorsocial[[1]],
                                        coxmeCF.maternal$vcoef$factornatal[[1]],
                                        estvars[i]))
  
  coxmeCF.mother.logliks[i] <- 2*diff(coxmeCF.maternal.limits$loglik)[1]
}

plot(sqrt(estvars), coxmeCF.mother.logliks,
     xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(coxmeCF.maternal$loglik)[1] - qchisq(.95, 1), lty=2)

# to get the confidence intervals:

temp <- 2*diff(coxmeCF.maternal$loglik)[1] - coxmeCF.mother.logliks
approx(temp[1:10], sqrt(estvars[1:10]), 3.84)$y
# so since this is effectively a zero, I think it is safe to
# assume the variance component lower bound is zero

approx(temp[11:20], sqrt(estvars[11:20]), 3.84)$y



# This model has a lot of random effects for the data.
# If the model is simplified to only birdID and MotherID
# will the proportion of variance accounted for by the 
# mother still be zero? Will it remain less than the social
# brood?

{
  coxmeCF.maternalandbirdonly <- coxme(Surv(start, end, finish)~ strata(wall)+
                              zdeld2 + zdeld2d12 +
                              (1|factorbirdid) + (1|factormum),
                            data=edgecfYe)
  
  print(coxmeCF.maternalandbirdonly)
  
  # and with the social brood as well, to show that the
  # variance that she accounts for is actually social brood
  # variance:
  
  coxmeCF.maternalsocialbird <- coxme(Surv(start, end, finish)~ strata(wall)+
                                         zdeld2 + zdeld2d12 +
                                         (1|factorbirdid) + (1|factorsocial) +
                                        (1|factormum),
                                       data=edgecfYe)
  
  print(coxmeCF.maternalsocialbird)
}

# How does this compare to the model with all effects,
# and the model with just the social brood?

{
  print(coxmeCF.maternalandbirdonly)
  print(coxmeCF.maternal)
  print(coxmeCF.reduced.n)
  # so. The best integrated loglikelihood is for the model 
  # that only has social brood or has all effects.
  # But the mother does account for variance in the model 
  # where she is the only effect.
}

# Is the same true when we add the other fixed effects?

{
  coxmeCF.full.maternalandbird <- coxme(Surv(start, end, finish)~ strata(wall)+
                            zdeld2 + zdeld2d12 +
                            location + noise + factorwd1to3 +
                            factorcohort + releaser + ztime +
                            zendprob +
                            (1|factorbirdid) + (1|factormum),
                          data=edgecfYe)
  
  print(coxmeCF.full.maternalandbird)
  
  #  and add social brood
  coxmeCF.full.maternalsocialbird <- coxme(Surv(start, end, finish)~ strata(wall)+
                                          zdeld2 + zdeld2d12 +
                                          location + noise + factorwd1to3 +
                                          factorcohort + releaser + ztime +
                                          zendprob +
                                          (1|factorbirdid) + (1|factorsocial) +
                                          (1|factormum),
                                        data=edgecfYe)
  
  print(coxmeCF.full.maternalsocialbird)
  
  coxmeCF.full.maternal <- coxme(Surv(start, end, finish)~ strata(wall)+
                                             zdeld2 + zdeld2d12 +
                                             location + noise + factorwd1to3 +
                                             factorcohort + releaser + ztime +
                                             zendprob +
                                             (1|factorbirdid) + (1|factorsocial) +
                                             (1|factornatal) + (1|factormum),
                                           data=edgecfYe)
  
  print(coxmeCF.full.maternal)
}

{
  # compared to the model with all random effects and with
  # the social brood:
  print(coxmeCF.full.maternalandbird)
  print(coxmeCF.full.maternalsocialbird)
  print(coxmeCF.full)
  print(coxmeCF.full.n)
  # so the social mother has a really similar effect to 
  # the natal brood
}

############################################################
############################################################

# another suggestion is that I should not include
# broods where I do not know the mass of a nestling
# because the hierarchy calculations for these
# broods do not reflect reality.

# In addition, in October 2015 I established that there
# were nestlings whose identities were uncertain. These
# nestlings were nail-clipped and cross-fostered, and 
# either there was an issue at cross-fostering or their
# nail cuts grew out leading to uncertainty over their
# exact identity. There is probably overlap between these
# nestlings and the nestlings that are missing day two
# mass, but it will be important to check:

# nestlings with the following bird IDs:
# 5342, 5344, 6283, 6284, 6285, 6286, 6287, 6288, 6289,
# 6279, 6280, 6281, 6282, 6869, 6937, 6979, 6986, 6300, 6874
# and brood ids 1366, 1368, and 1522.

# I think it is valid to use these broods for the
# main models, but to secondarily see whether the
# model conclusions change if I exclude these broods.
# But first, I need to know which broods they are:

# list which rows contain missing data:
na12 <- which(is.na(ar$mass))
na2 <- which(is.na(ar$d2mass))


# how many individuals are missing one or both?
length(unique(ar$birdid[which(is.na(ar$mass))]))
length(unique(ar$birdid[which(is.na(ar$d2mass))]))

# pull out the associated broods from those rows:
s.exclude <- unique(ar$social[na12])
s.exclude2 <- unique(ar$social[na2])



# 26 social broods to exclude from my first
# sets of models with the full data set. These
# models only consider the social brood hierarchy
# but the following models compare social and 
# natal brood hierarchies, so I need to know
# which natal hierarchies to also exclude for
# these models:

n.exclude <- unique(ar$natal[na12])
n.exclude2 <- unique(ar$natal[na2])

# In addition, in October 2015 I established that there
# were nestlings whose identities were uncertain. These
# nestlings were nail-clipped and cross-fostered, and 
# either there was an issue at cross-fostering or their
# nail cuts grew out leading to uncertainty over their
# exact identity. There is probably overlap between these
# nestlings and the nestlings that are missing day two
# mass, but it will be important to check:

# nestlings with the following bird IDs:
# 5342, 5344, 6283, 6284, 6285, 6286, 6287, 6288, 6289,
# 6279, 6280, 6281, 6282, 6869, 6937, 6979, 6986, 6300, 6874
# and brood ids 1366, 1368, and 1522.


# these broods are (mostly) not in the current 'exclude' lists:

n.exclude3 <- c(1366, 1368, 1522)
s.exclude3 <- c(1366, 1368, 1522)

# and I want to know the individuals' broods too:

exclude.ids <- c(5342, 5344, 6283, 6284, 6285, 6286, 6287,
                 6288, 6289, 6279, 6280, 6281, 6282, 6869, 
                 6937, 6979, 6986, 6300, 6874)

# which of these birds turn up in the data set and where:
match(ar$birdid, exclude.ids)
which(match(ar$birdid, exclude.ids)>0)
# and in what broods:
s.exclude4 <- ar$social[which(match(ar$birdid, exclude.ids)>0)]
n.exclude4 <- ar$natal[which(match(ar$birdid, exclude.ids)>0)]

length(unique(s.exclude4))
length(unique(n.exclude4))
length(unique(c(s.exclude4,n.exclude4)))


socialtoexclude <- unique(c(s.exclude, s.exclude2, s.exclude3, s.exclude4))
socialtoexclude
length(socialtoexclude)
# now 30 broods to exclude. Not bad. Not perfect.


nataltoexclude <- unique(c(n.exclude, n.exclude2, n.exclude3, n.exclude4))
nataltoexclude
length(nataltoexclude)
# 34 natal broods.


# match this to the data set. With the whole data
# set, I am primarily concerned with the social broods
# and therefore the ones to exclude are the social
# broods from list socialtoexclude
edgeall$soc.ex <- match(edgeall$social,socialtoexclude)
edgeall$soc.ex[is.na(edgeall$soc.ex)] <- 0
edgeall$soc.ex

edgesocex <- subset(edgeall, edgeall$soc.ex==0)
head(edgesocex)
edgesocex$soc.ex==0

# re-scale the data set:

edgesocex$ztime <- scale(edgesocex$time)
edgesocex$zsocbroodsz <- scale(edgesocex$socbroodsz)
edgesocex$zendprob <- scale(edgesocex$endprob)
edgesocex$zd2deltasoc <- scale(edgesocex$d2deltasoc)
edgesocex$zd12deltasoc <- scale(edgesocex$d12deltasoc)
edgesocex$zd2mass <- scale(edgesocex$d2mass)
edgesocex$zd12mass <- scale(edgesocex$d12mass)
edgesocex$zresidmass <- scale(edgesocex$residmass)

length(edgeall[,1])
length(edgesocex[,1])
# 272 observations lost

###

# but for the later models using data set edgecfYe I
# have to exclude both natal and social broods, since 
# the analysis is now about both natal and social broods:

edgecfYe$soc.ex <- match(edgecfYe$social,socialtoexclude)
edgecfYe$soc.ex[is.na(edgecfYe$soc.ex)] <- 0

edgecfYe$nat.ex <- match(edgecfYe$natal,nataltoexclude)
edgecfYe$nat.ex[is.na(edgecfYe$nat.ex)] <- 0

edgeYex <- subset(edgecfYe, edgecfYe$soc.ex==0)
edgeYex <- subset(edgeYex, edgeYex$nat.ex==0)
head(edgeYex)
summary(edgeYex$soc.ex)
summary(edgeYex$nat.ex)

length(edgecfYe[,1])
length(edgeYex[,1])
# lost 231 observations. Ouch


edgeYex$zd2deltasoc <- scale(edgeYex$d2deltasoc)
edgeYex$zd12deltasoc <- scale(edgeYex$d12deltasoc)

edgeYex$zdeld2 <- scale(edgeYex$deld2)
edgeYex$zdeld2d12 <- scale(edgeYex$deld2d12)

edgeYex$ztime <- scale(edgeYex$time)
edgeYex$zsocbroodsz <- scale(edgeYex$socbroodsz)
edgeYex$zendprob <- scale(edgeYex$endprob)
edgeYex$zd2mass <- scale(edgeYex$d2mass)
edgeYex$zd12mass <- scale(edgeYex$d12mass)
edgeYex$zresidmass <- scale(edgeYex$residmass)


summary(edgeYex)
summary(edgesocex)
names(edgeYex)
names(edgesocex)




# remember there was a difference in activity between
# the cross-fostering categories earlier? What happens
# when these nestlings are removed:

coxme1.CF.conservative <- coxme(Surv(start, end, finish)~ strata(wall)+
                                  zd2deltasoc * zd12deltasoc + relevel(CFN,2) + 
                                  zd12mass +
                                      (1|factorbirdid) + (1|factorsocial),
                                    data=edgesocex)

print(coxme1.CF.conservative)


coxme1.CFonly.conservative <- coxme(Surv(start, end, finish)~ strata(wall)+
                         relevel(CFN,2) + 
                         (1|factorbirdid) + (1|factorsocial),
                       data=edgesocex)

print(coxme1.CFonly.conservative)

# removing these nestlings really knocks this relationship 
# down.


# mothers:

coxme1.maternal.conservative <- coxme(Surv(start, end, finish)~ strata(wall)+
                           zd2deltasoc*zd12deltasoc + relevel(CFN,2) + 
                           zd12mass + 
                           (1|factorbirdid) + (1|factorsocial) + (1|factormum),
                         data=edgesocex)

print(coxme1.maternal)



# do my conclusions about the change between days
# two and 12 and about the natal and social hierarchies
# remain the same?


coxmeCF.reduced.conservative <- coxme(Surv(start, end, finish)~ strata(wall)+
                           zdeld2 + zdeld2d12 +
                           (1|factorbirdid) + (1|factorsocial) + 
                           (1|factornatal),
                         data=edgeYex)

print(coxmeCF.reduced.conservative)


# and the mum:


coxmeCF.maternal.conservative <- coxme(Surv(start, end, finish)~ strata(wall)+
                            zdeld2 + zdeld2d12 +
                            (1|factorbirdid) + (1|factorsocial) + 
                            (1|factornatal) + (1|factormum),
                          data=edgeYex)

print(coxmeCF.maternal.conservative)


###################################################################
###################################################################

# 23rd November 2015
# Discussion with Mirre Simons - is it possible to attribute the
# social brood variance to social brood interactions? What else 
# could it be? One possibility is environmental similarity within
# a day that does not extend across days.

# first test: for nestlings tested on both days 10 and 12, is the
# social brood covariance a similar effect between the two days?
# take whole data set and reduce to days 10 and 12 because there
# are not many day 13 measures to calculate a covariance:
{
  head(ar)
  table(ar$age)
  ar10to12 <- subset(ar, ar$age<13)
  
  summary(ar10to12)
  table(ar10to12$age)
}

# and then do an MCMC analysis with activity as the dependent 
# variable and social brood and bird ID as random effects that 
# interact with age

# as Gaussian:
{
  library(MCMCglmm)
  
  # make factors for analysis:
  
  ar10to12$factorbirdid <- as.factor(ar10to12$birdid)
  ar10to12$factorsocial <- as.factor(ar10to12$social)
  ar10to12$factorage <- as.factor(ar10to12$age)
  
  # as a Gaussian trait, following the heritability work
  # in Winney et al xxxx:
  priorIW2 <- list(R=list(V=diag(2), nu=0.002),
                   G=list(G1=list(V=diag(2), nu=0.002),
                          G2=list(V=diag(2), nu=0.002)))

  socialbroodcovGaussian <- MCMCglmm(log(total-0.5)~factorage,
                             random=~us(factorage):factorbirdid +
                               us(factorage):factorsocial,
                             rcov=~idh(factorage):units,
                             data=ar10to12,
                             prior=priorIW2,
                             nitt=1000000,
                             thin=800,
                             burnin=200000)
}
# plot Gaussian output:
plot(socialbroodcovGaussian)

# model checking: autocorrelation <0.1 and good sampling per estimate:
{autocorr(socialbroodcovGaussian$Sol)
 autocorr(socialbroodcovGaussian$VCV)
 summary(socialbroodcovGaussian)}
# Yes all good. Autocorrelation essentially below 0.05 
# and numbers of samples per estimate high, sometimes
# exceeding 1000

# results to quote in Table S3:
posterior.mode(socialbroodcovGaussian$Sol)
HPDinterval(socialbroodcovGaussian$Sol)
posterior.mode(socialbroodcovGaussian$VCV)
HPDinterval(socialbroodcovGaussian$VCV)

# as Poisson:
{
  socialbroodcovPoisson <- MCMCglmm(total~factorage,
                             random=~us(factorage):factorbirdid +
                               us(factorage):factorsocial,
                             rcov=~idh(factorage):units,
                             data=ar10to12,
                             family="poisson",
                             prior=priorIW2,
                             nitt=1000000,
                             thin=800,
                             burnin=200000)
}
# plot Poisson output:
plot(socialbroodcovPoisson)

# the Gaussian is more conservative with the social
# brood covariance.


# model checking: autocorrelation <0.1 and good sampling per estimate:
{autocorr(socialbroodcovPoisson$Sol)
 autocorr(socialbroodcovPoisson$VCV)
 summary(socialbroodcovPoisson)}
# good as well. Autocorrelation generally below 0.08 and sampling
# all above 1000 per estimate.



###################################################################
###################################################################


# 15th December 2015
# will any of my models have different results if I
# use only the first observation from each nestling?
# This first observation is free from habituation so
# this simplifies the investigation.

# I created a first observations only data set in order to check
# whether any of my hierarchies were best included as linear 
# or quadratic or factor effects. Now I can re-use this 
# data set, called edgesingle, for this analysis:

str(edgesingle)

# re-scale the data:

edgesingle$factorbirdid <- as.factor(edgesingle$birdid)
edgesingle$factornatal <- as.factor(edgesingle$natal)
edgesingle$factorsocial <- as.factor(edgesingle$social)
edgesingle$factorsocial <- as.factor(edgesingle$social)
edgesingle$ztime <- scale(edgesingle$time)
edgesingle$zsocbroodsz <- scale(edgesingle$socbroodsz)
edgesingle$zendprob <- scale(edgesingle$endprob)

# and my coefficients of interest:

edgesingle$zd2deltasoc <- scale(edgesingle$d2deltasoc)
edgesingle$zd12deltasoc <- scale(edgesingle$d12deltasoc)
edgesingle$zd2mass <- scale(edgesingle$d2mass)
edgesingle$zd12mass <- scale(edgesingle$d12mass)
edgesingle$zresidmass <- scale(edgesingle$residmass)


# re-run the first models, 

single.hierarchies.reduced <- coxme(Surv(start, end, finish)~ strata(wall)+
                                   zd2deltasoc*zd12deltasoc + CFN+ zd12mass + 
                                   (1|factorsocial),
                                 data=edgesingle)

print(single.hierarchies.reduced)

# the conclusions that I would draw from this model would
# be the same as the equivalent model with all data. No
# relationship between hierarchy and activity. Cross-fostered
# nestlings appear quite different to each other.

# now the full model but MINUS factorwd1to3 because the
# data now has only single observations.

single.hierarchies.full <- coxme(Surv(start, end, finish)~ strata(wall)+
                       zd2deltasoc*zd12deltasoc + CFN+ 
                       zd12mass + zd2mass + zresidmass +
                       location + noise + factorcohort + 
                         releaser + ztime + zendprob +
                       (1|factorsocial),
                     data=edgesingle)

print(single.hierarchies.full)
# endprob hardly matters now. How weird.
# Some of the coefficients change but the main conclusions
# remain the same: no hierarchy effects, differences between
# cross-fostering groups, variance to social brood ID.



# now on to the cross-fostered and changed hierarchy
# place subset, where the nestlings are being tested for
# the effect of a change of hierarchy place:


# make subset:
edgecfYesingle <- subset(edgesingle, edgesingle$CFN=="cfY")

# re-scale the data:
edgecfYesingle$zd2deltasoc <- scale(edgecfYesingle$d2deltasoc)
edgecfYesingle$zd12deltasoc <- scale(edgecfYesingle$d12deltasoc)

edgecfYesingle$zdeld2 <- scale(edgecfYesingle$deld2)
edgecfYesingle$zdeld2d12 <- scale(edgecfYesingle$deld2d12)

edgecfYesingle$ztime <- scale(edgecfYesingle$time)
edgecfYesingle$zsocbroodsz <- scale(edgecfYesingle$socbroodsz)
edgecfYesingle$zendprob <- scale(edgecfYesingle$endprob)
edgecfYesingle$zd2mass <- scale(edgecfYesingle$d2mass)
edgecfYesingle$zd12mass <- scale(edgecfYesingle$d12mass)
edgecfYesingle$zresidmass <- scale(edgecfYesingle$residmass)

edgecfYesingle$factorbirdid <- as.factor(edgecfYesingle$birdid)
edgecfYesingle$factorsocial <- as.factor(edgecfYesingle$social)
edgecfYesingle$factornatal <- as.factor(edgecfYesingle$natal)




single.changeplace.full <- coxme(Surv(start, end, finish)~ strata(wall)+
                        zdeld2 + zdeld2d12 + location + noise + 
                        factorcohort + releaser + ztime + zendprob +
                        (1|factorsocial) + (1|factornatal),
                      data=edgecfYesingle)

print(single.changeplace.full)


single.changeplace.fullCIsocial <- coxmeCI(single.changeplace.full, n.random=2, 
                     desired.random=1, low=0.02, 
                     high=1, n.estimates=20)

single.changeplace.fullCInatal <- coxmeCI(single.changeplace.full, n.random=2, 
                                           desired.random=2, low=0.02, 
                                           high=1, n.estimates=20)


# to work out the significance of each random effect, knock
# them out one at a time and compare the model chisq to the 
# full model:

single.changeplace.full.s <- coxme(Surv(start, end, finish)~ strata(wall)+
                                   zdeld2 + zdeld2d12 + location + noise + 
                                   factorcohort + releaser + ztime + zendprob +
                                   (1|factornatal),
                                 data=edgecfYesingle)

print(single.changeplace.full.s)

single.changeplace.full.n <- coxme(Surv(start, end, finish)~ strata(wall)+
                                   zdeld2 + zdeld2d12 + location + noise + 
                                   factorcohort + releaser + ztime + zendprob +
                                   (1|factorsocial),
                                 data=edgecfYesingle)

print(single.changeplace.full.n)

# significance of social brood:
1-pchisq(18.53-18.23, 1)


# significance of natal brood:
1-pchisq(18.53-18.35, 1)

# the one without fixed effects:

single.changeplace.reduced <- coxme(Surv(start, end, finish)~ strata(wall)+
                                   zdeld2 + zdeld2d12 +
                                   (1|factorsocial) + (1|factornatal),
                                 data=edgecfYesingle)

print(single.changeplace.reduced)

# There is quite a reasonable correlation between the
# mass on day 12 and the hierarchy change between days
# two and 12.


plot(edgecfYesingle$deld2d12, edgecfYesingle$d12mass)
cor(edgecfYesingle$deld2d12, edgecfYesingle$d12mass)

# some of the significance of the change in hierarchy 
# comes from the change in absolute mass:


single.changeplace.reduced.m <- coxme(Surv(start, end, finish)~ strata(wall)+
                                      zdeld2 + zdeld2d12 + zd12mass +
                                      (1|factorsocial) + (1|factornatal),
                                    data=edgecfYesingle)

print(single.changeplace.reduced.m)


# which comes out as expected. However, in the full model:



single.changeplace.full.m <- coxme(Surv(start, end, finish)~ strata(wall)+
                                   zdeld2 + zdeld2d12 + location + noise + 
                                   factorcohort + releaser + ztime + zendprob +
                                   zd12mass +
                                   (1|factorsocial) + (1|factornatal),
                                 data=edgecfYesingle)

print(single.changeplace.full.m)

# adding mass now swings the variance towards the natal
# brood. This could be because nestlings are more likely
# to be cross-fostered with their social nest-mates.

# significances and estimates:

single.changeplace.full.mCIsocial <- coxmeCI(single.changeplace.full.m, n.random=2, 
                                           desired.random=1, low=0.02, 
                                           high=1, n.estimates=20)

single.changeplace.full.mCInatal <- coxmeCI(single.changeplace.full.m, n.random=2, 
                                          desired.random=2, low=0.02, 
                                          high=1, n.estimates=20)


# to work out the significance of each random effect, knock
# them out one at a time and compare the model chisq to the 
# full model:

single.changeplace.full.m.s <- coxme(Surv(start, end, finish)~ strata(wall)+
                                     zdeld2 + zdeld2d12 + location + noise + 
                                     factorcohort + releaser + ztime + zendprob +
                                       zd12mass +
                                     (1|factornatal),
                                   data=edgecfYesingle)

print(single.changeplace.full.m.s)

single.changeplace.full.m.n <- coxme(Surv(start, end, finish)~ strata(wall)+
                                     zdeld2 + zdeld2d12 + location + noise + 
                                     factorcohort + releaser + ztime + zendprob +
                                       zd12mass +
                                     (1|factorsocial),
                                   data=edgecfYesingle)

print(single.changeplace.full.m.n)

# significance of social brood:
1-pchisq(25.86-25.82, 1)


# significance of natal brood:
1-pchisq(25.86-25.01, 1)






# how do I figure out how many nestlings each individual
# was cross-fostered with?


# calculate original brood sizes:
ardu$one <- 1
broodsizes <- aggregate(ardu$one, list(ardu$social), FUN=sum)

head(broodsizes)

# the below expression takes ardu and counts the 
# unique numbers of natal brood IDs within each 
# social brood ID.
with(ardu, tapply(natal, social, FUN = function(x) length(unique(x))))

table(with(ardu, tapply(natal, social, FUN = function(x) length(unique(x)))))

length(unique(ardu$social))
# Just 30 broods out of 184 have nestlings from a mixture of
# broods, so it is hardly surprising that the variance is
# difficult to assign.

# and that is in the WHOLE data set. What about in the
# subset?


with(arducfY, tapply(natal, social, FUN = function(x) length(unique(x))))

table(with(arducfY, tapply(natal, social, FUN = function(x) length(unique(x)))))

length(unique(arducfY$social))
# better here, 29 out of 87, but still. Two thirds of nestlings
# remain with all their original nest mates.



######################################################################
######################################################################

# Question from JS 20160324
# In the model comparing cross-fostered individuals, what is the control
# for the experimental treatment of changing place in the hierarchy?
# Can the manipulated but not changed place individuals be included because
# they are part of the general manipulation but did not experience the change,
# so would be zero for delta hierachy?

# make an appropriate data set
{
  edgeCF <- subset(edgeall, edgeall$CFN!="N")
  
  table(edgeCF$CF)
  
  edgeCF$zd2deltasoc <- scale(edgeCF$d2deltasoc)
  edgeCF$zd12deltasoc <- scale(edgeCF$d12deltasoc)
  
  edgeCF$zdeld2 <- scale(edgeCF$deld2)
  edgeCF$zdeld2d12 <- scale(edgeCF$deld2d12)
  
  edgeCF$ztime <- scale(edgeCF$time)
  edgeCF$zsocbroodsz <- scale(edgeCF$socbroodsz)
  edgeCF$zendprob <- scale(edgeCF$endprob)
  edgeCF$zd2mass <- scale(edgeCF$d2mass)
  edgeCF$zd12mass <- scale(edgeCF$d12mass)
  edgeCF$zresidmass <- scale(edgeCF$residmass)
  
  edgeCF$factorbirdid <- as.factor(edgeCF$birdid)
  edgeCF$factorsocial <- as.factor(edgeCF$social)
  edgeCF$factornatal <- as.factor(edgeCF$natal)
}

# did this work how I wanted?
{
  table(edgeCF$CF)
  summary(edgeCF$deld2)
  hist(edgeCF$deld2)
  
  plot(edgeCF$deld2, edgeCF$end)
  plot(edgeCF$deld2d12, edgeCF$end)
}

# the basic, no frills model
{
  
  coxmeCF.reduced.control <- coxme(Surv(start, end, finish)~ strata(wall)+
                             zdeld2 + zdeld2d12 +
                             (1|factorbirdid) + (1|factorsocial) + 
                             (1|factornatal),
                           data=edgeCF)
  
  print(coxmeCF.reduced.control)
}

# the deld2 and deld2d12 have spookily similar z and p values and
# coefficient estimates. Are they overlapping? 

{
  
  coxmeCF.reduced.control.d2 <- coxme(Surv(start, end, finish)~ strata(wall)+
                                     zdeld2 +
                                     (1|factorbirdid) + (1|factorsocial) + 
                                     (1|factornatal),
                                   data=edgeCF)
  
  print(coxmeCF.reduced.control.d2)
  
  coxmeCF.reduced.control.d2d12 <- coxme(Surv(start, end, finish)~ strata(wall)+
                                     zdeld2d12 +
                                     (1|factorbirdid) + (1|factorsocial) + 
                                     (1|factornatal),
                                   data=edgeCF)
  
  print(coxmeCF.reduced.control.d2d12)
}