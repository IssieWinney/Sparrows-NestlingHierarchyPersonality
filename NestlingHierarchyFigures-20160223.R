# 2nd November 2015

# Plots to accompany peas in a pod manuscript:

# Figure 1:
# the purpose of these plots is to show that heavy
# nestlings move further both in the raw data and
# in the model


###
# Figure 1a - this plots the DATA not the MODEL
###


library(ggplot2)
library(survival)
library(gtools)
library(grid)

# run the survival model I want to plot:

# use one measure per nestling: use data set ardu

edgesingle$massd12.threecategories <- quantcut(edgesingle$d12mass, 
                                               seq(0,1,by=1/3))

summary(edgesingle$massd12.threecategories)

plotf4 <- survfit(Surv(start, end, finish)~massd12.threecategories +
                    strata(wall),
                  data=edgesingle)


# first, make a data frame from desired survival model

survdata <- data.frame(time=plotf4$time,
                       surv=plotf4$surv,
                       lower=plotf4$lower,
                       upper=plotf4$upper,
                       censored=ifelse(plotf4$n.censor>0, 1, 0))
head(survdata)

# add names stating which fixed effects are
# associated with each data point
survdata$strata <- rep(names(plotf4$strata), plotf4$strata)

table(survdata$strata)

# the names for strata are awful. I'd like to
# replace them with something easier to understand

stratanams <- data.frame("strata"=names(plotf4$strata))
stratanams

# label these names with a --> f so that in the key they
# are written in order, otherwise 9 comes after 2
stratanams$better <- c("a: 9.8 to 21.3 g, before wall",
                       "b: 9.8 to 21.3 g, after wall",
                       "c: 21.3 to 23.8 g, before wall",
                       "d: 21.3 to 23.8 g, after wall",
                       "e: 23.8 to 29.8 g, before wall",
                       "f: 23.8 to 29.8 g, after wall")
stratanams

survdata$Mass <- stratanams$better[match(survdata$strata, stratanams$strata)]

head(survdata$Mass)
# yay!

# next, make the blank plot frame to take the plot

# I want to use six paired colours to describe the data
# with the light version of each colour used for 'before 
# touching the wall' and the dark for 'after touching the
# wall'. I am basing this on the ColorBrewer palatte but
# I am removing the green from the palatte because I don't
# want a red-green colour blindness clash. So, the palatte
# for six colours:

ColourSeq <- c("#a6cee3", "#1f77b4",
               "#b2df8a", "#33a02c",
               "#fdbf6f", "#ff7f00")

fig1a <- ggplot(survdata, aes(x=time, y=surv)) +
  geom_blank() +
  xlab(xlab) +
  ylab(ylab) +
  theme_bw() +
  scale_colour_manual(values=ColourSeq) +
  scale_fill_manual(values=ColourSeq) +
  xlab("Nestling activity (squares)") +
  ylab("Proportion still active") +
  ggtitle("a)") +
  theme(legend.key.size = unit(0.4, "cm"),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10),
        plot.title=element_text(size=10, hjust=0, vjust=1),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10, face="plain"),
        panel.border=element_rect(fill=NA))

# make the y-axis go to zero
fig1a <- fig1a + ylim(0,1)


# add confidence intervals

# Create a data frame with stepped lines from the existing data
n <- nrow(survdata)
ys <- rep(1:n, each = 2)[-2*n] #duplicate row numbers and remove the last one
xs <- c(1, rep(2:n, each=2))   #first row 1, and then duplicate row numbers
scurve.step<-data.frame(time=survdata$time[xs], 
                        lower=survdata$lower[ys], 
                        upper=survdata$upper[ys],  
                        surv=survdata$surv[ys], 
                        Mass=survdata$Mass[ys])
# this is the line
fig1a <- fig1a +
  geom_ribbon(data=scurve.step, 
              aes(x=time,ymin=lower, 
                  ymax=upper, 
                  fill=Mass), 
              alpha=0.3)


# add plusses for censored points
# first, make a data frame picking out the location of 
# plusses
plusses <- subset(survdata, survdata$censored==1)
head(plusses)

# then add these plusses to the plot
fig1a <- fig1a +
  geom_point(data=plusses,
             aes(x=time, y=surv, col=Mass), pch=3)




# add the stepped survival line
# this is done after the other lines
# so that the survival line can be
# plotted over the top

#Survival stepped line
fig1a <- fig1a + 
  geom_step(data=survdata,
            aes(x=time, y=surv, col=Mass))

# remove legend for 1a (have in 1b instead):
fig1a <- fig1a + theme(legend.position="none")

fig1a



###
# Figure 1b - this plots the MODEL not the DATA
###



# run the survival model I want to plot:

# use the data set of single measures to avoid
# pseudoreplication

edgesingle$fmass <- quantcut(edgesingle$d12mass,seq(0,1,by=1/3))
table(edgesingle$fmass)



# this is for model fitted lines:
cph1 <- coxph(Surv(start, end, finish)~ strata(wall)+fmass,
              data=edgesingle)
summary(cph1)


# this is the dummy data I need to plot the coxph
# model
# carefully ordered so that the coefficients will
# be in the desired order for the legend and the 
# lines will be plotted in the right order (see
# stratanams to check this matches)
dummyd <- data.frame(wall=c(0,1,0,1,0,1), 
                     # double it up so that each delh2 is represented 
                     # with a 0 wall and a 1 wall
                     fmass=c("[9.8,21.3]",
                             "[9.8,21.3]",
                             "(21.3,23.8]",
                             "(21.3,23.8]",
                             "(23.8,29.8]",
                             "(23.8,29.8]")) 


# first, make a data frame from desired survival model
# that plots raw data

# Jan 2015: add a data frame for the model lines from cph1:

cph2 <- survfit(cph1, newdata=dummyd)
summary(cph2)

survdata2 <- data.frame(time=cph2$time,
                        surv=cph2$surv,
                        lower=cph2$lower,
                        upper=cph2$upper,
                        censored=ifelse(cph2$n.censor>0, 1, 0))

# add names stating which fixed effects are
# associated with each data point
survdata2$strata <- rep(names(cph2$strata), cph2$strata)

head(survdata2)

# use the stratanams data frame from before
# to name the data. This time though the fmass
# category in model cph2 has been defined as the
# numbers 1--6. So, add this to stratanams:

stratanams$names <- seq(1,6,1)

survdata2$Mass <- stratanams$better[match(survdata2$strata, 
                                          stratanams$names)]

head(survdata2)

# next, make the blank plot frame to take the plot
  
  fig1b <- ggplot(survdata2, aes(x=time, y=surv)) +
    geom_blank() +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw() +
  scale_colour_manual(values=ColourSeq) +
  scale_fill_manual(values=ColourSeq) +
    xlab("Nestling activity (squares)") +
    ylab("Proportion still active") +
    ggtitle("b)") +
    theme(legend.key.size = unit(0.4, "cm"),
          axis.text=element_text(size=8),
          axis.title=element_text(size=10),
          plot.title=element_text(size=10, hjust=0, vjust=1),
          legend.text=element_text(size=8),
          legend.title=element_text(size=10, face="plain"),
          legend.position="bottom",
          legend.direction="vertical",
          panel.border=element_rect(fill=NA))
  
  # make the y-axis go to zero
  fig1b <- fig1b + ylim(0,1)
  
  
  # add confidence intervals
  
  # Create a data frame with stepped lines from the existing data
  
  # for the model
  n <- nrow(survdata2)
  ys <- rep(1:n, each = 2)[-2*n] #duplicate row numbers and remove the last one
  xs <- c(1, rep(2:n, each=2))   #first row 1, and then duplicate row numbers
  scurve.step2<-data.frame(time=survdata2$time[xs], 
                           lower=survdata2$lower[ys], 
                           upper=survdata2$upper[ys],  
                           surv=survdata2$surv[ys], 
                           Mass=survdata2$Mass[ys])
  
  
  
  # this is the line for the data
  
  # add the line for the model
  fig1b <- fig1b +
    geom_ribbon(data=scurve.step2, 
                aes(x=time,ymin=lower, ymax=upper, fill=Mass), 
                alpha=0.3)
  
  # add plusses for censored points
  # first, make a data frame picking out the location of 
  # plusses
  plusses <- subset(survdata2, survdata2$censored==1)
  head(plusses)
  
  # then add these plusses to the plot
  fig1b <- fig1b +
    geom_point(data=plusses,
               aes(x=time, y=surv, col=Mass), pch=3)
  
  
  
  
  # add the stepped survival line
  # this is done after the other lines
  # so that the survival line can be
  # plotted over the top
  
  #Survival stepped line
  fig1b <- fig1b + 
    geom_step(data=survdata2,
              aes(x=time, y=surv, col=Mass))
  
  
  
  fig1b


##########################################################
# Figure 1c - 20160224
##########################################################

# During feedback, Echo said reading plots 1a and 1b was
# challenging because of the number of lines. Therefore,
# I would like to add a plot of the distance that nestlings
# move against their mass. I will need to separate the
# distance moved in to distance before touching the wall 
# and distance after touching the wall.
{
  # make a variable 'distance', which is the difference
  # between where a nestling starts moving and where it 
  # stops moving either before or after touching the wall:

  # first, because I know this plot looks like nothing is happening,
  # let's see how the data shows this relationship:
  summary(lm(log(ardu$total+0.5)~ardu$d12mass))
  # so, this first model shows no relationship in general between
  # mass and distance moved.
  summary(lm(log(ardu$total+0.5)~ardu$massd12.threecategories))
  # Whereas this model suggests the heaviest nestlings are the ones moving
  # further.
  summary(lm(log(ardu$total+0.5)~ardu$d12mass+I(ardu$d12mass^2)))
  # though this model cannot corroborate this.
  
  summary(lm(log(edgesingle$end-edgesingle$start+0.5)~edgesingle$d12mass*edgesingle$wall))
  # This model is the distance split in to what a nestling does
  # before it touches the wall and after. Obviously, before touching the
  # wall is associated with not moving far. In fact, heavier nestlings
  # move less far, probably because they move quickly to the edge!
  # But after a nestling has touched the wall, the nestling moves much further
  # if the nestling is heavier.
  
   
  plot(ardu$d12mass, jitter(ardu$total), log="y", 
         pch=16, col=rgb(0,0,0,0.5))
    
    
  summary(lm(log(edgesingle$end-edgesingle$start+0.5)~edgesingle$d12mass*edgesingle$wall))
  
  # make the colours I would like to use:
  
  edgesingle$fillcolour <- ifelse(edgesingle$wall==0,
                                  ColourSeq[1],
                                  ColourSeq[2])
  
  fig1c <- ggplot(edgesingle, aes(x=d12mass, y=end-start))+
    theme(legend.key.size = unit(0.4, "cm"),
          axis.text=element_text(size=8, colour="black"),
          axis.title=element_text(size=10),
          plot.title=element_text(size=10, hjust=0, vjust=1),
          legend.text=element_text(size=8),
          legend.title=element_text(size=10, face="plain"),
          legend.position="bottom",
          legend.direction="vertical",
          panel.background=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_rect(fill=NA)) +
    ylab("Nestling activity (squares)") +
    xlab("Day 12 mass (grams)") +
    ggtitle("c)") +
    geom_point(aes(colour=fillcolour), size=1) +
    scale_colour_identity(guide="legend",
                          name="Before/After touching wall",
                          labels=c("After", "Before"))
  
  fig1c
}

##########################################################
##########################################################

# grab legend with the grob:
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend1b <- g_legend(fig1b)
library(grid)
grid.draw(legend1b)

legend1c <- g_legend(fig1c)
grid.draw(legend1c)

# and update the plots to have no legend:

fig1bnoleg <- fig1b + theme(legend.position="none")
fig1cnoleg <- fig1c + theme(legend.position="none")


##########################################################
##########################################################


# now plot together

library(gridExtra)

png(filename = "Figures/Figure1-20160224.png",
    width=177, height=120, units="mm", res=600)

grid.arrange(fig1a, fig1bnoleg, fig1cnoleg,
             grid.rect(gp=gpar(col="white")), legend1b, legend1c,
             ncol=3, nrow=2, heights=c(2/3, 1/3))

dev.off()

##################################################


# Figure 2:
# within-brood similarity could be driven by within-
# brood competition to be the same. If this was the
# case, we would expect a relationship where broods
# with more variable mass nestlings are less variable
# in their activity. However, this plot of broood
# standard deviations in day 12 mass versus activity
# suggests this is not the case.

# standard deviation withiin a brood for the nestlings
# used in the analysis:
d12sds <- aggregate(ardu$d12mass, by=list(ardu$social), FUN=sd)
head(d12sds)

names(d12sds) <- c("social", "sdd12")
head(d12sds)


# variance of activity within a brood based on all
# measures (this means in 2011 it might be less accurate
# because of one measure rather than three per individual):

d12act <- aggregate(ar123m12tarsus$total, 
                    by=list(ar123m12tarsus$social), FUN=sd)
head(d12act)

names(d12act) <- c("social", "actd12")
head(d12act)

# add to first data frame:
d12sds$actd12 <- d12act$actd12[match(d12sds$social, d12act$social)]
head(d12act)
summary(d12sds)

# some broods where I don't have standard deviation 
# in mass (makes sense where brood size = 1) and one
# where there is no standard deviation in activity
# (a brood from 2011?)
d12sds[which(is.na(d12sds$actd12)),]
# well it is missing both mass and activity.
ardu[which(ardu$social==1522),]
# yup, only one data point.


# plot the standard deviation in day 12 mass against
# the standard deviation in activity. As a result of
# some zeros, and the data being log-normal, I have
# added one to the y-variables to log the y-axis:


png(filename = "Figure2-20151102.png",
    width=85, height=85, units="mm", res=600)

par(mar=c(3.5,3.5,1,1))

plot(d12sds$sdd12, (d12sds$actd12+1), 
     log="y", pch=16, col=rgb(0,0,0,0.5),
     ylab="",
     xlab="",
     cex.lab=1, cex.axis=0.8, cex=0.8)

mtext(side=1, "social brood SD mass (g)",
      line=2, cex=1)
mtext(side=2, "social brood SD activity (squares+1)",
      line=2, cex=1)

dev.off()



#####################################################

# Figure S1: two plots together.
# The fist shows that mass and tarsus on day 12 
# have a linear relationship
# The second shows that mass on days two and 12
# is not completely dependent, so the relationship
# between them and activity can be separated.


png(filename = "FigureS1-20151102.png",
    width=177, height=95, units="mm", res=600)


par(mfrow=c(1,2),
    mar=c(4.5,4.5,2,1))

# Figure S1a:

# plot showing the relationship between tarsus and
# mass on day 12 is mostly linear, so that I can 
# interpret it as a linear effect:

plot(ardu$tarsus, ardu$mass,
     pch=16, col=rgb(0,0,0,0.5),
     cex.lab=1, cex.axis=0.8,
     xlab="Day 12 tarsus (mm)", ylab="Day 12 mass (g)")

mtext("a)", side=3, line=1, adj=0, cex=1)


# Figure S1b:
# plot day two against day 12 mass to show
# that they aren't the same thing:

plot(ardu$d2mass, ardu$mass,
     pch=16, col=rgb(0,0,0,0.5),
     cex.lab=1, cex.axis=0.8,
     xlab="Day 2 mass (g)", ylab="Day 12 mass (g)")

mtext("b)", side=3, line=1, adj=0, cex=1)

dev.off()


#
