
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                         PRELIM 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


############################### SET UP ###############################

rm(list=ls())

root.dir = "~/Dropbox/Personal computer/Independent studies/Uncanny Valley III (UV3)/UV3_OSF/uv3_git/5_Expt_2"


# disable scientific notation because causes big numbers to be treated as equal
options(scipen=999)


##### Set directories #####
# these need to match the directories in data_prep.R
# location of code
code.dir = paste( root.dir, "/Code", sep="" )
setwd(code.dir)
source("helper_overall_analysis.R")

# load MM's favorite utility functions
# ~~~ LATER MAKE THIS PUBLIC
setwd("~/Dropbox/Personal computer/Reference sheets/R/mm_package")
source("R/functions.R")

# "here" package was being a butt
#data.dir = here("Data")
data.dir = paste( root.dir, "/Data", sep="" )

# where to save results
results.dir = here("Results")


##### Read in Data #####
setwd(data.dir)
library(readr)
l = read_csv("long_prepped.csv")
nrow(l)

# face-level dataset with emotion ratings (we'll adjust for this)
setwd("~/Dropbox/Personal computer/Independent studies/Uncanny Valley III (UV3)/UV3_OSF/uv3_git/5_Expt_2/Materials/Qualtrics materials")
face.means = read_csv("validated_stimuli_urls_html.csv")
# for merging purposes
face.means$stim.name = paste( "face.", 1:nrow(face.means), sep = "" )

# merge them
l = merge( l, face.means, by = "stim.name" )
# this is the mh score from current subjects
names(l)[ names(l) == "mh.x" ] = "mh" 
# this is the mh score from validation subjects
names(l)[ names(l) == "mh.y" ] = "mh.val" 

# remove stupid columns
l = l[ , !names(l) %in% c("X.x", "X.y") ]


############################### "TABLE ONE" ###############################

# # make sure the following vars are well-behaved
# vars = c( "cat", "xdev", "xflips", "speed", "rxnt", "area", "mh", "lik" )
# 
# library(tableone)
# CreateTableOne(data = l[,vars], includeNA = TRUE)
# 
# ( summary.list = lapply( vars, FUN = function(x) summary(l[[x]]) ) )


############################### EXCLUSIONS ###############################

# exclude trials with extreme values of mediators
l$exclude = "no"

# ~~~ NEED TO MANUALLY LOOK FOR SUBJECTS REPORTING PROBLEMS

med.names = c("area",
              "xdev",
              "rxnt",
              "speed",
              "xflips")

med.outliers = matrix(NA, ncol=length(med.names), nrow=nrow(l))

for ( i in 1:length(med.names) ) {
  med.outliers[,i] = is.outlier( l[[ med.names[i] ]] )
}

# proportion of each mediator that are outliers
# ~~~ should save and report this
colMeans(med.outliers)

# exclude trials that were outlying on any of these
outlier.count = apply( med.outliers, 1, sum )

l$exclude[ outlier.count > 1 ] = "outlying mediator"

# for reporting in paper (proportion trials excluded): 7%
( prop.excluded.med = sum(l$exclude == "outlying mediator") / nrow(l) )

l = l[ l$exclude == "no", ]
dim(l)


#CreateTableOne(vars = vars, data = l, includeNA = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                         UV CURVE 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


############################### UV CURVE ###############################

# polynomial regression
# not yet using inverse-weighting by likeability variance

# ##### Decide What Functional Form to Use When Adjusting For Emotion #####
# # see what functional form we should use when adjusting for emotion
# library(ggplot2)
# ggplot( data = l, aes( x = mean.emot, y = mh) ) +
#   geom_point() +
#   stat_smooth() +
#   theme_classic()
# 
# # see what functional form we should use when adjusting for emotion
# ggplot( data = l, aes( x = mean.emot, y = lik) ) +
#   geom_point() +
#   stat_smooth() +
#   theme_classic()
# # linear should be reasonable for both


##### Fit GEE Polynomial Model #####
# http://ww2.coastal.edu/kingw/statistics/R-tutorials/simplenonlinear.html
# GEE with polynomial terms
# working exchangeable structure by face
library(geepack)

# # lm version
# poly4.unadj = lm( lik ~ mh + I(mh^2) + I(mh^3) + I(mh^4),
#                 data = l ); poly4

# adjusted (preregistered) model
poly4.lm.adj = lm( lik ~ mh + I(mh^2) + I(mh^3) + I(mh^4) +
                  mean.emot,
                  data = l )

# GEE version, adjusting for emotion
poly4.gee.adj = geeglm( lik ~ mh + I(mh^2) + I(mh^3) + I(mh^4) +
                  mean.emot,
              id = stim.name,
              data = l,
              corstr = "exchangeable" )

# test each polynomial coefficient (adding terms sequentially)
anova(poly4)
# third-order model is best

best.mod = poly4.gee.adj



# 
# # robust SES
# library(sandwich)
# library(lmtest)
# best.mod = lm( l$lik ~ l$mh + I(l$mh^2) + I(l$mh^3) + I(l$mh^4) )
# coeftest(best.mod, vcov = vcovHC(best.mod, "HC1"))

library(broom)
tidy(best.mod)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                         UV CURVE 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


################################## UV SCATTERPLOT WITH FITTED CURVES ################################## 

# fitted equations for 3 models
eq1 = function(x) {
  model = best.mod
  coef(model)[1] +
    coef(model)[2]*x +
    coef(model)[3]*(x^2) +
    coef(model)[4]*(x^3) +
    coef(model)[5]*(x^4)
}

# not adjusted for emotion
eq2 = function(x) {
  model = poly4.lm.adj
  coef(model)[1] +
    coef(model)[2]*x +
    coef(model)[3]*(x^2) +
    coef(model)[4]*(x^3) +
    coef(model)[5]*(x^4)
}


# get CI for the analysis model
#fit = predict(best.mod, se.fit=TRUE) # se.fit doesn't work with GEE
fit = data.frame( fit = predict(best.mod),
                  se.fit = 0 )  # ~~ TEMP ONLY

predframe = data.frame("lik" = fit$fit, "lwr"=fit$fit-1.96*fit$se.fit,
                       "upr"=fit$fit+1.96*fit$se.fit, "mh"=l$mh)

xlab="Mechano-humanness score (-100 to 100)"
ylab="Likability (-100 to 100)"
shapes = c(45, 43)

library(ggplot2)
ggplot(l, aes(x=mh, y=lik) ) +
  geom_hline(yintercept=0, color="gray", lwd=.6) +  # reference line for neutrality
  #geom_point(alpha = 0.1) +  # can comment out to improve speed
  theme_classic() +
  
  geom_smooth() +  # LOESS
  #geom_ribbon(data=predframe, aes(ymin=lwr, ymax=upr), alpha=0.4) +  # CI band for main analysis model
  #stat_function(fun=eq1, color="red", lwd=1.2) +  # best-fitting poly
  stat_function(fun=eq2, color="green", lwd=1.2) +  # lm
  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  scale_y_continuous(breaks=seq(-100, 100, 25)) +
  #scale_color_manual(values=col, labels=labels, name="") +
  scale_shape_manual(values=shapes) +
  theme(text = element_text(size=28) ) +
  
  theme(legend.position="none")  # remove in order to see legend


##### Aggregate Across Faces #####

# this shows more of a UV than individual-level
# might mean that the faces that "pull" especially hard on the UV have
#  a lot of variability across subjects, so they count for more in the 
#  unweighted aggregate analysis vs. individual-level?

# also note that this one is not emotion-adjusted

# just curious what this looks like
# for comparison with UV2
library(dplyr)
faces = l %>% group_by(stim.name) %>%
  summarise( mh = mean(mh), 
             mh.val = mean(mh.val),
             lik = mean(lik),
             mean.emot = mean(mean.emot) ) # this one already static within face

# sanity check: MH concordance between validation and new subjects
plot( faces$mh, faces$mh.val )


poly4.agg.unadj = lm( faces$lik ~ faces$mh + I(faces$mh^2) + I(faces$mh^3) + I(faces$mh^4) )
tidy(poly4.agg.unadj)

poly4.agg.adj = lm( faces$lik ~ faces$mh + I(faces$mh^2) + I(faces$mh^3) + I(faces$mh^4) +
                      faces$mean.emot)
tidy(poly4.agg.adj)

eq_unadj = function(x) {
  model = poly4.agg.unadj
  coef(model)[1] +
    coef(model)[2]*x +
    coef(model)[3]*(x^2) +
    coef(model)[4]*(x^3) +
    coef(model)[5]*(x^4)
}


eq_adj = function(x) {
  model = poly4.agg.adj
  coef(model)[1] +
    coef(model)[2]*x +
    coef(model)[3]*(x^2) +
    coef(model)[4]*(x^3) +
    coef(model)[5]*(x^4)
}


ggplot(faces, aes(x=mh, y=lik) ) +
  geom_hline(yintercept=0, color="gray", lwd=.6) +  # reference line for neutrality
  geom_point(alpha=0.5) +
  theme_classic() +
  
  geom_smooth() +  # LOESS
  stat_function(fun=eq_adj, color="red", lwd=1.2, lty = 1) +  # emotion-adjusted regression curve
  stat_function(fun=eq_unadj, color="blue", lwd=1.2, lty = 2) +  # unadjusted regression curve
  #geom_ribbon(data=predframe, aes(ymin=lwr, ymax=upr), alpha=0.4) +  # CI band for main analysis model
  
  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  scale_y_continuous(breaks=seq(-100, 100, 25)) +
  #scale_color_manual(values=col, labels=labels, name="") +
  scale_shape_manual(values=shapes) +
  theme(text = element_text(size=28) ) +
  
  theme(legend.position="none")  # remove in order to see legend



# CONCLUSION: BIG DIFFERENCES IN ESTIMATES NEAR INITIAL PEAK:
# 1.) GEE at individual subject level shows almost monotonic increase
# 2.) LM or LOESS at IPD level (ignores correlation) shows something more like a UV
# 3.) Taking means by face as we did before (even not adjusting for emotion) looks more like #2




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 CATEGORY CONFUSION 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



############################### DESCRIPTIVE PLOTS ###############################

##### Plot MH vs. Each Mediator #####

med.names = c("area",
              "xdev",
              "rxnt",
              "speed",
              "xflips")


plot_y = function(y.name) {
  
  # remove any infinite values (for speed)
  temp = l[ l[[y.name]] < Inf, ]
  
  # get beta for linear relationship
  ct = cor.test( temp$mh, temp[[y.name]] )
  corr = ct$estimate
  pval = ct$p.value
  
  ggplot( data = temp, aes_string( x = "mh", y = y.name ) ) +
    geom_point(alpha=0.4) +
    geom_smooth() +
    theme_classic() +
    ylab(y.name) +
    ggtitle( paste("r = ", round(corr,3), ", p = ", round(pval,2),
                   sep = "")
             )
}

p = lapply( med.names, plot_y )

library(gridExtra)
nCol = floor(sqrt(length(p)))
do.call("grid.arrange", c(p, ncol=nCol))
# setwd(results.dir)
# ggsave("test.pdf", do.call("marrangeGrob", c(p,nrow=3,ncol=2)))



##### Plot Each Mediator vs. Likability #####

plot_x = function(x.name) {
  
  # remove any infinite values (for speed)
  temp = l[ l[[x.name]] < Inf, ]
  
  # get beta for linear relationship
  ct = cor.test( temp[[x.name]], temp$lik )
  corr = ct$estimate
  pval = ct$p.value
  
  ggplot( data = temp, aes_string( x = x.name, y = "lik" ) ) +
    geom_point(alpha=0.4) +
    geom_smooth() +
    theme_classic() +
    xlab(x.name) +
    ggtitle( paste("r = ", round(corr,3), ", p = ", round(pval,2),
                   sep = "")
    )
  
  
}

p = lapply( med.names, plot_x )

library(gridExtra)
nCol = floor(sqrt(length(p)))
do.call("grid.arrange", c(p, ncol=nCol))
# setwd(results.dir)
# ggsave("test.pdf", do.call("marrangeGrob", c(p,nrow=3,ncol=2)))


############################### CATEGORY CONFUSION ###############################


# see if any have problems running

# # for this one, default knots is too many
# # 8 is OK
# med_analysis(sims=15,
#              med.name = "xflips")

# just to try
temp = med_analysis(sims=100,
             med.name = "xflips")

med.res = lapply( med.names,
                  FUN = function(x) med_analysis(sims=100,
                                                 med.name = x) )


# put in table
res.tab = data.frame( mediator = rep(NA, length(med.names)),
                      prop.med = rep(NA, length(med.names)),
                      ci = rep(NA, length(med.names)),
                      pval = rep(NA, length(med.names)),
                      interaction = rep(NA, length(med.names))
                      )

for ( i in 1:length(med.names) ) {
  
  CI = format_CI( med.res[[i]]$lo, med.res[[i]]$hi, 2 )
  
  res.tab[i,] = c( med.res[[i]]$med.name,
                   my_round( med.res[[i]]$prop.med, 2 ),
                   CI,
                   format_pval( med.res[[i]]$pval ),
                   interaction )
}
res.tab
# all CIs for proportion mediated are completely uninformative (span [-1,1])


############################### EXPERIMENT TO UNDERSTAND PRECISION ISSUES ###############################


##### Sanity check: Does CI precision change with simpler (unrealistic) models for mediator and outcome? #####

# YES. Much more precise this way.

med.name = "rxnt"

l$med = l[[med.name]]

med.lm = lm( med ~ mh + I(mh^2) + I(mh^3) + mean.emot,
           data = l )
plot(l$mh, predict(med)) # expect UV

out.lm = lm( lik ~ med*mh +
            I(med^2) +
            I(med^3) +
            I(mh^2) +
            I(mh^3) +
            + mean.emot,
          data = l )
plot(l$mh, predict(out.lm)) # expect attenuated UV because we've controlled for mediator
plot(l$med, predict(out.lm))  # expect monotonically increasing

library(mediation)
res = mediate( model.m = med.lm,
               model.y = out.lm,
               boot=TRUE,
               sims=100,
               treat="mh",
               mediator="med" )
summary(res)


# use average proportion mediated, taken across distribution of mh
list( med.name = med.name,
      prop.med = res$n.avg,
      pval = res$n.avg.p,
      lo = res$n.avg.ci[1],
      hi = res$n.avg.ci[2] ) 
# NOW CI IS VERY PRECISE AROUND 0. 

# compare the coefficients in GAM outcome model 
med.gam = med.res[[ which(res.tab$mediator == med.name) ]]$med.model
out.gam = med.res[[ which(res.tab$mediator == med.name) ]]$out.model
summary(out.gam)
summary(out)
# in either case, MH score still highly significant after controlling for mediator terms
# but indeed, using LM gives *much* more precise CIs


#### What if only mediator model is GAM? #####

res = mediate( model.m = med.gam,
               model.y = out.lm,
               boot=TRUE,
               sims=100,
               treat="mh",
               mediator="med" )
list( med.name = med.name,
      prop.med = res$n.avg,
      pval = res$n.avg.p,
      lo = res$n.avg.ci[1],
      hi = res$n.avg.ci[2] ) 
# SUBSTANTIALLY LESS PRECISE, BUT NOT AS BAD AS DOUBLE-GAM.
# CONCLUSION: IT DEFINITELY IS THE GAM'S FAULT THAT IT'S SO IMPRECISE.


#### What about double-GAM with much bigger sample size? #####

# resample from dataset until we have the proposed minimum sample size
#  of 328*183 = 60024 observations
library(dplyr)
fake = sample_n( l, size = 60000, replace = TRUE )

# yikes...doesn't help precision at all
temp = med_analysis(sims=200,
             med.name = "xflips",
             data = fake)

# BOOKMARK: WAS GOING TO SEE IF PRECISION FOR ESTIMATING MEDIATED EFFECT
# (RATHER THAN PROP. MED) IS ANY BETTER
summary( temp$mediate.object )

# with sample size of 60K, it's a little more precise ([-0.45, 0.62])


# GAME PLAN: 
#  1.) Use LM with lots of poly terms (as above) and robust SEs
#   (latter is a direct argument for mediate fn). This also makes
#   more sense than GAM because not sure we can do robust SEs with GAM.
#  
# 2.) As sensitivity analysis, fit GAM models and plot the predictions to see
#   if similar predictions.

  

