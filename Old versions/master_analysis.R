
# Bookmark: Trying to figure out what to do with mediation analyses. 
# lmer gives crazy results for mediator model, but GAM doesn't account for clustering.
# so I think we should maybe try lmer with centered/scaled predictors and see if it is still 
# crazy (via the plots). 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                         READ IN DATA 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# should analyses be conducted on individual trial data


root.dir = "~/Dropbox/Personal computer/Independent studies/Uncanny Valley III (UV3)/UV3_OSF"
code.dir = paste( root.dir, "/uv3_git/5_Expt_2/Code", sep="" )
data.dir = paste( root.dir, "/uv3_git/5_Expt_2/Data", sep="" )
results.dir = paste( root.dir, "/uv3_git/5_Expt_2/Results", sep="" )

setwd(code.dir)
source("helper_overall_analysis.R")

med.names = c("area",
              "xdev",
              "rxnt",
              "speed",
              "xflips")

setwd(data.dir)
library(readr)
l = read_csv( "random_subsample.csv" )  # ~~~ TEST ONLY: RANDOM SUBSAMPLE
# l = read_csv("long_prepped.csv")
# library(testthat)
# expect_equal( nrow(l), 59455 )


##### Make Aggregated Dataset #####

library(dplyr)

# faces dataset

# variables to summarize
vars = c(#"emot.mean", # first two already aggregated from validation study
         #"emot.sd",
         "human",
         med.names,
         "mh",
         "mh.val",
         "lik")

f = l %>% group_by(stim.name) %>%
  summarise_at( vars, funs(mean, sd, median) )

plot(f$mh_mean, f$lik_mean)

# for mediation analyses, want same names as in long data for 
#  ease of calling fn
f2 = l %>% group_by(stim.name) %>%
  summarise_at( vars, mean )


# add means to long data
l = merge( l, f, by = "stim.name" )
# should look exactly like the above, just with repeated copies of each point
plot(l$mh_mean, l$lik_mean)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   UV CURVE AND BOUNDARY
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


############################### FUNCTIONAL FORM FOR EMOTION ###############################

# polynomial regression
# not yet using inverse-weighting by likeability variance

##### Decide What Functional Form to Use When Adjusting For Emotion #####
# see what functional form we should use when adjusting for emotion
library(ggplot2)
ggplot( data = l, aes( x = mean.emot, y = mh) ) +
  geom_point() +
  stat_smooth() +
  theme_classic()

# see what functional form we should use when adjusting for emotion
ggplot( data = l, aes( x = mean.emot, y = lik) ) +
  geom_point() +
  stat_smooth() +
  theme_classic()
# linear should be reasonable for both



############################### FIT UV CURVE MODELS ###############################

##### Fit GEE Polynomial Model #####
# http://ww2.coastal.edu/kingw/statistics/R-tutorials/simplenonlinear.html
# robust SEs to deal with correlation within subjects, faces, sites, etc.
library(sandwich)
library(lmtest)
library(lme4)

# other models we considered: 
# lm with robust SEs (but hard to get CI for fitted values, not just coefficients)
# geeglm freezes up even with 10% subsample



##### Main-Analysis (Preregistered) Nested Models #####

# 4th-order polymomial
poly4.lmer.adj = lmer( lik ~ mh + I(mh^2) + I(mh^3) + I(mh^4) +
                     mean.emot + (1|stim.name) + (1|w1_uID),
                   data = l )


# 3rd-order polymomial
poly3.lmer.adj = lmer( lik ~ mh + I(mh^2) + I(mh^3) +
                         mean.emot + (1|stim.name) + (1|w1_uID),
                       data = l )
vcov.best = vcov(poly3.lmer.adj)


# 2nd-order polymomial
poly2.lmer.adj = lmer( lik ~ mh + I(mh^2) +
                         mean.emot + (1|stim.name) + (1|w1_uID),
                       data = l )

# 3rd-order wins again
anova( poly4.lmer.adj, poly3.lmer.adj, poly2.lmer.adj )

best.mod = poly3.lmer.adj




##### Sanity-Check Models #####

# not adjusting for emotion
poly3.lmer.unadj = lmer( lik ~ mh + I(mh^2) + I(mh^3) +
                         + (1|stim.name) + (1|w1_uID),
                       data = l )

# OLS (unbiased but likely wrong inference)
poly3.lm.adj = lm( lik ~ mh + I(mh^2) + I(mh^3) +
                     mean.emot,
                   data = l )

# GEE
library(gee)
# data have to be sorted by cluster var for this to work per the help
l = l[ order(l$stim.name), ]

poly3.gee.adj = gee( lik ~ mh + I(mh^2) + I(mh^3) +
                       mean.emot,
                     id = as.factor(l$stim.name), 
                     corstr="exchangeable",
                     data = l )

poly4.gee.adj = gee( lik ~ mh + I(mh^2) + I(mh^3) + I(mh^4) +
                       mean.emot,
                     id = as.factor(l$stim.name), 
                     corstr="exchangeable",
                     data = l )

# GEE with aggregated X, but not Y

poly3.gee.agg.adj = gee( lik ~ mh.mean + I(mh.mean^2) + I(mh.mean^3) +
                       mean.emot,
                     id = as.factor(l$stim.name), 
                     corstr="exchangeable",
                     data = l )

# compare fits at a specific point (near previous UV)
fitted_y( x = -75, model = poly3.lmer.unadj, model.type = "lmer" )
fitted_y( x = -75, model = poly3.lm.adj, model.type = "lm" )
fitted_y( x = -75, model = poly3.gee.adj, model.type = "lm" )
fitted_y( x = -75, model = poly3.gee.agg.adj, model.type = "lm" )
fitted_y( x = -75, model = poly3.lmer.adj, model.type = "lmer" )

# compare some of the coefficient estimates
cbind( fixef(poly3.lmer.adj),
       coef(poly3.gee.adj),
       coef(poly3.lm.adj) )


################################## UV SCATTERPLOT WITH FITTED CURVES ################################## 

##### Inference for Best Model #####

# the SE for the Y-hats, and conditional on cluster, is just the
# in simplified notation:
# Var( Yhat | gamma ) = Var( b0 + b1X + b2C + gamma )
#  = Var( b0 + b1X + b2C )
#  = sum of the coefficients' variances plus their covariances
# so is independent of X
se.best = sqrt( sum(vcov.best) )


# set emotion to mean for the SEs
mh.grid = seq(-100, 100, 1)
fit = as.numeric( fitted_y(mh.grid, model = best.mod, model.type = "lmer") )
#plot(mh.grid, fit)  # sanity check

predframe = data.frame("lik" = fit,
                       "lwr" = fit - qnorm(.975) * se.best,
                       "upr" = fit + qnorm(.975) * se.best,
                       "mh" = mh.grid )



##### Make the Plot #####

xlab="Mechano-humanness score (-100 to 100)"
ylab="Likability (-100 to 100)"
shapes = c(45, 43)


library(ggplot2)
ggplot(l, aes(x=mh, y=lik) ) +
  geom_hline(yintercept=0, color="gray", lwd=.6) +  # reference line for neutrality
  #geom_point(alpha = 0.1) +  # can comment out to improve speed
  theme_classic() +
  
  geom_smooth(lty = 2,
              aes(color = "Unclustered GAM") ) +  # LOESS (another sanity check)
  
  geom_ribbon(data=predframe,
              aes(ymin=lwr, ymax=upr),
              alpha=0.2) +  # CI band for main analysis model
  
  # best-fitting poly
  stat_function(fun = function(x) fitted_y(x = x, model = best.mod, model.type = "lmer"),
                aes( color="LMM, emotion-adjusted (main)" ),
                lwd=1.2, lty=2) +  
  
  
  # sanity check: LM
  stat_function(fun = function(x) fitted_y(x = x, model = poly3.lm.adj, model.type = "lm"),
                aes( color="OLS" ),
                lwd=1, lty = 2) + 
  
  # # sanity check: LMER not adjusting for emotion
  # stat_function(fun = function(x) fitted_y(x = x, model = poly3.lmer.unadj, model.type = "lmer"),
  #               aes( color="LMM, unadjusted" ),
  #               lwd=1, lty = 2) + 
  
  # sanity check: GEE
  stat_function(fun = function(x) fitted_y(x = x, model = poly3.gee.adj, model.type = "lm"),
                aes( color="GEE (3), adjusted" ),
                lwd=1, lty = 2) + 
  
  # stat_function(fun = function(x) fitted_y(x = x, model = poly4.gee.adj, model.type = "lm"),
  #               aes( color="GEE (4), adjusted" ),
  #               lwd=1, lty = 2) +
  
  stat_function(fun = function(x) fitted_y(x = x, model = poly3.gee.agg.adj, model.type = "lm"),
                aes( color="GEE (3), agg X, adjusted" ),
                lwd=1, lty = 2) +

  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  scale_y_continuous(breaks=seq(-100, 100, 25)) +
  #scale_color_manual(values=col, labels=labels, name="") +
  scale_shape_manual(values=shapes) +
  theme(text = element_text(size=20) )
  
  #theme(legend.position="none")  # remove in order to see legend


setwd(results.dir)
ggsave("uv_curve_trial_data.pdf",
       width = 10, 
       height = 6)



##### Sanity Check: Aggregate Across Faces #####

# this shows more of a UV than individual-level
# might mean that the faces that "pull" especially hard on the UV have
#  a lot of variability across subjects, so they count for more in the 
#  unweighted aggregate analysis vs. individual-level?

# also note that this one is not emotion-adjusted

# sanity check: MH concordance between validation and new subjects
plot( faces$mh, faces$mh.val )

# fit 3rd-order poly models to aggregate 
poly3.agg.unadj = lm( lik ~ mh + I(mh^2) + I(mh^3),
                      data = faces)

poly3.agg.adj = lm( lik ~ mh + I(mh^2) + I(mh^3) +
                      mean.emot,
                    data = faces)

# sanity check model: weight by inverse variance, as in UV2
poly3.agg.adj.wtd = lm( lik ~ mh + I(mh^2) + I(mh^3) +
                      mean.emot,
                      weights = 1/(faces$lik.sd^2),
                    data = faces)

# sanity check model: weight by inverse variance, as in UV2
poly3.agg.adj.wtd2 = lm( lik ~ mh + I(mh^2) + I(mh^3) +
                          mean.emot,
                        weights = faces$lik.sd^2,
                        data = faces)

##### Plot Aggregate UV Curves #####

ggplot(faces, aes(x=mh, y=lik) ) +
  geom_hline(yintercept=0, color="gray", lwd=.6) +  # reference line for neutrality

  theme_classic() +
  
  geom_smooth( aes( color="GAM, unadjusted" ) ) +  # LOESS
  
  stat_function( fun = function(x) fitted_y(x = x, model = poly3.agg.adj, model.type = "lm"),
                 aes( color="LM, emotion-adjusted" ),
                 lwd=1, lty = 1) +  # emotion-adjusted regression curve
  
  # stat_function( fun = function(x) fitted_y(x = x, model = poly3.agg.unadj, model.type = "lm"),
  #                aes( color="LM, unadjusted" ),
  #                lwd=1, lty = 1) +  # unadjusted regression curve
  
  stat_function( fun = function(x) fitted_y(x = x, model = poly3.agg.adj.wtd, model.type = "lm"),
                 aes( color="LM, inverse-var-wtd, adjusted" ),
                 lwd=1, lty = 1) +  # unadjusted regression curve
  
  stat_function( fun = function(x) fitted_y(x = x, model = poly3.agg.adj.wtd2, model.type = "lm"),
                 aes( color="LM, var-wtd, adjusted" ),
                 lwd=1, lty = 1) +  # unadjusted regression curve
  
  geom_point( aes(alpha= lik.sd), size = 2 ) +

  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  scale_y_continuous(breaks=seq(-100, 100, 25)) +
  #scale_color_manual(values=col, labels=labels, name="") +
  scale_shape_manual(values=shapes) +
  theme(text = element_text(size=20) )
  
  #theme(legend.position="none")  # remove in order to see legend

# inverse-variance weighting does not really change anything
setwd(results.dir)
ggsave("uv_curve_face_aggregate_data.pdf",
       width = 10, 
       height = 6)



# CONCLUSION: BIG DIFFERENCES IN ESTIMATES NEAR INITIAL PEAK:
# 1.) LMM at individual subject level shows almost monotonic increase
# 2.) LM or GAM at IPD level (ignores correlation) shows something more like a UV
# 3.) Taking means by face as we did before (even not adjusting for emotion) looks more like #2



############################ INVESTIGATE DIFFERENCES BETWEEN CLUSTERED VS. UNCLUSTERED MODELS ############################ 

# MH score vs. SD(likability)
ggplot( data = faces, aes( x = mh, y = lik.sd ) ) +
  geom_point() +
  theme_bw()


# ICC within faces, subjects, and sites
library(ICC)
ICCbareF(x = stim.name, y = lik, data = l)
ICCbareF(x = w1_uID, y = lik, data = l)
ICCbareF(x = w1_site, y = lik, data = l)
# as expected, almost all the clustering is due to faces, not the others


# CONCLUSION:
# The faces with the most clustering (low SDs) are the ones with extreme MH scores.
# I think that the clustered models basically penalize these faces because 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                               ESTIMATE APEX AND NADIR OF UV 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# From prereg:
# We will use this parametric model to estimate the MH scores marking the apex and nadir of the Uncanny
# Valley.

best.mod

# find the initial max
optimize( function(x) fitted_y(x = x, model = best.mod, model.type = "lmer"),
          interval=c(-100, 100), maximum=TRUE)

# find the min
optimize( function(x) fitted_y(x = x, model = best.mod, model.type = "lmer"),
          interval=c(-100, 100), maximum=FALSE)

# find the global max (fitted value for a fully human face)
optimize( function(x) fitted_y(x = x, model = best.mod, model.type = "lmer"),
          interval=c(70, 100), maximum=TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                               ESTIMATE CATEGORY BOUNDARY LOCATION 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# From prereg:
# Additionally, we will estimate the location of the category boundary
# (i.e., the MH score at which the proportion of subjects categorizing
# the face as “human” is closest to 50%) via polynomial regression.


# 3rd-order polymomial
library(lme4)

l$cat.human = as.numeric(l$cat == "Human")

# tried logistic or log-linear glmer, but both failed to fit
# category.lmer = glmer( cat.human ~ mh + I(mh^2) + I(mh^3) +
#                          mean.emot + (1|stim.name) + (1|w1_uID),
#                        family = "binomial",
#                        data = l )

# regular Poisson regression
category.pois = glm( cat.human ~ mh + I(mh^2) + I(mh^3) +
                         mean.emot,
                       family = "poisson",
                       data = l )

# prob of being categorized as "Human" for MH score = 0
# sanity check: this is corroborated by GAM fit in below plot :)
p0 = exp( coef(category.pois)["(Intercept)"] )

# sanity check: based on GAM below, expect this somewhat close to 0.5
fitted_y( x = 60, model = category.pois, model.type = "poisson" )



##### Find Category Boundary Via Grid Search #####

# minimize distance from fitted value and 0.50
boundary.mh = optimize( function(x) abs( fitted_y( x = x, model = category.pois, model.type = "poisson" ) - 0.50 ),
          interval=c(-100, 100), maximum=FALSE)$minimum

# could bootstrap a CI for this


##### Plot Probability of Human Categorization By MH Score #####

# # set emotion to mean for the SEs
# mh.grid = seq(-100, 100, 1)
# fit = as.numeric( fitted_y(mh.grid, model = category.lmer, model.type = "lmer") )
# #plot(mh.grid, fit)  # sanity check
# 
# predframe = data.frame("lik" = fit,
#                        "mh" = mh.grid )

l = as.data.frame(l)

ylab = 'Probability of "human" categorization'
ggplot(l, aes(x=mh, y=cat.human) ) +
  geom_hline(yintercept=.50, color="gray", lwd=.6) +  # reference line for 50% categorization
  geom_vline( aes( xintercept=boundary.mh, color="Boundary (est. from GLM)" ), lwd=.6) +  # reference line for 50% categorization
  
  theme_classic() +
  
  geom_smooth( aes( color="GAM, unadjusted" ) ) +  
  
  stat_function( fun = function(x) fitted_y( x = x, model = category.pois, model.type = "poisson" ),
                 aes( color="Poisson GLM, emotion-adjusted" ),
                 lwd=1, lty = 2) +  # emotion-adjusted regression curve
  
  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  
  theme(text = element_text(size=20) )


setwd(results.dir)
ggsave("mh_vs_categorization.pdf",
       width = 10, 
       height = 6)


# CONCLUSION
# Very interesting. It's much closer to "human" than is the point of minimum likability, even
#  when the latter is estimated on the aggregate data. 



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

# for formatting stats
library(MetaUtility)
library(ggplot2)


# ~~ note that this fn currently uses ID link even for xflips
# which for GEE is fine
p = lapply( med.names, function(x) mh_vs_mediator(med.name = x, use.lmer = FALSE) )

library(gridExtra)
setwd(results.dir)
ggsave("exposure_mediator_plots.pdf",
       do.call("arrangeGrob", p),
       width = 15, 
       height = 6)


##### Plot Each Mediator vs. Likability #####

p = lapply( med.names, mediator_vs_lik )

library(gridExtra)
setwd(results.dir)
ggsave("mediator_outcome_plots.pdf",
       do.call("arrangeGrob", p),
       width = 15, 
       height = 6)



# CONCLUSIONS
# All the mediators increase roughly monotonically in MH score.
# They peak around MH-score of 50-100.
#
# But the mediator-outcome relationships are less what we'd expect:
# Some even have positive relationships with outcome, such that more confusion 
# predicts more likability. 
# 
# Preliminarily, this suggests that confusion doesn't decrease likability. 

# ~~~ NOTE THAT I'M JUST USING LM FOR NOW BECAUSE THE LMER FITS ARE CRAZY HERE - WHY?
# THE GAM AND LM MAKE SENSE.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 MEDIATION ANALYSES 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# temporarily leave out xflips because it's being a butt
med.names = c("area",
              "xdev",
              "rxnt",
              "speed" )
             # "xflips")

##### Run For All of Them #####


# fake = med_analysis(
#   boot = FALSE, 
#   med.name = "xflips",
#   med.model = "lm",
#   out.model = "lm" )

start.time = Sys.time()
med.res = lapply( med.names,
                  FUN = function(x) med_analysis(
                                                 boot = TRUE, 
                                                 med.name = x,
                                                 med.model = "gam",
                                                 out.model = "gam" ) )
end.time = Sys.time()
( total.mediation.time = end.time - start.time )


stats.lists = lapply( med.res, function(x) x$stats )
res = do.call( rbind,
         stats.lists )


res


# With GAM for both models: does not mediate


