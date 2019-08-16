
# Bookmark: 
# Write a fn that chooses and returns optimal poly model

# https://stackoverflow.com/questions/3822535/fitting-polynomial-model-to-data-in-r
# poly function computes orthogonal polynomials
polyfit = function(i) x = AIC( lm( lik ~ poly(mh, i) +
                                       mean.emot,
                                     weights = 1/(f2$lik_sd^2),
                                     data = f2) )
as.integer( optimize( polyfit,
                      interval = c( 1, 10 ) )$minimum)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                         READ IN DATA 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# should analyses be conducted on individual trial data or on face-aggregated data?
#aggregated = TRUE

# use random subsample for testing code?
subsample = FALSE

include.trial.curve = TRUE

# # should plots be in their full sensitivity analysis glory for Appendix?
# plots.for.app = TRUE

root.dir = "~/Dropbox/Personal computer/Independent studies/Uncanny Valley III (UV3)/UV3_OSF"
code.dir = paste( root.dir, "/uv3_git/5_Expt_2/Code", sep="" )
data.dir = paste( root.dir, "/uv3_git/5_Expt_2/Data", sep="" )
results.dir = paste( root.dir, "/uv3_git/5_Expt_2/Results", sep="" )

setwd(code.dir)
source("helper_overall_analysis.R")

# read in long data
setwd(data.dir)
library(readr)
if ( subsample == TRUE) l = read_csv( "long_prepped_subsample.csv" )
if ( subsample == FALSE){
  l = read_csv("long_prepped.csv")
  library(testthat)
  expect_equal( nrow(l), 59455 )
} 

# read in face data
f2 = read.csv("face_aggregated_simple.csv")

# generic data name depending on specification
if ( aggregated == FALSE ) dat = l
if ( aggregated == TRUE ) dat = f2


# center variables
# l$mh = l$mh - mean(l$mh)
# l$mean.emot = l$mean.emot - mean(l$mean.emot)

# initialize master list and dataframe for storing results
res.list = list()
res.stats = data.frame( matrix( ncol=2 ) )
names(res.stats) = c("name", "value")
res.stats = res.stats[ -1,]
# clunky! 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   I. UV CURVE AND BOUNDARY
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


############################### FUNCTIONAL FORM FOR EMOTION CONFOUNDER ###############################

# polynomial regression
# not yet using inverse-weighting by likability variance

##### Decide What Functional Form to Use When Adjusting For Emotion #####
# commented out for speed

# # see what functional form we should use when adjusting for emotion
# library(ggplot2)
# ggplot( data = dat, aes( x = mean.emot, y = mh) ) +
#   geom_point() +
#   stat_smooth() +
#   theme_classic()
# 
# # see what functional form we should use when adjusting for emotion
# ggplot( data = dat, aes( x = mean.emot, y = lik) ) +
#   geom_point() +
#   stat_smooth() +
#   theme_classic()
# # linear should be reasonable for both



############################### FIT ALL UV CURVE MODELS (AGGREGATED AND TRIAL-LEVEL) ###############################

library(lmtest)
library(lme4)

# other models we tried as sensitivity analyses: 
# lm with robust SEs (but hard to get CI for fitted values, not just coefficients)
# geeglm freezes up even with 10% subsample

##### Main-Analysis OLS Models #####
# main analysis models to compare: weight by inverse variance, as in UV2

# find order of best-fitting model
polyfit = function(i) x = AIC( lm( lik ~ poly(mh, i) +
                                     mean.emot,
                                   weights = 1/(f2$lik_sd^2),
                                   data = f2) )
( poly.order = as.integer( optimize( polyfit,
                      interval = c( 1, 10 ) )$minimum) )

# fit the winning model
poly2.agg.adj.wtd = lm( lik ~ mh + I(mh^2) +
                          mean.emot,
                        weights = 1/(f2$lik_sd^2),
                        data = f2)

poly3.agg.adj.wtd = lm( lik ~ mh + I(mh^2) + I(mh^3) +
                          mean.emot,
                        weights = 1/(f2$lik_sd^2),
                        data = f2)

# turns out to be winner
poly4.agg.adj.wtd = lm( lik ~ mh + I(mh^2) + I(mh^3) + I(mh^4) + 
                          mean.emot,
                        weights = 1/(f2$lik_sd^2),
                        data = f2)

poly5.agg.adj.wtd = lm( lik ~ mh + I(mh^2) + I(mh^3) + I(mh^4) + I(mh^5) +
                          mean.emot,
                        weights = 1/(f2$lik_sd^2),
                        data = f2)

poly7.agg.adj.wtd = lm( lik ~ mh + I(mh^2) + I(mh^3) + I(mh^4) + I(mh^5) + I(mh^6) + I(mh^7) +
                          mean.emot,
                        weights = 1/(f2$lik_sd^2),
                        data = f2)
summary(poly7.agg.adj.wtd)

( anova.comparison = anova( poly2.agg.adj.wtd, poly3.agg.adj.wtd, poly4.agg.adj.wtd, poly5.agg.adj.wtd ) )

best.agg.mod = poly7.agg.adj.wtd

# sanity check: no weighting
poly4.agg.adj.unwtd = lm( lik ~ mh + I(mh^2) + I(mh^3) + I(mh^4) +
                            mean.emot,
                          data = f2)


##### Main-Analysis GEE Models #####

# HAVE THE NUMBER OF TERMS MATCH THAT IN WINNING OLS MODEL

library(gee)
# data have to be sorted by cluster var for this to work per the help
l = l[ order(l$stim.name), ]

poly2.gee.adj = gee( lik ~ mh + I(mh^2) +
                       mean.emot,
                     id = as.factor(l$stim.name), 
                     corstr="exchangeable",
                     data = l )

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

##### Compare Fit of Main Analysis Models #####
best.trial.mod = poly3.gee.adj
vcov.best = poly3.gee.adj$robust.variance

##### Sanity-Check Models: LMM #####
# 3rd-order polymomial
poly3.lmer.adj = lmer( lik ~ mh + I(mh^2) + I(mh^3) +
                         mean.emot + (1|stim.name) + (1|w1_uID),
                       data = l )

##### Sanity-Check Models: Misc #####
# not adjusting for emotion
poly3.gee.unadj = gee( lik ~ mh + I(mh^2) + I(mh^3),
                       id = as.factor(l$stim.name), 
                       corstr="exchangeable",
                       data = l )

# OLS (unbiased but likely wrong inference)
poly3.lm.adj = lm( lik ~ mh + I(mh^2) + I(mh^3) +
                     mean.emot,
                   data = l )




############################ PLOT 1: SENSITIVITY APPENDIX PLOT ON TRIAL DATA ############################# 

xlab="Mechano-humanness score (-100 to 100)"
ylab="Likability (-100 to 100)"
shapes = c(45, 43)

library(ggplot2)


##### Inference for GEE Model #####

# IMPORTANT: we're setting emotion to 0 in calculating the SEs
# make fake design matrix for which to plot the fitted Ys and calculate the SEs 
mh.grid = seq(-100, 100, 1)
nfake = length(mh.grid)
X = matrix( c( rep(1, nfake),
                   mh.grid, 
                   mh.grid^2,
                   mh.grid^3,
                   rep(0, nfake) ),
                nrow = nfake )

# ~~~ CHECK THIS COVARIANCE MATRIX PROPERTY
CovYhat = X %*% vcov.best %*% t(X)

predframe = data.frame( lik = X %*% coef(best.trial.mod),
                        se = sqrt( diag(CovYhat) ) )

predframe$lwr = predframe$lik - qnorm(.975) * predframe$se
predframe$upr = predframe$lik + qnorm(.975) * predframe$se
predframe$mh = mh.grid
plot(mh.grid, predframe$lik)  # sanity check


##### Make the Plot #####
p = ggplot(l, aes(x=mh, y=lik) ) +
  geom_hline(yintercept=0, color="gray", lwd=.6) +  # reference line for neutrality
  
  theme_classic() +
  
  geom_ribbon(data=predframe,
              aes(ymin=lwr, ymax=upr),
              alpha=0.2) +  # CI band for main analysis model
  
  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  scale_y_continuous(breaks=seq(-100, 100, 25)) +
  #scale_color_manual(values=col, labels=labels, name="") +
  scale_shape_manual(values=shapes) +
  theme(text = element_text(size=20) )

p = p + 
  
  # best-fitting GEE
  stat_function(fun = function(x) fitted_y(x = x, model = poly3.gee.adj, model.type = "marginal"),
                aes( color="*GEE, adjusted" ),
                lwd=1, lty = 2) +
  
  # sanity check: GAM
  geom_smooth(lty = 2,
              se = FALSE,
              aes(color = "Unclustered GAM") ) +
  
  # sanity check: LMM
  stat_function(fun = function(x) fitted_y(x = x, model = poly3.lmer.adj, model.type = "lmer"),
                aes( color="LMM, adjusted" ),
                lwd=1.2, lty=2) +  
  
  # sanity check: LM
  stat_function(fun = function(x) fitted_y(x = x, model = poly3.lm.adj, model.type = "marginal"),
                aes( color="OLS" ),
                lwd=1, lty = 2) + 
  
  # sanity check: GEE, unadjusted for emotion
  stat_function(fun = function(x) fitted_y(x = x, model = poly3.gee.unadj, model.type = "marginal"),
                aes( color="GEE, crude" ),
                lwd=1, lty = 2) +
  
  guides(color=guide_legend(title="Model"))

setwd(results.dir)
ggsave("appendix_uv_curve_trial.pdf",
       width = 10, 
       height = 6)


############################ PLOT 2: SENSITIVITY APPENDIX PLOT ON BOTH DATASETS ############################# 

# this shows more of a UV than individual-level
# might mean that the faces that "pull" especially hard on the UV have
#  a lot of variability across subjects, so they count for more in the 
#  unweighted aggregate analysis vs. individual-level?


##### Plot Aggregate UV Curves #####

# inference for OLS model
# get CI for the analysis model

# ~~~ COULD USE SAME FUNCTION AS FOR GEE TO GET INFERENCE AT MORE POINTS
X = matrix( c( rep(1, nfake),
               mh.grid, 
               mh.grid^2,
               mh.grid^3,
               mh.grid^4,
               rep(0, nfake) ),
            nrow = nfake )

# ~~~ CHECK THIS COVARIANCE MATRIX PROPERTY
CovYhat = X %*% vcov.best %*% t(X)

predframe = data.frame( lik = X %*% coef(best.agg.mod),
                        se = sqrt( diag(CovYhat) ) )

predframe$lwr = predframe$lik - qnorm(.975) * predframe$se
predframe$upr = predframe$lik + qnorm(.975) * predframe$se
predframe$mh = mh.grid
plot(mh.grid, predframe$lik)  # sanity check

xlab="Mechano-humanness score (-100 to 100)"
ylab="Likability (-100 to 100)"
shapes = c(45, 43)

# for plotting joy
# percent weight for each face in inverse-analysis
expected.wt = 1/nrow(f2)  # if all faces contributed equally
inv.var.wt = (1/f2$lik_sd^2) / sum( (1/f2$lik_sd^2) )
f2$rel.wt = inv.var.wt / expected.wt


p = ggplot(f2, aes(x=mh, y=lik) ) +
  geom_hline(yintercept=0, color="gray", lwd=.6) +  # reference line for neutrality
  
  geom_ribbon(data=predframe,
              aes(ymin=lwr, ymax=upr),
              alpha=0.2) +  # CI band for main analysis model
  
  theme_classic() +
  
  geom_smooth( aes( color="GAM, unadjusted" ), 
               lty = 2,
               se = FALSE ) +  # LOESS
  
  stat_function( fun = function(x) fitted_y(x = x, model = poly5.agg.adj.wtd, model.type = "marginal"),
                 aes( color="OLS, adjusted, unwtd" ),
                 lwd=1, lty = 2) +  # emotion-adjusted regression curve
  
  stat_function( fun = function(x) fitted_y(x = x, model = best.agg.mod, model.type = "marginal"),
                 aes( color="*OLS, adjusted, wtd" ),
                 lwd=1, lty = 2) +  # unadjusted regression curve
  
  geom_point( aes(alpha= rel.wt), size = 2 ) +
  
  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  scale_y_continuous(breaks=seq(-100, 100, 25)) +
  scale_shape_manual(values=shapes) +
  theme(text = element_text(size=20) ) +
  
  guides(color=guide_legend(title="Model")) +
  guides(alpha=guide_legend(title="Relative analysis weight"))

if ( include.trial.curve == TRUE ) {
  p + stat_function(fun = function(x) fitted_y(x = x, model = poly3.gee.adj, model.type = "marginal"),
                    aes( color="GEE, trial-level, adjusted" ),
                    lwd=1, lty = 2)
}


# inverse-variance weighting does not really change anything

# save the plot
setwd(results.dir)
ggsave("appendix_uv_curve_agg_and_trial.pdf",
       width = 10, 
       height = 6)


# CONCLUSION: BIG DIFFERENCES IN ESTIMATES NEAR INITIAL PEAK:
# 1.) LMM at individual subject level shows almost monotonic increase
# 2.) LM or GAM at IPD level (ignores correlation) shows something more like a UV
# 3.) Taking means by face as we did before (even not adjusting for emotion) looks more like #2


############################ ICCS BY FACE AND SUBJECT ############################ 

# ICC within faces, subjects, and sites
library(ICC)

# as expected, almost all the clustering is due to faces, not the others

res.stats = add_row( res.stats, name = "ICC.stim",
         value = ICCbareF( x = stim.name, y = lik, data = l ) )

res.stats = add_row( res.stats, name = "ICC.site",
         value = ICCbareF( x = w1_site, y = lik, data = l ) )

res.stats = add_row( res.stats, name = "ICC.uID",
         value = ICCbareF( x = w1_uID, y = lik, data = l ) )
res.stats

############################ ESTIMATE APEX AND NADIR OF UV ############################

# From prereg:
# We will use this parametric model to estimate the MH scores marking the apex and nadir of the Uncanny
# Valley.

best.agg.mod

# find the initial max

res.stats = add_row( res.stats,
                     name = c( "initial.max.mh.aggmodel", "initial.max.lik.aggmodel" ),
                     value = optimize( function(x) fitted_y(x = x, model = best.agg.mod, model.type = "marginal"),
                                       interval=c(-100, 0), maximum=TRUE) )


res.stats = add_row( res.stats,
                     name = c( "global.min.mh.aggmodel", "global.min.lik.aggmodel"),
                     value = optimize( function(x) fitted_y(x = x, model = best.agg.mod, model.type = "marginal"),
                                       interval=c(-100, 100), maximum=FALSE) )

res.stats = add_row( res.stats,
                     name = c( "global.max.mh.aggmodel", "global.max.lik.aggmodel" ),
                     value = optimize( function(x) fitted_y(x = x, model = best.agg.mod, model.type = "marginal"),
                                       interval=c(0, 100), maximum=TRUE) )
res.stats

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                             II. ESTIMATE CATEGORY BOUNDARY LOCATION 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# From prereg:
# Additionally, we will estimate the location of the category boundary
# (i.e., the MH score at which the proportion of subjects categorizing
# the face as “human” is closest to 50%) via polynomial regression.

l$cat.human = as.numeric(l$cat == "Human")

library(dplyr)
temp = l %>% group_by(stim.name) %>%
  summarise( prop.human = mean(cat.human) )

# merge into faces dataset
f2 = merge( f2, temp )

# proportion of faces that were 0% human
res.stats = add_row( res.stats,
                     name = "prop.faces.0perc.human",
                     value = mean( f2$prop.human == 0 ) )

# proportion of faces that were 0% human
res.stats = add_row( res.stats,
                     name = "prop.faces.100perc.human",
                     value = mean( f2$prop.human == 1 ) )

edge.face = (f2$prop.human == 0) | (f2$prop.human == 1)


# do analysis for both aggregated and trial data
# keep order of loop same so that we have the aggregated model at the end
for (aggregated in c(FALSE, TRUE)) {
  
  if ( aggregated == TRUE ) {
    # remove faces with zero probability since they're hard to model with poly regression
    # not weighted because here the outcome is prob. of human categorization
    category.mod.main = lm( prop.human ~ mh + I(mh^2) + I(mh^3) + I(mh^4) +
                         mean.emot,
                       data = f2[ edge.face == FALSE, ] )
  }
  
  if ( aggregated == FALSE ) {
    # remove faces with zero probability since they're hard to model with poly regression
    # regular Poisson regression
    category.mod.main = glm( cat.human ~ mh + I(mh^2) + I(mh^3) + I(mh^4) +
                            mean.emot,
                            family = "poisson",
                            data = l )
  }

  ##### Find Category Boundary Via Grid Search #####
  
  # minimize distance from fitted value and 0.50
  if ( aggregated == TRUE ) {
    model.type = "marginal"
    dat = as.data.frame( f2[ edge.face == FALSE, ] )
    plot.y = "prop.human"
    plot.model.name = "*OLS, agg"
    res.string = "boundary.mh.aggmodel"
  }
  if ( aggregated == FALSE ) {
    model.type = "poisson"
    dat = as.data.frame(l)
    plot.y = "cat.human"
    plot.model.name = "*Poisson, trial-level"
    res.string = "boundary.mh.trialmodel"
  }
  
  ( boundary.mh = optimize( function(x) abs( fitted_y( x = x,
                                                       model = category.mod.main,
                                                       model.type = model.type )
                                             - 0.50 ),
                            interval=c(0, 100), maximum=FALSE)$minimum )
  # sanity check
  fitted_y( x = boundary.mh,
            model = category.mod.main,
            model.type = model.type )
  
  res.stats = add_row( res.stats,
                       name = res.string,
                       value = boundary.mh )
  
  # ~~~ could bootstrap a CI for this
  
  
  if ( aggregated == TRUE ) {
    # sensitivity analysis: don't exclude any faces
    category.mod.sens = lm( prop.human ~ mh + I(mh^2) + I(mh^3) + I(mh^4) +
                              mean.emot,
                            weights = 1/(f2$lik_sd^2),
                            data = f2 )
    
    ( boundary.mh = optimize( function(x) abs( fitted_y( x = x,
                                                         model = category.mod.sens,
                                                         model.type = model.type )
                                               - 0.50 ),
                              interval=c(0, 100), maximum=FALSE)$minimum )
    # sanity check
    fitted_y( x = boundary.mh,
              model = category.mod.sens,
              model.type = model.type )
    
    res.stats = add_row( res.stats,
                         name = "boundary.mh.aggmodel.sens",
                         value = boundary.mh )
  }
  
}



 
##### Plot Probability of Human Categorization By MH Score (Only for Aggregated Model) #####

library(testthat)
expect_equal(aggregated, TRUE)

# for plotting joy
# find minimum and max mh score for a face with prop.human < 1 and >0
mh.plot.min = sort( f2$mh[ edge.face == FALSE ] )[1]
mh.plot.max = sort( f2$mh[ edge.face == FALSE ], decreasing = TRUE )[1]

res.stats = add_row( res.stats,
                     name = "min.mh.nonedgeface",
                     value = mh.plot.min )

res.stats = add_row( res.stats,
                     name = "max.mh.nonedgeface",
                     value = mh.plot.max )

# some are weird lists
res.stats$value = as.numeric(res.stats$value)

# so that in plot, the regression line is only shown
#  through the range of points actually included
trunc_fitted_y = function(x) {
  fits = fitted_y( x = x, model = category.mod.main, model.type = "marginal" )
  fits[ x < mh.plot.min | x > mh.plot.max ] = NA
  fits
}

trunc_fitted_y(boundary.mh)

ylab = 'Probability of "human" categorization'
ggplot( f2, aes(x=mh, y=prop.human) ) +
  geom_point() +
  geom_hline(yintercept=.50, color="gray", lwd=.6) +  # reference line for 50% categorization
  geom_vline( aes( xintercept = res.stats$value[ res.stats$name == "boundary.mh.aggmodel"][1],
                   color="Estimated boundary" ), lwd=.6) +  # reference line for 50% categorization
  
  theme_classic() +
  
  # GAM as sanity check
  # geom_smooth( aes( color="GAM, unadjusted" ), se = FALSE ) +  
  
  stat_function( fun = trunc_fitted_y,
                 aes( color="*OLS, agg" ),
                 lwd=1, lty = 1) +  # emotion-adjusted regression curve
  
  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(0, 100, 25)) +
  
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

# # come back to this...needs to be for lm models
# 
# ##### Plot MH vs. Each Mediator #####
# 
# med.names = c("area",
#               "xdev",
#               "rxnt",
#               "speed",
#               "xflips")
# 
# # for formatting stats
# library(MetaUtility)
# library(ggplot2)
# 
# 
# # ~~ note that this fn currently uses ID link even for xflips
# # which for GEE is fine
# p = lapply( med.names, function(x) mh_vs_mediator(med.name = x, use.lmer = FALSE) )
# 
# library(gridExtra)
# setwd(results.dir)
# ggsave("exposure_mediator_plots.pdf",
#        do.call("arrangeGrob", p),
#        width = 15, 
#        height = 6)
# 
# 
# ##### Plot Each Mediator vs. Likability #####
# 
# p = lapply( med.names, mediator_vs_lik )
# 
# library(gridExtra)
# setwd(results.dir)
# ggsave("mediator_outcome_plots.pdf",
#        do.call("arrangeGrob", p),
#        width = 15, 
#        height = 6)
# 
# 
# 
# # CONCLUSIONS
# # All the mediators increase roughly monotonically in MH score.
# # They peak around MH-score of 50-100.
# #
# # But the mediator-outcome relationships are less what we'd expect:
# # Some even have positive relationships with outcome, such that more confusion 
# # predicts more likability. 
# # 
# # Preliminarily, this suggests that confusion doesn't decrease likability. 
# 
# # ~~~ NOTE THAT I'M JUST USING LM FOR NOW BECAUSE THE LMER FITS ARE CRAZY HERE - WHY?
# # THE GAM AND LM MAKE SENSE.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 MEDIATION ANALYSES 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# prereg:
# For the mediator models for the positive, continuous variables (maximum deviation, curve area,
# peak velocity, and reaction time), we will use generalized additive models (GAM) with the
# identity link to regress the measure of category confusion on a cubic regression spline basis
# for MH score.


# temporarily leave out xflips because it's being a butt
med.names = c( "xflips",
              "area",
              "xdev",
              "rxnt",
              "speed" )
             # "xflips")

##### Run For All of Them #####
start.time = Sys.time()
med.res = lapply( med.names,
                  FUN = function(x) med_analysis(
                                                 boot = FALSE, 
                                                 med.name = x,
                                                 med.model = "lm",
                                                 out.model = "lm",
                                                 data = f2 ) )
end.time = Sys.time()
( total.mediation.time = end.time - start.time )


stats.lists = lapply( med.res, function(x) x$stats )
res = do.call( rbind,
         stats.lists )
res %>% filter( stat == "prop.med" )
res %>% filter( stat == "nie.avg" )
res %>% filter( stat == "nde.avg" )

# With GAM for both models:
# prop med from .1 to .53
# totally useless CIs
# NIEs have pval > .1

# With LM for both models: 
# prop med from .1 to .53
# totally useless CIs
# NIEs have pval > .1


##### Sanity Check: Fit One Manually #####
f2$med = f2$xflips
med = lm( med ~ mh + I(mh^2) + I(mh^3) + 
                   mean.emot,
                 data = f2 )

out = lm( lik ~ mh + I(mh^2) + I(mh^3) + 
                med + I(med^2) + I(med^3) +
                #med * mh +
                mean.emot,
              data = f2 )

library(mediation)
temp = mediate( model.m = med,
               model.y = out,
               boot=FALSE,
               sims=500,
               treat="mh",
               mediator="med"
)

summary(temp)

# doesn't really match...
