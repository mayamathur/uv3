

# To do: 
# :) 

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
  expect_equal( nrow(l), 60101 )
} 

# read in face data
f2 = read.csv("face_aggregated_simple.csv")

# # generic data name depending on specification
# if ( aggregated == FALSE ) dat = l
# if ( aggregated == TRUE ) dat = f2


# center variables
l.mh.mean = mean(l$mh)
f2.mh.mean = mean(f2$mh)
l$mhc = l$mh - mean(l$mh)
f2$mhc = f2$mh - mean(f2$mh)

# initialize master list and dataframe for storing results
# res.list = list()
# res.stats = data.frame( matrix( ncol=2 ) )
# names(res.stats) = c("name", "value")
# res.stats = res.stats[ -1,]
# # clunky! 

res.stats = data.frame( name = character(), 
                        value = double() )

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
# ggplot( data = f2, aes( x = mean.emot, y = mh) ) +
#   geom_point() +
#   stat_smooth() +
#   theme_classic()
# 
# # see what functional form we should use when adjusting for emotion
# ggplot( data = f2, aes( x = mean.emot, y = lik) ) +
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

# https://stackoverflow.com/questions/3822535/fitting-polynomial-model-to-data-in-r
# poly function computes orthogonal polynomials

# find order of best-fitting model
polyfit = function(i) x = AIC( lm( lik ~ poly(mhc, i, raw = TRUE) +
                                     mean.emot,
                                   weights = 1/(f2$lik_sd^2),
                                   data = f2) )
# try polys of orders 1-10
( poly.order = as.integer( optimize( polyfit,
                      interval = c( 1, 10 ) )$minimum) )

# fit the winning model
poly6.agg.adj.wtd = lm( lik ~ poly(mhc, poly.order, raw = TRUE) +
                          mean.emot,
                        weights = 1/(f2$lik_sd^2),
                        data = f2)
summary(poly6.agg.adj.wtd)

best.agg.mod = poly6.agg.adj.wtd

# sanity check: no weighting
poly6.agg.adj.unwtd = lm( lik ~ poly(mhc, poly.order, raw = TRUE) +
                          mean.emot,
                        data = f2)


##### Main-Analysis GEE Models #####

library(gee)
# data have to be sorted by cluster var for this to work per the help
l = l[ order(l$stim.name), ]

poly6.gee.adj = gee( lik ~ poly(mhc, poly.order, raw = TRUE) +
                       mean.emot,
                     id = as.factor(l$stim.name), 
                     corstr="exchangeable",
                     data = l )

##### Compare Fit of Main Analysis Models #####
best.trial.mod = poly6.gee.adj


##### Sanity-Check Models: LMM #####
library(lme4)
poly6.lmer.adj = lmer( lik ~ poly(mhc, poly.order, raw = TRUE) +
                         mean.emot + (1|stim.name) + (1|w1_uID),
                       data = l )

##### Sanity-Check Models: Misc #####

# OLS (unbiased but likely wrong inference)
poly6.lm.adj = lm( lik ~ poly(mhc, poly.order, raw = TRUE) +
                     mean.emot,
                   data = l )




############################ PLOT 1: SENSITIVITY APPENDIX PLOT ON TRIAL DATA ############################# 

xlab="Mechano-humanness score (-100 to 100)"
ylab="Likability (-100 to 100)"
shapes = c(45, 43)

library(ggplot2)


##### Inference for LM Model #####

# IMPORTANT: we're setting emotion to 0 in calculating the SEs
# make fake design matrix for which to plot the fitted Ys and calculate the SEs 
# mh.grid is uncentered here for plotting reasons
mh.grid = seq(-100, 100, 1)
nfake = length(mh.grid)

# center mh when making design matrix
X = matrix( c( rep(1, nfake),
               poly(mh.grid - f2.mh.mean, poly.order, raw = TRUE),
               rep(0, nfake) ),
            nrow = nfake )

# ~~~ CHECK THIS COVARIANCE MATRIX PROPERTY
# Cov(XB) = X Cov(B) X' since X is fixed
CovYhat = X %*% vcov(best.agg.mod) %*% t(X)

predframe.agg = data.frame( lik = X %*% coef(best.agg.mod),
                        se = sqrt( diag(CovYhat) ) )

predframe.agg$lwr = predframe.agg$lik - qnorm(.975) * predframe.agg$se
predframe.agg$upr = predframe.agg$lik + qnorm(.975) * predframe.agg$se

# for plotting joy, include centered mh score in the dataframe
predframe.agg$mhc = mh.grid - f2.mh.mean
plot(mh.grid, predframe.agg$lik)  # sanity check



##### Inference for GEE Model #####

# ~~~ CHECK THIS COVARIANCE MATRIX PROPERTY
# Cov(XB) = X Cov(B) X' since X is fixed
# middle term is var-cov matrix of beta-hats
CovYhat = X %*% best.trial.mod$robust.variance %*% t(X)

predframe.trial = data.frame( lik = X %*% coef(best.trial.mod),
                        se = sqrt( diag(CovYhat) ) )

predframe.trial$lwr = predframe.trial$lik - qnorm(.975) * predframe.trial$se
predframe.trial$upr = predframe.trial$lik + qnorm(.975) * predframe.trial$se

# centering by f2.mh.mean here because that's what is used in the x aesthetic for 
#  the below ggplot (because there the dataset being plotted is f2)
predframe.trial$mhc = mh.grid - f2.mh.mean
plot(mh.grid, predframe.trial$lik)  # sanity check



# ##### Make the Plot #####
# p = ggplot(l, aes(x=mh, y=lik) ) +
#   geom_hline(yintercept=0, color="gray", lwd=.6) +  # reference line for neutrality
#   
#   theme_classic() +
#   
#   geom_ribbon(data=predframe,
#               aes(ymin=lwr, ymax=upr),
#               alpha=0.2) +  # CI band for main analysis model
#   
#   xlab(xlab) + ylab(ylab) +
#   scale_x_continuous(breaks=seq(-100, 100, 25)) +
#   scale_y_continuous(breaks=seq(-100, 100, 25)) +
#   #scale_color_manual(values=col, labels=labels, name="") +
#   scale_shape_manual(values=shapes) +
#   theme(text = element_text(size=20) )
# 
# p = p + 
#   
#   # sanity check: LM
#   stat_function(fun = function(x) fitted_y(x = x, model = poly6.agg.adj, model.type = "marginal"),
#                 aes( color="OLS" ),
#                 lwd=1, lty = 2) + 
#   
#   # best-fitting GEE
#   stat_function(fun = function(x) fitted_y(x = x, model = poly6.gee.adj, model.type = "marginal"),
#                 aes( color="*GEE, adjusted" ),
#                 lwd=1, lty = 2) +
#   
#   # sanity check: GAM
#   geom_smooth(lty = 2,
#               se = FALSE,
#               aes(color = "Unclustered GAM") ) +
#   
#   # sanity check: LMM
#   stat_function(fun = function(x) fitted_y(x = x, model = poly3.lmer.adj, model.type = "lmer"),
#                 aes( color="LMM, adjusted" ),
#                 lwd=1.2, lty=2) +  
# 
#   
#   # sanity check: GEE, unadjusted for emotion
#   stat_function(fun = function(x) fitted_y(x = x, model = poly3.gee.unadj, model.type = "marginal"),
#                 aes( color="GEE, crude" ),
#                 lwd=1, lty = 2) +
#   
#   guides(color=guide_legend(title="Model"))
# 
# setwd(results.dir)
# ggsave("appendix_uv_curve_trial.pdf",
#        width = 10, 
#        height = 6)


############################ PLOT 2: SENSITIVITY APPENDIX PLOT ON BOTH DATASETS ############################# 

# this shows more of a UV than individual-level
# might mean that the faces that "pull" especially hard on the UV have
#  a lot of variability across subjects, so they count for more in the 
#  unweighted aggregate analysis vs. individual-level?


##### Plot Aggregate UV Curves #####

xlab="Mechano-humanness score (-100 to 100)"
ylab="Likability (-100 to 100)"
shapes = c(45, 43)

# for plotting joy
# percent weight for each face in inverse-analysis
expected.wt = 1/nrow(f2)  # if all faces contributed equally
inv.var.wt = (1/f2$lik_sd^2) / sum( (1/f2$lik_sd^2) )
f2$rel.wt = inv.var.wt / expected.wt

# want to show the boundary location, found by reading in previously saved results
# since that analysis happens below
setwd(results.dir)
if ( "res_stats.csv" %in% list.files() ) {
  saved.res = read.csv( "res_stats.csv" )
  ( boundary.mh = saved.res$value[ saved.res$name == "boundary.mh.agg" ] )
} else {
  boundary.mh = NA
}


# WILL HIT AN ERROR IF BOUNDARY.MH IS NA BECAUSE RES_STATS.CSV DOESN'T EXIST
# Base version to be customized for main text vs. appendix
# Un-center MH score for interpretability
# note that mhc here is with respect to the f2 mean, not the l mean! 
base = ggplot(f2, aes(x = mhc + f2.mh.mean, y=lik) ) +
  theme_classic() +
  
  # reference line for neutrality
  geom_hline(yintercept=0, color="gray", lwd=.6) +  
  
  # boundary MH score
  geom_vline( xintercept = boundary.mh, 
              lty = 2, 
              aes(color = "Estimated category boundary") ) +
  
  # CI band for best agg model
  geom_ribbon(data=predframe.agg,
              aes(ymin=lwr, ymax=upr),
              alpha=0.2) +  # CI band for main analysis model
  
  geom_line( data = predframe.agg, 
             aes(color = "*OLS-6, agg"),
             lwd = 1, lty = 1) +
  
  geom_point( aes(alpha= rel.wt), size = 2 ) +
  
  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  scale_y_continuous(breaks=seq(-100, 100, 25)) +
  scale_shape_manual(values=shapes) +
  theme(text = element_text(size=20) ) +
  
  guides(color=guide_legend(title="Model")) +
  guides(alpha=guide_legend(title="Relative analysis weight"))
base


# Main-text version doesn't need the model legend
uv.plot.main = base + guides(color=FALSE)
uv.plot.main  


# Appendix version of plot
uv.plot.app = base +  
  # CI band for best trial-level model
  geom_ribbon(data=predframe.trial,
              aes(ymin=lwr, ymax=upr),
              alpha=0.2) +
  
  geom_line( data = predframe.trial, 
             aes(color = "GEE-6, trial"),
             lwd = 1, lty = 2 ) +
  
  
  # LOESS, agg
  geom_smooth( aes( color="LOESS, agg, unadj" ), 
               lty = 2,
               method = "loess",
               se = FALSE ) +  # LOESS, agg
  
  # # ~~ GAM seems to do an awful job on agg data
  # # this will be important for mediation analyses...
  # geom_smooth( aes( color="GAM, agg, unadj" ), 
  #              lty = 2,
  #              method = "gam",
  #              se = FALSE ) +  # LOESS, agg
  
  # # this line seizes up and crashes unless you let it use GAM! 
  # geom_smooth( data = l, aes( color="LOESS, trial, unadj" ), 
  #              lty = 2,
  #              method = "gam",
  #              se = FALSE ) +  # LOESS, agg

  # this one doesn't have a predframe (since we don't care about its inference), 
  #  so using stat_function instead of plotting the predframe points
  stat_function( fun = function(x) fitted_y(x = x,
                                            model = poly6.agg.adj.unwtd,
                                            model.type = "marginal",
                                            needs.center = TRUE, 
                                            center.mean = f2.mh.mean),
                 aes( color="OLS-6, agg, adj, unwtd" ),
                 lwd=1, lty = 2)
    
uv.plot.app


# save the plots
setwd(results.dir)
ggsave("main_uv_curve_agg.pdf",
       plot = uv.plot.main, 
       width = 10, 
       height = 6)

ggsave("appendix_uv_curve_agg_and_trial.pdf",
       plot = uv.plot.app, 
       width = 10, 
       height = 6)


############################ ICCS BY FACE AND SUBJECT ############################ 

# ICC within faces, subjects, and sites
library(ICC)
library(dplyr)

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


# find the initial max
# put them in the dataset as uncentered mh score
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

# because optimize function creates weird lists
res.stats$value = as.numeric( res.stats$value )

# uncenter the chosen mh scores because the functions being optimized used mhc
res.stats$value[ grepl( ".mh.", res.stats$name ) == TRUE ] = res.stats$value[ grepl( ".mh.", res.stats$name ) == TRUE ] + f2.mh.mean

# all seems sensible :) 
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

# proportion of faces that were 100% human
res.stats = add_row( res.stats,
                     name = "prop.faces.100perc.human",
                     value = mean( f2$prop.human == 1 ) )

edge.face = (f2$prop.human == 0) | (f2$prop.human == 1)


##### On Aggregated Data ##### 

# remove faces with zero probability since they're hard to model with poly regression
# not weighted because here the outcome is prob. of human categorization

# find order of best-fitting model
polyfit.cat = function(i) x = AIC( lm( prop.human ~ poly(mhc, i, raw = TRUE) +
                                         mean.emot,
                                       data = f2[ edge.face == FALSE, ] ) )

# try polys of orders 1-10
( poly.order.cat = as.integer( optimize( polyfit.cat,
                                         interval = c( 1, 10 ) )$minimum) )

cat.mod.agg = lm( prop.human ~ poly(mhc, poly.order.cat, raw = TRUE) +
                          mean.emot,
                        data = f2[ edge.face == FALSE, ] )


##### find category boundary 
# minimize distance from fitted value and 0.50
( boundary.mh.agg = optimize( function(x) abs( fitted_y( x = x,
                                                     model = cat.mod.agg,
                                                     model.type = model.type )
                                           - 0.50 ),
                          interval=c(0, 100), maximum=FALSE)$minimum + f2.mh.mean )
# sanity check
fitted_y( x = boundary.mh - f2.mh.mean,
          model = cat.mod.agg,
          model.type = "marginal" )

res.stats = add_row( res.stats,
                     name = "boundary.mh.agg",
                     value = boundary.mh.agg )


##### sensitivity analysis: keep all faces
cat.mod.agg.sens = lm( prop.human ~ poly(mhc, poly.order.cat, raw = TRUE) +
                                              mean.emot,
                                            data = f2 )

( boundary.mh = optimize( function(x) abs( fitted_y( x = x,
                                                     model = cat.mod.agg.sens,
                                                     model.type = "marginal" )
                                           - 0.50 ),
                          interval=c(0, 100), maximum=FALSE)$minimum )
# sanity check
fitted_y( x = boundary.mh,
          model = category.mod.sens,
          model.type = model.type )

res.stats = add_row( res.stats,
                     name = "boundary.mh.agg.sens",
                     value = boundary.mh )




##### On Trial-Level Data ##### 

# regular Poisson regression
# no need to remove faces here because at individual trial level

# find order of best-fitting model
polyfit.cat = function(i) x = AIC( glm( cat.human ~ poly(mhc, i, raw = TRUE) +
                                          mean.emot,
                                        family = "poisson",
                                        data = l ) )

# try polys of orders 1-10
( poly.order.cat = as.integer( optimize( polyfit.cat,
                                         interval = c( 1, 10 ) )$minimum) )

cat.mod.trial = glm( cat.human ~ poly(mhc, poly.order.cat, raw = TRUE) +
                           mean.emot,
                         family = "poisson",
                         data = l )


##### find category boundary 
# minimize distance from fitted value and 0.50
( boundary.mh.trial = optimize( function(x) abs( fitted_y( x = x,
                                                         model = cat.mod.trial,
                                                         model.type = "poisson" )
                                               - 0.50 ),
                              interval=c(0, 100), maximum=FALSE)$minimum + l.mh.mean )


# sanity check
# manually calculate estimated probability for that 
exp( sum( coef(cat.mod.trial) * c( 1, (boundary.mh.trial - l.mh.mean)^(1:8), 0 ) ) ) 

res.stats = add_row( res.stats,
                     name = "boundary.mh.trial",
                     value = boundary.mh.trial )

 
##### Plot Probability of Human Categorization By MH Score (Only for Aggregated Model) #####

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
  fits = fitted_y( x = x,
                   model = cat.mod.agg,
                   model.type = "marginal",
                   needs.center = TRUE, 
                   center.mean = f2.mh.mean )
  fits[ x < mh.plot.min | x > mh.plot.max ] = NA
  fits
}

# sanity check: should be 0.50
trunc_fitted_y(boundary.mh.agg)


ylab = 'Probability of "human" categorization'
ggplot( f2, aes(x=mh, y=prop.human) ) +

  geom_hline(yintercept=.50, color="gray", lwd=.6) +  # reference line for 50% categorization
  geom_vline( aes( xintercept = res.stats$value[ res.stats$name == "boundary.mh.aggmodel"][1],
                   color="Estimated boundary"
                   ), lwd=.6, lty = 2, lwd = 3) +  # reference line for 50% categorization
  
  theme_classic() +
  
  # GAM as sanity check
  # geom_smooth( aes( color="GAM, unadjusted" ), se = FALSE ) +  
  
  stat_function( fun = trunc_fitted_y,
                 aes( color="*OLS, agg" ),
                 lty = 1, lwd = 1) +  # emotion-adjusted regression curve
  
  geom_point( alpha = 0.4, size = 2 ) +
  
  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  
  theme(text = element_text(size=20) ) + 
  guides(color=FALSE)


setwd(results.dir)
ggsave("mh_vs_categorization.pdf",
       width = 10, 
       height = 6)


# CONCLUSION
# Very interesting. It's much closer to "human" than is the point of minimum likability, even
#  when the latter is estimated on the aggregate data. 


# # temp only so that we can add to uv curve plots above
# setwd(results.dir)
# write.csv( res.stats,
#            "res_stats.csv" )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 CATEGORY CONFUSION 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



############################### DESCRIPTIVE PLOTS ###############################

# come back to this...needs to be for lm models
# Think about whether GAM fits are sensible here for use in the analysis below

##### Plot MH vs. Each Mediator #####

med.names = c("area",
              "xdev",
              "rxnt",
              "speed",
              "xflips")


# make centered mediator variables


# for formatting stats
library(MetaUtility)
library(ggplot2)


#~bm


p = lapply( med.names, function(x) mh_vs_mediator(med.name = x) )

library(gridExtra)
setwd(results.dir)
ggsave("exposure_mediator_plots.pdf",
       do.call("arrangeGrob", p),
       width = 12,
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
