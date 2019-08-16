
# Contact: Maya B. Mathur


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      PRELIMINARIES 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
library(MetaUtility) # for formatting stats
library(ggplot2)
library(dplyr)  # for add_row, etc.
library(tableone) 
library(stringr) # for str_replace
library(ICC)
library(dplyr)  # for add_row, etc.
library(readr)  # for read_csv
library(gee)

# use random subsample of long data for testing code?
subsample = FALSE

# make plots looking at functional forms for emotion adjustment?
make.emotion.plots = FALSE

# make the other plots?
make.plots = TRUE

# run simulation-based mediation from scratch?
mediate.from.scratch = TRUE

# if TRUE, it will overwrite the results in the paper! 
overwrite.existing.mediation.results = TRUE

root.dir = "~/Dropbox/Personal computer/Independent studies/Uncanny Valley III (UV3)/UV3_OSF"
code.dir = paste( root.dir, "/4_Main_Experiment/Code", sep="" )
data.dir = paste( root.dir, "/4_Main_Experiment/Data", sep="" )
results.dir = paste( root.dir, "/4_Main_Experiment/Results", sep="" )

setwd(code.dir)
source("helper_overall_analysis.R")

# read in long data
setwd(data.dir)
if ( subsample == TRUE) l = read_csv( "long_prepped_subsample.csv" )
if ( subsample == FALSE){
  l = read_csv("long_prepped.csv")
  library(testthat)
  expect_equal( nrow(l), 55430 )
} 

# read in face data
f2 = read.csv("face_aggregated_simple.csv")

# read in detailed face data
# only used for getting number of validation subjects rating each face
f = read.csv("face_aggregated_detailed.csv")

# initialize master list and dataframe for storing results
res.stats = data.frame( name = character(), 
                        value = double() )

# original MH score means prior to centering
f2.mh.mean = mean(f2$mh)
l.mh.mean = mean(l$mh)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    DESCRIPTIVE STATS 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### ICC within Faces, Subjects, and Sites #####
# as expected, almost all the clustering is due to faces, not the others
res.stats = add_row( res.stats, name = "ICC.stim",
                     value = ICCbareF( x = stim.name, y = lik, data = l ) )

res.stats = add_row( res.stats, name = "ICC.site",
                     value = ICCbareF( x = w1_site, y = lik, data = l ) )

res.stats = add_row( res.stats, name = "ICC.uID",
                     value = ICCbareF( x = w1_uID, y = lik, data = l ) )


##### Descriptive Stats on Faces #####
res.stats = add_row( res.stats, name = "n.faces",
                     value = nrow(f2) )

res.stats = add_row( res.stats, name = "n.actual.humans",
                     value = sum(f2$actually.human == 1) )

res.stats = add_row( res.stats, name = "n.actual.robots",
                     value = sum(f2$actually.human == 0) )

res.stats = add_row( res.stats, name = "mean.mh.actual.robots",
                     value = mean( f2$mh[ f2$actually.human == 0 ] ) )

res.stats = add_row( res.stats, name = "mean.mh.actual.humans",
                     value = mean( f2$mh[ f2$actually.human == 1 ] ) )

res.stats = add_row( res.stats, name = "mean.mh.all",
                     value = mean( f2$mh ) )

res.stats = add_row( res.stats, name = "mean.lik.actual.robots",
                     value = mean( f2$lik[ f2$actually.human == 0 ] ) )

res.stats = add_row( res.stats, name = "mean.lik.actual.humans",
                     value = mean( f2$lik[ f2$actually.human == 1 ] ) )

res.stats = add_row( res.stats, name = "mean.lik.all",
                     value = mean( f2$lik ) )

# number of subjects who rated faces in validation
res.stats = add_row( res.stats, name = "mean.valid.subj",
                     value = mean( f$n.ratings ) )

res.stats = add_row( res.stats, name = "min.valid.subj",
                     value = min( f$n.ratings ) )

##### Descriptive Stats on Subjects #####
res.stats = add_row( res.stats, name = "n.subjects",
                     value = length(unique(l$w1_uID)) )

res.stats = add_row( res.stats, name = "n.ratings",
                     value = nrow(l) )

res.stats = add_row( res.stats, name = "perc.female.subj",
                     value = 100 * mean( l$Female[ !duplicated(l$w1_uID) ] ) )

res.stats = add_row( res.stats, name = "mean.age.subj",
                     value = mean( l$Age[ !duplicated(l$w1_uID) ], na.rm = TRUE ) )


setwd(results.dir)
write.csv( res.stats,
           "res_stats.csv",
           row.names = FALSE,
           quote = FALSE )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   AIM 1. UV CURVE 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


############################### FUNCTIONAL FORM FOR EMOTION CONFOUNDER ###############################

# polynomial regression
# not yet using inverse-weighting by likability variance

##### Decide What Functional Form to Use When Adjusting For Emotion #####
# commented out for speed

# see what functional form we should use when adjusting for emotion

if ( make.emotion.plots == TRUE ) {
  ggplot( data = f2, aes( x = mean.emot, y = mh) ) +
    geom_point() +
    stat_smooth() +
    theme_classic()
  
  # see what functional form we should use when adjusting for emotion
  ggplot( data = f2, aes( x = mean.emot, y = lik) ) +
    geom_point() +
    stat_smooth() +
    theme_classic()
  # linear should be reasonable for both
}



############################### FIT ALL UV CURVE MODELS (AGGREGATED AND TRIAL-LEVEL) ###############################

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

# data have to be sorted by cluster var for this to work per the help
l = l[ order(l$stim.name), ]

poly6.gee.adj = gee( lik ~ poly(mhc, poly.order, raw = TRUE) +
                       mean.emot,
                     id = as.factor(l$stim.name), 
                     corstr="exchangeable",
                     data = l )

##### Compare Fit of Main Analysis Models #####
best.trial.mod = poly6.gee.adj


##### Sanity-Check Model: OLS #####

# OLS (unbiased but likely wrong inference)
poly6.lm.adj = lm( lik ~ poly(mhc, poly.order, raw = TRUE) +
                     mean.emot,
                   data = l )


############################ PLOT: UV CURVE ############################# 

if ( make.plots == TRUE ) {
  
  xlab="Mechano-humanness score (-100 to 100)"
  ylab="Likability (-100 to 100)"
  shapes = c(45, 43)
  
  
  ##### Inference for Aggregated Model #####
  
  # note that we're setting emotion to 0 in calculating the SEs
  # make fake design matrix for which to plot the fitted Ys and calculate the SEs 
  # mh.grid is uncentered here for plotting reasons
  mh.grid = seq(-100, 100, 1)
  nfake = length(mh.grid)
  
  # center mh when making design matrix
  # since we fit the poly models using mhc
  X = matrix( c( rep(1, nfake),
                 poly(mh.grid - f2.mh.mean, poly.order, raw = TRUE),
                 rep(0, nfake) ),
              nrow = nfake )
  
  # Cov(XB) = X Cov(B) X' since X is fixed
  # e.g., http://www.stat.ucla.edu/~nchristo/introeconometrics/introecon_fitted.pdf
  CovYhat = X %*% vcov(best.agg.mod) %*% t(X)
  
  predframe.agg = data.frame( lik = X %*% coef(best.agg.mod),
                          se = sqrt( diag(CovYhat) ) )
  
  predframe.agg$lwr = predframe.agg$lik - qnorm(.975) * predframe.agg$se
  predframe.agg$upr = predframe.agg$lik + qnorm(.975) * predframe.agg$se
  
  # for plotting joy, include centered mh score in the dataframe
  predframe.agg$mhc = mh.grid - f2.mh.mean
  plot(mh.grid, predframe.agg$lik)  # sanity check
  
  
  
  ##### Inference for GEE Model #####

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

}

############################ PLOT: APPENDIX UV PLOT ON BOTH DATASETS ############################# 

# this shows more of a UV than individual-level
# might mean that the faces that "pull" especially hard on the UV have
#  a lot of variability across subjects, so they count for more in the 
#  unweighted aggregate analysis vs. individual-level?


##### Plot Aggregate UV Curves #####

xlab="Mechano-humanness score (-100 to 100)"
ylab="Likability (-100 to 100)"
shapes = c(16, 17)
text.size = 14
point.size = 3

# for plotting joy
# percent weight for each face in inverse-analysis vs. its "expected weight"
#  if all faces contributed equally
expected.wt = 1/nrow(f2)  # if all faces contributed equally
inv.var.wt = (1/f2$lik_sd^2) / sum( (1/f2$lik_sd^2) )
f2$rel.wt = inv.var.wt / expected.wt

# also for plotting joy
f2$actually.human.pretty[ f2$actually.human == 1 ] = "Truly human"
f2$actually.human.pretty[ f2$actually.human == 0 ] = "Truly robot"


# Base version to be customized for main text vs. appendix
# x represents *uncentered* MH score for interpretability
base = ggplot(f2, aes(x = mhc + f2.mh.mean, y=lik) ) +
  theme_classic() +

  # reference line for neutrality
  geom_hline(yintercept=0, color="gray", lwd=.6) +  

  # CI band for best agg model
  geom_ribbon(data=predframe.agg,
              aes(ymin=lwr, ymax=upr),
              alpha=0.2,
              lwd = 1.1) +  # CI band for main analysis model
  
  geom_line( data = predframe.agg, 
             #aes(color = "*OLS-6, agg"),
             lwd = 1, lty = 1) +
  
  geom_point( aes(alpha = rel.wt,
                  shape = actually.human.pretty ), size = point.size ) +
  
  xlab(xlab) + ylab(ylab) +
  scale_x_continuous(breaks=seq(-100, 100, 25)) +
  scale_y_continuous(breaks=seq(-100, 100, 25)) +
  scale_shape_manual(values=shapes) +
  theme(text = element_text(size = text.size) ) +
  
  guides(alpha=guide_legend(title="Analysis weight")) +
  guides(shape=guide_legend(title="Face type"))
base


# want to show the boundary location, found by reading in previously saved results
# since that analysis happens below
setwd(results.dir)
if ( "res_stats.csv" %in% list.files() ) {
  saved.res = read.csv( "res_stats.csv" )
  ( boundary.mh = saved.res$value[ saved.res$name == "boundary.mh.agg" ] )
  ( mh.nadir = saved.res$value[ saved.res$name == "global.min.mh.aggmodel" ] )
  
  uv.plot.main =   base + 
    geom_vline( aes(xintercept = boundary.mh, color = "Category boundary"), 
                lty = 2, lwd = 1 )
  
  uv.plot.main =   uv.plot.main + 
    geom_vline( aes(xintercept = mh.nadir, color = "MH score at UV nadir"), 
                lty = 2, lwd = 1 ) +
  
   scale_color_manual(name = "statistics", values = c(`Category boundary` = "blue",
                                                      `MH score at UV nadir` = "orange")) +
    
    guides(color=guide_legend(title="Model estimates"))
    
} 


# # Main-text version doesn't need the model legend
# #uv.plot.main = base + guides(color=FALSE)
# uv.plot.main = base
# uv.plot.main  


# Appendix version of plot
uv.plot.app = base +  
  # CI band for best trial-level model
  geom_ribbon(data=predframe.trial,
              aes(ymin=lwr, ymax=upr),
              alpha=0.2) +
  
  geom_line( data = predframe.trial, 
             aes(color = "GEE, trial"),
             lwd = 1, lty = 2 ) +
  
  # # LOESS, agg
  # geom_smooth( aes( color="LOESS, agg, unadj" ), 
  #              lty = 2,
  #              method = "loess",
  #              se = FALSE ) +  # LOESS, agg
  
  
  # this one doesn't have a predframe (since we don't care about its inference), 
  #  so using stat_function instead of plotting the predframe points
  stat_function( fun = function(x) fitted_y(x = x,
                                            model = poly6.agg.adj.unwtd,
                                            model.type = "marginal",
                                            needs.center = TRUE, 
                                            center.mean = f2.mh.mean),
                 aes( color="OLS, unwtd, face-level" ),
                 lwd=1, lty = 2) +
  
  # put back the line for the main model, this time with label
  geom_line( data = predframe.agg, 
             aes(color = "* OLS, wtd, face-level"),
             lwd = 1, lty = 1) +
  
  guides(color=guide_legend(title="Model"))
    
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
#                             AIM 2. ESTIMATE CATEGORY BOUNDARY LOCATION 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# From prereg:
# Additionally, we will estimate the location of the category boundary
# (i.e., the MH score at which the proportion of subjects categorizing
# the face as “human” is closest to 50%) via polynomial regression.


############################### RELEVANT DESCRIPTIVE STATS ###############################


# proportion of faces that were 0% human
res.stats = add_row( res.stats,
                     name = "perc.faces.0perc.human",
                     value = 100*mean( f2$prop.human == 0 ) )

res.stats = add_row( res.stats,
                     name = "n.faces.0perc.human",
                     value = sum( f2$prop.human == 0 ) )

# proportion of faces that were 100% human
res.stats = add_row( res.stats,
                     name = "perc.faces.100perc.human",
                     value = 100*mean( f2$prop.human == 1 ) )

res.stats = add_row( res.stats,
                     name = "n.faces.100perc.human",
                     value = sum( f2$prop.human == 1 ) )

edge.face = (f2$prop.human == 0) | (f2$prop.human == 1)



############################### FIT BOUNDARY MODEL ###############################


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
                                                     model.type = "marginal" )
                                           - 0.50 ),
                          interval=c(0, 100), maximum=FALSE)$minimum + f2.mh.mean )
# sanity check
# should be very close to 0.50
fitted_y( x = boundary.mh.agg - f2.mh.mean,
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
          model = cat.mod.agg.sens,
          model.type = "marginal" )

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

 
############################### PLOT: MH SCORE VS. P(HUMAN CATEGORIZATION) ###############################

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
#trunc_fitted_y(boundary.mh.agg)



if ( make.plots == TRUE ) {
  ylab = 'Probability of "human" categorization'
  ggplot( f2, aes(x=mh, y=prop.human) ) +
    
    geom_hline(yintercept=.50, color="gray", lwd=.6) +  # reference line for 50% categorization
    
    geom_vline( aes( xintercept = res.stats$value[ res.stats$name == "boundary.mh.agg"],
                     color="Category boundary"
    ), lty = 2, lwd = 1) +  # reference line for 50% categorization
    
    geom_vline( aes( xintercept = res.stats$value[ res.stats$name == "global.min.mh.aggmodel"],
                     color="MH score at UV nadir"
    ), lty = 2, lwd = 1) +  
    
    theme_classic() +
    
    # GAM as sanity check
    # geom_smooth( aes( color="GAM, unadjusted" ), se = FALSE ) +  
    
    stat_function( fun = trunc_fitted_y,
                   #aes( color="*OLS, agg" ),
                   lty = 1, lwd = 1) +  # emotion-adjusted regression curve
    
    geom_point( aes(shape = actually.human.pretty), 
                size = point.size,
                alpha = 0.4 ) +
    
    xlab(xlab) + ylab(ylab) +
    scale_x_continuous(breaks=seq(-100, 100, 25)) +
    
    theme(text = element_text(size=text.size) ) +
    
    scale_color_manual(name = "Model estimates", values = c(`Category boundary` = "blue", `MH score at UV nadir` = "orange")) +
    guides(shape=guide_legend(title="Face type"))
  
  
  
  setwd(results.dir)
  ggsave("mh_vs_categorization.pdf",
         width = 10, 
         height = 6)
}




##### Find faces closest to key points on MH spectrum #####

# for boxing faces in the matrix figure

mh.initial.apex = res.stats$value[ res.stats$name == "initial.max.mh.aggmodel" ]
f2$stim.name[ which.min( abs( f2$mh - mh.initial.apex ) ) ]

mh.nadir = res.stats$value[ res.stats$name == "global.min.mh.aggmodel" ]
f2$stim.name[ which.min( abs( f2$mh - mh.nadir ) ) ]

global.max.mh.aggmodel = res.stats$value[ res.stats$name == "global.max.mh.aggmodel" ]
f2$stim.name[ which.min( abs( f2$mh - global.max.mh.aggmodel ) ) ]

boundary = res.stats$value[ res.stats$name == "boundary.mh.agg" ]
f2$stim.name[ which.min( abs( f2$mh - boundary ) ) ]




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                               AIM 3. MEDIATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


############################### RELEVANT DESCRIPTIVE STATS ###############################

med.names = c("xflips",
              "xdev",
              "area",
              "medsum",
              "rxnt",
              "speed")

# the mediators are very highly correlated (r = 0.68-0.98)
cor( f2[,med.names])
sort( cor(f2[,med.names]) )

# correlations for the ones used in the composite
primary.med.corr = cor( f2[ , c( "xflips", "xdev", "area" ) ] )

res.stats = add_row( res.stats, name = "min.corr.primary.meds",
                     value = min( primary.med.corr ) )
res.stats = add_row( res.stats, name = "max.corr.primary.meds",
                     value = max( primary.med.corr[ !primary.med.corr == 1 ] ) )
write.csv( res.stats,
           "res_stats.csv",
           row.names = FALSE,
           quote = FALSE )


############################### MEDIATOR PLOTS ###############################

# this part also adds rows to results file for min and max points
if ( make.plots == TRUE ) {
  ##### Plot MH vs. Each Mediator #####
  
  # using polynomials in mhc
  p = lapply( med.names, function(y) polyx_vs_y_plot(xname = "mhz",
                                                     yname = y,
                                                     put.max.in.df = TRUE,
                                                     maximizer = "gam", 
                                                     have.legend = FALSE,
                                                     plot.lm = FALSE ) )
  
  library(gridExtra)
  setwd(results.dir)
  ggsave("exposure_mediator_continuous_plots.pdf",
         do.call("arrangeGrob", p),
         width = 8,
         height = 8)

  
  ##### Plot Each Mediator vs. Likability #####
  
  # using polynomials in mediator
  p = lapply( med.names, function(x) polyx_vs_y_plot(xname = x,
                                                     yname = "lik",
                                                     put.max.in.df = TRUE,
                                                     maximizer = "gam", 
                                                     have.legend = FALSE,
                                                     plot.lm = FALSE ) )
  
  library(gridExtra)
  setwd(results.dir)
  ggsave("mediator_outcome_continuous_plots.pdf",
         do.call("arrangeGrob", p),
         width = 8,
         height = 8)
}


# add unstandardized MH maximizers to the results df
temp = res.stats[ grepl(x = res.stats$name, pattern = "mhz.maximizing") == TRUE,  ]
temp$value = temp$value * sd(f2$mh) + mean(f2$mh)
library(stringr)
temp$name = str_replace( temp$name, pattern = "mhz", replacement = "mh" )

res.stats = rbind( res.stats, temp )

setwd(results.dir)
write.csv( res.stats,
           "res_stats.csv",
           row.names = FALSE,
           quote = FALSE )


############################### RUN MEDIATION ANALYSES ###############################

# Prereg:
# For the mediator models for the positive, continuous variables (maximum deviation, curve area,
# peak velocity, and reaction time), we will use generalized additive models (GAM) with the
# identity link to regress the measure of category confusion on a cubic regression spline basis
# for MH score.

# Note: Because of the stochastic nature of both point estimation and especially inference here, 
#  you will get slightly different results from those in the paper if you re-run this from scratch

if ( mediate.from.scratch == TRUE ) {
  ##### Run For All of Them #####
  start.time = Sys.time()
  med.res = lapply( med.names,
                    FUN = function(x) med_analysis( med.name = x,
                                                    boot = TRUE,
                                                    sims = 1000,
                                                    seed = 451 ) )
  end.time = Sys.time()
  ( total.mediation.time = end.time - start.time )
  
  
  stats.lists = lapply( med.res, function(x) x$stats )
  res = do.call( rbind,
                 stats.lists )


  # setwd(results.dir)
  # write.csv(res, "all_mediation_results.csv")
  # write.csv(res[ res$stat == "prop.med", ], "propmed_mediation_results.csv")
  # write.csv(res[ res$stat == "nde.avg", ], "nde_mediation_results.csv")
  # write.csv(res[ res$stat == "nie.avg", ], "nie_mediation_results.csv")
  # write.csv(res[ res$stat == "te", ], "te_mediation_results.csv")
  
  # convert from prop med to percent mediated
  res$est[ res$stat == "prop.med" ] = 100 * res$est[ res$stat == "prop.med" ]
  res$lo[ res$stat == "prop.med" ] = 100 * res$lo[ res$stat == "prop.med" ]
  res$hi[ res$stat == "prop.med" ] = 100 * res$hi[ res$stat == "prop.med" ]
  res$stat = as.character(res$stat)
  res$stat[ res$stat == "prop.med" ] = "perc.med"
  
  setwd(results.dir)
  write.csv(res, "all_mediation_results.csv", row.names = FALSE)
  
  # sanity check on one of them
  # note that when there is no interaction term, printed summary
  # reports everything for "0" group
  # so doesn't agree with what I saved in the res
  summary(med.res[[2]]$mediate.object)$n1.p
  summary(med.res[[2]]$mediate.object)$n0.p
  
  
  ##### Table for Paper #####
  # first format the stats
  res2 = res
  
  res2$est.CI[ res2$stat != "perc.med" ] = paste( round( res2$est[ res2$stat != "perc.med" ], 1 ), 
                                                  format_CI( res2$lo[ res2$stat != "perc.med" ],
                                                             res2$hi[ res2$stat != "perc.med" ], 1 ) )
  
  res2$est.CI[ res2$stat == "perc.med" ] = paste( round( res2$est[ res2$stat == "perc.med" ], 0 ), 
                                                  format_CI( res2$lo[ res2$stat == "perc.med" ],
                                                             res2$hi[ res2$stat == "perc.med" ], 0 ) )
  
  
  res2$pval.pretty = format_stat(res2$pval)
  
  # drop unwanted columns
  res2 = dplyr::select( res2, med.name, stat, est.CI, pval.pretty )
  
  
  # sort by mediator name, then stat
  # and don't report total effect because table too long
  res2 = res2[ !res2$stat == "te", ]
  my.order.1 = c("xflips", "xdev", "area", "medsum", "rxnt", "speed")
  my.order.2 = c("nde.avg", "nie.avg", "prop.med" ) 
  res2 = res2[ order( match( res2$med.name, my.order.1 ),
                      match( res2$stat, my.order.2 ) ), ]
  
  library(xtable)
  print( xtable(res2), include.rownames = FALSE )
  
  # add some internal header rows
  res2 = add_row( res2, 
                  med.name = "Primary measures",
                  stat = "",
                  est.CI = "",
                  pval.pretty = "",
                  .before = 1
                  )
  
  res2 = add_row( res2, 
                  med.name = "Secondary measures",
                  stat = "",
                  est.CI = "",
                  pval.pretty = "",
                  .after = 12
  )
  
  # rename things to be pretty
  res2$stat[ res2$stat == "nde.avg" ] = "Direct effect"
  res2$stat[ res2$stat == "nie.avg" ] = "Indirect effect"
  res2$stat[ res2$stat == "perc.med" ] = "% mediated"
  
  res2$med.name = as.character(res2$med.name)
  res2$med.name[ res2$med.name == "xflips" ] = "x-flips"
  res2$med.name[ res2$med.name == "xdev" ] = "x-deviation"
  res2$med.name[ res2$med.name == "area" ] = "Area"
  res2$med.name[ res2$med.name == "medsum" ] = "Composite"
  res2$med.name[ res2$med.name == "rxnt" ] = "Reaction time"
  res2$med.name[ res2$med.name == "speed" ] = "Peak speed"
  
  names(res2) = c("Confusion variable",
                  "Statistic",
                  "Estimate [95% CI]",
                  "p-value")
  
  
  # if we are NOT supposed to overwrite the existing results, 
  # write the new results in a file with today's date
  if ( overwrite.existing.mediation.results == TRUE ) {
    setwd(results.dir)
    write.csv(res2, "short_mediation_results.csv", row.names = FALSE)
  } else {
    write.csv( res2, paste( Sys.Date(), "_short_mediation_results.csv", sep = "" ),
              row.names = FALSE )
  }

}





