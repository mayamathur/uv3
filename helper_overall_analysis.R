

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                     FNS FOR DATA PREP 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# fix known data idiosyncrasies and resulting exclusions
fix_site_idiosyncrasies = function( dat, wave.num ) {
  
  sitevar = paste( "w", wave.num, "_site", sep = "" )
  idvar = paste( "w", wave.num, "_uID", sep = "" )
  
  ##### Politecnico di Milano #####
  # two subjects had same ID for the first wave
  # fix the second one
  dat[[idvar]][ dat[[sitevar]] == "Politecnico di Milano" ]
  dat[[idvar]][ dat[[idvar]] == "Politecnico di Milano RVU" & dat$RecordedDate == "2019-02-21 03:52:35" ] = "Politecnico di Milano OWH"
  
  ##### Eotvos Lorand #####
  dat[[idvar]][ dat[[sitevar]] == "Eotvos Lorand University" ]
  
  if ( wave.num == 1 ) {
    #Eotvos Lorand University 114 should be Caucasian
    dat$w1_race[ dat$w1_uID == "Eotvos Lorand University 114" ] = "Caucasian"
  }
  
  # one subject entered univerisity ID instead of experiment ID
  dat[[idvar]][ dat[[idvar]] == "Eotvos Lorand University JXX096"] = "Eotvos Lorand University 24"
  
  ##### Exclude Subjects With Known Idiosyncrasies #####
  
  # the below issues for Eotvos Lorand should only be in wave 2 
  exclude.ids = c( paste( "Eotvos Lorand University", 31:38, sep = " " ), # waves run out of order
                   "Politecnico di Milano XAQ", # waves run out of order
                   "Politecnico di Milano RA TEST", # not a real subject
                   "Politecnico di Milano XXX1" )  # same
  
  previous.rows = nrow(dat)
  dat = dat[ !dat[[idvar]] %in% exclude.ids, ]
  new.rows = nrow(dat)
  
  # "rows" rather than "subjects" because in wave 2, for example, 
  #  some of the bad IDs are duplicated
  cat( "\n", previous.rows - new.rows,
       " rows excluded because waves run out of order or they were tests" )
  
  
  ###### Exclude Subjects Who Entered Mismatched IDs #####
  idvar1 = paste( "w", wave.num, "_id1", sep = "" )
  idvar2 = paste( "w", wave.num, "_id2", sep = "" )
  
  doesnt.match = as.character( dat[[idvar1]] ) != as.character( dat[[idvar2]] )
  
  # for Wave 2, preserve header rows needed for URLs
  if ( wave.num == 2 ) doesnt.match[1] = FALSE
  
  # exclude them
  previous.rows = nrow(dat)
  dat = dat[ doesnt.match == FALSE, ]
  new.rows = nrow(dat)
  cat( "\n", previous.rows - new.rows, " subjects with mismatched IDs excluded" )
  
  ##### Exclude Subjects Who Didn't Finish #####
  
  didnt.finish = (dat$Finished != "True")
  
  # for Wave 2, preserve header rows needed for URLs
  if ( wave.num == 2 ) didnt.finish[1] = FALSE
  
  previous.rows = nrow(dat) 
  dat = dat[ didnt.finish == FALSE, ]
  new.rows = nrow(dat)
  cat( "\n", previous.rows - new.rows, " subjects who didn't finish excluded" )
  
  return(dat)
}

# checks whether variable is outlier as defined in prereg
is.outlier = function(x) {
  summ = summary(x)
  Q1 = summ["1st Qu."]
  Q3 = summ["3rd Qu."]
  IQR = Q3 - Q1
  lo = Q1 - 1.5 * IQR
  hi = Q3 + 1.5 * IQR
  
  return( x < lo | x > hi )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                FNS FOR FITTING/PLOTTING POLY MODELS 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# given a model function with a single argument called "order",
#  searches across order to find the best fit based on AIC
best_poly = function( model.fn,
                      max.order = 10 ) {
  
  # find order of best-fitting model
  # try polys of orders 1-10
  # doesn't work in multiple variables...just returns start values! 
  # poly.order = optim( par = rep(1, npar),  # initial values
  #                                 fn = function(par) AIC( model.fn(round(par)) ),
  #                                  method = "L-BFGS-B",
  #                                  lower = c( 1, 1 ),
  #                                  upper = rep( max.order, npar ) )
  
  poly.order = as.integer( optimize(
                      f = function(par) AIC( model.fn( par ) ),
                      lower = 1,
                      upper = max.order )$minimum )
  
  if ( poly.order == 10 ) warning( "Might need to try higher orders. Best order was 10 (max attempted).")
  
  # fit and return best one
  return( model.fn( poly.order ) )
}

# # test
# model.fn = function(order) {
#   lm( f2$lik ~ poly( f2[["area"]], order, raw = TRUE) +
#         mean.emot,
#       data = f2)
# }
# 
# model.fn(7)
# 
# best_poly(model.fn)



# plot some x-variable vs. some y-variable with polynomial fit
# xname should be UNCENTERED one
# assumes the df has a variable called "xnamec" and that there is a global var
#  called "f2.xname.mean" representing the pre-centering mean

polyx_vs_y_plot = function( xname,
                            yname,
                            xlab = paste(xname, "c", sep=""),
                            ylab = yname,
                            put.max.in.df = FALSE,
                            have.legend = TRUE,
                            text.size = 16,
                            maximizer,
                            plot.lm ) {
  
  
  # now makes sense even for xflips because it's a mean  
  family = "gaussian"
  
  # make a temporary dataset so that we can make generically-named variables
  temp = f2
  temp$x = f2[[xname]]
  temp$xc = f2[[xname]] - mean( f2[[xname]], na.rm = TRUE )
  temp = temp[ !is.na( temp[[xname]] ) & !is.na( temp[[yname]] ), ]
  x.mean = mean( temp$x )
  xmin = min(temp$x)
  xmax = max(temp$x)
  
  ##### Fit Polynomial Model #####
  polyfit = function(i) x = AIC( glm( temp[[yname]] ~ poly( xc, i, raw = TRUE ) +
                                        mean.emot,
                                      family = family,
                                      data = temp ) )
  
  # try polys of orders 1-10
  ( poly.order = as.integer( optimize( polyfit,
                                       interval = c( 1, 10 ) )$minimum) )
  
  mod.lm = glm( temp[[yname]] ~ poly( xc, poly.order, raw = TRUE ) +
                                mean.emot,
                              family = family,
                              data = temp )
  
  # # sanity - should look kinda similar
  # plot( temp$xc, temp[[yname]] )
  # plot( temp$xc, predict(mod.lm) )
  
  
  ##### Fit GAM Model ####
  
  library(mgcv)
  mod.gam = gam( temp[[yname]] ~ s(xc) + mean.emot,
                               data = temp,
                               family = family )

  # make grid of 200 evenly-spaced x-values
  xmax = max( temp$x )
  xmin = min( temp$x )
  jump.size = ( xmax - xmin ) / 200
  x.grid = seq( xmin, xmax, jump.size )

  gam.predframe = data.frame( xc = x.grid - x.mean,
                              mean.emot = 0 )

  gam.Yhat = predict( mod.gam, newdata = gam.predframe )
  gam.predframe[[yname]] = gam.Yhat

  # sanity
  #plot( gam.predframe$xc, gam.predframe[[yname]] )

  # for plotting joy
  gam.predframe$x = gam.predframe$xc + x.mean

  # find the (uncentered) x at which y is maximized
  # maximize over the observed range of x
  # based on the lm fit
  
  if ( maximizer == "lm" ) {
    x.maximizer = optimize( function(i) fitted_y(x = i,
                                                 xname = "xc",  # because model is fit with "xc" as the var name
                                                 model = mod.lm,
                                                 model.type = "lm"),
                            interval=c(xmin - x.mean, xmax - x.mean),
                            maximum=TRUE)$maximum + x.mean
    max.label = "Argmax (LM)"
    
  }
  
  if ( maximizer == "gam" ) {
    x.maximizer = gam.predframe$x[ which.max( gam.Yhat ) ]
    max.label = "Argmax (GAM)"
  }

  # expects there to be a global variable called res.stats
  if (put.max.in.df == TRUE) {
    if ( !exists("res.stats") ) stop("res.stats df doesn't exist!")
  
    name = paste( xname, ".maximizing.", yname, sep="" )
    res.stats <<- add_row( res.stats, name = name,
             value = x.maximizer )
  }

  
  # figure out pretty axis labels
  xlab = xname
  ylab = yname
  
  if ( xname == "area" ) xlab = "* Area (std.)"
  if ( xname == "xdev" ) xlab = "* x-deviation (std.)"
  if ( xname == "speed" ) xlab = "Peak speed (std.)"
  if ( xname == "rxnt" ) xlab = "Reaction time (std.)"
  if ( xname == "xflips" ) xlab = "* x-flips (std.)"
  if ( xname == "medsum" ) xlab = "Composite"
  if ( yname == "lik") ylab = "Likability"
  
  if ( xname == "mh") xlab = "MH score"
  if ( xname == "mhz") xlab = "MH score (std.)"
  if ( yname == "area" ) ylab = "* Area (std.)"
  if ( yname == "xdev" ) ylab = "* x-deviation (std.)"
  if ( yname == "speed" ) ylab = "Peak speed (std.)"
  if ( yname == "rxnt" ) ylab = "Reaction time (std.)"
  if ( yname == "xflips" ) ylab = "* x-flips (std.)"
  if ( yname == "medsum" ) ylab = "Composite"

  message("Remember we are plotting centered x on the x-axis")
  
  plot = ggplot( data = temp, aes( x=xc, y=temp[[yname]] ) ) +
    geom_vline( aes( xintercept=x.maximizer - x.mean ),
                lwd=.6, lty = 2) + 
    
    geom_point(alpha = 0.4) +
    
    theme_classic() +
    
    #geom_smooth( aes( color="GAM, unadjusted" ) ) +  
    
    # GAM predictions
    geom_line( data = gam.predframe,
               aes(x = xc,
                   y = gam.predframe[[yname]],
                   color = "GAM"),
               lwd = 1, lty = 2) +
  
    scale_x_continuous( limits = c(min(temp$xc), max(temp$xc)) ) +
    
    xlab(xlab) + ylab(ylab) +
    
    guides(color=guide_legend(title="")) +
  
  theme(text = element_text(size=text.size) )
  
  if ( have.legend == FALSE ) plot = plot + theme(legend.position = "none")
  
  if ( plot.lm == TRUE ) plot = plot +  stat_function( fun = function(x) fitted_y( x = x,
                                                                                      xname = "xc",
                                                                                      model = mod.lm,
                                                                                      model.type = "lm",
                                                                                      needs.center = FALSE, 
                                                                                      center.mean = x.mean ),
                                                          aes( color="LM" ),
                                                          lwd=1, lty = 2)
  
  return(plot)
}


# plot a categorical x vs. y
catx_vs_y_plot = function( xname, yname ) {
  
  ggplot( data = f2, aes( x = f2[[xname]], y = f2[[yname]] ) ) +
    geom_violin() +
    # means
    stat_summary(fun.y=mean, geom="point", shape=23, size=2, color = "red") +
    xlab(xname) + 
      ylab(yname)
}


# for an OLS, LMER, or GEE model, get the yhat when setting mean.emot to 0
# x is the x-value at which to evaluate function
# model.type = "marginal" works for GEE or LM
# xname: its name in the dataset (used for choosing coefficients from model)
fitted_y = Vectorize( function( x,
                                xname = "mh",
                                model,
                                model.type,
                                needs.center = FALSE,
                                center.mean = NA ) {
  
 
  if ( model.type == "lmer" ) b = fixef(model)
  if ( model.type != "lmer" ) b = coef(model)
  
  # stop if xname doesn't make sense
  if ( sum( grepl( xname, names(b) ) ) == 0 ) stop("xname never occurs in names(coef(model))")
  
  # do we need to center x? 
  if ( needs.center == TRUE ) {
    x = x - center.mean
  }
  
  # figure out how many parameters are for mh and its
  #  higher-order terms
  # then add 1 for intercept
  # ignore any terms for mean.emot because evaluating at mean
  p = sum( grepl(xname, names(b)) ) + 1
  
  # this works because it's NOT vector multiplication as in math
  # but rather element-wise
  powers = 0:(p-1)  # MH score powers
  
  if ( model.type != "poisson") {
    # subset in case there is extra coefficient for mean.emot at the end
    return( sum( b[1:p] * x^powers ) )
  }

  # for P(human categorization) analyses
  if ( model.type == "poisson" ) {
    return( exp( sum( b[1:p] * x^powers ) ) )
  }
}, vectorize.args = "x" )

# # test
# fitted_y( x = 0, model = best.mod, model.type = "lmer" )
# fitted_y( x = 0, model = poly3.lm.adj, model.type = "lm" )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                FNS FOR MEDIATION ANALYSIS 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# do mediation analysis for one mediator
# prints out results of gam.check for model fit
med_analysis = function( med.name,
                         boot = TRUE, 
                         sims = 500,
                         seed = NA ) {
  
  cat("\n\n\n************Starting mediation analysis for ", med.name )

  # to avoid confusing the mediation package, need to make a variable
  #  for the mediator
  f2$med = f2[[med.name]]


  # GAM
  # (note that package does not handle poly terms, whether coded as poly() or as 
  # I(mh^2), etc., so we cannot do that as a sensitivity analysis)
  med.mod = gam( med ~ s(mhz) + mean.emot,
                             data = f2)
  
  out.small = gam( lik ~ s(mhz) + s(med) + 
                   mean.emot,
                 data = f2) 
  
  out.big = gam( lik ~ s(mhz) + s(med) + 
                   te(mhz, med) +
                   mean.emot,
             data = f2) 

  
  # pick the outcome model with smaller AIC
  if ( AIC(out.small) < AIC(out.big) ) {
    out.mod = out.small
    interaction = "no"  # record ultimate decision
  } else {
    out.mod = out.big
    interaction = "yes"
  }

  
  # # sanity check: fitted vs. true
  # plot( f2$mhc, predict( med.mod) ); points(f2$mhc, f2$medc, add=TRUE, col="red")
  # plot( f2$mhc, predict( out.mod) ); points(f2$mhc, f2$lik, add=TRUE, col="red")
  
  # mediate fn is weird about tibbles, etc.
  f2 = as.data.frame(f2)
  
  library(mediation)
  if ( !is.na(seed) ) set.seed(seed)
  res = mediate( model.m = med.mod,
                 model.y = out.mod,
                 boot=boot,
                 sims=sims,
                 treat="mhz",
                 mediator="med",
                 treat.value = .5, # 1/2-SD increase in MH score from the mean
                control.value = 0  # start at mean MH score since centered
  )

  
  cat("\n************Survived mediation analysis for ", med.name )
  
  
  # use average proportion mediated, taken across distribution of mh
  return( list( stats = data.frame( med.name = rep( med.name, 4 ),
                                    stat = c("prop.med", "nie.avg", "nde.avg", "te"),
                                    est = c( res$n.avg, res$d.avg, res$z.avg, res$tau.coef ),
                                    pval = c( res$n.avg.p, res$d.avg.p, res$z.avg.p, res$tau.p ),
                                    lo = c( res$n.avg.ci[1], res$d.avg.ci[1], res$z.avg.ci[1], res$tau.ci[1]),
                                    hi = c( res$n.avg.ci[2], res$d.avg.ci[2], res$z.avg.ci[2], res$tau.ci[2] ) ),
                interaction = interaction,
                med.model = med.mod,
                out.model = out.mod,
                mediate.object = res ) )
}


