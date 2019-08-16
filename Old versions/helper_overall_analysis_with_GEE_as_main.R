


mh_vs_mediator = function( med.name,
                           use.lmer = TRUE ) {
  
  #browser()
  
  # ~~~ NOTE THAT WE'RE FITTING LINEAR LINK FN. EVEN FOR XFLIPS RIGHT NOW
  #if ( med.name != "xflips" ) {
  confusion.lmer.adj = lmer( l[[med.name]] ~ mh + I(mh^2) + I(mh^3) +
                               mean.emot + (1|stim.name),
                             data = l )
  
  confusion.gee.adj = gee( l[[med.name]] ~ mh + I(mh^2) + I(mh^3) +
                         mean.emot,
                       id = as.factor(l$stim.name), 
                       corstr="exchangeable",
                       data = l )
  
  agg.med.name = paste(med.name, "mean", sep="_")
  confusion.lm.agg.adj = lm( f[[agg.med.name]] ~ mh_mean + I(mh_mean^2) + I(mh_mean^3) +
                           emot.mean_mean,
                         data = f )
  
  #model.type = "lmer"
  
  #}
  
  
  # fitted_y( x = 90, model = confusion.lmer.adj, model.type = "lmer" )
  # fitted_y( x = 90, model = confusion.lmer.adj, model.type = "lmer" )
  # fitted_y( x = 90, model = confusion.lm.adj, model.type = "lm" )
  
  # find the MH score with max confusion
  # from lmer
  if ( use.lmer == TRUE ) {
    mh.max = optimize( function(x) fitted_y(x = x, model = confusion.lmer.adj, model.type = "lmer"),
                       interval=c(-100, 100), maximum=TRUE)$maximum
  } else {
    # from lm
    mh.max = optimize( function(x) fitted_y(x = x, model = confusion.lm.agg.adj, model.type = "lm"),
                       interval=c(-100, 100), maximum=TRUE)$maximum
  }
  
  
  ylab = med.name
  
  plot = ggplot( l, aes( x=mh, y=l[[med.name]] ) ) +
    # geom_hline( yintercept=.50, color="gray", lwd=.6 ) +  # reference line for 50% categorization
    geom_vline( aes( xintercept=mh.max, color="MH with max confusion" ), lwd=.6) +  # reference line for 50% categorization
    
    theme_classic() +
    
    geom_smooth( aes( color="GAM, unadjusted" ) ) +  
    
    stat_function( fun = function(x) fitted_y( x = x, model = confusion.lm.agg.adj, model.type = "lm" ),
                   aes( color="LM, agg, emotion-adjusted" ),
                   lwd=1, lty = 2) +  # emotion-adjusted regression curve +
    
    stat_function( fun = function(x) fitted_y( x = x, model = confusion.gee.adj, model.type = "lm" ),
                   aes( color="GEE, emotion-adjusted" ),
                   lwd=1, lty = 2 )  + # emotion-adjusted regression curve
    
    xlab(xlab) + ylab(ylab) +
    scale_x_continuous(breaks=seq(-100, 100, 25)) +
    
    theme(text = element_text(size=20) )
  
  if ( use.lmer == TRUE ) {
    plot = plot + stat_function( fun = function(x) fitted_y( x = x, model = confusion.lmer.adj, model.type = "lmer" ),
                                 aes( color="LMER, emotion-adjusted" ),
                                 lwd=1, lty = 2 )  # emotion-adjusted regression curve
  }
  
  return(plot)
}


# not controlling for MH score
mediator_vs_lik = function( med.name ) {
  
  l$med = l[[med.name]]
  

  lik.gee.adj = gee( l[[med.name]] ~ med + I(med^2) + I(med^3) +
                             mean.emot,
                           id = as.factor(l$stim.name), 
                           corstr="exchangeable",
                           data = l )
  
  agg.med.name = paste(med.name, "mean", sep="_")
  lik.lm.agg.adj = lm( lik_mean ~ f[[agg.med.name]] + I(f[[agg.med.name]]^2) + I(f[[agg.med.name]]^3) +
                               emot.mean_mean,
                             data = f )
  

  plot = ggplot( l, aes( x=l[[med.name]], y=lik ) ) +
 
    theme_classic() +
    
    geom_smooth( aes( color="GAM, unadjusted" ) ) +  
    
    stat_function( fun = function(x) fitted_y( x = x, model = lik.lm.agg.adj, model.type = "lm" ),
                   aes( color="LM, agg, emotion-adjusted" ),
                   lwd=1, lty = 2) +  # emotion-adjusted regression curve +
    
    stat_function( fun = function(x) fitted_y( x = x, model = lik.gee.adj, model.type = "lm" ),
                   aes( color="GEE, emotion-adjusted" ),
                   lwd=1, lty = 2 )  + # emotion-adjusted regression curve
    
    xlab(med.name) + ylab("Likability") +
    scale_x_continuous(breaks=seq(-100, 100, 25)) +
    
    theme(text = element_text(size=20) )
  

  return(plot)
}


# x represents an MH score
# model.type = "marginal" works for GEE or LM
fitted_y = Vectorize( function( x, model, model.type ) {
  
 
  if ( model.type == "lmer" ) b = fixef(model)
  if ( model.type != "lmer" ) b = coef(model)
  
  # figure out how many parameters are for mh and its
  #  higher-order terms
  # then add 1 for intercept
  # ignore any terms for mean.emot because evaluating at mean
  p = sum( grepl("mh", names(b)) ) + 1
  
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


# fitted_y( x = 0, model = best.mod, model.type = "lmer" )
# fitted_y( x = 0, model = poly3.lm.adj, model.type = "lm" )







# model: GAM or lmer

# ~~~ NEED TO UPDATE THIS SO IT ACCOUNTS FOR CORRELATION IN ALL THE GAM MODELS, 
# NOT JUST 
med_analysis = function( sims = 100,
                         med.name,
                         boot = TRUE,
                         med.model,
                         out.model,
                         data = l ) {
  
  # # bookmark
  # # TEST ONLY
  # med.name = "xflips"
  # boot = FALSE
  # sims = 100
  # med.model = "lmer"
  # out.model = "lmer"
  # data = l
  
  # just in case it's some kind of godawful tibble
  data = as.data.frame(data)

  cat("\nStarting mediation analysis for ", med.name )
  
  # all mediators are of type "positive" except the count one
  med.type = "positive"
  if ( med.name == "xflips" ) med.type = "count"

  # to avoid confusing the mediation package, need to make a variable
  #  for the mediator
  data$med = data[[med.name]]
  
  # https://cran.r-project.org/web/packages/mediation/vignettes/mediation-old.pdf
  library(mgcv)
  library(lme4)
  
  # mediation package doesn't handle NB, so have to use Poisson
  family = "gaussian"
  if ( med.name == "xflips" ) family = "poisson"
  
  ##### Fit mediator model #####
  if ( med.model == "gam" ) {
    med = gam( med ~ s(mh, bs="re") + mean.emot,
               data = data,
               family = family)
  }
  

  if ( med.type == "positive" & med.model == "lmer" ) {
    
    # throughout the lmers, had to remove subject random intercept to
    #  avoid the error: "mediate does not support more than two levels per model"
    med = lmer( med ~ mh + I(mh^2) + I(mh^3) + 
                   mean.emot + (1 | stim.name),
                 data = data )
  }
  
  if ( med.type == "count" & med.model == "lmer") {
    
    # THIS GETS A WEIRD ERROR
    # I THINK IT WANTS ME TO RESCALE THE PREDICTORS
    # ~~~ BOOKMARK
    med = glmer( med ~ mh + I(mh^2) + I(mh^3) + 
                   mean.emot + (1 | stim.name),
                 data = data,
                 family = "poisson")
  }
  
  if ( med.type == "positive" & med.model == "lm" ) {
    med = lm( med ~ mh + I(mh^2) + I(mh^3) + 
                  mean.emot,
                data = data )
  }
  
  if ( med.type == "count" & med.model == "lm") {

    med = glm( med ~ mh + I(mh^2) + I(mh^3) + 
                   mean.emot,
                 data = data,
                 family = "poisson")
  }
  
  
  ##### Fit outcome model #####
  if ( out.model == "gam" ) {
    # fit both no-interaction and interaction models
    out.small = gam( lik ~ s(med, bs="re", k=8) +
                       s(mh, bs="re") + mean.emot,
                     data = data )
    
    out.big = gam( lik ~ s(med, bs="re", k=8) +
                     s(mh, bs="re") +
                     # tensor product of bases representing the two variables
                     te( med, mh) + mean.emot,
                   data = data )
  }
  
  if ( out.model == "lmer" ) {
    
    out.small = lmer( lik ~ mh + I(mh^2) + I(mh^3) + 
                        med + I(med^2) + I(med^3) +
                  mean.emot + (1 | stim.name),
                data = data )
    
    # note that this obviously isn't all possible nonlinear interactions
    out.big = lmer( lik ~ mh + I(mh^2) + I(mh^3) + 
                        med + I(med^2) + I(med^3) +
                        med * mh +
                        mean.emot + (1 | stim.name),
                      data = data )
  }
  
  
  if ( out.model == "lm" ) {
    
    out.small = lm( lik ~ mh + I(mh^2) + I(mh^3) + 
                        med + I(med^2) + I(med^3) +
                        mean.emot,
                      data = data )
    
    # note that this obviously isn't all possible nonlinear interactions
    out.big = lm( lik ~ mh + I(mh^2) + I(mh^3) + 
                      med + I(med^2) + I(med^3) +
                      med * mh +
                      mean.emot,
                    data = data )
  }
  

  
  # pick the one with smaller AIC
  if ( AIC(out.small) < AIC(out.big) ) {
    out = out.small
    interaction = "no"  # record ultimate decision
  } else {
    out = out.big
    interaction = "yes"
  }
  
  
  # # get fitted values to check model fit
  # .d$med.pred = fitted.values(med)
  # .d$out.pred = fitted.values(out)

  # # FAKE TEST
  # med = lm( med ~ mh, data = data )
  # out = lm( xflips ~ med + mh, data = data )
  # res = mediate( model.m = med,
  #          model.y = out,
  #          treat="mh",
  #          mediator="med"
  # )
  
  library(mediation)
  res = mediate( model.m = med,
                 model.y = out,
                 boot=boot,
                 sims=sims,
                 treat="mh",
                 mediator="med"
                 )
  
  cat("\nSurvived mediation analysis for ", med.name )
  
  
  # use average proportion mediated, taken across distribution of mh
  return( list( stats = data.frame( med.name = rep( med.name, 3 ),
                                       stat = c("prop.med", "nie.avg", "nde.avg"),
                                       est = c( res$n.avg, res$d.avg, res$z.avg ),
                                       pval = c( res$n.avg.p, res$d.avg.p, res$z.avg.p),
                                       lo = c( res$n.avg.ci[1], res$d.avg.ci[1], res$z.avg.ci[1]),
                                       hi = c( res$n.avg.ci[2], res$d.avg.ci[2], res$z.avg.ci[2]) ),
                interaction = interaction,
                med.model = med,
                out.model = out,
                mediate.object = res ) )
}




is.outlier = function(x) {
  summ = summary(x)
  Q1 = summ["1st Qu."]
  Q3 = summ["3rd Qu."]
  IQR = Q3 - Q1
  lo = Q1 - 1.5 * IQR
  hi = Q3 + 1.5 * IQR
  
  return( x < lo | x > hi )
}