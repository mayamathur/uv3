
############################### SET UP ###############################

library(readr)
library(dplyr)

rm(list=ls())

# should we prep each wave from scratch?
prep.from.scratch = FALSE

root.dir = "~/Dropbox/Personal computer/Independent studies/Uncanny Valley III (UV3)/UV3_OSF"

# location of code
code.dir = paste( root.dir, "/4_Main_Experiment/Code", sep="" )
setwd(code.dir)
source("helper_overall_analysis.R")

# disable scientific notation because causes big numbers to be treated as equal
options(scipen=999)

# real data vs. pilot?
# this matters for the number of stimuli and whether we reorder
is.real.data = TRUE


##### Name of raw data files #####

if ( is.real.data == TRUE ) {
  w1.data.name = "w1_raw_data.csv"
  w2.data.name = "w2_raw_data.csv"
  w3.data.name = "w3_raw_data.csv"
}

if ( is.real.data == FALSE ) {
  w1.data.name = "pilot_w1.csv"
  w2.data.name = "pilot_w2.csv"
  w3.data.name = "pilot_w3.csv"
}


##### Set directories #####
# these need to match the directories in data_prep.R
# location of code
setwd(root.dir)
code.dir = paste(root.dir,
                 "/4_Main_Experiment/Code",
                 sep="")

# location of specialized helper fns only used for this validation study
# and probably not useful for others
special.code.dir = code.dir

# load helper fns
setwd(code.dir)
source("general_helper_w2.R")

# other directories depend on whether we're analyzing pilot or real data
if ( is.real.data == TRUE ) {
  # location of raw Qualtrics data
  data.dir = paste(root.dir,
                   "/4_Main_Experiment/Data",
                   sep="")

  # where to save key
  key.dir = data.dir

  # where to save results
  results.dir = paste(root.dir,
                      "/4_Main_Experiment/Results",
                      sep="")
  
  valid.sites = c("Eindhoven University of Technology",
                  "Eotvos Lorand University",
                  "Ithaca College",
                  "Occidental College",
                  "University of Pennsylvania",
                  "Politecnico di Milano")
}

if ( is.real.data == FALSE ) {
  data.dir = paste(root.dir,
                   "/4_Main_Experiment/Site pilots/Data",
                   sep="")

  # where to save key
  key.dir = data.dir

  # where to save results
  results.dir = paste(root.dir,
                      "/4_Main_Experiment/Site pilots/Results",
                      sep="")

  # which site are we currently checking?
  #pilot.site = "Ithaca College"  # Brandy
  pilot.site = "University of Plymouth"
}


############################### PREP ALL THREE WAVES ###############################

if ( prep.from.scratch == TRUE ) {
  setwd(code.dir)
  source("data_prep_w1.R")
  
  setwd(code.dir)
  source("data_prep_w2.R")
  
  setwd(code.dir)
  source("data_prep_w3.R")
}


############################### MERGE THE DATASETS ###############################

setwd(data.dir)
w1 = read.csv("w1_long_data_prepped.csv")
w2 = read.csv("w2_long_data_prepped.csv")
w3 = read.csv("w3_long_data_prepped.csv")

# check for expected sample sizes
library(testthat)
expect_equal( length( unique(w1$w1_uID) ), 489 )
expect_equal( length( unique(w2$w2_uID) ), 401 )
expect_equal( length( unique(w3$w3_uID) ), 373 )


# expected rows for merged data
length( w1$w1_uID[ w1$w1_uID %in% w2$w2_uID ] )

# number of unique subjects in merged data
length( unique( w1$w1_uID[ w1$w1_uID %in% w2$w2_uID ] ) )

l = merge( w1, w2, by.x = c("w1_id1", "w1_site", "stim.name"),
           by.y = c("w2_id1", "w2_site", "stim.name") )

l = merge( l, w3, by.x = c("w1_id1", "w1_site", "stim.name"),
           by.y = c("w3_id1", "w3_site", "stim.name") )


# total merged subjects
nrow(l) / 183
# 358


############################### MERGE IN FACE MEANS FROM VALIDATION FOR ADJUSTMENT ###############################

# face-level dataset with emotion ratings (we'll adjust for this)
# also says whether face is actually human 
setwd(root.dir)
setwd("4_Main_Experiment/Materials/Qualtrics materials")
face.means = read.csv("validated_stimuli_urls_html.csv")
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


# sanity check: should always be 0 or 1 since static within face
aggregate( human ~ stim.name, l, mean)


############################### TRIAL-LEVEL EXCLUSIONS ###############################

# exclude trials with extreme values of mediators
l$exclude = "no"

med.names = c("area",
              "xdev",
              "rxnt",
              "speed",
              "xflips")

med.outliers = matrix(NA, ncol=length(med.names), nrow=nrow(l))

# mediator data very occasionally missing 
l$w1_uID[ which(is.na(l[,med.names])) ]


for ( i in 1:length(med.names) ) {
  med.outliers[,i] = is.outlier( l[[ med.names[i] ]] )
}


# proportion of each mediator that are outliers
med.df = data.frame( prop.outliers = colMeans(med.outliers, na.rm = TRUE) )
row.names(med.df) = med.names
setwd(results.dir)
write.csv( med.df,
           "prop_outliers_mediators.csv" )

# exclude trials that were outlying on any of these
# this variable is the sum of outlying mediator values for each trial
outlier.count = apply( med.outliers, 1, sum )

l$exclude[ outlier.count > 0 ] = "outlying mediator"

# for reporting in paper (proportion trials excluded): 7%
( prop.excluded.med = sum(l$exclude == "outlying mediator") / nrow(l) )
setwd(results.dir)
write.csv( prop.excluded.med,
           "prop_rows_excluded_mediator_outliers.csv" )

l = l[ l$exclude == "no", ]
dim(l)
# 55430


############################### MAKE FINAL FACE-AGGREGATED DATASET ###############################

# was face CATEGORIZED as human? (not whether it really is human)
l$cat.human = (l$cat == "Human")

# was face ACTUALLY human?
l$actually.human = l$human

# variables to summarize
med.names = c("area",
              "xdev",
              "rxnt",
              "speed",
              "xflips")
cont.vars = c(
  "actually.human",
  "cat.human",
  med.names,
  "mh",
  "lik")

static.vars = c( "mh.val",
                 "url",
                 "n.ratings",
                 "face"  # face name with "r" codes rather than in URL ordering
                 )

# fns to summarize while removing NA values
mean2 = function(x) mean(x, na.rm = TRUE)
sd2 = function(x) sd(x, na.rm = TRUE)
median2 = function(x) median(x, na.rm = TRUE)

##### Make Detailed Face-Level Dataset #####
# some variables are summarized by their continuous values...
f_cont = l %>% group_by(stim.name) %>%
  summarise_at( cont.vars, funs(mean2, sd2, median2) )

# ...while others are summarized by their first value becasue
#  static within face
f_static = l %>% group_by(stim.name) %>%
  summarise_at( static.vars, first )

f = merge(f_static, f_cont, by = "stim.name" )

# rename variable for proportion of subjects categorizing face as human
names(f)[ names(f) == "cat.human_mean2" ] = "prop.human"

# remove "2"s from names of full face dataset
library(stringr)
names(f) = str_remove( names(f), "2" )

# order by mh score in current study
f = f[ order(f$mh_mean), ]

# quick sanity check: UV in aggregated data
plot(f$mh_mean, f$lik_mean)


##### Make Simple Face-Level Dataset #####
# for mediation analyses, want same names as in long data for 
#  ease of calling fn
f2 = l %>% group_by(stim.name) %>%
  summarise_at( c(cont.vars, "mean.emot"), mean2 )
f2$lik_sd = f$lik_sd

# rename variable for proportion of subjects categorizing face as human
names(f2)[ names(f2) == "cat.human" ] = "prop.human"

# order by mh score in current study
f2 = f2[ order(f2$mh), ]

##### Add Means to Long Data #####
l = merge( l, f, by = "stim.name" )
# should look exactly like the above, just with repeated copies of each point
#plot(l$mh_mean, l$lik_mean)


############################### SUBJECT DEMOGRAPHICS ###############################

##### Browser and OS #####
table(l$w2_browser_Browser)

l$Browser = unlist( lapply( strsplit( as.character(l$w2_browser_Browser), " "),
                            FUN = function(x) x[1] ) )

l$`Operating system` = unlist( lapply( strsplit( as.character(l$w2_browser_Operating.System), " "),
                                       FUN = function(x) x[1] ) )
l$`Operating system`[ l$`Operating system` == "CrOS" ] = "Chrome OS"


##### Demographics #####

# recode sex
l$Female = rep( NA, nrow(l) )
l$Female[ l$w1_sex == "Female" ] = 1
l$Female[ l$w1_sex == "Male" ] = 0

names(l)[ names(l) == "w1_age" ] = "Age"
names(l)[ names(l) == "w1_educ" ] = "Education"

# recode race
l$`Black/African American` = grepl( "Black/African American", l$w1_race )
l$`Caucasian` = grepl( "Caucasian", l$w1_race )
l$`Native American` = grepl( "Native American", l$w1_race )
l$`East Asian` = grepl( "East Asian", l$w1_race )
l$`Hispanic` = grepl( "Hispanic", l$w1_race )
l$`Middle Eastern` = grepl( "Middle Eastern", l$w1_race )
l$`Southeast Asian` = grepl( "Southeast Asian", l$w1_race )
l$`South Asian` = grepl( "South Asian", l$w1_race )


vars = c("Browser",
         "Operating system",
         "Female",
         "Age",
         "Education",
         "Black/African American",
         "Caucasian",
         "Native American",
         "East Asian",
         "Hispanic",
         "Middle Eastern",
         "Southeast Asian",
         "South Asian")

# dataframe with first row per subject
subj.df = l %>% group_by( w1_uID ) %>% filter( row_number()==1 ) %>% select(vars)

expect_equal(nrow(subj.df), 358)

library(tableone)
table1 = CreateTableOne( data = subj.df[ , vars ] )

setwd(results.dir)

write.csv( subj.df,
           "subject_dataset.csv",
           row.names = FALSE )

write.csv( print(table1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE),
           "table1.csv")



############################### VARIABLE MANIPULATIONS ###############################

##### Mediator Quintiles #####
library(Hmisc)

for ( m in med.names ) {
  new.name = paste( m, "5", sep="" )
  f2[[new.name]] = cut2( f2[[ m ]], g=5 ) 
}


##### Center MH Score #####
l$mhc = l$mh - mean(l$mh)
f2$mhc = f2$mh - mean(f2$mh)


##### Standardize the Mediators and Emotion #####

# i.e., all variables to be used as predictors in mediation analysis
to.standardize = c("mean.emot", med.names)
f2[ , to.standardize ] = apply( f2[ , to.standardize ], 2, function(col) ( col - mean(col) ) / sd(col) )

# also make a standardized (not just centered) version of MH score for same reason
f2$mhz = f2$mhc / sd( f2$mh )

# mediator composite
f2$medsum = rowSums( f2[,med.names] )


# for ( m in med.names ) {
#   # name for centered version of mediator
#   medc.name = paste( m, "c", sep="" )
# 
#   f2[[medc.name]] = f2[[m]] - mean( f2[[m]], na.rm = TRUE )
# }
# 
# 
# # try collapsing the primary measures into a single mediator
# # sum of standardized mediators
# med.z = apply( f2[ , c("area", "xdev", "xflips") ], 2, function(col) ( col - mean(col) ) / sd(col) )
# # sanity check
# ( f2$area[1] - mean(f2$area) ) / sd(f2$area); med.z[1,1]



############################### LOOK AT MEDIATOR DISTRIBUTIONS ###############################

# in aggregated data
hist(f2$area)
hist(f2$xdev)
hist(f2$speed)
hist(f2$xflips)
hist(f2$rxnt)

# in trial-level data
hist(l$area)
hist(l$xdev)
hist(l$speed)
hist(l$xflips)
hist(l$rxnt)

# many are bimodal in the aggregated data
# everything is super skewed in the long data
# should use bootstrapping for inference



############################### WRITE DATA ###############################

# save long dataset
setwd(data.dir)
write.csv(l, "long_prepped.csv", row.names = FALSE)

# save random subsample of long dataset for testing code
ids = sample( l$w1_id1, size = 30 )
write.csv( l[ l$w1_id1 %in% ids, ],
           "long_prepped_subsample.csv", 
           row.names = FALSE )

# save face-aggregated datasets
write.csv(f, "face_aggregated_detailed.csv", row.names = FALSE)
write.csv(f2, "face_aggregated_simple.csv", row.names = FALSE)

