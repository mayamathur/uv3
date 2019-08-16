
# Contact: Maya Mathur (mmathur@stanford.edu)

############################### SET UP ###############################

# load raw, wide-format Qualtrics data
setwd(data.dir)
library(readr)
d = read_csv(w1.data.name)

library(testthat)
# does sample size match Qualtrics? (the +2 is for extra header rows)
if ( is.real.data ) expect_equal( nrow(d), 497 + 2 )

# remove rows with NA site
d = d[ !is.na(d$w1_site), ]


# number of real, non-training faces
# prior to removing the accidental duplicate face
if ( is.real.data == FALSE ) n.stim = 6
if ( is.real.data == TRUE ) n.stim = 183

# stimulus names
stim.names = paste( "face.", 1:n.stim, sep="" )



# remove non-finishing subjects
exclude = rep("no", dim(d)[1])

# exclude people who did not reach the last demographic question
#  because stopped taking survey or failed attention check
exclude[ d$race == "" ] = "didnt.finish"
d = d[ exclude == "no", ]
# no subjects excluded here

# only for pilot data
if ( is.real.data == FALSE ) {
  # pretend there were only 6 faces because otherwise there are NAs and it becomes confused
  # grab columns with radio button decisions for each LM stimulus in nonrandom order
  # the "_" is because Qualtrics names the variables "1_cat", etc.
  cols = grep( "_mh", names(d) )
  cols2 = cols[1:n.stim]
  drop = cols[ !cols %in% cols2 ]
  d = d[ , -drop ]
  # this means the data are basically fake
}


# remove superfluous header rows
d = d[ -c(1:2), ]

if ( is.real.data == TRUE ) {
  d = d[ d$w1_site %in% valid.sites, ]
}

if ( is.real.data == FALSE ) {
  d = d[ d$w1_site %in% pilot.site, ]
}

# 495 subjects
nrow(d)



############################### HANDLE IDIOSYNCRASIES ###############################

# make unique subject-site ID
d$w1_uID = paste( d$w1_site, d$w1_id1, sep=" ") 


d = fix_site_idiosyncrasies( dat = d,
                             wave.num = 1 )

nrow(d)
# 491


############################### RESHAPE WIDE TO LONG ###############################

( mh.names = names(d)[ grep( "_mh", names(d) ) ] )


# no more tibble or else reshape can't handle it
d = as.data.frame(d)
# reshape wide to long
# https://stackoverflow.com/questions/12466493/reshaping-multiple-sets-of-measurement-columns-wide-format-into-single-columns
library(car)
l = reshape( d, varying = list(
  MH = mh.names
),
v.names=c(
  "mh"
),
idvar = "w1_uID",
times = stim.names,
direction="long" )

names(l)[ names(l) == "time" ] = "stim.name"

expect_equal( nrow(d) * n.stim, nrow(l) )


############################### REMOVE ACCIDENTAL DUPLICATE FACE ###############################

# remove face 156 (accidental duplicate image)
l = l[ l$stim.name != "face.156", ]


############################### SAVE LONG DATA ###############################

setwd(data.dir)
write.csv(l, "w1_long_data_prepped.csv")



