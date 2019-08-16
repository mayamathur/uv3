
# Contact: Maya Mathur (mmathur@stanford.edu)

############################### SET UP ###############################

# load raw, wide-format Qualtrics data
setwd(data.dir)
library(readr)
d = read_csv(w3.data.name)

# from Qualtrics (the +2 is for extra header rows)
library(testthat)
expect_equal( nrow(d), 383 + 2 )

# remove rows with NA site
d = d[ !is.na(d$w3_site), ]


# number of real, non-training faces
if ( is.real.data == FALSE ) n.stim = 6
if ( is.real.data == TRUE ) n.stim = 183

# stimulus names
stim.names = paste( "face.", 1:n.stim, sep="" )


# only for pilot data
if ( is.real.data == FALSE ) {
  # pretend there were only 6 faces because otherwise there are NAs and it becomes confused
  # grab columns with radio button decisions for each LM stimulus in nonrandom order
  # the "_" is because Qualtrics names the variables "1_cat", etc.
  cols = grep( "_lik", names(d) )
  cols2 = cols[1:n.stim]
  drop = cols[ !cols %in% cols2 ]
  d = d[ , -drop ]
  # this means the data are basically fake
}

# remove superfluous header rows
d = d[ -c(1:2), ]


if ( is.real.data == TRUE ) {
  d = d[ d$w3_site %in% valid.sites, ]
}

if ( is.real.data == FALSE ) {
  d = d[ d$w3_site %in% pilot.site, ]
}

nrow(d) # 378


############################### HANDLE IDIOSYNCRASIES ###############################

# make unique subject-site ID
d$w3_uID = paste( d$w3_site, d$w3_id1, sep=" ") 

d = fix_site_idiosyncrasies( dat = d,
                                wave.num = 3 )

nrow(d)  # 373



############################### RESHAPE WIDE TO LONG ###############################

( lik.names = names(d)[ grep( "_lik", names(d) ) ] )

d = as.data.frame(d)

# reshape wide to long
# https://stackoverflow.com/questions/12466493/reshaping-multiple-sets-of-measurement-columns-wide-format-into-single-columns
library(car)
l = reshape( d, varying = list(
  Lik = lik.names
),
v.names=c(
  "lik"
),
idvar = "w3_uID",
times = stim.names,
direction="long" )

names(l)[ names(l) == "time" ] = "stim.name"

expect_equal( nrow(l), nrow(d) * n.stim )


############################### REMOVE ACCIDENTAL DUPLICATE FACE ###############################

# remove face 156 (accidental duplicate image)
l = l[ l$stim.name != "face.156", ]


############################### SAVE LONG DATA ###############################

setwd(data.dir)
write.csv(l, "w3_long_data_prepped.csv")

