################################################################################

# Task assigned from the 7/18 check-in call with the SAS:

# Mann-Kendall tests (monotonic trends) for all the accepted indices--For this,
# I think I want to do each time series, because it's difficult to detect much
# change over abbreviated periods--more ideal to have >10yr series, but we deal
# with what we got.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### REMINDER: when we chose to pick the nominal index,
### Fixed sites == delta method
### Random sites == stratified (arithmetic)
### OTHERWISE, run with the index selected...pretty rare occurrence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(trend) # for MKtest
library(dplyr) # for dataframe manipulations

# Date last modified: 8/15/2023


Full<-read.csv("//dnrnas/Bonneau/Userdata/HoffmanKy/SCDNR/SAS-herring/2023RiverHerringIndices_07-21-23.csv")
# At home, this is under Index/final.scripts_indices
Full<-X2023RiverHerringIndices_07_21_23

summary(Full)
# Somewhat interesting to see as a whole--the earliest survey we have starts in 
# 1959 (pretty wild). I think, because these are all output data anyway, I should
# just be able to set the "ForModelingUse" column as a factor, and then I can 
# select the ones (i.e., the ones we use for further modeling), and then I can 
# filter by unique Data_Source and by Region

Full$ForModelingUse<-as.factor(Full$ForModelingUse)
unique(Full$ForModelingUse) # 0 , 1
is.factor(Full$ForModelingUse) # Check

Full$Life.Stage<-as.factor(Full$`Life Stage`)
unique(Full$Life.Stage)

Mods <- filter(Full, ForModelingUse==1)
# cuts the ones we aren't using

unique(Mods$SurveyID)
# So, I have what appears to be 81 different surveys that are to be used...
# Maybe I'll just start by filtering the regions out?

unique(Mods$Region)
# ""        "MAT"     "SAT"     "NNE"     "CAN-NNE" "MNE"     "SNE"   
# Well, a couple fall in the "no region" category, but I can at least start with
# all the rest...

SAT <- filter(Mods, Region=="SAT")
# cool--just those three surveys..a couple more filtering steps..

St.Johns <- filter(SAT, System=="St Johns")
FLpushnet_FINAL <- filter(St.Johns, Life.Stage=="YOY")
FLelectro_FINAL <- filter(St.Johns, Life.Stage=="Adult")

Cape.Fear <- filter(SAT, System=="Cape Fear")

#------------------------------------------------------------------------------
# First attempt, with FL-pushnet index I created:

FL<-FLpushnet_FINAL

summary(FL)
# Looks pretty straightforward at this point..just annual values

plot(Index~Year, data=FL)
# quick and dirty plot here to see what we're looking at...
# I already have data aggregated per year (as means), so there's really no 
# reason to go any further with it.

mk.test(FL$Index)
# Tau comes back as a moderate increase (0.47) over the series, and this is
# a significant trend (p = 0.01). SO, annual YOY catch rates have increased
# in the St. John's River from 2006-present.


# HOW about the adult EF survey from the St. John's?

FL.2<-FLelectro_FINAL

summary(FL.2)
# Again, not much to see here...2 yr shorter time series

plot(Index~Year, data=FL.2)
# not as definitive a trend here--looks more like "no change"

mk.test(FL.2$Index)
# Tau depicts a very small, negative trend. This is not significant (p=0.83), so
# my thought is this confirms the "no change" in annual adult catch rates in the
# St. John's River.

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
### THIS IS FROM A DIFFERENT SOURCE ###
# Santee Basin YOY electrofishing: 
# (NOT SURE if the SAS wants this, considering I JUST worked it out (July '23))

SC.2<-SCjuve_EF_FINAL %>%
  select(!c(1))
# This one resulted in using the delta method as well, so I'll use those #'s

summary(SC.2)
# I doubt we will see much change here--only a 5 year time series, which we will
# probably end up dropping just for that reason

plot(Delta.mean~Year, data=SC.2)
# Kind of lackluster really--'15,'18, and '21 look to be "boom" years, but all 
# others are lower

mk.test(SC.2$Delta.mean)
# Slight downward trend (probably from those "boom years") but a non-significant
# test result--looks to be another case where we need more years worth of data
# to say anything definitively
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Cape Fear adults (electrofishing survey):

NC<-Cape.Fear

summary(NC)
# Just missing one year (2016) in the series, but otherwise same old.

plot(Index~Year, data=NC)
# Kind of all over the place, but I'd say largely an increasing trend from start
# to the end of the series..

mk.test(NC$Index)
# Well, we have a non-significant trend detected (T = 0.2, mod. pos. increase),
# so not much more we can do...although, I wonder if 2016 is somewhat damning..

# I went in and manually changed that 2016-zero value to 0.5 (lower than the
# lowest obs value in the series), and nothing changed for the mann-kendall 
# output. SO, I guess this is an instance where we need more data to say anything
# definitively changed.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~END SAT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


                          # START MAT #

MAT <- filter(Mods, Region=="MAT")
# a lot more going on here...how about filters by waterbody?

unique(MAT$Waterbody)
# a little rough to start filtering here (n=19), but maybe by the system?

unique(MAT$System)
# [1] "Connecticut River" "Chesapeake Bay"    "Delaware Bay"      "Albermarle Sound"  "Nuese River"      
# [6] "Chowan River"      "Unkown"            "Hudson River"      "James River"       "York River"

# a little better here..I think I'll keep going South-to-North so I don't get
# too confused---Neuse, Albermarle, Chowan (in that order)


      # North Carolina #

Neuse <- filter(MAT, System=="Nuese River")
Albermarle <- filter(MAT, System=="Albermarle Sound")
Chowan <- filter(MAT, System=="Chowan River")

NC.2<-Neuse

summary(NC.2)
# good news is this is just a single survey (all SurveyID_160), so no more 
# filtering required here

plot(Index~Year, data=NC.2)
# looks like a fairly definitive trend going upward from 2004-present..particularly
# after ~2010

mk.test(NC.2$Index)
# Good news--in this case, we have a significant trend with a moderate positive
# increase (p=0.0007, Tau=0.59), so adult abundance has been increasing over the
# time series

summary(Albermarle)
# Need some more filtering steps here--multiple surveys appear to make up this
# dataframe

unique(Albermarle$Species)
# good enough place to start here because we have repeated surveyID's per species

Al.BLH <- filter(Albermarle, Species=="Blueback")
# surveys: 34, 35, 41, & 43

Al.Ale <- filter(Albermarle, Species=="Alewife")
# surveys: 32, 37, & 39

# SO:
# surveys 32 + 34 are the 2.5/3.0 mesh IGNS--Adults
# surveys 35 + 37 are seine surveys (@ "core 11 stations")--YOY
# surveys 39 + 41 are trawl surveys--Adults
# survey 43 is a spawning stock survey--Adults (BLH only)

#---------------------- Gillnet survey ----------------------------------------#
NC.3 <- filter(Al.Ale, SurveyID=="32")

summary(NC.3)
# 1991-2019, no missing values

plot(Index~Year, data=NC.3)
# looks like a pretty significant upward trend in the abundance of these adults
# (that I think are gillnet caught in 2.5/3.0 inch mesh)

mk.test(NC.3$Index)
# This may be the biggest increase that I've seen so far in these data--fairly
# substantial (Tau=0.63, p=0.000002)--good increase in abundance for these Ale

NC.3.cut<-filter(NC.3, Year>="2009") 
# because this increasing trend looks well defined in the 2000's
mk.test(NC.3.cut$Index)

# SAME survey, what about herring?

NC.4 <- filter(Al.BLH, SurveyID=="34")

summary(NC.4)
# 1991-2019, no missing values

plot(Index~Year, data=NC.4)
# more like a "shotgun blast" graphic here--see what mk thinks..

mk.test(NC.4$Index)
# well, just as we thought, a non-significant test result with a very slight
# declining trend (Tau=-0.07, p=0.59)

# This is more of a "U"-shaped series though--seems like the trend is at it's 
# lowest right around 2012, but has only gone up in recent years..

NC.4.cut <- filter(NC.4, Year>"2008")

plot(Index~Year, data=NC.4.cut)
# definitely a different picture here--drastic increase graphically

mk.test(NC.4.cut$Index)
# Now, there's a significant moderate increase detected (Tau=0.64, p=0.008)

#---------------------------- Seine survey ------------------------------------#

NC.5 <- filter(Al.Ale, SurveyID=="37")

summary(NC.5)
# 1972-present, only a couple missing values for the CV--irrelevant at this point

plot(Index~Year, data=NC.5)
# not much to see here (I don't believe)

mk.test(NC.5$Index)
# slight declining trend in YOY-alewife abundance, but not significant
# no real indication to cut down to that modern period with some zero years and
# some peak years..


NC.6 <- filter(Al.BLH, SurveyID=="35")

summary(NC.6)
# 1972-present, basically no missing values again

plot(Index~Year, data=NC.6)
# not a whole lot going on in recent years, but sig. decline from those really
# early records (prior to mid-80's, index values > 100!)

mk.test(NC.6$Index)
# Yep, significant mod. decline detected (Tau=-0.38, p=0.00009) so good evidence
# to suggest YOY abundance is historically low...I wonder in modern years though...

NC.6.cut <- filter(NC.6, Year>"2008")

plot(Index~Year, data=NC.6.cut)
# a little bit different of a picture here when you cut off those early 
# (extremely high) mean catch records.

mk.test(NC.6.cut$Index)
# Now, you have a slightly downward trend that is non-significant. So, my
# interpretation of that would be no real change in YOY abundance here since
# about 1985, but historically lower than what WAS

#-------------------------- Trawl survey --------------------------------------#

NC.7 <- filter(Al.Ale, SurveyID=="39")

summary(NC.7)
# a couple missing CV values, but otherwise good. 1982-present

plot(Index~Year, data=NC.7)
# just kind of looks like "boom" years mixed in with "bust" years..

mk.test(NC.7$Index)
# Yep, non-significant trend (tau=-0.07) that is slightly declining.

NC.8 <- filter(Al.BLH, SurveyID=="41")

summary(NC.8)
# same thing as for alewife--1982 to the present

plot(Index~Year, data=NC.8)
# looks like all the "good years" happened prior to 2010..

mk.test(NC.8$Index)
# a moderate decrease in adult BLH abundance was detected from this survey 
# (tau= -0.29, p=0.01)

# How about a cut towards those more recent years?
NC.8.cut <- filter(NC.8, Year>"2008")

plot(Index~Year, data=NC.8.cut)
# This just really looks like near zero catch for the herring in this survey
# over the past 13 years or so..

mk.test(NC.8.cut$Index)
# mann kendall seems to support this--no change in the modern period, but 
# definitely declined from the historic standards..

#----------------- Spawning Stock (BLH) survey --------------------------------#

NC.9 <- filter(Al.BLH, SurveyID=="43")

summary(NC.9)
# no missing values, data from 2012-present

plot(Index~Year, data=NC.9)
# looks like an increasing trend to me..

mk.test(NC.9$Index)
# this one is non-significant (p=0.11), but is a mod. increasing trend (tau=0.42)
# perhaps we just need more years of data to say something more definitive?

#------------------------------------------------------------------------------#

        # Chowan River, electrofishing survey (adults) #

summary(Chowan)
# looks like we just have two surveys, one for each species caught.

NC.10 <- filter(Chowan, SurveyID=="117")

summary(NC.10)
# 2006-2022, no missing data

plot(Index~Year, data=NC.10)
# looks like a fairly definitive positive trend here

mk.test(NC.10$Index)
# yep, a moderate increase in adult abundance is detected (tau=0.49, p=0.007)


NC.11 <- filter(Chowan, SurveyID=="119")

summary(NC.11)
# same as the other-- 2006-2022

plot(Index~Year, data=NC.11)
# does not look as definitive as the alewife data, but the scale is completely
# different, so that may be a decent increase after all

mk.test(NC.11$Index)
# yep, same thing as the alewife. Mod. increase in adult abundance was detected
# (tau=0.35, p=0.053)--I guess you could consider this a marginal result, but
# this trend is quite apparent 2010-present

#------------------------------------------------------------------------------#

      # Virginia #

# So, after a bit of investigating the data, VA has:
# Appomattox and Chickahominy, along with James River, proper
# The Mattaponi (York River)
# The Rappahannock
# The Potomac
# AND also the VIMS_NEAMAP survey 
# (basically the Chesapeake, but specific locations undetermined)

# The weird bit is that these are a mix of Waterbody and System..so I have to be
# careful to separate out each dataframe (don't miss any)

Appomattox <- filter(MAT, Waterbody=="Appomattox River")
# Should include SurveyIDs: 156, 157

Chickahominy <- filter(MAT, Waterbody=="Chickahominy River")
# should include SurveyIDs: 45, 52, 158, 159

James <- filter(MAT, Waterbody=="James River")
# should have #65, 69, 73, 74

Mattaponi <- filter(MAT, System=="York River")
# surveys 168 and 169

Rappahannock <- filter(MAT, Waterbody=="Rappahannock River")
# surveys 107, 111, 112, 113

NEAMAP.1 <-filter(Mods, SurveyID=="17")
NEAMAP.2 <-filter(Mods, SurveyID=="18")
# The VIMS-NEAMAP survey---for ALE and BLH


### several filtering steps are required after these to get the individual surveys,
### but this is a good start..


          # Appomattox River #

VA.1<-filter(Appomattox, SurveyID=="156")
# Alewife 1995-2018

summary(VA.1)
# looks okay: 18 years of data (all adults)

plot(Index~Year, data=VA.1)
# might indicate a slight increase in abundance here...a little tough to call

mk.test(VA.1$Index)
# A non-significant, moderate increase in adult abundance is indicated here
# (tau=0.29, p=0.103)--"no change" discernable

# what about for the "modern era"?

VA.1.cut<-filter(VA.1, Year>="2009")
mk.test(VA.1.cut$Index)
# Same general result when cutting down to these recent years..no discernable
# trend

VA.2<-filter(Appomattox, SurveyID=="157")

summary(VA.2)
# BLH, 1995-2016--nothing crazy looking

plot(Index~Year, data=VA.2)
# looks like an overall increasing trend..

mk.test(VA.2$Index)
# well, this one too is non-significant (tau=0.11, p=0.54), and I can't really
# cut this down because the survey ended in 2016--tried cutting down to 2005, and
# got the same result anyway (non-significant, moderate pos. increase)


        # Chickahominy River #

VA.3<-filter(Chickahominy, SurveyID=="158")

summary(VA.3)
# All Alewife from 2000-2020

plot(Index~Year, data=VA.3)
# an upward trend exists, but TBD...it looks like they did not consecutively 
# perform this survey until 2011, so I guess I'll just cut to that year for
# comparison with the full data..
    # unique(VA.3$Year)
  # 2000 2001 2007 2008 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020

mk.test(VA.3$Index)
# yep, another non-significant test result (tau= -0.033, p=0.91)

VA.3.cut<-filter(VA.3, Year>="2010")
mk.test(VA.3.cut$Index)

VA.4<-filter(Chickahominy, SurveyID=="159")

summary(VA.4)
# all BLH from 2000-2020

plot(Index~Year, data=VA.4)
# a more distinct trend here...but once again, looks like data wasn't continuous
# until 2011...
# unique(VA.4$Year)
# 2000 2001 2008 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020

mk.test(VA.4$Index)
# a definitive increase in adult BLH abundance is indicated (tau=0.72, p=0.0008)

VA.4.cut<-filter(VA.4, Year>="2010")
mk.test(VA.4.cut$Index) 
# basically the same result here--I'll defer to the rest of the SAS on whether
# to go with the complete series (2011-2020) or all data since 2000

VA.5<-filter(Chickahominy, SurveyID=="45")

summary(VA.5)
# YOY-Alewife series; 2014-2022

plot(Index~Year, data=VA.5)
# doesn't look like much of a trend..

mk.test(VA.5$Index)
# Non-significant mod. declining trend here (tau= -0.33, p=0.25)

VA.6<-filter(Chickahominy, SurveyID=="52")

summary(VA.6)
# YOY-Blueback series; 2014-2022

plot(Index~Year, data=VA.6)
# not great, but not sure if we have enough data to say there's a trend

mk.test(VA.6$Index)
# same as the alewife trend above (tau = -0.22, p=0.47) non-sig mod. decline



# James River #

VA.7<-filter(James, SurveyID=="65")

summary(VA.7)
# looks like 19 years of adult alewife catch, all from survey 65

plot(Index~Year, data=VA.7)
# maybe a positive trend from 2000-2020, but a little hard to discern

mk.test(VA.7$Index)
# no real trend detected (tau=0.059, p=0.75)

VA.7.cut<-filter(VA.7, Year>="2008")

mk.test(VA.7.cut$Index)
# Same result--no trend, so I'll just roll with the full data version


VA.8<-filter(James, SurveyID=="69")

summary(VA.8)
# only 7 years of data--not sure how this one got through..

plot(Index~Year, data=VA.8)
# looks like no distinct trend at all, really

mk.test(VA.8$Index)
# yeah..tau is 0.05 with p-value (1)..no info at all


VA.9<-filter(James, SurveyID=="73")
# this one looks like the blueback herring listing from the same exact survey
# 2015-2021

plot(Index~Year, data=VA.9)
# kind of looks like no trend again..

mk.test(VA.9$Index)
# tau=0.14 at pval=0.76--so no trend


VA.10<-filter(James, SurveyID=="74")

summary(VA.10)
# wow..a 50 year survey..that's nice!

plot(Index~Year, data=VA.10)
# looks like we have a gap in the data around the late 70's, but otherwise 
# sampling looks consistent

mk.test(VA.10$Index)
# non-significant trend here, but I'll cut this down to the modern era to see
# if there are any other things to be seen (tau=0.16, p=0.11)

VA.10.cut<-filter(VA.10, Year>=2009)

plot(Index~Year, data=VA.10.cut)
# doesn't look like too much change overall..

mk.test(VA.10.cut$Index)
# nothing significant here (tau=0.16, p=0.44)


VA.11<-filter(Mattaponi, SurveyID=="168")

summary(VA.11)
# adult alewife survey from 2000-2014

plot(Index~Year, data=VA.11)
# looks like we have a few missing years in the series, but I'll try to run
# without any manipulation

mk.test(VA.11$Index)
# non-significant trend here as well (tau=-0.36, p=0.11)


VA.12<-filter(Mattaponi, SurveyID=="169")

summary(VA.12)
# the blueback version of this same survey..

plot(Index~Year, data=VA.12)
# pretty similar to the last...not sure there is much trend here

mk.test(VA.12$Index)
# yeah, another non-significant trend (slightly positive..tau=0.03, p=1)

VA.13<-filter(Rappahannock, SurveyID=="107")

summary(VA.13)

plot(Index~Year, data=VA.13)
# not much of a trend, visually

mk.test(VA.13$Index)
# yep, tau=-0.08 at pval=0.69--no change

VA.14<-filter(Rappahannock, SurveyID=="111")

summary(VA.14)
# 2000-2020 data here; adult alewife CPUE

plot(Index~Year, data=VA.14)
# looks like data haven't changed much over the period...some years better or worse
# than others, but that's about it..

mk.test(VA.14$Index)
# yeah, not much to be seen here...tau=0.08 at p=0.61
# maybe I'll try for a cutoff around 2009

VA.14.cut<-filter(VA.14, Year>="2009")

plot(Index~Year, data=VA.14.cut)

mk.test(VA.14.cut$Index)
# closer, but really the same result; tau=0.24, p=0.30


VA.15<-filter(Rappahannock, SurveyID=="112")
summary(VA.15)
# 2000-2020

plot(Index~Year, data=VA.15)
# now that looks like an increasing trend

mk.test(VA.15$Index)
# sure enough, a moderate increase; tau=0.47, p=0.003

VA.15.cut<-filter(VA.15, Year>="2009")

plot(Index~Year, data=VA.15.cut)

mk.test(VA.15.cut$Index)
# an even greater increase is observed with the cut data--happy to go with whichever
# the SAS wants

VA.16<-filter(Rappahannock, SurveyID=="113")

summary(VA.16)
# sheesh, 50 years of data here--1967-2022

plot(Index~Year, data=VA.16)
# looks like a couple years missing in late 70's, but with all the other data, 
# that might be a moot point

mk.test(VA.16$Index)
# No change again; tau=0.15, p=0.12

VA.16.cut<-filter(VA.16, Year>="2009")

plot(Index~Year, data=VA.16.cut)

mk.test(VA.16.cut$Index)
# no change from the full data--tau=0.03, p=0.91, so I'll run with full data

   # NEAMAP surveys #

summary(NEAMAP.1)
# 2008-present--undetermined life stage caught

plot(Index~Year, data=NEAMAP.1)
# doesn't look like much..

mk.test(NEAMAP.1$Index)
# Tau=0.066, p=0.78 so no discernable trend--I wouldn't cut the data down either
# because the period is relatively short too.

summary(NEAMAP.2)
# same thing

plot(Index~Year, data=NEAMAP.2)
# same really...doesn't look like much of a trend here, just shows some "peak years"

mk.test(NEAMAP.2$Index)
# yeah: tau=-0.077, p=0.74 so no trend detected

#------------------------------------------------------------------------------#

      # Maryland #

Potomac <- filter(MAT, Waterbody=="Potomac River")
unique(Potomac$SurveyID)
# surveys 101 + 103

Nanticoke <- filter(MAT, Waterbody=="Nanticoke")
unique(Nanticoke$SurveyID)
# surveys 80, 81, 82, 85, 89, and 91

Choptank <- filter(MAT, Waterbody=="Choptank River")
unique(Choptank$SurveyID)
# surveys 191 and 192

NorthEast <- filter(MAT, Waterbody=="North East River")
unique(NorthEast$SurveyID)
# surveys 93 and 96

HOB <- filter(MAT, Waterbody=="Head of Bay")
unique(HOB$SurveyID)
# surveys 57 and 61


      # Potomac River #

MD.1<-filter(Potomac, SurveyID=="101")
summary(MD.1)
# sheesh...1959-present..lot of years there

plot(Index~Year, data=MD.1)
# quite robust--doesn't look like any years are missing, really

mk.test(MD.1$Index)
# so, no change long term (tau=-0.085, p=0.34)

MD.1.cut<-filter(MD.1, Year>="2009")

plot(Index~Year, data=MD.1.cut)

mk.test(MD.1.cut$Index)
# same result--no evidence for a change in abundance

MD.2<-filter(Potomac, SurveyID=="103")

plot(Index~Year, data=MD.2)
# just kind of looks like a different scale, with the same pattern..

mk.test(MD.2$Index)
# tau=-0.099, p=0.25..trying the cut version..

MD.2.cut<-filter(MD.2, Year>="2009")

plot(Index~Year, data=MD.2.cut)

mk.test(MD.2.cut$Index)
# still non-significant change


    # Nanticoke River #

MD.3<-filter(Nanticoke, SurveyID=="82")
summary(MD.3)
# 1959-2021--lot of data

plot(Index~Year, data=MD.3)
# no discernable trend, for the most part...

mk.test(MD.3$Index)
# tau=-0.19, p=0.03---looks like we have a long term declining trend, probably
# spurred on by those early series values? Modern period the same?

MD.3.cut<-filter(MD.3, Year>="2009")

plot(Index~Year, data=MD.3.cut)
# doesn't look crazy..

mk.test(MD.3.cut$Index)
# marginal significance here (p=0.09) with a moderate declining trend (-0.38)

MD.4<-filter(Nanticoke, SurveyID=="85")
summary(MD.4)
# 1959-2021--lot of data

plot(Index~Year, data=MD.4)
# perhaps a more definitive decline in long term abundance through this survey..

mk.test(MD.4$Index)
# tau=-0.23, p=0.007---looks like we have a long term declining trend, probably
# spurred on by those early series values? Modern period the same?

MD.4.cut<-filter(MD.4, Year>="2009")

plot(Index~Year, data=MD.4.cut)
# doesn't look as crazy, but still a potential decline..

mk.test(MD.4.cut$Index)
# no trend detected here--just had the one year with a really "outlier-ish"
# value (>20) in 2011, and all others were steadily below 5


MD.5<-filter(Nanticoke, SurveyID=="89")
summary(MD.5)
# 1990-2021

plot(Index~Year, data=MD.5)
# definitely looks like a declining trend here, but to what extent?

mk.test(MD.5$Index)
# Yeah, this one is pretty rough--tau=-0.62, p=0.000002

MD.5.cut<-filter(MD.5, Year>="2009")

mk.test(MD.5.cut$Index)
# TAU is now slightly positive, but non-significant p-value to support this trend
# more data needed?

MD.6<-filter(Nanticoke, SurveyID=="91")
summary(MD.6)
# 1989-2021

plot(Index~Year, data=MD.6)
# this one looks just as bad as the alewife survey that pairs with this...

mk.test(MD.6$Index)

MD.6.cut<-filter(MD.6, Year>="2009")

mk.test(MD.6.cut$Index)
# same thing repeated here...tau=0.13, p=0.65

MD.7<-filter(Nanticoke, SurveyID=="80")
summary(MD.7)
# 1999-2021

plot(Index~Year, data=MD.7)
# kind of all over the place..unlikely to establish a trend

mk.test(MD.7$Index)
# Yep, no significant trend (p=0.38), with tau=-0.14

MD.8<-filter(Nanticoke, SurveyID=="81")
summary(MD.8)
# same time series

plot(Index~Year, data=MD.8)
# I don't see much here either

mk.test(MD.8$Index)
# right--a really slight downward trend that is non-significant


    # Choptank River #

MD.9<-filter(Choptank, SurveyID=="191")
summary(MD.9)
# 1959-2022

plot(Index~Year, data=MD.9)
# looks kind of flat to me, but we'll see

mk.test(MD.9$Index)
# marginal (p=0.07) slight decline (tau=-0.16) in YOY-ALE abundance here

MD.9.cut<-filter(MD.9, Year>="2009")

mk.test(MD.9.cut$Index)
# same thing through this time period

MD.10<-filter(Choptank, SurveyID=="192")
summary(MD.10)
# 1959-2022

plot(Index~Year, data=MD.10)
# man, this one is ALL over the place--got the "shotgun blast" of high and low years

mk.test(MD.10$Index)
# looks like we detect a very slight increase in YOY-BLH abundance here 
# (tau=0.21, p=0.02)

MD.10.cut<-filter(MD.10, Year>="2009")

mk.test(MD.10.cut$Index)
# non-significant result here, so I'll just keep the full data version

    # North East River #

MD.11<-filter(NorthEast, SurveyID=="93")
summary(MD.11)
# not sure how this one slipped through the cracks (6yr data..): 2015-2021

plot(Index~Year, data=MD.11)

mk.test(MD.11$Index)
# non-significant result, go figure

MD.12<-filter(NorthEast, SurveyID=="96")
summary(MD.12)
# not sure how this one slipped through the cracks (6yr data..): 2015-2021

plot(Index~Year, data=MD.12)

mk.test(MD.12$Index)
# non-significant result, go figure


    # Head of Bay #

MD.13<-filter(HOB, SurveyID=="57")
summary(MD.13)
# 1959-2021

plot(Index~Year, data=MD.13)

mk.test(MD.13$Index)
# non-significant, very slight decline from this series 

MD.13.cut<-filter(MD.13, Year>="2009")
mk.test(MD.13.cut$Index)
# nothing additional to be gained here

MD.14<-filter(HOB, SurveyID=="61")
summary(MD.14)
# 1959-2021

plot(Index~Year, data=MD.14)
# doesn't look much different from the alewife graphic

mk.test(MD.14$Index)
# just about the same as the alewife, indeed...what about the modern period?

MD.14.cut<-filter(MD.14, Year>="2009")
mk.test(MD.14.cut$Index)
# nope, nothing to be gained really

#------------------------------------------------------------------------------#

      # Delaware + New Jersey #

# so, here I have some surveys from Delaware Bay and from the Delaware River
# to deal with: DB==SurveyID's: 125, 127, 130 and DR==133, 137

# Also, for NJ, I have some unknown locations captured by surveys: 162, 165

DB <- filter(MAT, Waterbody=="Delaware Bay")
unique(DB$SurveyID)
# cool: survey ID's 125, 127, 130

DR <- filter(MAT, Waterbody=="Delaware River")
unique(DR$SurveyID)
# cool: surveys 133 and 137

    # Delaware BAY #

DB.1 <- filter(DB, SurveyID=="125")
DB.2 <- filter(DB, SurveyID=="127")
DB.3 <- filter(DB, SurveyID=="130")

summary(DB.1)
# YOY Alewife from 1991-2021

plot(Index~Year, data=DB.1)
# kind of looks like a decline from earliest in the survey period, but the scale
# is pretty low to begin with..

mk.test(DB.1$Index)
# Yeah, MK detected a moderate decline--tau=-0.35 at p=0.008, but what if we
# cut off at the modern period?

DB.1.cut<-filter(DB.1, Year>="2009")
mk.test(DB.1.cut$Index)
# actually there is a moderate increase that is not significant, but perhaps it's
# just a matter of collecting more years data? (tau=0.297 at p=0.19)

summary(DB.2)
# 1990-2021, still YOY-alewife

plot(Index~Year, data=DB.2)
# kind of scattered all over the place, with recent values a bit higher than old

mk.test(DB.2$Index)
# a slight, non-significant, increase (tau=0.17 at p=0.18)

DB.2.cut<-filter(DB.2, Year>="2009")
mk.test(DB.2.cut$Index)
# just to be sure (and consistent), I tried this same cut period, but no change
# in the interpretation, so moving on.

summary(DB.3)
# 1992-2021, YOY-bluebacks

plot(Index~Year, data=DB.3)
# doesn't look like much--just shows some peak years, I suspect

mk.test(DB.3$Index)
# a slight, non-significant, decrease (tau=-0.17 at p=0.21)

DB.3.cut<-filter(DB.3, Year>="2009")
mk.test(DB.3.cut$Index)
# no change to the interpretation


    # Delaware RIVER #

DR.1 <- filter(DR, SurveyID=="133")
DR.2 <- filter(DR, SurveyID=="137")

summary(DR.1)
# 1987-2021, YOY-alewife

plot(Index~Year, data=DR.1)
# this one looks pretty "flat" too

mk.test(DR.1$Index)
# a slight, significant, decrease (tau=-0.24 at p=0.05) detected over the entire
# series--trying the modern period..

DR.1.cut<-filter(DR.1, Year>="2009")
mk.test(DR.1.cut$Index)
# not significant, but a slight increase is noted in this modern period...will
# look for guidance on what we should do in these instances


summary(DR.2)
# 1987-2021, YOY-bluebacks

plot(Index~Year, data=DR.2)
# sheesh--now this looks like a pretty substantial decline

mk.test(DR.2$Index)
# a moderate, significant, decline in blh abundance is detected 
# (tau=-0.43, p=0.0004)--just to be double sure, following up with modern period

DR.2.cut<-filter(DR.2, Year>="2009")
mk.test(DR.2.cut$Index)
# still a negative trend and non-significant, so I'll go with the long-term

NJ.1 <- filter(MAT, SurveyID=="162")
NJ.2 <-filter(MAT, SurveyID=="165")

summary(NJ.1)
# 1989-2020, alewife of unknown age.

plot(Index~Year, data=NJ.1)
# kind of looks sinusoidal (up-down-little up in recent years)

mk.test(NJ.1$Index)
# a slight, non-significant, decline in ALE abundance is detected 
# (tau=-0.19, p=0.12)-- following up with modern period

NJ.1.cut<-filter(NJ.1, Year>="2009")
mk.test(NJ.1.cut$Index)
# no change really...non-significant, slight decrease--probably just not a 
# monotonic trend?


summary(NJ.2)
# 1989-2020, blueback of unknown age.

plot(Index~Year, data=NJ.2)
# now this looks a lot more like an increasing trend

mk.test(NJ.2$Index)
# a moderate, significant, increase in BLH abundance is detected 
# (tau=0.33, p=0.007)-- following up with modern period, though it doesn't look
# like much would change..

NJ.2.cut<-filter(NJ.2, Year>="2009")
mk.test(NJ.2.cut$Index)
# yeah, non-signifcant positive trend in the modern period--go with the first 
# result to incorporate more years' data

#------------------------------------------------------------------------------#

      # New York #

# looks like all the surveys come from the Hudson, so first step is to filter by
# that waterbody..

NY <- filter(MAT, Waterbody=="Hudson River")
unique(NY$SurveyID)
# surveys 148, 150, 152, 154--check!

NY.1 <- filter(NY, SurveyID=="148")
NY.2 <- filter(NY, SurveyID=="150")
NY.3 <- filter(NY, SurveyID=="152")
NY.4 <- filter(NY, SurveyID=="154")

summary(NY.1)
# 2012-2022, adult alewife

plot(Index~Year, data=NY.1)
# this looks like an increasing trend, but might not have enough data to say

mk.test(NY.1$Index)
# a slight increase is detected, but not enough info to say, apparently
# (tau=0.02, p=1)

summary(NY.2)
# 2012-2022, adult bluebacks

plot(Index~Year, data=NY.2)
# this looks like a pretty substantial decline, but might be more of a "U" shaped
# curve with values coming back up in the 2022 season..

mk.test(NY.2$Index)
# a moderate decrease is detected, but is not significant--down years and peak years
# are apparent throughout..need more data? (tau=-0.29, p=0.28)

summary(NY.3)
# 1980-2022, YOY-Ale

plot(Index~Year, data=NY.3)
# kind of looks like no "boom" years happened until ~1990--maybe a slight increase
# in abundance throughout?

mk.test(NY.3$Index)
# a moderate increase is detected--I'll check the modern period too, but:
# (tau=0.32, p=0.003)

NY.3.cut <- filter(NY.3, Year>="2009")
mk.test(NY.3.cut$Index)
# non-significant trend detected over the modern period, so moving on with the
# first impression

summary(NY.4)
# 1980-2022, YOY-BLH

plot(Index~Year, data=NY.4)
# this one looks all over the place--pretty massive scale compared to ALE

mk.test(NY.4$Index)
# slight decrease detected, but is non-significant:
# (tau=-0.12, p=0.27)

NY.4.cut <- filter(NY.4, Year>="2009")
mk.test(NY.4.cut$Index)
# still a non-significant trend (slightly positive though)

#------------------------------------------------------------------------------#

        # Connecticut #

# all these surveys appeared to come from Long Island Sound and the Conn.River,
# so I'll sort by those waterbodies first..

# Now, the LIS surveys did not filter by region, so I'll just pull those directly
# from the full data (Mods)

LIS.1 <- filter(Mods, SurveyID=="22")
LIS.2 <- filter(Mods, SurveyID=="23")

CR <- filter(MAT, Waterbody=="Connecticut River")
unique(CR$SurveyID)
# survey 121 and 122, check

CR.1 <- filter(CR, SurveyID=="121")
CR.2 <- filter(CR, SurveyID=="122")

summary(LIS.1)
# 1984-2021, alewife of unknown age

plot(Index~Year, data=LIS.1)
# increasing, then decreasing sort of trend...but the highest peaks appear in
# the modern portion of the entire series (2000+)

mk.test(LIS.1$Index)
# a slight increase is detected, with marginal significance
# (tau=0.21, p=0.065)

LIS.1.cut <- filter(LIS.1, Year>="2009")
mk.test(LIS.1.cut$Index)
# just to be thorough, even though the first one wasn't outright significant..

summary(LIS.2)
# 1984-2021, alewife of unknown age

plot(Index~Year, data=LIS.2)
# this one is a lot more flat...hard to say if any trend is really going on..

mk.test(LIS.2$Index)
# a slight decrease is detected, with non-significant test result
# (tau=-0.13, p=0.27)

LIS.2.cut <- filter(LIS.2, Year>="2009")
mk.test(LIS.2.cut$Index)
# no change here either

summary(CR.1)
# 1979-2021, YOY-BLH

plot(Index~Year, data=CR.1)
# strange trend in appearance--hard to say what the test will make of it.

mk.test(CR.1$Index)
# a moderate decrease in abundance is apparent from early in the series
# (tau=-0.36, p=0.0006)

CR.1.cut <- filter(CR.1, Year>="2009")
mk.test(CR.1.cut$Index)
# non-significant slight decrease is what resulted for the recent years, so I 
# suspect we go with the full data in this instance


summary(CR.2)
# 2013-2022, adult-BLH

plot(Index~Year, data=CR.2)
# probably not enough years to say much

mk.test(CR.2$Index)
# a slight decline detected, but nowhere near significant--need more years
# (tau=-0.06, p=0.92)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~END MAT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


            # START SNE #

SNE <- filter(Mods, Region=="SNE")
unique(SNE$Waterbody)
# well, we definitely have more than just the Narrow River...
# according to my records, we have Narrow, Narragansett Bay, and "N. of Cape Cod"
# I'll have to sort out the rest of the surveys from the exact SurveyID's

NR<-SNE
# just to make sure I don't mess up the naming conventions

NB <- filter(Mods, System=="Narragansett Bay")
unique(NB$SurveyID)
# cool...both surveys are there (24 + 25)

CC <- filter(Mods, System=="North of Cape Cod")
unique(CC$SurveyID)
# survey #27, cool


summary(NR)
# 1994-2022, YOY-ALE

plot(Index~Year, data=NR)
# kind of all over the place

mk.test(NR$Index)
# a non-significant slight decrease in abundance..
# (tau=-0.21, p=0.11)

NR.cut <- filter(NR, Year>="2009")
mk.test(NR.cut$Index)
# roughly the same result when we cut down to that same "modern series"--go with
# the full data instead

NB.1 <- filter(NB, SurveyID=="24")
NB.2 <- filter(NB, SurveyID=="25")


summary(NB.1)
# 2012-2022, alewife of unknown age

plot(Index~Year, data=NB.1)
# nearly zeros in all years, minus a handful of peak years..

mk.test(NB.1$Index)
# a non-significant slight increase in abundance..
# (tau=0.16, p=0.53)
# aaannnddd no reason to do a cut to the modern period because this is a new one

summary(NB.2)
# 2012-2022, BLH of unknown age

plot(Index~Year, data=NB.2)
# basically the same thing depicted as the series for alewife, just smaller scale

mk.test(NB.2$Index)
# a non-significant slight increase in abundance..
# (tau=0.02, p=1)
# aaannnddd no reason to do a cut to the modern period because this is a new one

summary(CC)
# 1978-2021, YOY-ALE

plot(Index~Year, data=CC)
# generally looks like an upward trend, with lowest values above zero in the
# modern period.

mk.test(CC$Index)
# a significant moderate increase detected over the series
# (tau=0.24, p=0.02)

CC.cut <- filter(CC, Year>="2009")
mk.test(CC.cut$Index)
# this would not approximate (tau=0) for some reason--so, running with the full
# data!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~END SNE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # START MNE #

MNE <- filter(Mods, Region=="MNE")
unique(MNE$Waterbody) # Great Bay--check
unique(MNE$SurveyID) # just survey 170, check

GB<-MNE

summary(GB)
# 1997-2021, YOY-BLH

plot(Index~Year, data=GB)
# looks like a sharp declining trend to ~2010, and increasing afterward ("U"-shape)

mk.test(GB$Index)
# a significant moderate decrease is detected over the series
# (tau=-0.36, p=0.01)

# BUT, this seems to be reversing a bit more than that tau is depicting...

GB.cut <- filter(GB, Year>="2009")
plot(Index~Year, data=GB.cut)
mk.test(GB.cut$Index)
# Well, this seems to be trending in the complete reverse direction
# (tau=0.37, p=0.09)---seems like a full-SAS question how to handle these type
# of trends

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~END MNE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


        # START NNE #

NNE <- filter(Mods, Region=="NNE")
GB.2 <- filter(NNE, SurveyID=="174")
# This one is the corresponding survey for ALE to the BLH survey above from 
# Great Bay, NH

MM <- filter(NNE, SurveyID=="172")
# This one is the only other survey to pop up under the Regional filter--
# this survey is from Merrymeeting Bay


summary(GB.2)
# 1997-2021, YOY-ALE

plot(Index~Year, data=GB.2)
# more of a "normal" situation depicted here---a lot of close to zero years, and
# then you see peak years in a handful or so years

mk.test(GB.2$Index)
# just about the smallest possible increase, which is not significant
# (tau=0.026, p=0.87)

# juust to be consistent with the other survey...

GB.2.cut <- filter(GB.2, Year>="2009")
plot(Index~Year, data=GB.2.cut)
mk.test(GB.2.cut$Index)
# Well, at least this modern trend seems to be the same as the BLH
# (tau=0.31, p=0.16)---just non-significant, but more years may help that out


summary(MM)
# 1982-2021, YOY-ALE

plot(Index~Year, data=MM)
# looks like an upward trend to me

mk.test(MM$Index)
# Marginal significance, and moderate positive trend
# (tau=0.20, p=0.07)


MM.cut <- filter(MM, Year>="2009")
plot(Index~Year, data=MM.cut)
mk.test(MM.cut$Index)
# Well, the modern trend by itself is quite the turnaround--significant pos. trend
# (tau=0.59, p=0.006)--I'll see what the SAS thinks

### for the paired (BLH) version of this same survey...

MM.2 <- filter(Mods, SurveyID=="29")
unique(MM.2$Waterbody)
# yep, MM-bay

summary(MM.2)
# 1982-2021, YOY-BLH

plot(Index~Year, data=MM.2)
# a lot of zeros early on, but way more positive catch since mid-90's

mk.test(MM.2$Index)
# Sure thing--moderate positive, significant, trend going on here
# (tau=0.39, p=0.0005)


MM.2.cut <- filter(MM.2, Year>="2009")
plot(Index~Year, data=MM.2.cut)
mk.test(MM.2.cut$Index)
# Interestingly, this is about the same trend we're seeing for the Alewife in
# this survey...bring to the SAS for consideration: (tau=0.59, p=0.006)


### SO, now we have a series of surveys that did not have a region description
### but are clearly within the NNE (ALE), CAN-NNE (BLH) region

ME.1 <- filter(Mods, SurveyID=="1")
ME.2 <- filter(Mods, SurveyID=="3")
ME.3 <- filter(Mods, SurveyID=="5")
ME.4 <- filter(Mods, SurveyID=="7")

summary(ME.1)
# SurveyID=1, Adult+/orJuve ALE, 2000-2021

plot(Index~Year, data=ME.1)
# looks like a positive increase to me

mk.test(ME.1$Index)
# slight positive increase that is non-significant
# (tau=0.15, p=0.34)


ME.1.cut <- filter(ME.1, Year>="2009")
plot(Index~Year, data=ME.1.cut)
mk.test(ME.1.cut$Index)
# no evidence to change from the initial interpretation (non-sig. result)


summary(ME.2)
# SurveyID=1, Adult+/orJuve BLH, 2000-2021

plot(Index~Year, data=ME.2)
# looks like a positive increase to me, but we'll see

mk.test(ME.2$Index)
# slight positive increase that is non-significant
# (tau=0.09, p=0.59)


ME.2.cut <- filter(ME.2, Year>="2009")
plot(Index~Year, data=ME.2.cut)
mk.test(ME.2.cut$Index)
# no evidence to change from the initial interpretation (non-sig. result)


summary(ME.3)
# SurveyID=5, Adult+/orJuve ALE, 2001-2021

plot(Index~Year, data=ME.3)
# quite the positive trend here..

mk.test(ME.3$Index)
# slight positive increase that is significant
# (tau=0.43, p=0.009)


ME.3.cut <- filter(ME.3, Year>="2009")
plot(Index~Year, data=ME.3.cut)
mk.test(ME.3.cut$Index)
# no evidence to change from the initial interpretation (non-sig. result)


summary(ME.4)
# SurveyID=7, Adult+/orJuve BLH, 2001-2021

plot(Index~Year, data=ME.4)
# another positive trend here.

mk.test(ME.4$Index)
# slight positive increase that is marginally significant
# (tau=0.32, p=0.055)


ME.4.cut <- filter(ME.4, Year>="2009")
plot(Index~Year, data=ME.4.cut)
mk.test(ME.4.cut$Index)
# some evidence here to go with the modern series over the full series--in that
# this produced a significant result that did not change the overall 
# interpretation: mod. positive increase (tau=0.64, p=0.005)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~END NNE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


      # START NEFSC series #

# SO, the last bit here are a series of surveys from the NEFSC--the regions
# covered in these surveys are mostly NNE-MAT (from their frequency map), but
# some data from SAT exists also, just likely less frequent.

# As such, there is no single region expressed, and I'll handle separately:


NEFSC.1 <- filter(Mods, SurveyID=="9")
NEFSC.2 <- filter(Mods, SurveyID=="10")
NEFSC.3 <- filter(Mods, SurveyID=="11")
NEFSC.4 <- filter(Mods, SurveyID=="12")
NEFSC.5 <- filter(Mods, SurveyID=="13")
NEFSC.6 <- filter(Mods, SurveyID=="14")
NEFSC.7 <- filter(Mods, SurveyID=="15")
NEFSC.8 <- filter(Mods, SurveyID=="16")
NEFSC.9 <- filter(Mods, SurveyID=="21")

# --------------PAIR #1------------#
summary(NEFSC.1)
# SurveyID=9, Adult ALE, 1975-2008

plot(Index~Year, data=NEFSC.1)
# looks like this series took off after 1995

mk.test(NEFSC.1$Index)
# moderate positive increase that is significant
# (tau=0.37, p=0.002)

# We cannot do the modern period (survey ended), but perhaps this is a good
# "baseline"?

summary(NEFSC.2)
# SurveyID=10, Adult BLH, 1975-2008

plot(Index~Year, data=NEFSC.2)
# looks like this series took off after 1995, but maybe more like 2000

mk.test(NEFSC.2$Index)
# slight positive increase that is not significant
# (tau=0.17, p=0.18)

# ------------PAIR #2----------- #
summary(NEFSC.3)
# SurveyID=11, Adult ALE, 2009-2022

plot(Index~Year, data=NEFSC.3)
# looks like a rough trend here; straight down

mk.test(NEFSC.3$Index)
# moderate negative trend, but is not significant
# (tau=-0.21, p=0.36)


summary(NEFSC.4)
# SurveyID=12, Adult BLH, 2009-2022

plot(Index~Year, data=NEFSC.4)
# looks like a rough trend here; straight down

mk.test(NEFSC.4$Index)
# moderate negative trend, but is not significant
# (tau=-0.36, p=0.099)

# ------------PAIR #3----------- #
summary(NEFSC.5)
# SurveyID=13, Adult ALE, 1976-2008

plot(Index~Year, data=NEFSC.5)
# pretty good positive trend here..

mk.test(NEFSC.5$Index)
# moderate positive, significant trend
# (tau=0.34, p=0.006)


summary(NEFSC.6)
# SurveyID=14, Adult BLH, 1976-2008

plot(Index~Year, data=NEFSC.6)
# this one looks more like a flat trend

mk.test(NEFSC.6$Index)
# a very slight negative trend that is not significant
# (tau=-0.05, p=0.68)

# ------------PAIR #4----------- #
summary(NEFSC.7)
# SurveyID=15, Adult ALE, 2009-2022

plot(Index~Year, data=NEFSC.7)
# bit of a negative trend depicted here

mk.test(NEFSC.7$Index)
# moderate negative trend, that is significant
# (tau=-0.58, p=0.004)


summary(NEFSC.8)
# SurveyID=16, Adult BLH, 2009-2022

plot(Index~Year, data=NEFSC.8)
# looks like kind of a flat trend here

mk.test(NEFSC.8$Index)
# no real trend here
# (tau=-0.01, p=1)


# ------------Shrimp survey----------- #
summary(NEFSC.9)
# SurveyID=21, Adult ALE, 1983-2022

plot(Index~Year, data=NEFSC.9)
# looks like a positive trend here; particularly since ~2000 to the present

mk.test(NEFSC.9$Index)
# moderate positive trend, thaT IS significant
# (tau=0.56, p=0.0000005)





