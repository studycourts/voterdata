#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# More fun with Washoe County                   ####

setwd("~/Dropbox (Personal)/Washoe") # <-- I'M STILL LAZY

# If you haven't already run it, uncomment
# and do this first:
#
source("CleanIt.R")
#
# Packages:

library(spikes)
library(readxl)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Vote shares by candidate within each race...  ####
#
# Create a "Race-Party" variable:

dfl$Race.PID<-paste(dfl$Race,dfl$Party,sep=".")

# Tallies within party/races:

CandVoteCounts<-aggregate(Vote~Race.Candidate.PID,data=dfl,sum,na.rm=TRUE)

# These totals exactly mirror those currently on the NV SOS office,
# at https://silverstateelection.nv.gov/county-results/washoe.shtml.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Precinct-level data                     ####
#
# Read in the 5/24/2024 precinct-level voter registration
# data from the SOS website. The link is:
#
# https://www.washoecounty.gov/voters/2024-election/2024-election-files/precinct%20count%20detail%2005-24-2024.xls
#
# Reading it directly is giving me fits rn, so I'm
# just going to keep a copy with that filename in the
# Dropbox folder, and read that instead:

pvr<-read_excel("precinct count detail 05-24-2024.xls",n_max=456)

# Extract only the variables needed for the registration. Note that
# only REPs can vote in REP races, and only DEMs can vote in 
# DEM races, but anyone can vote in NO PARTY races.

pvr<-data.frame(PrecinctName=pvr$NAME,TotalReg=pvr$TOTAL,
                DemReg=pvr$DEM,RepReg=pvr$REP)

# These give us the total number of registered voters eligible 
# to vote in each precinct.
#
# In the "dfl" data frame, fix the precinct names:

dfl$PrecinctName<-substr(dfl$PrecinctPortion,1,nchar(dfl$PrecinctPortion)-7)

# Aggregate vote counts to the precinct /candidate level, by candidate:

p.cands<-aggregate(dfl,Vote~PrecinctName+Race+Party+CandName+Race.PID+
                 Race.Candidate.PID,sum,na.rm=TRUE)
p.cands$CandVotes<-p.cands$Vote

# Those give us the total number of votes each candidate received in each
# precinct. To get the total number of votes cast in each race at the 
# precinct level, we can aggregate *those* ("p.cants") data by race:

p.races<-aggregate(p.cands,Vote~PrecinctName+Race.PID,sum,na.rm=TRUE)
p.races$RaceVotes<-p.races$Vote
p.races$Vote<-NULL

# Now merge those two:

p.cands<-merge(p.cands,p.races,by=c("PrecinctName","Race.PID"))

# And now (maybe?) merge in the precinct-level voter registration data:

p.cands<-merge(p.cands, pvr, by=c("PrecinctName"))

# Flag precincts where recorded turnout was greater than
# recorded voter registration:

p.cands$flag<-p.cands$TotalReg<p.cands$RaceVotes

# Now, looking for "fraud"...
#
# First, the Republican County Commission race in
# District Four. There were five candidates running:
# Andriola,* Lawson, Hilton-Thomas, Gomez, and Walter.
#
# Extract that race:

CCD4<-p.cands[p.cands$Race=="COUNTY.COMMISSION.DISTRICT.4.REP",]

# and cereate small / minimal data frames for the spikes
# analysis:

df.CA<-with(CCD4[CCD4$CandName=="ANDRIOLA.CLARA",],
            data.frame(N=RepReg,t=RaceVotes,v=CandVotes))
df.ML<-with(CCD4[CCD4$CandName=="LAWSON.MARK.A.",],
            data.frame(N=RepReg,t=RaceVotes,v=CandVotes))
df.TH<-with(CCD4[CCD4$CandName=="HILTON.THOMAS.TRACEY",],
            data.frame(N=RepReg,t=RaceVotes,v=CandVotes))
df.TG<-with(CCD4[CCD4$CandName=="GOMEZ.TRISTA",],
            data.frame(N=RepReg,t=RaceVotes,v=CandVotes))
df.JW<-with(CCD4[CCD4$CandName=="WALTER.II.JOHN.L..LITTLE.JOHN.",],
            data.frame(N=RepReg,t=RaceVotes,v=CandVotes))

# Spikes analyses:

set.seed(68801)

sp.CA<-spikes(df.CA)
sp.MAL<-spikes(df.ML)
sp.TH<-spikes(df.TH)
sp.TG<-spikes(df.TG)
sp.JW<-spikes(df.JW)

# CIs:

set.seed(68801)

ci.CA<-confInt(sp.CA)
ci.MAL<-confInt(sp.MAL)
ci.TH<-confInt(sp.TH)
ci.TG<-confInt(sp.TG)
ci.JW<-confInt(sp.JW)


# put all these together:

CCD4.names<-c("Andriola","Lawson","Hilton","Gomez","Walter")

CCD4.results<-rbind(summary(ci.CA),summary(ci.MAL),summary(ci.TH),
                    summary(ci.TG),summary(ci.JW))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# The second is for school board trustee, District G
# (at-large). There were seven candidates: Rosenstein,*
# Nicolet, Woo, White, Lehman, Di Carlo, and
# Phillipps.
#
# Extract those data:

SBDG<-p.cands[p.cands$Race=="SCHOOL.BOARD.TRUSTEE.DISTRICT.G.AT.LARGE",]

# There are two precincts in this race (RENO-VERDI 5020 
# and RENO-VERDI 9103) that show zero registered voters in
# the SOS voter registration data, but which have votes
# in this race. This is likely because of late registrations
# between the 5/28 SOS data pull and the day of the primary 
# in June. We'll drop those precincts:

SBDG<-SBDG[SBDG$flag==FALSE,]

# Similarly, there are four precincts where the total 
# number of votes in the race ("RaceVotes") was zero. 
# We're dropping those precincts as well:

SBDG<-SBDG[SBDG$RaceVotes>0,]

# Now, create a column that is the number of registered
# Ds if the ballot is a Democratic one, the number of GOPers
# if it's Republican, and (Total - Dem - Rep) if it's
# neither:

SBDG$RegVoters<-ifelse(SBDG$Party=="DEM",SBDG$DemReg,NA)
SBDG$RegVoters<-ifelse(SBDG$Party=="REP",SBDG$RepReg,SBDG$RegVoters)
SBDG$RegVoters<-ifelse(SBDG$Party=="NO PARTY",(SBDG$TotalReg-SBDG$DemReg-SBDG$RepReg),
                       SBDG$RegVoters)

# Make minimal data frames for spikes analyses:

df2.PR<-with(SBDG[SBDG$CandName=="ROSENSTEIN.PERRY",],
            data.frame(N=RegVoters,t=RaceVotes,v=CandVotes))
df2.DN<-with(SBDG[SBDG$CandName=="NICOLET.DIANE",],
             data.frame(N=RegVoters,t=RaceVotes,v=CandVotes))
df2.AW<-with(SBDG[SBDG$CandName=="WOO.ALICIA",],
             data.frame(N=RegVoters,t=RaceVotes,v=CandVotes))
df2.PW<-with(SBDG[SBDG$CandName=="WHITE.PAUL.D.",],
             data.frame(N=RegVoters,t=RaceVotes,v=CandVotes))
df2.ML<-with(SBDG[SBDG$CandName=="LEHMANN.MONICA" ,],
             data.frame(N=RegVoters,t=RaceVotes,v=CandVotes))
df2.JD<-with(SBDG[SBDG$CandName=="DI.CARLO.JACQLYN",],
             data.frame(N=RegVoters,t=RaceVotes,v=CandVotes))
df2.NP<-with(SBDG[SBDG$CandName=="PHILLIPPS.NATHANIEL.NATE.",],
             data.frame(N=RegVoters,t=RaceVotes,v=CandVotes))

# Spikes analyses:

set.seed(68803)

sp.PR<-spikes(df2.PR)
sp.DN<-spikes(df2.DN)
sp.AW<-spikes(df2.AW)
sp.PW<-spikes(df2.PW)
sp.ML<-spikes(df2.ML)
sp.JD<-spikes(df2.JD)
sp.NP<-spikes(df2.NP)

# CIs:

set.seed(68803)

ci.PR<-confInt(sp.PR) # nope
ci.DN<-confInt(sp.DN) # ok
ci.AW<-confInt(sp.AW) # ok
ci.PW<-confInt(sp.PW) # ok
ci.ML<-confInt(sp.ML) # ok
ci.JD<-confInt(sp.JD) # ok
ci.NP<-confInt(sp.NP) # ok

# Put all these together:

rbind(summary(ci.PR),summary(ci.DN),summary(ci.AW),
      summary(ci.PW),summary(ci.ML),summary(ci.JD),
      summary(ci.NP))


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Looking for identical ballots...              ####
#
# "BallotType" identifies each unique ballot type. There
# are 164 different ballot types; each ballot type has 
# a different number of possible candidates.
#
# A ballot type is defined by location and party;
# the ballot type in turn tells us which candidates
# are available to vote for.
#
# Within each ballot type, we can calculate the possible
# number of unique voting patterns; because each candidate
# can have a vote for (=1) or against (=0); note that some
# races include a "None of these candidates" option, which
# is included here. In a race with N candidates, in which 
# the voter chooses one, the number of possible patterns
# in that race is just N (formally, (N! / (1! * (N-1)!))).
# 
# If a ballot has K races on it, and each race has N_k 
# candidates (k=1...K), then the total possible
# number of vote patterns is:
#
# \prod_{k=1}^{K} N_{k}
#
# So a ballot with four races, each of which has 5, 4, 2, and
# 8 candidates running, respectively, has a possible
# 5*4*2*8 = 320 patterns of votes.
#
# 

