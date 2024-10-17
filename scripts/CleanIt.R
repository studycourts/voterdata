#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Weird Washoe stuff...                       ####

setwd("~/Library/CloudStorage/GoogleDrive-rebecca.gill@unlv.edu/My Drive/Washoe/CVRwashoe") # <-- I'M A LAZY MF,
#                                          change this obvi
library(stringr) # install this if you haven't already

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Read the terrible data                      ####

h<-read.csv("2024cvr.csv",header=FALSE,skip=1,nrows=3)
vars<-apply(h,2,function(x) paste(x,collapse="_"))
vars<-gsub("__","",vars)
# data:
df<-read.csv("2024cvr.csv",header=FALSE,skip=4,col.names=vars)
rm(h)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Cleaning...                                 ####
#
# Extract every character to the left of the first 
# space in columns 17 - 123 (to get rid of the dumb
# percentages) and make them numeric:

num.f<-function(x) {
  as.numeric(sub(" .*", "", x))
}
cols<-colnames(df[,17:123])
df[cols]<-lapply(df[cols],num.f)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Make it long (twss)                         ####

dfl<-reshape(df,idvar="CvrNumber",ids=df$CvrNumber,timevar="Race.Candidate",
             times=cols,v.names="Vote",varying=list(names(df[cols])),
             direction="long")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Clean up the "Race.Candidate" field and extract the race, 
# the candidate name, and (from the ballot type) the party
# for each candidate:

dfl$Race.Candidate<-gsub("MEMBER..","MEMBER.",dfl$Race.Candidate)
dfl$Race.Candidate<-gsub("REGENT..","REGENT.",dfl$Race.Candidate)

dfl$Race<-sub("\\.\\..*", "", dfl$Race.Candidate) # race

dfl$Party<-substr(dfl$BallotType,1,3) # party
dfl$Party<-ifelse(dfl$Party=="NP6",paste("NO PARTY"),dfl$Party)

dfl$LastName<-str_extract(dfl$Race.Candidate, # candidate name
                               "(?<=Vote\\.For\\.1\\._)[^\\.]+(?=\\.\\.)")
dfl$LastName<-ifelse(is.na(dfl$LastName),paste("NO.NAME"),dfl$LastName)

# Create race-party-candidate identifier:

dfl$Race.Candidate.PID<-paste(dfl$Race,dfl$LastName,dfl$Party,sep=".")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now add Party the non-"long" data frame:    ####

df$Party<-substr(df$BallotType,1,3) # party
df$Party<-ifelse(df$Party=="NP6",paste("NO PARTY"),df$Party)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Summary                                     ####
#
# - There are 80,262 total ballots cast; 33,777 
#   Democratic, 35,513 GOP, and 10972 non-party:

table(df$Party)

# - "BallotType" identifies the specific ballot
#   (distinguished by both party and set of down-
#   ticket races).
# - In the "long" data, "LastName" is the last 
#   name of the candidate. Note that there are 
#   a few candidates that share last names 
#   (e.g., "Anderson") but - happily - none who
#   are in the same race. Which means...
# - ...in the "long" data, the "Race.Candidate" 
#   variable uniquely identifies the combination of 
#   race / office and candidate. Each voter appears
#   once for each unique value of this variable:

unique(table(dfl$Race.Candidate))

# - "Vote" is numeric, and equals one if the voter
#   in question voted for that candidate in that
#   race, zero if they voted for a different candidate
#   in that race, and empty / missing if they did not
#   votre in that race at all. This in turn means 
#   that -- because of the way the data are 
#   structured -- there are empty cells for "Vote"
#   where candidates did not appear on a ballot.
#   So, for example, Jacky Rosen has 33,777 empty 
#   cells for "Vote" on the GOP ballot:

length(unique(dfl[dfl$Race.Candidate.PID=="UNITED.STATES.SENATE.ROSEN.DEM",]$CvrNumber))
table(dfl[dfl$Race.Candidate.PID=="UNITED.STATES.SENATE.ROSEN.REP",]$Vote)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Spit out some .CSVs                         ####

write.csv(df,"Washoe.csv",row.names=FALSE)
write.csv(dfl,"Washoe-Long.csv",row.names=FALSE)

# Go to "CrunchIt.csv" for more terrible fun
#
# \fin