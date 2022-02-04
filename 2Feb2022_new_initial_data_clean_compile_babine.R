# --------------------
# Babine Lake sockeye. Clean data
# Celeste Kieran
# 2 Feb 2022
# --------------------


#### Packages ####
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plyr)


#### Dataframes ####

## Here are the dataframes I've been working with. I'd like to start from scratch

#babine <- read.csv("babine_updated_8oct2021.csv")
#pdo.iso <- read.csv("pdo.iso.merge.11oct2021.csv")
#Full <- read.csv("very.full.babine.df.12oct2021.csv")



#### Re-making babine isotope and biodata dataframe 3 Feb 2022 ####
#### Full Skeena biodata for Babine fish (from Michael Price)
# This has biodata from all Skeena watershed sockeye sampled for scales, sex, length, weight, scale measrements (incomplete)
#rm(list=ls())
fullskeena <- read.csv("Skeena_biodata_full.csv", 
                   stringsAsFactors=TRUE,
                   na.strings=c('', 'NA'),
                   strip.white=TRUE)


#head(fullskeena)
#names(fullskeena)

# Select Babine Lake sockeye
#levels(fullskeena$Population)
full_bab_bio <- fullskeena %>% 
  filter(Population == "Babine")

#Length_tally <- full_bab_bio %>% group_by(year, Length..inches...1.4.s.) %>% 
#  summarise(Count_of_Length_Obs = n()) # We have some length data post-1957 but unfortunately not for the fish we sampled for isotopes




## I want to merge isotope data with more complete biodata set (with scale measurements, age, etc.)
# ISSUES: 
#1. Pre 1918 data has a different multi-row format for entering growth data (each section of circuli growth has its own row, with a column 'LH' which tells life history (Freshwater1styear, saltwater2ndyear etc), and a column 'GW' which has the growth of that circuli. ) eg. 1 fish has 5 rows of data
#2. There is no unique identifying code for each seperate fish (multiple fish from same year with same fish number)
#3. 1968 has multiple duplicated fish numbers in the same scale book.  


# First rename our variables and change units of measurements from imperical to metric

# Switch units of measurement 
inch <- function(i) {
  return(i*2.54)
}

lbs <- function(t) {
  return(t*453.59237)
}

bio <- full_bab_bio %>% 
  dplyr::mutate(Length.cm = inch(as.numeric(Length..inches...1.4.s.))) %>% 
  dplyr::mutate(Weight.g = lbs(Weight..lbs.)) %>% 
  dplyr::select(Year = year, Length.cm, Weight.g, Age, Sex, Population, Proportion, everything())





#### Fix growth data and make unique fish codes 
# In the early data, each section of circuli growth has its own row, with a column 'LH' which tells life history (Freshwater1styear, saltwater2ndyear etc), and a column 'GW' which has the growth of that circuli. 

# Here let's seperate each time unit
pre1918 <- bio %>% 
  dplyr::filter(Year <= 1916)

just1968 <- bio %>% 
  dplyr::filter(Year == 1968)

after1916 <- bio %>% 
  dplyr::filter(!Year == 1968) %>% 
  dplyr::filter(Year > 1916) 


# Create unique fish code (FishNo, Scale Bk, and Year)
# for pre 1916
pre1918$Fish.Code <- paste(pre1918$Fish.No, pre1918$Scale.Bk.No, pre1918$Year, sep=".") 

# Post 1916
after1916$Fish.Code <- paste(after1916$Fish.No, after1916$Scale.Bk.No, after1916$Year, sep=".") 

# And for 1968, fish.no + scalebook # + Age (both isotope date and biodata have different ages for the duplicated fish, it's only fish no 4 and 5 that are duplicated)
just1968$Fish.Code <- paste(just1968$Fish.No, just1968$Scale.Bk.No, just1968$Age, just1968$Year, sep=".") 


### Check for duplicated fish

# Pre 1918:
#length(pre1918$Fish.Code[duplicated(pre1918$Fish.Code)]) # 87 duplicated fish codes in early years
#uniquefish <- pre1918$Fish.Code[duplicated(pre1918$Fish.Code)]
#length(unique(uniquefish)) # From 35 unique fish

# Just to triple check what the duplicates are
#df <- pre1918
#dup <- data.frame(as.numeric(duplicated(df$Fish.Code))) #creates df with binary var for duplicated rows
#colnames(dup) <- c("dup") #renames column for simplicity
#df2 <- cbind(df, dup) #bind to original df
#df3 <- subset(df2, dup == 1)
#df3 <- df3 %>% 
#  dplyr::select(Fish.Code, everything())
# Checks out, multiple rows for same fish



# After 1916
#length(after1916$Fish.Code[duplicated(after1916$Fish.Code)]) # 7 duplicated fish
#df <- after1916
#dup <- data.frame(as.numeric(duplicated(df$Fish.Code))) 
#colnames(dup) <- c("dup") 
#df2 <- cbind(df, dup) 
#df3 <- subset(df2, dup == 1)
#df3 <- df3 %>% 
#  dplyr::select(Fish.Code, everything())
# From 2002, 2003, 2004, 2006, these fish have no scale book numbers associated. But our fish sampled for isotopes from the 2000s all had scale books, so not an issue

# Just 1968
#just1968$Fish.Code[duplicated(just1968$Fish.Code)]
# No duplicates

# In the early data, each section of circuli growth has its own row, with a column 'LH' which tells life history (Freshwater1styear, saltwater2ndyear etc), and a column 'GW' which has the growth of that circuli. Unsure of in what units, something to ask Mike. 
# Here let's seperate each row so we can combine them
FW1_df <- pre1918 %>% 
  dplyr::filter(LH == "FW1")

FW2_df <- pre1918 %>% 
  dplyr::filter(LH == "FW2")

SW1_df <- pre1918 %>% 
  dplyr::filter(LH == "SW1")

SW2_df <- pre1918 %>% 
  dplyr::filter(LH == "SW2")

SW3_df <- pre1918 %>% 
  dplyr::filter(LH == "SW3")




# Ok, now rename all the GWs of each LH so we can distinguish them once we recombine, also only take what we need from the other ones
FW1_df1 <- FW1_df %>% 
  dplyr::rename(FW1.GW = GW) %>% 
  dplyr::select(!c(LH, FW2, Brood.2, SW1, Brood.3, SW2, SW3, SW4))

FW2_df1 <- FW2_df %>% 
  dplyr::select(Fish.Code, FW2.GW = GW)

SW1_df1 <- SW1_df %>% 
  dplyr::select(Fish.Code, SW1.GW = GW)

SW2_df1 <- SW2_df %>% 
  dplyr::select(Fish.Code, SW2.GW = GW)

SW3_df1 <- SW3_df %>% 
  dplyr::select(Fish.Code, SW3.GW = GW, SW4.GW = SW4)



fulldf_LH <- merge(FW1_df1, FW2_df1, by = "Fish.Code", all.x = TRUE)
fulldf_LH1 <- merge(fulldf_LH, SW1_df1, by = "Fish.Code", all.x = TRUE)
fulldf_LH2 <- merge(fulldf_LH1, SW2_df1, by = "Fish.Code", all.x = TRUE)
fulldf_LH3 <- merge(fulldf_LH2, SW3_df1, by = "Fish.Code", all.x = TRUE)


fullpre1918 <- fulldf_LH3
# 36 observations. This is 1 more than unique fish found earlier
# compare fullpre1918 with uniquefish
#pre1918check <- fullpre1918["Fish.Code"]

#unicheck <- as.data.frame(unique(uniquefish))

#anti_join(pre1918check,unicheck, by = c("Fish.Code" = "unique(uniquefish)"))
# The extra fish has Fish.Code 327.76889.1913
# Checked original dataframe, this fish had no duplicated entries because there were no growth data recorded, not an issue

# Now attach pre1918, just1968, and after1916
#names(fullpre1918)
#names(just1968)
#names(after1916)

fullpre1918 <- fullpre1918 %>% 
  dplyr::select(!Brood)

just1968 <- just1968 %>% 
  dplyr::select(!c("Brood", "Brood.3", "Brood.2", "LH")) %>% 
  dplyr::rename(FW1.GW = GW, FW2.GW = FW2, SW1.GW = SW1, SW2.GW = SW2, SW3.GW = SW3, SW4.GW = SW4)

after1916 <- after1916 %>% 
  dplyr::select(!c("Brood", "Brood.3", "Brood.2", "LH")) %>% 
  dplyr::rename(FW1.GW = GW, FW2.GW = FW2, SW1.GW = SW1, SW2.GW = SW2, SW3.GW = SW3, SW4.GW = SW4)

fullbiobio <- rbind(fullpre1918, just1968, after1916)

# just reorder columns
fullbiobiobio <- fullbiobio %>% 
  dplyr::select(Year, Fish.Code, Population, Sex, Weight.g, Length.cm, Age, FW1.GW, FW2.GW, SW1.GW, SW2.GW, SW3.GW, SW4.GW, edge.growth, everything())

fullbiobiobio$Fish.Code[duplicated(fullbiobiobio$Fish.Code)]

# Duplicates?
df <- fullbiobiobio
dup <- data.frame(as.numeric(duplicated(df$Fish.Code)))
colnames(dup) <- c("dup") 
df2 <- cbind(df, dup)
df3 <- subset(df2, dup == 1) 
# All in 2000's, lacking scale book #s. We've decided this is not an issue because none of our isotope scales in the 2000s are lacking scale book #s.

#write.csv(fullbiobiobio, "fullbabinebiodata_newFishCode3Feb2022.csv", row.names = FALSE)





#### Now merge updated & cleaned biodataframe with isotope data 
#rm(list=ls())

# Raw isotope data
### Isotope results from Isotope Archeology lab at SFU (Michael Richards, Lab tech - Reba), sent to me in March

# Here we have isotope data and partial biodata for 167 Babine Lake Sockeye salmon from 1913 - 2014
# Isotopic measurement errors are: 
# d13C +/- 0.1
# d15N +/- 0.2
# d34S +/- 0.5 per mil (all at 1 s.d.)
# 155 valid measurements out of 165 samples 

isotoperaw <- read.csv("Isotope_Results_Babine_salmon_March2021-3_SFU_ArchLab.csv", 
                       stringsAsFactors=TRUE,
                       na.strings=c('', 'NA'),
                       strip.white=TRUE)

### Some scale measurements failed: find which scales failed (NA) and remove
#NAiso <- isotoperaw[is.na(isotoperaw$X_15NAIR), ]

#NAiso <- NAiso %>% 
#  dplyr::select(Year, Scale.Bk.No., Fish.No.)

# We lost measurements for the following 10 scale samples
# Year  Scl.B.No
# 1982	Bk. 6	3
#	1988	Bk. 8 (35422)	2
#	1993	83413	4
#	1998	31466	2
#	1998	31466	4
#	1998	31466	5
#	2000	50602	5
#	2005	37006	2
#	2010	90838	1
#	2010	90838	3




# Remove these observations (also remove the 3 summary lines at end of d
iso_raw <- isotoperaw[!is.na(isotoperaw$X_15NAIR), ] # We now have 157 observations



# I need to 1) remove old incomplete biodata from isotope data, 2) give iso dataframe the same fishcodes i did in biodata, 3) then merge together

names(iso_raw)
# 1) Take everything except the incomplete biodata (and Age, which I need for fish code in 1968)
iso1 <- iso_raw %>% 
  dplyr::select(Year, d13C = X_13CVPDB, d15N = X_15NAIR, d34S = X_34SVCDT,  C.Natomic, C.Satomic, Fish.No = Fish.No., Scale.Bk.No = Scale.Bk.No., S.SFU, Sample.code, Sample.weight = Sample.weight..mg., Sample.SFU = Sample..SFU..., Wt.C = wt..C, Wt.N = wt..N, X.S, Age) 


# 2) Make "fish code" (same as for biodata)
pre1918 <- iso1 %>% 
  dplyr::filter(Year <= 1916)

just1968 <- iso1 %>% 
  dplyr::filter(Year == 1968)

after1916 <- iso1 %>% 
  dplyr::filter(!Year == 1968) %>% 
  dplyr::filter(Year > 1916) 



# for pre 1916
pre1918$Fish.Code <- paste(pre1918$Fish.No, pre1918$Scale.Bk.No, pre1918$Year, sep=".") 


# Post 1916
after1916$Fish.Code <- paste(after1916$Fish.No, after1916$Scale.Bk.No, after1916$Year, sep=".") 


# And for 1968, fish.no + scalebook # + Age (both isotope data and biodata have different ages for the duplicated fish, it's only fish no 4 and 5 that are duplicated)
just1968$Fish.Code <- paste(just1968$Fish.No, just1968$Scale.Bk.No, just1968$Age, just1968$Year, sep=".") 


fullisodata <- rbind(pre1918, after1916, just1968)

# Check for duplicates
#df <- fullisodata
#dup <- data.frame(as.numeric(duplicated(df$Fish.Code))) 
#colnames(dup) <- c("dup") 
#df2 <- cbind(df, dup) 
#df3 <- subset(df2, dup == 1) 
# No duplicates


fulliso1 <- fullisodata %>% 
  dplyr::select(!c(Fish.No, Scale.Bk.No, Age))

# Full biod data with new fish codes
full_bio <- read.csv("fullbabinebiodata_newFishCode3Feb2022.csv")
full_bio1 <-full_bio %>% 
  select(!Year)

completedata <- merge(fulliso1, full_bio1, by = "Fish.Code", all.x = TRUE)

# Next, remove unwanted columns and add new data (20)
names(completedata)

completedata1 <- completedata %>% 
  dplyr::select(!c(LH.1, LH.2, LH.3, LH.4,  LH.5, Brood.4, Brood.5, Brood.1, Length..inches...1.4.s.,Weight..lbs.)) %>% 
  dplyr::select(Year, d13C, d15N, d34S, Age, Weight.g, Length.cm, Sex, everything())

#write.csv(completedata1, "bio.iso.data.babine.scales.1913-2014.2Feb2020.csv" , row.names = FALSE)



#### Add modern isotope data 3 Feb 2022 (this needs a check-up to add some extra columns) #### 

### This data is from scales collected by me (Celeste N. Kieran) in August 2021 in collaboration with Lake Babine Nation Fisheries, with  isotope measurements made at SFUs isotope archaeology lab by lab technician Reba Macdonald.
iso_new <- read.csv("isotope_biodata_Babine_fromsummer2021_3feb2022.copy.csv")
#names(iso_new)
#hist(iso_new$δ13CVPDB)
#plot(δ13CVPDB ~ δ15NAIR, iso_new)


# Change names and lbs to grams, I will come back to add the rest of the columns (date, scale number, etc) later
lbs <- function(t) {
  return(t*453.59237)
}



iso_new1 <- iso_new %>% 
dplyr::select(Year, δ15N = δ15NAIR, δ13C = δ13CVPDB, δ34S = δ34SVCDT, Length.cm, Weight.lbs, Sex, C.Natomic, C.Satomic, Fish.No., Scale.Bk.No.) %>% 
  dplyr::mutate(Weight.g = lbs(Weight.lbs)) %>% 
  dplyr::select(!Weight.lbs)


# Make fish.code
iso_new1$Fish.Code <- paste(iso_new1$Fish.No, iso_new1$Scale.Bk.No, iso_new1$Year, sep=".")    


## Change old names so we can merge
names(iso_new1)
names(iso_old)

iso_old <- read.csv("bio.iso.data.babine.scales.1913-2014.2Feb2020.csv")

iso_old1 <- iso_old %>% 
  select(Year, δ15N = d15N, δ13C = d13C, δ34S = d34S, Length.cm, Weight.g, Sex, C.Natomic, C.Satomic, everything())

temp.iso.bio.babine.1913to2021 <- rbind.fill(iso_old1, iso_new1)

#write.csv(temp.iso.bio.babine.1913to2021, "temp.iso.bio.babine.1913to2021.3Feb2022.csv", row.names = FALSE)







#### --------------------------- ENVIRONMENTAL VARIABLES --------------------------------####
#### Suess correction ####
# Dataframe with isotope and biodata
data <- read.csv("temp.iso.bio.babine.1913to2021.3Feb2022.csv")

## Here I write a function that will take our d13C data and correct it for the anthropogenic increase in CO2 (depleted in C13)
## using the Suess Effect Correction Factor from Misarti et al. 2009. 

# Suess Effect Correction Factor: a*exp(b*0.027)
# Where a = maximum annual rate of d13C decrease in the North Pacific (here I will use -0.014 derived from Quay et al. 1992 but I may need to justify a different number later on) 
# b = number of years since 1850
# 0.027 = curve represented by Gruber et al. 1999 for change in d13C in ocean. 

Suess.fun <- function(c, b) {
  return(c - (-0.014)*exp(b*0.027))
}


# Now I need to take our dataframe and add another column that is the number of years since 1850 (b), ie. sample collection year - 1850
class(data$Year)

data1 <- data %>% 
  dplyr::mutate(b = Year - 1850)



# Ok, I think I can run the correction now. 
data2 <- data1 %>% 
  dplyr::mutate(Suess.δ13C = Suess.fun(δ13C, b))


#head(data1)
#names(data2)

data2 <- data2 %>% 
  dplyr::select(Year, δ15N, δ13C, Suess.δ13C,δ34S, everything()) %>% 
  dplyr::select(!b)

#write.csv(data2, "iso.bio.1913.2022.suess.3feb2022.csv", row.names = FALSE)

#### PDO ####

# Dataframe with isotope and biodata
#isobio <- read.csv("temp.iso.bio.babine.1913to2021.3Feb2022.csv")


#rm(list=ls())

data <- read.csv("temp.iso.bio.babine.1913to2021.3Feb2022.csv")




