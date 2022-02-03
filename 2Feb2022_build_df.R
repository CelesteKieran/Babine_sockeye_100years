# --------------------
# Babine Lake sockeye. Clean data
# Celeste Kieran
# 2 Feb 2022
# --------------------


#### Dataframes ####

## Here are the dataframes I've been working with. I'd like to start from scratch
babine <- read.csv("babine_updated_8oct2021.csv")
pdo.iso <- read.csv("pdo.iso.merge.11oct2021.csv")
Fulll <- read.csv("very.full.babine.df.12oct2021.csv")



# Here we have isotope data and partial biodata for 167 Babine Lake Sockeye salmon from 1913 - 2014
# Isotopic measurement errors are: 
# d13C +/- 0.1
# d15N +/- 0.2
# d34S +/- 0.5 per mil (all at 1 s.d.)
# 155 valid measurements out of 165 samples

#### Full biodata for Babine fish (from Michael Price)
full_scale_data <- read.csv("Full_Biodata_Babine_FromSkeena_13april2021.csv")
head(full_scale_data)
head(full_scale_data)
# an original issue was that neither file had unique fish numbers for all fish, and the isotope dataframe doesnt have weight or length data for most of the fish. So our unique fish code needs to be just scale book number + fish number, except for 1968, which has multiple duplicate fish for one scale book.  All we need to do is get these fricken dataframes together, if the fish code is clunky it doesn't matter! Just no duplicates is what we want. Doesnt need to be replicable for future codes, we can rename them. 








#### Switch units of measurement ####
inch <- function(i) {
  return(i*2.54)
}

lbs <- function(t) {
  return(t*453.59237)
}

babine1 <- babine %>% 
  dplyr::mutate(Length.cm = inch(Length.inches)) %>% 
  dplyr::mutate(Weight.g = lbs(Weight.lbs)) %>% 
  dplyr::select(Year, Fish.Code, Length.cm, Weight.g, Sex, Age, d13C = dC13, Suess.d13C = Suess.dC13, d15N = dN15, d34S = dS34, FW1.GW, FW2.GW, SW1.GW, SW2.GW, SW3.GW, SW4.GW, edge.growth, Date.Caught.Approx, Date.actual.notes, everything()) %>% 
  dplyr::select(!c(Length.inches, Weight.lbs, b))

