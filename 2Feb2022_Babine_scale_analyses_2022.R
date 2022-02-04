# --------------------
# Babine Lake sockeye. Analysis
# Celeste Kieran
# 2 Feb 2022
# --------------------

#### Packages ####
library(dplyr)
library(ggplot2)
library(tidyverse)

#### Figure function ####
pdf.f <- function(f, file, ...) {
  cat(sprintf('Writing %s\n', file))
  pdf(file, ...)
  on.exit(dev.off())
  f()
}

## now, create a function with no arguments around your figure code

myfig <- function() {
  
  ## ----------------------------------------
  ## PUT ALL FIGURE CODE HERE
  
  ## E.G., here's some code to draw a polygon
  par(oma=c(0.1,0.1,0.1,0.1), mar=c(3, 3, 0.1, 0.1),
      mgp=c(2,0.2,0), tcl=0, cex.axis=1, cex.main=1, pty='s')
  
  plot(NA, ylim=c(0,10), xlim=c(0,10), xlab='x', ylab='y', las=1)
  cc <- 'red'
  x.vals <- c(1,3,2,1)
  y.vals <- c(1,6,5,2)
  points(x.vals,y.vals,col=cc,pch=16)
  polygon(x=x.vals,
          y=y.vals,
          col=makeTransparent(cc, alpha=0.25), border=NA)
  ## ----------------------------------------
}

## now, call pdf.f to make your figure

pdf.f(f=myfig, file='~/Desktop/myfigure.pdf', height=3, width=3)

### First look at 2021 data

data <- read.csv("iso.bio.1913.2022.suess.3feb2022.csv")
names(data)

hist(data$δ15N) # N15 still distributed pretty normally
hist(data$δ13C) # Also
hist(data$δ34S) # Fairly normal, some high values around 17.5

plot(δ15N ~ Year, data) # Looks less variable but quite low in 2020!
plot(δ13C ~ Year, data) # 2020 follows low trend
plot(δ34S ~ Year, data) # Also lower trend in 2020

# N
qqnorm(data$δ15N) 
qqline(data$δ15N, col = "steelblue", lwd = 2) # some funky ones at the ends. Would be good to plot and see who our outliers are

# C
qqnorm(data$δ13C) 
qqline(data$δ13C, col = "steelblue", lwd = 2) 

# S
qqnorm(data$δ34S) 
qqline(data$δ34S, col = "steelblue", lwd = 2) 



####  Isotopes ~ time with Standard deviation the mean ####

# I'm not sure if it is good to use SEM here, I'd like to ask. Because not sure if my 'population' is really a true population
# Is this a pseudoreplication issue? I think likely. But ok for now. Might need to change to sd
babine <- data



# make a new dataframe with each year, the mean N15 of that year, and then the sd of that mean N15
newdat <- babine %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(mean_N = mean(δ15N, na.rm = TRUE),
            sd_N = sd(δ15N, na.rm = TRUE),
            N_N = n(),
            se = sd_N/sqrt(N_N),
            upper_limit = mean_N+se,
            lower_limit = mean_N-se
  )



plot <- ggplot(newdat, aes(x=Year, y=mean_N)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean_N - se, ymax=mean_N + se), width=1) +
  labs(x = "Year", y = "Mean δ15N") + 
  scale_x_continuous(breaks = seq(1800, 2021, by = 10)) +
theme_classic();plot
  

ggsave(path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Figures/Babine_figures2022", filename = "d15N_year_sd_3feb2022.png")

## carbon (Suess)
newdat <- babine %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(mean = mean(Suess.δ13C, na.rm = TRUE),
            sd = sd(Suess.δ13C, na.rm = TRUE),
            N_N = n(),
            se = sd/sqrt(N_N),
            upper_limit = mean+se,
            lower_limit = mean-se
  )



plot<-ggplot(newdat, aes(x=Year, y=mean)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=1) +
  labs(x = "Year", y = "mean δ13C (Suess corrected)") +
  scale_x_continuous(breaks = seq(1800, 2021, by = 10)) +
  theme_classic() ;plot

ggsave(path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Figures/Babine_figures2022", filename = "Suess.corrected.sd.time3feb2022.png")
####



## carbon (not suess corrected)
newdat <- babine %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(mean = mean(δ13C, na.rm = TRUE),
                   sd = sd(δ13C, na.rm = TRUE),
                   N_N = n(),
                   se = sd/sqrt(N_N),
                   upper_limit = mean+se,
                   lower_limit = mean-se
  )



plot<- ggplot(newdat, aes(x=Year, y=mean)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=1) +
  labs(x = "Year", y = "Mean δ13C (uncorrected)") +
  scale_x_continuous(breaks = seq(1800, 2021, by = 10)) +
  theme_classic();plot 

ggsave(path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Figures/Babine_figures2022", filename = "uncorrectedsuess.c13.sd.time.3feb2022.png")

# Sulphur
newdat <- babine %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(mean = mean(δ34S, na.rm = TRUE),
            sd = sd(δ34S, na.rm = TRUE),
            N_N = n(),
            se = sd/sqrt(N_N),
            upper_limit = mean+se,
            lower_limit = mean-se
  )



plot <- ggplot(newdat, aes(x=Year, y=mean)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=1) +
  labs(x = "Year", y = "Mean δ34S") +
  scale_x_continuous(breaks = seq(1800, 2021, by = 10)) +
  theme_classic();plot 

ggsave(path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Figures/Babine_figures2022", filename = "s34.time.sd.3feb2022.png")

dev.off()

#### Babine sockeye productivity with PDO and isotopes #### 

#### Scale growth ####

# Look at scale growth over time
# and look at C:N ratio in comparison with different scale growth (first year fresh, last year salt)

# First make scale growth proportions

ggplot(newdat, aes(x=Year, y=mean_N)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean_N - se, ymax=mean_N + se), width=1) +
  labs(x = "Year", y = "mean δ15N") +
  theme_classic() 




## carbon
newdat <- babine %>% 
  group_by(Year) %>% 
  summarize(mean = mean(Suess.δ13C, na.rm = TRUE),
            sd = sd(Suess.δ13C, na.rm = TRUE),
            N_N = n(),
            se = sd/sqrt(N_N),
            upper_limit = mean+se,
            lower_limit = mean-se
  )



ggplot(newdat, aes(x=Year, y=mean)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=1) +
  labs(x = "Year", y = "mean δ13C") +
  theme_classic() 
####



# Sulphur
newdat <- babine %>% 
  group_by(Year) %>% 
  summarize(mean = mean(d34S, na.rm = TRUE),
            sd = sd(d34S, na.rm = TRUE),
            N_N = n(),
            se = sd/sqrt(N_N),
            upper_limit = mean+se,
            lower_limit = mean-se
  )



ggplot(newdat, aes(x=Year, y=mean)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=1) +
  labs(x = "Year", y = "mean δ34S") +
  theme_classic() 


