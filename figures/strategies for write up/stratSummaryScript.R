library(tidyverse)

rm(list=ls())
setwd("C:/Users/jw777/OneDrive - University of Exeter/Dynamic programming/0.6 C++/newDead/figures/strategies for write up")

folders <- list.files() %>%
  .[-length(.)]

timesteps <- 101
tides <- 60

# save how strategy changes the mean waving proportion and energy level ####

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(folders), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# for(f in folders) {
#   setTxtProgressBar(pb, which(folders == f))
#   if(f %in% folders[4:6]) {
#     subfolders <- list.files(f)
#   } else {
#     subfolders <- c("")
#   }
# 
#   for(z in subfolders) {
#     sizes <- unlist(read.table(paste(f, "/", z, "/simSizes.txt", sep = ""), sep = ",", header=F)[1,])
#     
#     simEnergy <- read.table(paste(f, "/", z, "/simEnergy.txt", sep = ""), sep = ",", header = F) %>%
#       mutate(ID = 1:nrow(.)) %>%
#       gather(timestep, energy, V1:all_of(paste("V",(length(.)-1),sep=""))) %>%
#       mutate(time = as.numeric(substring(timestep, first = 2)),
#              energy = as.numeric(energy)) %>%
#       select(-timestep) %>%
#       filter(time > ((tides/2) * (timesteps+1)) & time <= (((tides/2)+1) * (timesteps+1)))
#     
#     for(i in 1:nrow(simEnergy)) {
#       simEnergy$size[i] <- sizes[simEnergy$ID[i]]
#     }
#     
#     meanEnergy <- aggregate(simEnergy, by = list(simEnergy$time, simEnergy$size), mean) %>%
#       mutate(time = time - min(time)+1)
#     
#     meanEnergy$size <- ifelse(meanEnergy$size == 0, "Small", "Large")
# 
#     simBehav <- read.table(paste(f, "/", z, "/simBehav.txt", sep = ""), sep = ",", header = F, fileEncoding="latin1") %>%
#       mutate(ID = 1:nrow(.)) %>%
#       gather(timestep, behav, V1:all_of(paste("V",(length(.)-1),sep=""))) %>%
#       mutate(time = as.numeric(substring(timestep, first = 2)),
#              wave = ifelse(behav == "W", 1, 0)) %>%
#       select(-c(timestep, behav))  %>%
#       filter(time > ((tides/2) * (timesteps+1)) & time <= (((tides/2)+1) * (timesteps+1)))
#     
#     for(i in 1:nrow(simBehav)) {
#       simBehav$size[i] <- sizes[simBehav$ID[i]]
#     }
#     
#     meanWaving <- aggregate(simBehav, by = list(simBehav$time, simBehav$size), mean) %>%
#       mutate(time = time - min(time)+1)
#     
#     meanWaving$size <- ifelse(meanWaving$size == 0, "Small", "Large")
# 
#     if(f == folders[1]) {
#       mean_df <- data.frame(energy = meanEnergy$energy,
#                             wave = meanWaving$wave,
#                             time = meanEnergy$time,
#                             size = meanEnergy$size,
#                             strategy = ifelse(subfolders[1] == "", substring(f, first = 4), substring(z, first = 6))
#       )
#     } else {
#       mean_df <- rbind(mean_df,
#                        data.frame(energy = meanEnergy$energy,
#                                   wave = meanWaving$wave,
#                                   time = meanEnergy$time,
#                                   size = meanEnergy$size,
#                                   strategy = ifelse(subfolders[1] == "", substring(f, first = 4), substring(z, first = 6))
#                        )
#       )
#     }
#   }
# }
# saveRDS(mean_df, "strategy_mean_df.rds")

#load in summary df ####

df <- readRDS("strategy_mean_df.rds")

for(i in 1:nrow(df)) {
  if(grepl("alpha",df$strategy[i])) {
    df$alpha[i] <- substring(df$strategy[i], unlist(gregexpr('ha', df$strategy[i]))[1]+2, 9)
    if(grepl(" ", df$alpha[i])) df$alpha[i] <- substring(df$alpha[i], 1, 3)
    
    df$zeta[i] <- substring(df$strategy[i], nchar(df$strategy[i])-3)
    if(grepl("a", df$zeta[i])) df$zeta[i] <- substring(df$zeta[i], 2)
    
    df$q[i] <- 0.5
    
  } else if(grepl("q", df$strategy[i])) {
    df$alpha[i] <- substring(df$strategy[i], unlist(gregexpr('a', df$strategy[i]))[1]+1, 11)
    if(grepl(" ", df$alpha[i])) df$alpha[i] <- substring(df$alpha[i], 1, 3)
    
    df$zeta[i] <- substring(df$strategy[i], nchar(df$strategy[i])-3)
    if(grepl("z", df$zeta[i])) df$zeta[i] <- substring(df$zeta[i], 2)
    
    df$q[i] <- substring(df$strategy[i], 2, 5)
  } else {
    df$alpha[i] <- 0
    
    df$zeta[i] <- 0
    
    df$q[i] <- 1
  }
}

df$alpha <- as.numeric(df$alpha)
df$zeta <- as.numeric(df$zeta)
df$q <- as.numeric(df$q)

ggplot(data = df[df$strategy %in% unique(df$strategy)[c(3,31)],],
       aes(x = time, y = wave, colour = strategy)) +
  #geom_point(show.legend = F) + 
  geom_smooth(se = F, show.legend = T) + 
  scale_color_manual(values = c("red", "blue4"), name = "Strategy") + 
  theme_bw(base_size = 20)

cFun <- colorRampPalette(c("red","blue"))
cols <- cFun(2)

a <- ggplot(data = df[df$q == 0.5 & df$alpha == 0.25, ],
       aes(x = time, y = wave, colour = factor(size))) + 
  geom_smooth(se = F, show.legend = T) + 
  scale_colour_manual(values = cols, name = "size") +
  theme_bw(base_size = 20) + 
  facet_wrap(vars(factor(zeta)))
a
b <- ggplot(data = df[df$q == 0.5 & df$alpha == 0.5, ],
            aes(x = time, y = energy, colour = factor(size))) + 
  geom_smooth(se = F, show.legend = T) + 
  scale_colour_manual(values = cols, name = "size") +
  theme_bw(base_size = 20) + 
  facet_wrap(vars(factor(zeta)))

c <- ggplot(data = df[df$q == 0.5 & df$alpha == 0.75, ],
            aes(x = time, y = energy, colour = factor(size))) + 
  geom_smooth(se = F, show.legend = T) + 
  scale_colour_manual(values = cols, name = "size") +
  theme_bw(base_size = 20) + 
  facet_wrap(vars(factor(zeta)))


ggplot(data = df[df$q == 0.5 & df$zeta == 0.5,],
       aes(x = time, y = wave, colour = alpha)) +
  geom_smooth(se = F, show.legend = T) + 
  scale_colour_manual(values = cols, name = "alpha.zeta") +
  theme_bw() + 
  facet_wrap(vars(size))
