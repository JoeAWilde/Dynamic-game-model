library(tidyverse)
library(ggpubr)
library(ggplotify)
library(officer)
library(rvg)
library(ggtext)

rm(list=ls())

if(dir.exists("E:/")) {
  setwd("E:/OneDrive - University of Exeter/Dynamic programming/0.6 C++/newDead/figures/new strategies for write up")
} else {
  setwd("C:/Users/jw777/OneDrive - University of Exeter/Dynamic programming/0.6 C++/newDead/figures/new strategies for write up")
}

folders <- list.files()[1:6]

timesteps <- 101
tSteps <- 101
tides <- 60

HT_cols <- seq(tSteps+1, tides*(tSteps+1), tSteps+1)

# save how strategy changes the mean waving proportion and energy level ####

# pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
#                      max = length(folders), # Maximum value of the progress bar
#                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                      width = 50,   # Progress bar width. Defaults to getOption("width")
#                      char = "=")   # Character used to create the bar
# 
# for(f in folders) {
#   setTxtProgressBar(pb, which(folders == f))
#   if(f %in% folders[c(3:5)]) {
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
#     meanEnergy <- aggregate(simEnergy$energy, by = list(simEnergy$time, simEnergy$size),
#                             FUN = function(x) c(meanE = mean(x), sdE = sd(x)))
# 
#     meanEnergy$energy <- meanEnergy$x[,1]
#     meanEnergy$sd_energy <- meanEnergy$x[,2]
#     meanEnergy <- meanEnergy[,-3]
#     names(meanEnergy) <- c("time", "size", "energy", "sd_energy")
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
#                             energy_sd = meanEnergy$sd_energy,
#                             wave = meanWaving$wave,
#                             time = meanEnergy$time,
#                             size = meanEnergy$size,
#                             strategy = ifelse(subfolders[1] == "",
#                                               substring(f, first = 4),
#                                               substring(z,
#                                                         first = unlist(gregexpr(')', z))[1]+2))
#       )
#     } else {
#       mean_df <- rbind(mean_df,
#                        data.frame(energy = meanEnergy$energy,
#                                   energy_sd = meanEnergy$sd_energy,
#                                   wave = meanWaving$wave,
#                                   time = meanEnergy$time,
#                                   size = meanEnergy$size,
#                                   strategy = ifelse(subfolders[1] == "",
#                                                     substring(f, first = 4),
#                                                     substring(z,
#                                                               first = unlist(gregexpr(')', z))[1]+2))
#                        )
#       )
#     }
#   }
# }
# saveRDS(mean_df, "new_strategy_mean_df.rds")

#load in summary df ####

df <- readRDS("new_strategy_mean_df.rds")

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
    df$alpha[i] <- 0.00
    
    df$zeta[i] <- 0.00
    
    df$q[i] <- 1
  }
}

df$alpha <- as.numeric(df$alpha)
df$zeta <- as.numeric(df$zeta)
df$q <- as.numeric(df$q)


# plot baseline strategy ####

brl<-read.table("1) baseline (no game, refrac)/largebrstrat.txt", header=F, sep = ",")

eMax<-nrow(brl)
tides<-(length(brl))/(tSteps+1)

eLevels <- 0:(eMax-1)
brl<-cbind(eLevels, brl)

vDfLength <- paste("V",(length(brl)-1),sep="")

dataLong<-gather(data = brl, timestep, pWave, V1:all_of(vDfLength),factor_key = FALSE)
timestepNew <- rep_len(rep(1:(tSteps+1), each = eMax), length.out = nrow(dataLong))
dataLong$timestep <- as.numeric(timestepNew)
names(dataLong)<-c("energy","timestep","pWave")

dataLong$tide <- rep(1:tides, each = (eMax*(tSteps+1)))

dataLong <- subset(dataLong, !timestep %in% c(tSteps, tSteps+1))

dataLong$pWave <- as.numeric(dataLong$pWave)

midTideL <- subset(dataLong, tide == (tides/2))

midTideL$bin <- ifelse(midTideL$pWave<0.45, 0, 
                       ifelse(midTideL$pWave<0.55, 0.5, 1))

midTideL$half_line <- ifelse(midTideL$pWave > 0.5, 1, 0)

e_switch <- midTideL[midTideL$pWave > 0.5,]

for(i in 1:99) {
  if(i == 1) {
    es <- c(min(e_switch$energy[e_switch$timestep == i]))
  } else {
    es <- append(es, min(e_switch$energy[e_switch$timestep == i]))
  }
}

es_df <- data.frame(time = 1:99,
                    energy = es)

for(i in 2:nrow(es_df)) {
  if(es_df$energy[i] != es_df$energy[i-1]) {
    es_df <- rbind(es_df[1:i-1,], data.frame(time = es_df$time[i-1],
                                            energy = es_df$energy[i]),
                   es_df[i:nrow(es_df),])
  }
}



es_df <- rbind(data.frame(time = 0,
                          energy = 43),
               es_df, 
               data.frame(time = es_df$time[nrow(es_df)],
                          energy = es_df$energy[nrow(es_df)]),
               data.frame(time = 100,
                          energy = 47))
es_df$time[(nrow(es_df)-1)] <- es_df$time[(nrow(es_df)-1)]-1

a1<-ggplot()+
  geom_tile(data=midTideL, aes(x = timestep, y = energy, fill = factor(bin)))+
  geom_line(data=es_df, aes(x = time, y = energy-0.5), linewidth = 2, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(0, tSteps, 25), 
                     limits = c(0,101),
                     expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,50, 10),
                     expand = c(-0.0092, 0.5))+
  scale_fill_manual(name = "Probability of \nwaving",
                    values = c("0" = "white", "0.5" = "goldenrod1", "1" = "goldenrod4"),
                    labels = c("0.0 - 0.45", "0.45 - 0.55", "0.55 - 1.0")) + 
  xlab("Time")+
  ylab("Energy level") +
  theme_classic(base_size = 40) +
  theme(legend.text = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.key.height = unit(1.35, 'cm'),
        legend.key.width = unit(1, "cm"))
a1
ggsave(plot = a1, "figures/q1.0/baseline_strategy.png", units = "px", height = 5200, width = 7980)

#plot new strategy diagrams ####

brl<-read.table("1) baseline (no game, refrac)/largebrstrat.txt", header=F, sep = ",")

eMax<-nrow(brl)
tides<-(length(brl))/(tSteps+1)

eLevels <- 0:(eMax-1)
brl<-cbind(eLevels, brl)

vDfLength <- paste("V",(length(brl)-1),sep="")

dataLong<-gather(data = brl, timestep, pWave, V1:all_of(vDfLength),factor_key = FALSE)
timestepNew <- rep_len(rep(1:(tSteps+1), each = eMax), length.out = nrow(dataLong))
dataLong$timestep <- as.numeric(timestepNew)
names(dataLong)<-c("energy","timestep","pWave")

dataLong$tide <- rep(1:tides, each = (eMax*(tSteps+1)))

dataLong <- subset(dataLong, !timestep %in% c(tSteps, tSteps+1))

dataLong$pWave <- as.numeric(dataLong$pWave)

midTideL <- subset(dataLong, tide == (tides/2))

midTideL$bin <- ifelse(midTideL$pWave<0.45, 0, 
                       ifelse(midTideL$pWave<0.55, 0.5, 1))

e_switch <- midTideL[midTideL$pWave > 0.5,]

for(i in 1:99) {
  if(i == 1) {
    es <- c(min(e_switch$energy[e_switch$timestep == i]))
  } else {
    es <- append(es, min(e_switch$energy[e_switch$timestep == i]))
  }
}

es_df1 <- data.frame(time = 1:99,
                    energy = es)

for(i in 2:nrow(es_df1)) {
  if(es_df1$energy[i] != es_df1$energy[i-1]) {
    es_df1 <- rbind(es_df1[1:i-1,], data.frame(time = es_df1$time[i-1],
                                             energy = es_df1$energy[i]),
                   es_df1[i:nrow(es_df1),])
  }
}

es_df1 <- rbind(es_df1, 
               data.frame(time = es_df1$time[nrow(es_df1)],
                          energy = es_df1$energy[nrow(es_df1)]))
es_df1$time[(nrow(es_df1)-1)] <- es_df1$time[(nrow(es_df1)-1)]-1


brl<-read.table("2) game/largebrstrat.txt", header=F, sep = ",")

eMax<-nrow(brl)
tides<-(length(brl))/(tSteps+1)

eLevels <- 0:(eMax-1)
brl<-cbind(eLevels, brl)

vDfLength <- paste("V",(length(brl)-1),sep="")

dataLong<-gather(data = brl, timestep, pWave, V1:all_of(vDfLength),factor_key = FALSE)
timestepNew <- rep_len(rep(1:(tSteps+1), each = eMax), length.out = nrow(dataLong))
dataLong$timestep <- as.numeric(timestepNew)
names(dataLong)<-c("energy","timestep","pWave")

dataLong$tide <- rep(1:tides, each = (eMax*(tSteps+1)))

dataLong <- subset(dataLong, !timestep %in% c(tSteps, tSteps+1))

dataLong$pWave <- as.numeric(dataLong$pWave)

midTideL <- subset(dataLong, tide == (tides/2))

midTideL$bin <- ifelse(midTideL$pWave<0.45, 0, 
                       ifelse(midTideL$pWave<0.55, 0.5, 1))

e_switch <- midTideL[midTideL$pWave > 0.5,]

for(i in 1:99) {
  if(i == 1) {
    es <- c(min(e_switch$energy[e_switch$timestep == i]))
  } else {
    es <- append(es, min(e_switch$energy[e_switch$timestep == i]))
  }
}

es_df2 <- data.frame(time = 1:99,
                     energy = es)

for(i in 2:nrow(es_df2)) {
  if(es_df2$energy[i] != es_df2$energy[i-1]) {
    es_df2 <- rbind(es_df2[1:i-1,], data.frame(time = es_df2$time[i-1],
                                               energy = es_df2$energy[i]),
                    es_df2[i:nrow(es_df2),])
  }
}

es_df2 <- rbind(es_df2, 
                data.frame(time = es_df2$time[nrow(es_df2)],
                           energy = es_df2$energy[nrow(es_df2)]))

es_df2$time[(nrow(es_df2)-1)] <- es_df2$time[(nrow(es_df2)-1)]-1
es_df2 <- rbind(es_df2[1:101,],
                data.frame(time = 97,
                           energy = 47),
                es_df2[102:nrow(es_df2),])

brl<-read.table("6) changing pFemMin/largebrstrat.txt", header=F, sep = ",")

eMax<-nrow(brl)
tides<-(length(brl))/(tSteps+1)

eLevels <- 0:(eMax-1)
brl<-cbind(eLevels, brl)

vDfLength <- paste("V",(length(brl)-1),sep="")

dataLong<-gather(data = brl, timestep, pWave, V1:all_of(vDfLength),factor_key = FALSE)
timestepNew <- rep_len(rep(1:(tSteps+1), each = eMax), length.out = nrow(dataLong))
dataLong$timestep <- as.numeric(timestepNew)
names(dataLong)<-c("energy","timestep","pWave")

dataLong$tide <- rep(1:tides, each = (eMax*(tSteps+1)))

dataLong <- subset(dataLong, !timestep %in% c(tSteps, tSteps+1))

dataLong$pWave <- as.numeric(dataLong$pWave)

midTideL <- subset(dataLong, tide == (tides/2))

midTideL$bin <- ifelse(midTideL$pWave<0.45, 0, 
                       ifelse(midTideL$pWave<0.55, 0.5, 1))

e_switch <- midTideL[midTideL$pWave > 0.5,]

for(i in 1:99) {
  if(i == 1) {
    es <- c(min(e_switch$energy[e_switch$timestep == i]))
  } else {
    es <- append(es, min(e_switch$energy[e_switch$timestep == i]))
  }
}

es_df3 <- data.frame(time = 1:99,
                     energy = es)

for(i in 2:nrow(es_df3)) {
  if(es_df3$energy[i] != es_df3$energy[i-1]) {
    es_df3 <- rbind(es_df3[1:i-1,], data.frame(time = es_df3$time[i-1],
                                               energy = es_df3$energy[i]),
                    es_df3[i:nrow(es_df3),])
  }
}

es_df3 <- rbind(es_df3, 
                data.frame(time = es_df3$time[nrow(es_df3)],
                           energy = es_df3$energy[nrow(es_df3)]))
es_df3$time[(nrow(es_df3)-1)] <- es_df3$time[(nrow(es_df3)-1)]-1

es_df3 <- rbind(es_df3[1:101,],
                data.frame(time = 97,
                           energy = 47),
                es_df3[102:nrow(es_df2),])

strat_df <- data.frame(energy = c(es_df1$energy, es_df2$energy, es_df3$energy),
                       time = c(es_df1$time, es_df2$time, es_df3$time),
                       strategy = c(rep_len("Baseline", nrow(es_df1)),
                                    rep_len("Social game", nrow(es_df2)),
                                    rep_len("Variable *f*", nrow(es_df3)))
)

strat_df$strategy <- factor(strat_df$strategy,
                            levels = c("Variable *f*", 
                                       "Social game", 
                                       "Baseline"))
#manual jitter
strat_df$energy <- ifelse(strat_df$strategy == "Variable *f*", strat_df$energy-0.3,
                          ifelse(strat_df$strategy == "Social game", strat_df$energy+0.3, strat_df$energy))


cols4 <- c("goldenrod1","navyblue","green4")
#linetypes <- c("solid", "dashed", "dotdash")
all_strat <- ggplot(data = strat_df) + 
  geom_ribbon(aes(x = time, ymin = energy-0.5, ymax = 50, group = strategy),
              fill = "lightgrey", alpha = 0.5)+
  geom_line(aes(x = time, y = energy-0.5, colour = strategy),
            linewidth = 4) +
  scale_y_continuous(name = "Energy level", limits = c(0, 50), breaks = seq(0, 50, 10),
                     expand = c(0, 0)) + 
  scale_x_continuous(name = "Time", limits = c(0, 101), breaks = seq(0, 100, 25),
                     expand = c(0, 0)) + 
  scale_colour_manual(name = "Strategy",
                      values = cols4,
                      breaks = unique(strat_df$strategy)) +
  scale_fill_manual(name = "Strategy",
                    values = cols4,
                    breaks = unique(strat_df$strategy)) +
  theme_classic(base_size = 40) + 
  theme(legend.text = element_markdown(size = 35),
        legend.title = element_text(size = 35),
        legend.key.height = unit(1.35, 'cm'),
        legend.key.width = unit(1, "cm"))


ggsave(plot = all_strat, "figures/q1.0/all_strat.png", units = "px", height = 5200, width = 7980)


#plot prop waving for first three ####

cols2 <- c("goldenrod1","navyblue","green4")



p1 <- ggplot(data = df[df$strategy == "baseline (no game, refrac)" | df$strategy == "game" | df$strategy == "changing pFemMin",],
             aes(x = time-3060, y = wave, colour = factor(strategy))) + 
  geom_smooth(se = F, linewidth = 3, show.legend = T) + 
  scale_x_continuous(name = "Time",
                     expand = c(0.01, 0)) +
  scale_y_continuous(name = "Prop. waving", limits = c(0, 0.2),
                     breaks = seq(0, 0.2, 0.05),
                     expand = c(0, 0)) +
  scale_colour_manual(name = "Strategy", values = cols2,
                      breaks = c("baseline (no game, refrac)", "game", "changing pFemMin"),
                      labels = c("Baseline", "Social game","Variable *f*")) +
  theme_classic(base_size = 35) + 
  theme(legend.title = element_text(size = 30),
        legend.text = element_markdown(size = 25),
        legend.key.width = unit(1.1, "cm"))
p1

#plot energy for first three ####

p2 <- ggplot(data = df[df$strategy == "baseline (no game, refrac)" | df$strategy == "game" | df$strategy == "changing pFemMin",],
             aes(x = time-3060, y = energy, colour = factor(strategy))) + 
  geom_line(linewidth = 3, show.legend = F) + 
  scale_x_continuous(name = "",
                     expand = c(0.01, 0)) + 
  scale_y_continuous(name = "Energy level", limits = c(25, 40),
                     breaks = seq(25, 40, 5),
                     expand = c(0, 0)) +
  scale_colour_manual(name = "Strategy", values = cols2,
                      breaks = c("baseline (no game, refrac)", "game", "changing pFemMin"),
                      labels = c("Baseline","Social game","Variable *f*")) +
  theme_classic(base_size = 35) + 
  theme(legend.title = element_text(size = 30),
        legend.text = element_markdown(size = 25),
        legend.key.width = unit(1.1, "cm"))
p2

pMateG <- unlist(read.table("2) game/largeFinalpMate.txt", header=F, sep = ",")[1,]) %>%
  .[-c(which(. == "HT" | . == 0), is.na(.))]
pMateF <- unlist(read.table("6) changing pFemMin/largeFinalpMate.txt", header=F, sep = ",")[1,]) %>%
  .[-c(which(. == "HT" | . == 0), is.na(.))]

pMate_df <- data.frame(
  mate = c(as.numeric(pMateF), as.numeric(pMateG), rep_len(0.083, length(pMateG))), 
  time = rep_len((1:tSteps), (tSteps*tides)*3), 
  tide = rep_len(rep(1:tides, each = tSteps), (tSteps*tides)*3),
  strategy = rep(c("Variable *f*", "Social game", "Baseline"), each = length(pMateG))
) %>%
  filter(tide == tides/2)

pm1 <- ggplot(data = pMate_df, aes(x = time, y = mate, colour = factor(strategy, 
                                                                       levels = c("Baseline", "Social game", "Variable *f*")))) + 
  geom_line(linewidth = 3, show.legend = F) + 
  scale_x_continuous(name = "", limits = c(0, 100), breaks = seq(0, 100, 25),
                     expand = c(0, 0)) + 
  scale_y_continuous(name = "P(*m*)", limits = c(0.06, 0.09), breaks = seq(0.06, 0.09, 0.01),
                     expand = c(0, 0)) +
  scale_color_manual(name = "Strategy", values = cols2) +
  theme_classic(base_size = 35) + 
  theme(axis.title.y = element_markdown(),
        legend.text = element_markdown(size = 42),
        legend.title = element_text(size = 45),
        legend.key.width = unit(2, "cm"),
        axis.text.y = element_text(size = 30))
pm1

#plot proportion in timeout for first three####
# for(f in folders[c(1,2,6)]) {
#   z <- ""
#   
#   timeout <- read.table(paste(f, "/", z, "/simTimeout.txt", sep = ""), sep = ",", header = F) %>%
#     .[,-HT_cols]
#   
#   prop_tau <- colMeans(timeout)
#   
#   rm(timeout)
#   
#   if(f == folders[1]) {
#     t_df_all <- data.frame( overall_tStep = 1:length(prop_tau),
#                         prop_tau,
#                         strategy = rep_len(ifelse(z == "",
#                                                   substring(f, first = 4),
#                                                   substring(z,
#                                                             first = unlist(gregexpr(')', z))[1]+2)),
#                                            length(prop_tau))
#     )
#   } else {
#     t_df_all <- rbind(t_df_all, data.frame( overall_tStep = 1:length(prop_tau),
#                                     prop_tau,
#                                     strategy = rep_len(ifelse(z == "",
#                                                               substring(f, first = 4),
#                                                               substring(z,
#                                                                         first = unlist(gregexpr(')', z))[1]+2)),
#                                                        length(prop_tau))
#                         )
#     )
#   }
# }
# 
# t_df <- subset(t_df_all, overall_tStep >= 3060 & overall_tStep < 3160)
# t_df$strategy <- ifelse(t_df$strategy == "game", "Social game", 
#                         ifelse(t_df$strategy == "baseline (no game, refrac)", "Baseline", "Variable *f*"))
# 
# pt1 <- ggplot(data = t_df, aes(x = overall_tStep-3060, y = prop_tau, colour = factor(strategy, 
#                                                                                      levels = c("Baseline", "Social game", "Variable *f*")))) + 
#   geom_smooth(se = F, linewidth = 3) + 
#   scale_x_continuous(name = "Time", limits = c(0, 100), breaks = seq(0, 100, 25),
#                      expand = c(0, 0)) + 
#   scale_y_continuous(name = "Prop. in refractory period", limits = c(0.5, 0.65), breaks = seq(0.5, 0.65, 0.05),
#                      expand = c(0, 0)) +
#   scale_color_manual(name = "Strategy", values = cols2) +
#   theme_classic(base_size = 35) +
#   theme(legend.text = element_markdown(size = 42),
#         legend.title = element_text(size = 45),
#         legend.key.width = unit(2, "cm"),
#         axis.title.y = element_text(size = 28),
#         axis.text.y = element_text(size = 20))
# pt1

#plot grid ####

top_row <- ggarrange(pm1, p2, ncol = 2,
                     labels = c("a)", "b)"), font.label = list(size = 45))

bottom_row <- ggarrange(NULL, p1, NULL, ncol = 3, 
                        labels = c("", "c)", ""),
                        widths = c(1,3,1), font.label = list(size = 45))

gridPlot <- ggarrange(top_row, bottom_row, ncol = 1, 
                      hjust = 0)

ggsave(plot = gridPlot, "figures/q1.0/grid.png", units = "px", height = 4320, width = 7980)




#plot matrices of twoMorph plots ####

cFun <- colorRampPalette(c("orange","purple4"))
cols <- cFun(2)

plotList <- list()
counter <- 1

for(aa in unique(df$alpha)[c(1,3,2,4)]) {
  for(zz in unique(df$zeta)[c(1,3,2,4)]) {
    
    if(aa == 0 & zz == 0) {
      useDf <- df[df$strategy == "game",]
      legendBool <- F
    } else {
      useDf <- df[df$q == 0.5 & df$alpha == aa & df$zeta == zz,]
      legendBool <- T
    }
    
    if(zz == 0) {
      y_breaks <- seq(0, 0.5, 0.1)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.00) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time-3060, y = wave, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(0, 0.5), breaks = y_breaks) +
      scale_colour_manual(values = cols, name = "Size") +
      theme_classic(base_size = 50) +
      theme(legend.text=element_text(size=55), 
            legend.title = element_text(size = 55))
    
    plotList[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot <- ggarrange(
  plotList[[13]],plotList[[14]],plotList[[15]],plotList[[16]],
  plotList[[9]],plotList[[10]],plotList[[11]],plotList[[12]],
  plotList[[5]],plotList[[6]],plotList[[7]],plotList[[8]],
  plotList[[1]],plotList[[2]],plotList[[3]],plotList[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1.25, 1, 1, 1))
all_plot

ggsave(plot = all_plot, "figures/q0.5 waving.png", units = "px", width = 7980, height = 7980)


plotList1_2 <- list()
counter <- 1

for(aa in unique(df$alpha)[c(1,3,2,4)]) {
  for(zz in unique(df$zeta)[c(1,3,2,4)]) {
    
    if(aa == 0 & zz == 0) {
      useDf <- df[df$strategy == "game",]
      legendBool <- F
    } else {
      useDf <- df[df$q == 0.25 & df$alpha == aa & df$zeta == zz,]
      legendBool <- T
    }
    
    if(zz == 0) {
      y_breaks <- seq(0, 0.5, 0.1)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time-3060, y = wave, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(0, 0.5), breaks = y_breaks) +
      scale_colour_manual(values = cols, name = "Size") +
      theme_classic(base_size = 50) +
      theme(legend.text=element_text(size=55), 
            legend.title = element_text(size = 55))
    
    plotList1_2[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot1_2 <- ggarrange(
  plotList1_2[[13]],plotList1_2[[14]],plotList1_2[[15]],plotList1_2[[16]],
  plotList1_2[[9]],plotList1_2[[10]],plotList1_2[[11]],plotList1_2[[12]],
  plotList1_2[[5]],plotList1_2[[6]],plotList1_2[[7]],plotList1_2[[8]],
  plotList1_2[[1]],plotList1_2[[2]],plotList1_2[[3]],plotList1_2[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1.25, 1, 1, 1))
all_plot

ggsave(plot = all_plot1_2, "figures/q0.25 waving.png", units = "px", width = 7980, height = 7980)

plotList1_3 <- list()
counter <- 1

for(aa in unique(df$alpha)[c(1,3,2,4)]) {
  for(zz in unique(df$zeta)[c(1,3,2,4)]) {
    
    if(aa == 0 & zz == 0) {
      useDf <- df[df$strategy == "game",]
      legendBool <- F
    } else {
      useDf <- df[df$q == 0.75 & df$alpha == aa & df$zeta == zz,]
      legendBool <- T
    }
    
    if(zz == 0) {
      y_breaks <- seq(0, 0.5, 0.1)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time-3060, y = wave, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(0, 0.5), breaks = y_breaks) +
      scale_colour_manual(values = cols, name = "Size") +
      theme_classic(base_size = 50) +
      theme(legend.text=element_text(size=55), 
            legend.title = element_text(size = 55))
    
    plotList1_3[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot1_3 <- ggarrange(
  plotList1_3[[13]],plotList1_3[[14]],plotList1_3[[15]],plotList1_3[[16]],
  plotList1_3[[9]],plotList1_3[[10]],plotList1_3[[11]],plotList1_3[[12]],
  plotList1_3[[5]],plotList1_3[[6]],plotList1_3[[7]],plotList1_3[[8]],
  plotList1_3[[1]],plotList1_3[[2]],plotList1_3[[3]],plotList1_3[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1.25, 1, 1, 1))
#all_plot

ggsave(plot = all_plot1_3, "figures/q0.75 waving.png", units = "px", width = 7980, height = 7980)
#plotting energy levels ####

plotList2 <- list()
counter <- 1

for(aa in unique(df$alpha)[c(1,3,2,4)]) {
  for(zz in unique(df$zeta)[c(1,3,2,4)]) {
    
    if(aa == 0 & zz == 0) {
      useDf <- df[df$strategy == "game",]
      legendBool <- F
    } else {
      useDf <- df[df$q == 0.5 & df$alpha == aa & df$zeta == zz,]
      legendBool <- T
    }
    
    if(zz == 0) {
      y_breaks <- seq(25, 40, 5)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf) + 
      geom_smooth(aes(x = time-3060, y = energy, colour = factor(size)),
                  se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(25, 40), breaks = y_breaks) +
      scale_colour_manual(values = cols, name = "Size") +
      scale_fill_manual(values = cols, name = "Size") +
      theme_classic(base_size = 50) +
      theme(legend.text=element_text(size=55), 
            legend.title = element_text(size = 55))
    
    plotList2[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot2 <- ggarrange(
  plotList2[[13]],plotList2[[14]],plotList2[[15]],plotList2[[16]],
  plotList2[[9]],plotList2[[10]],plotList2[[11]],plotList2[[12]],
  plotList2[[5]],plotList2[[6]],plotList2[[7]],plotList2[[8]],
  plotList2[[1]],plotList2[[2]],plotList2[[3]],plotList2[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1.25, 1, 1, 1))
all_plot2

ggsave(plot = all_plot2, "figures/q0.5 energy.png", units = "px", width = 7980, height = 7980)

plotList2_2 <- list()
counter <- 1

for(aa in unique(df$alpha)[c(1,3,2,4)]) {
  for(zz in unique(df$zeta)[c(1,3,2,4)]) {
    
    if(aa == 0 & zz == 0) {
      useDf <- df[df$strategy == "game",]
      legendBool <- F
    } else {
      useDf <- df[df$q == 0.25 & df$alpha == aa & df$zeta == zz,]
      legendBool <- T
    }
    
    if(zz == 0) {
      y_breaks <- seq(25, 40, 5)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time-3060, y = energy, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(25, 40), breaks = y_breaks) +
      scale_colour_manual(values = cols, name = "Size") +
      theme_classic(base_size = 50) +
      theme(legend.text=element_text(size=55), 
            legend.title = element_text(size = 55))
    
    plotList2_2[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot2_2 <- ggarrange(
  plotList2_2[[13]],plotList2_2[[14]],plotList2_2[[15]],plotList2_2[[16]],
  plotList2_2[[9]],plotList2_2[[10]],plotList2_2[[11]],plotList2_2[[12]],
  plotList2_2[[5]],plotList2_2[[6]],plotList2_2[[7]],plotList2_2[[8]],
  plotList2_2[[1]],plotList2_2[[2]],plotList2_2[[3]],plotList2_2[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1.25, 1, 1, 1))
#all_plot2

ggsave(plot = all_plot2_2, "figures/q0.25 energy.png", units = "px", width = 7980, height = 7980)

plotList2_3 <- list()
counter <- 1

for(aa in unique(df$alpha)[c(1,3,2,4)]) {
  for(zz in unique(df$zeta)[c(1,3,2,4)]) {
    
    if(aa == 0 & zz == 0) {
      useDf <- df[df$strategy == "game",]
      legendBool <- F
    } else {
      useDf <- df[df$q == 0.75 & df$alpha == aa & df$zeta == zz,]
      legendBool <- T
    }
    
    if(zz == 0) {
      y_breaks <- seq(25, 40, 5)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time-3060, y = energy, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(25, 40), breaks = y_breaks) +
      scale_colour_manual(values = cols, name = "Size") +
      theme_classic(base_size = 50) +
      theme(legend.text=element_text(size=55), 
            legend.title = element_text(size = 55))
    
    plotList2_3[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot2_3 <- ggarrange(
  plotList2_3[[13]],plotList2_3[[14]],plotList2_3[[15]],plotList2_3[[16]],
  plotList2_3[[9]],plotList2_3[[10]],plotList2_3[[11]],plotList2_3[[12]],
  plotList2_3[[5]],plotList2_3[[6]],plotList2_3[[7]],plotList2_3[[8]],
  plotList2_3[[1]],plotList2_3[[2]],plotList2_3[[3]],plotList2_3[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1.25, 1, 1, 1))
#all_plot2

ggsave(plot = all_plot2_3, "figures/q0.75 energy.png", units = "px", width = 7980, height = 7980)
#extract mean number of matings ####

# pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
#                      max = length(folders), # Maximum value of the progress bar
#                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                      width = 50,   # Progress bar width. Defaults to getOption("width")
#                      char = "=")   # Character used to create the bar
# 
# 
# 
# for(f in folders) {
#   setTxtProgressBar(pb, which(folders == f))
#   if(f %in% folders[3:5]) {
#     subfolders <- list.files(f)
#   } else {
#     subfolders <- c("")
#   }
# 
#   for(z in subfolders) {
#     smallMating <- c()
#     largeMating <- c()
# 
#     sizes <- unlist(read.table(paste(f, "/", z, "/simSizes.txt", sep = ""), sep = ",", header=F)[1,])
# 
#     simMating <- read.table(paste(f, "/", z, "/simMating.txt", sep = ""), sep = ",", header = F)
# 
#     for(i in 1:length(sizes)) {
#       if(sizes[i] == 0) {
#         smallMating <- append(smallMating, simMating[i, length(simMating)-1])
#       } else {
#         largeMating <- append(largeMating, simMating[i, length(simMating)-1])
#       }
#     }
# 
#     df_z <- data.frame(size = c("Small", "Large"),
#                        mating = c(mean(smallMating), mean(largeMating)),
#                        sd_mating = c(sd(smallMating), sd(largeMating)),
#                        strategy = rep_len(ifelse(subfolders[1] == "",
#                                                  substring(f, first = 4),
#                                                  substring(z,
#                                                            first = unlist(gregexpr(')', z))[1]+2)),
#                                           2)
#     )
# 
#     if(f == folders[1]) {
#       mating_df <- df_z
#     } else {
#       mating_df <- rbind(mating_df, df_z)
#     }
#   }
# }
# saveRDS(mating_df, "new_mating_df.rds")

#plot mean matings per size ####
mdf <- readRDS("new_mating_df.rds")

cols <- c("orange", "purple4")

for(i in 1:nrow(mdf)) {
  if(grepl("alpha",mdf$strategy[i])) {
    mdf$alpha[i] <- substring(mdf$strategy[i], unlist(gregexpr('ha', mdf$strategy[i]))[1]+2, 9)
    if(grepl(" ", mdf$alpha[i])) mdf$alpha[i] <- substring(mdf$alpha[i], 1, 3)
    
    mdf$zeta[i] <- substring(mdf$strategy[i], nchar(mdf$strategy[i])-3)
    if(grepl("a", mdf$zeta[i])) mdf$zeta[i] <- substring(mdf$zeta[i], 2)
    
    mdf$q[i] <- 0.5
    
  } else if(grepl("q", mdf$strategy[i])) {
    mdf$alpha[i] <- substring(mdf$strategy[i], unlist(gregexpr('a', mdf$strategy[i]))[1]+1, 11)
    if(grepl(" ", mdf$alpha[i])) mdf$alpha[i] <- substring(mdf$alpha[i], 1, 3)
    
    mdf$zeta[i] <- substring(mdf$strategy[i], nchar(mdf$strategy[i])-3)
    if(grepl("z", mdf$zeta[i])) mdf$zeta[i] <- substring(mdf$zeta[i], 2)
    
    mdf$q[i] <- substring(mdf$strategy[i], 2, 5)
  } else {
    mdf$alpha[i] <- 0.00
    
    mdf$zeta[i] <- 0.00
    
    mdf$q[i] <- 1
  }
}

mdf$alpha <- as.numeric(mdf$alpha)
mdf$zeta <- as.numeric(mdf$zeta)
mdf$q <- as.numeric(mdf$q)

plotList3 <- list()
counter <- 1

for(aa in unique(df$alpha)[c(1,3,2,4)]) {
  for(zz in unique(df$zeta)[c(1,3,2,4)]) {
    
    if(aa == 0 & zz == 0) {
      useDf <- mdf[mdf$strategy == "game",]
    } else {
      useDf <- mdf[mdf$q == 0.5 & mdf$alpha == aa & mdf$zeta == zz,]
    }
    
    if(zz == 0) {
      y_breaks <- seq(0, 90, 15)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- c("Large", "Small")
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf) + 
      geom_segment(aes(x = "Large", xend = "Small",
                       y = mating[size == "Large"], yend = mating[size == "Small"]),
                   colour = "grey", linewidth = 3) + 
      geom_point(aes(x = factor(size), y = mating, colour = factor(size)), 
                 show.legend = F, size = 8) +  
      geom_segment(aes(x = factor(size), xend = factor(size),
                       y = mating - sd_mating, yend = mating + sd_mating, 
                       colour = factor(size)),
                   linewidth = 2.5, show.legend = F) + 
      scale_x_discrete(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(0, 90), breaks = y_breaks) + 
      scale_colour_manual(name = "Size", values = cols) +
      theme_classic(base_size = 50) + 
      theme(legend.text = element_text(size = 55),
            legend.title = element_text(size = 55))
    plotList3[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot3 <- ggarrange(
  plotList3[[13]],plotList3[[14]],plotList3[[15]],plotList3[[16]],
  plotList3[[9]],plotList3[[10]],plotList3[[11]],plotList3[[12]],
  plotList3[[5]],plotList3[[6]],plotList3[[7]],plotList3[[8]],
  plotList3[[1]],plotList3[[2]],plotList3[[3]],plotList3[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1, 1, 1, 1))
#all_plot3

ggsave(plot = all_plot3, "figures/q0.5 mating.png", units = "px", width = 7980, height = 7980)

plotList3_2 <- list()
counter <- 1

for(aa in unique(df$alpha)[c(1,3,2,4)]) {
  for(zz in unique(df$zeta)[c(1,3,2,4)]) {
    
    if(aa == 0 & zz == 0) {
      useDf <- mdf[mdf$strategy == "game",]
    } else {
      useDf <- mdf[mdf$q == 0.25 & mdf$alpha == aa & mdf$zeta == zz,]
    }
    
    if(zz == 0) {
      y_breaks <- seq(0, 90, 15)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- c("Large", "Small")
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf) + 
      geom_segment(aes(x = "Large", xend = "Small",
                       y = mating[size == "Large"], yend = mating[size == "Small"]),
                   colour = "grey", linewidth = 3) + 
      geom_point(aes(x = factor(size), y = mating, colour = factor(size)), 
                 show.legend = F, size = 8) +  
      geom_segment(aes(x = factor(size), xend = factor(size),
                       y = mating - sd_mating, yend = mating + sd_mating, 
                       colour = factor(size)),
                   linewidth = 2.5, show.legend = F) + 
      scale_x_discrete(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(0, 90), breaks = y_breaks) + 
      scale_colour_manual(name = "Size", values = cols) +
      theme_classic(base_size = 50) + 
      theme(legend.text = element_text(size = 55),
            legend.title = element_text(size = 55))
    plotList3_2[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot3_2 <- ggarrange(
  plotList3_2[[13]],plotList3_2[[14]],plotList3_2[[15]],plotList3_2[[16]],
  plotList3_2[[9]],plotList3_2[[10]],plotList3_2[[11]],plotList3_2[[12]],
  plotList3_2[[5]],plotList3_2[[6]],plotList3_2[[7]],plotList3_2[[8]],
  plotList3_2[[1]],plotList3_2[[2]],plotList3_2[[3]],plotList3_2[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1, 1, 1, 1))
#all_plot3

ggsave(plot = all_plot3_2, "figures/q0.25 mating.png", units = "px", width = 7980, height = 7980)

plotList3_3 <- list()
counter <- 1

for(aa in unique(df$alpha)[c(1,3,2,4)]) {
  for(zz in unique(df$zeta)[c(1,3,2,4)]) {
    
    if(aa == 0 & zz == 0) {
      useDf <- mdf[mdf$strategy == "game",]
    } else {
      useDf <- mdf[mdf$q == 0.75 & mdf$alpha == aa & mdf$zeta == zz,]
    }
    
    if(zz == 0) {
      y_breaks <- seq(0, 90, 15)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- c("Large", "Small")
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf) + 
      geom_segment(aes(x = "Large", xend = "Small",
                       y = mating[size == "Large"], yend = mating[size == "Small"]),
                   colour = "grey", linewidth = 3) + 
      geom_point(aes(x = factor(size), y = mating, colour = factor(size)), 
                 show.legend = F, size = 8) +  
      geom_segment(aes(x = factor(size), xend = factor(size),
                       y = mating - sd_mating, yend = mating + sd_mating, 
                       colour = factor(size)),
                   linewidth = 2.5, show.legend = F) + 
      scale_x_discrete(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(0, 90), breaks = y_breaks) + 
      scale_colour_manual(name = "Size", values = cols) +
      theme_classic(base_size = 50) + 
      theme(legend.text = element_text(size = 55),
            legend.title = element_text(size = 55))
    plotList3_3[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot3_3 <- ggarrange(
  plotList3_3[[13]],plotList3_3[[14]],plotList3_3[[15]],plotList3_3[[16]],
  plotList3_3[[9]],plotList3_3[[10]],plotList3_3[[11]],plotList3_3[[12]],
  plotList3_3[[5]],plotList3_3[[6]],plotList3_3[[7]],plotList3_3[[8]],
  plotList3_3[[1]],plotList3_3[[2]],plotList3_3[[3]],plotList3_3[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1, 1, 1, 1))
#all_plot3_3

ggsave(plot = all_plot3_3, "figures/q0.75 mating.png", units = "px", width = 7980, height = 7980)


#extract mortality curves ####

# pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
#                      max = length(folders), # Maximum value of the progress bar
#                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                      width = 50,   # Progress bar width. Defaults to getOption("width")
#                      char = "=")   # Character used to create the bar
# 
# 
# for(f in folders) {
#   setTxtProgressBar(pb, which(folders == f))
#   if(f %in% folders[3:5]) {
#     subfolders <- list.files(f)
#   } else {
#     subfolders <- c("")
#   }
# 
#   for(z in subfolders) {
# 
#     sizes <- unlist(read.table(paste(f, "/", z, "/simSizes.txt", sep = ""), sep = ",", header=F)[1,])
# 
#     alive <- read.table(paste(f, "/", z, "/simAlive.txt", sep = ""), sep = ",", header = F) %>%
#       .[,-HT_cols]
# 
#     small_indices <- which(sizes == 0)
#     large_indices <- which(sizes == 1)
# 
#     small_prop <- colMeans(alive[small_indices, seq(1, length(alive), 101)])
#     large_prop <- colMeans(alive[large_indices, seq(1, length(alive), 101)])
# 
#     rm(alive)
# 
#     a_df <- data.frame(
#       overall_tStep = rep_len(1:length(small_prop), length(small_prop)*2),
#       size = rep(c("Small", "Large"), each = length(small_prop)),
#       prop = c(small_prop, large_prop),
#       strategy = rep_len(ifelse(subfolders[1] == "",
#                                 substring(f, first = 4),
#                                 substring(z,
#                                           first = unlist(gregexpr(')', z))[1]+2)),
#                          length(small_prop)*2)
#     )
# 
#     if(f == folders[1]) {
#       alive_df <- a_df
#     } else {
#       alive_df <- rbind(alive_df, a_df)
#     }
#   }
# }
# saveRDS(alive_df, "new_alive_df.rds")

#plot mortality curves ####

adf <- readRDS("new_alive_df.rds")

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(adf), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for(i in 1:nrow(adf)) {
  if(grepl("alpha",adf$strategy[i])) {
    adf$alpha[i] <- substring(adf$strategy[i], unlist(gregexpr('ha', adf$strategy[i]))[1]+2, 9)
    if(grepl(" ", adf$alpha[i])) adf$alpha[i] <- substring(adf$alpha[i], 1, 3)

    adf$zeta[i] <- substring(adf$strategy[i], nchar(adf$strategy[i])-3)
    if(grepl("a", adf$zeta[i])) adf$zeta[i] <- substring(adf$zeta[i], 2)

    adf$q[i] <- 0.5

  } else if(grepl("q", adf$strategy[i])) {
    adf$alpha[i] <- substring(adf$strategy[i], unlist(gregexpr('a', adf$strategy[i]))[1]+1, 11)
    if(grepl(" ", adf$alpha[i])) adf$alpha[i] <- substring(adf$alpha[i], 1, 3)

    adf$zeta[i] <- substring(adf$strategy[i], nchar(adf$strategy[i])-3)
    if(grepl("z", adf$zeta[i])) adf$zeta[i] <- substring(adf$zeta[i], 2)

    adf$q[i] <- substring(adf$strategy[i], 2, 5)
  } else {
    adf$alpha[i] <- 0.00

    adf$zeta[i] <- 0.00

    adf$q[i] <- 1
  }
  setTxtProgressBar(pb, i)
}

adf$alpha <- as.numeric(adf$alpha)
adf$zeta <- as.numeric(adf$zeta)
adf$q <- as.numeric(adf$q)

plotList4 <- list()
counter <- 1

for(aa in unique(adf$alpha)[c(1,3,2,4)]) {
  for(zz in unique(adf$zeta)[c(1,3,2,4)]) {

    if(aa == 0 & zz == 0) {
      useDf <- adf[adf$strategy == "game",]
    } else {
      useDf <- adf[adf$q == 0.5 & adf$alpha == aa & adf$zeta == zz,]
    }

    if(zz == 0) {
      y_breaks <- seq(0.8, 1, 0.05)
    } else {
      y_breaks <- NULL
    }

    if(aa == 0.0) {
      x_breaks <- seq(0, 60, 20)
    } else {
      x_breaks <- NULL
    }

    p <- ggplot(data = useDf) +
      geom_line(aes(x = overall_tStep, y = prop, colour = factor(size)),
                linewidth = 2) +
      scale_x_continuous(name = "", breaks = x_breaks) +
      scale_y_continuous(name = "", limits = c(0.8, 1), breaks = y_breaks) +
      scale_colour_manual(name = "Size", values = cols) +
      theme_classic(base_size = 50) +
      theme(legend.text = element_text(size = 55),
            legend.title = element_text(size = 55))
    plotList4[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot4 <- ggarrange(
  plotList4[[13]],plotList4[[14]],plotList4[[15]],plotList4[[16]],
  plotList4[[9]],plotList4[[10]],plotList4[[11]],plotList4[[12]],
  plotList4[[5]],plotList4[[6]],plotList4[[7]],plotList4[[8]],
  plotList4[[1]],plotList4[[2]],plotList4[[3]],plotList4[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1.25,1,1,1))
#all_plot4

ggsave(plot = all_plot4, "figures/q0.5 mort.png", units = "px", width = 7980, height = 7980)

plotList4_2 <- list()
counter <- 1

for(aa in unique(adf$alpha)[c(1,3,2,4)]) {
  for(zz in unique(adf$zeta)[c(1,3,2,4)]) {

    if(aa == 0 & zz == 0) {
      useDf <- adf[adf$strategy == "game",]
    } else {
      useDf <- adf[adf$q == 0.25 & adf$alpha == aa & adf$zeta == zz,]
    }

    if(zz == 0) {
      y_breaks <- seq(0.8, 1, 0.05)
    } else {
      y_breaks <- NULL
    }

    if(aa == 0.0) {
      x_breaks <- seq(0, 60, 20)
    } else {
      x_breaks <- NULL
    }

    p <- ggplot(data = useDf) +
      geom_line(aes(x = overall_tStep, y = prop, colour = factor(size)),
                linewidth = 2) +
      scale_x_continuous(name = "", breaks = x_breaks) +
      scale_y_continuous(name = "", limits = c(0.8, 1), breaks = y_breaks) +
      scale_colour_manual(name = "Size", values = cols) +
      theme_classic(base_size = 50) +
      theme(legend.text = element_text(size = 55),
            legend.title = element_text(size = 55))
    plotList4_2[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot4_2 <- ggarrange(
  plotList4_2[[13]],plotList4_2[[14]],plotList4_2[[15]],plotList4_2[[16]],
  plotList4_2[[9]],plotList4_2[[10]],plotList4_2[[11]],plotList4_2[[12]],
  plotList4_2[[5]],plotList4_2[[6]],plotList4_2[[7]],plotList4_2[[8]],
  plotList4_2[[1]],plotList4_2[[2]],plotList4_2[[3]],plotList4_2[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1.25,1,1,1))
#all_plot4_2

ggsave(plot = all_plot4_2, "figures/q0.25 mort.png", units = "px", width = 7980, height = 7980)

plotList4_3 <- list()
counter <- 1

for(aa in unique(adf$alpha)[c(1,3,2,4)]) {
  for(zz in unique(adf$zeta)[c(1,3,2,4)]) {

    if(aa == 0 & zz == 0) {
      useDf <- adf[adf$strategy == "game",]
    } else {
      useDf <- adf[adf$q == 0.75 & adf$alpha == aa & adf$zeta == zz,]
    }

    if(zz == 0) {
      y_breaks <- seq(0.8, 1, 0.05)
    } else {
      y_breaks <- NULL
    }

    if(aa == 0.0) {
      x_breaks <- seq(0, 60, 20)
    } else {
      x_breaks <- NULL
    }

    p <- ggplot(data = useDf) +
      geom_line(aes(x = overall_tStep, y = prop, colour = factor(size)),
                linewidth = 2) +
      scale_x_continuous(name = "", breaks = x_breaks) +
      scale_y_continuous(name = "", limits = c(0.8, 1), breaks = y_breaks) +
      scale_colour_manual(name = "Size", values = cols) +
      theme_classic(base_size = 50) +
      theme(legend.text = element_text(size = 55),
            legend.title = element_text(size = 55))
    plotList4_3[[counter]] <- p
    counter <- counter + 1
  }
}

all_plot4_3 <- ggarrange(
  plotList4_3[[13]],plotList4_3[[14]],plotList4_3[[15]],plotList4_3[[16]],
  plotList4_3[[9]],plotList4_3[[10]],plotList4_3[[11]],plotList4_3[[12]],
  plotList4_3[[5]],plotList4_3[[6]],plotList4_3[[7]],plotList4_3[[8]],
  plotList4_3[[1]],plotList4_3[[2]],plotList4_3[[3]],plotList4_3[[4]],
  ncol = 4, nrow = 4, common.legend = T, legend = "right",
  widths = c(1.25,1,1,1))
# all_plot4_2

ggsave(plot = all_plot4_3, "figures/q0.75 mort.png", units = "px", width = 7980, height = 7980)


#plot strategy for game and l/s ####

brl<-read.table("2) game/largebrstrat.txt", header=F, sep = ",")

eMax<-nrow(brl)
tides<-(length(brl))/(tSteps+1)

eLevels <- 0:(eMax-1)
brl<-cbind(eLevels, brl)

vDfLength <- paste("V",(length(brl)-1),sep="")

dataLong<-gather(data = brl, timestep, pWave, V1:all_of(vDfLength),factor_key = FALSE)
timestepNew <- rep_len(rep(1:(tSteps+1), each = eMax), length.out = nrow(dataLong))
dataLong$timestep <- as.numeric(timestepNew)
names(dataLong)<-c("energy","timestep","pWave")

dataLong$tide <- rep(1:tides, each = (eMax*(tSteps+1)))

dataLong <- subset(dataLong, !timestep %in% c(tSteps, tSteps+1))

dataLong$pWave <- as.numeric(dataLong$pWave)

midTideL <- subset(dataLong, tide == (tides/2))

midTideL$bin <- ifelse(midTideL$pWave<0.45, 0, 
                       ifelse(midTideL$pWave<0.55, 0.5, 1))

e_switch <- midTideL[midTideL$pWave > 0.5,]

for(i in 1:99) {
  if(i == 1) {
    es <- c(min(e_switch$energy[e_switch$timestep == i]))
  } else {
    es <- append(es, min(e_switch$energy[e_switch$timestep == i]))
  }
}

es_df1 <- data.frame(time = 1:99,
                     energy = es)

for(i in 2:nrow(es_df1)) {
  if(es_df1$energy[i] != es_df1$energy[i-1]) {
    es_df1 <- rbind(es_df1[1:i-1,], data.frame(time = es_df1$time[i-1],
                                               energy = es_df1$energy[i]),
                    es_df1[i:nrow(es_df1),])
  }
}

es_df1 <- rbind(es_df1, 
                data.frame(time = es_df1$time[nrow(es_df1)],
                           energy = es_df1$energy[nrow(es_df1)]))
es_df1$time[(nrow(es_df1)-1)] <- es_df1$time[(nrow(es_df1)-1)]-1


brl<-read.table("3) q0.5/3_9) alpha0.75 zeta0.75/largebrstrat.txt", header=F, sep = ",")

eMax<-nrow(brl)
tides<-(length(brl))/(tSteps+1)

eLevels <- 0:(eMax-1)
brl<-cbind(eLevels, brl)

vDfLength <- paste("V",(length(brl)-1),sep="")

dataLong<-gather(data = brl, timestep, pWave, V1:all_of(vDfLength),factor_key = FALSE)
timestepNew <- rep_len(rep(1:(tSteps+1), each = eMax), length.out = nrow(dataLong))
dataLong$timestep <- as.numeric(timestepNew)
names(dataLong)<-c("energy","timestep","pWave")

dataLong$tide <- rep(1:tides, each = (eMax*(tSteps+1)))

dataLong <- subset(dataLong, !timestep %in% c(tSteps, tSteps+1))

dataLong$pWave <- as.numeric(dataLong$pWave)

midTideL <- subset(dataLong, tide == (tides/2))

midTideL$bin <- ifelse(midTideL$pWave<0.45, 0, 
                       ifelse(midTideL$pWave<0.55, 0.5, 1))

e_switch <- midTideL[midTideL$pWave > 0.5,]

for(i in 1:99) {
  if(i == 1) {
    es <- c(min(e_switch$energy[e_switch$timestep == i]))
  } else {
    es <- append(es, min(e_switch$energy[e_switch$timestep == i]))
  }
}

es_df2 <- data.frame(time = 1:99,
                     energy = es)

for(i in 2:nrow(es_df2)) {
  if(es_df2$energy[i] != es_df2$energy[i-1]) {
    es_df2 <- rbind(es_df2[1:i-1,], data.frame(time = es_df2$time[i-1],
                                               energy = es_df2$energy[i]),
                    es_df2[i:nrow(es_df2),])
  }
}

es_df2 <- rbind(es_df2, 
                data.frame(time = es_df2$time[nrow(es_df2)],
                           energy = es_df2$energy[nrow(es_df2)]))

es_df2$time[(nrow(es_df2)-1)] <- es_df2$time[(nrow(es_df2)-1)]-1


brl<-read.table("3) q0.5/3_9) alpha0.75 zeta0.75/smallbrstrat.txt", header=F, sep = ",")

eMax<-nrow(brl)
tides<-(length(brl))/(tSteps+1)

eLevels <- 0:(eMax-1)
brl<-cbind(eLevels, brl)

vDfLength <- paste("V",(length(brl)-1),sep="")

dataLong<-gather(data = brl, timestep, pWave, V1:all_of(vDfLength),factor_key = FALSE)
timestepNew <- rep_len(rep(1:(tSteps+1), each = eMax), length.out = nrow(dataLong))
dataLong$timestep <- as.numeric(timestepNew)
names(dataLong)<-c("energy","timestep","pWave")

dataLong$tide <- rep(1:tides, each = (eMax*(tSteps+1)))

dataLong <- subset(dataLong, !timestep %in% c(tSteps, tSteps+1))

dataLong$pWave <- as.numeric(dataLong$pWave)

midTideL <- subset(dataLong, tide == (tides/2))

midTideL$bin <- ifelse(midTideL$pWave<0.45, 0, 
                       ifelse(midTideL$pWave<0.55, 0.5, 1))

e_switch <- midTideL[midTideL$pWave > 0.5,]

for(i in 1:99) {
  if(i == 1) {
    es <- c(min(e_switch$energy[e_switch$timestep == i]))
  } else {
    es <- append(es, min(e_switch$energy[e_switch$timestep == i]))
  }
}

es_df3 <- data.frame(time = 1:99,
                     energy = es)

for(i in 2:nrow(es_df3)) {
  if(es_df3$energy[i] != es_df3$energy[i-1]) {
    es_df3 <- rbind(es_df3[1:i-1,], data.frame(time = es_df3$time[i-1],
                                               energy = es_df3$energy[i]),
                    es_df3[i:nrow(es_df3),])
  }
}

es_df3 <- rbind(es_df3, 
                data.frame(time = es_df3$time[nrow(es_df3)],
                           energy = es_df3$energy[nrow(es_df3)]))
es_df3$time[(nrow(es_df3)-1)] <- es_df3$time[(nrow(es_df3)-1)]-1

es_df3 <- rbind(es_df3[1:102,],
                data.frame(time = 97,
                           energy = 46),
                es_df3[103:nrow(es_df3),])

strat_df <- data.frame(energy = c(es_df1$energy, es_df2$energy, es_df3$energy),
                       time = c(es_df1$time, es_df2$time, es_df3$time),
                       strategy = c(rep_len("Social game", nrow(es_df1)),
                                    rep_len("q = 0.5, Large", nrow(es_df2)),
                                    rep_len("q = 0.5, Small", nrow(es_df3)))
)

strat_df$strategy <- factor(strat_df$strategy,
                            levels = c("Social game",
                                       "q = 0.5, Large",
                                       "q = 0.5, Small"))
#manual jitter
strat_df$energy <- ifelse(strat_df$strategy == "q = 0.5, Large", strat_df$energy-0.3, strat_df$energy)


cols4 <- c("navyblue","orange","purple2")
#linetypes <- c("solid", "dashed", "dotdash")
all_strat <- ggplot(data = strat_df) + 
  geom_ribbon(aes(x = time, ymin = energy-0.5, ymax = 50, group = strategy),
              fill = "lightgrey", alpha = 0.5)+
  geom_line(aes(x = time, y = energy-0.5, colour = strategy),
            linewidth = 4) +
  scale_y_continuous(name = "Energy level", limits = c(0, 50), breaks = seq(0, 50, 10),
                     expand = c(0, 0)) + 
  scale_x_continuous(name = "Time", limits = c(0, 100), breaks = seq(0, 100, 25),
                     expand = c(0, 0)) + 
  scale_colour_manual(name = "Strategy",
                      values = cols4,
                      breaks = unique(strat_df$strategy)) +
  scale_fill_manual(name = "Strategy",
                    values = cols4,
                    breaks = unique(strat_df$strategy)) +
  theme_classic(base_size = 40) +
  theme(legend.text = element_markdown(size = 35),
        legend.title = element_text(size = 35),
        legend.key.height = unit(1.35, 'cm'),
        legend.key.width = unit(1, "cm"))
all_strat

ggsave(plot = all_strat, "figures/q1.0/ls_strat.png", units = "px", height = 5200, width = 7980)
