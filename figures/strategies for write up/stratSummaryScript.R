library(tidyverse)
library(ggpubr)
library(ggplotify)
library(officer)
library(rvg)
library(ggtext)

rm(list=ls())

if(dir.exists("E:/")) {
  setwd("E:/OneDrive - University of Exeter/Dynamic programming/0.6 C++/newDead/figures/strategies for write up")
} else {
  setwd("C:/Users/jw777/OneDrive - University of Exeter/Dynamic programming/0.6 C++/newDead/figures/strategies for write up")
}

folders <- list.files()[1:7]

timesteps <- 101
tSteps <- 101
tides <- 60

# save how strategy changes the mean waving proportion and energy level ####

# pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
#                      max = length(folders), # Maximum value of the progress bar
#                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                      width = 50,   # Progress bar width. Defaults to getOption("width")
#                      char = "=")   # Character used to create the bar
# 
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
    df$alpha[i] <- 0.00
    
    df$zeta[i] <- 0.00
    
    df$q[i] <- 1
  }
}

df$alpha <- as.numeric(df$alpha)
df$zeta <- as.numeric(df$zeta)
df$q <- as.numeric(df$q)

df$time <- df$time - 3060

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
      y_breaks <- seq(0, 0.1, 0.05)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.00) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time, y = wave, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(0, 0.1), breaks = y_breaks) +
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

# doc <- read_pptx("figures/figures.pptx")
# plot <- dml(ggobj = all_plot)
# doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
# doc <- ph_with(x = doc, value = plot, location = ph_location_fullsize())
# print(doc, target = "figures/figures.pptx")

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
      y_breaks <- seq(0, 0.1, 0.05)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time, y = wave, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(0, 0.1), breaks = y_breaks) +
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
#all_plot

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
      y_breaks <- seq(0, 0.1, 0.05)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time, y = wave, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(0, 0.1), breaks = y_breaks) +
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
      y_breaks <- seq(15, 30, 5)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf) + 
      geom_smooth(aes(x = time, y = energy, colour = factor(size)),
                  se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(15, 30), breaks = y_breaks) +
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
      y_breaks <- seq(15, 30, 5)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time, y = energy, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(15, 30), breaks = y_breaks) +
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
      y_breaks <- seq(15, 30, 5)
    } else {
      y_breaks <- NULL
    }
    
    if(aa == 0.0) {
      x_breaks <- seq(0, 100, 25)
    } else {
      x_breaks <- NULL
    }
    
    p <- ggplot(data = useDf,
                aes(x = time, y = energy, colour = factor(size))) + 
      geom_smooth(se = F, show.legend = legendBool, linewidth = 2) + 
      scale_x_continuous(name = "", breaks = x_breaks) + 
      scale_y_continuous(name = "", limits = c(15, 30), breaks = y_breaks) +
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
# extract mean number of matings ####
# 
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
#   if(f %in% folders[4:6]) {
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
# saveRDS(mating_df, "mating_df.rds")

#plot mean matings per size ####
mdf <- readRDS("mating_df.rds")

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
      y_breaks <- seq(0, 60, 20)
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
      scale_y_continuous(name = "", limits = c(0, 60), breaks = y_breaks) + 
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
      y_breaks <- seq(0, 60, 20)
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
      scale_y_continuous(name = "", limits = c(0, 60), breaks = y_breaks) + 
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
      y_breaks <- seq(0, 60, 20)
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
      scale_y_continuous(name = "", limits = c(0, 60), breaks = y_breaks) + 
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
# HT_cols <- seq(tSteps+1, tides*(tSteps+1), tSteps+1)
# 
# for(f in folders) {
#   setTxtProgressBar(pb, which(folders == f))
#   if(f %in% folders[4:6]) {
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
#       .[,-HTcols]
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
# saveRDS(alive_df, "alive_df.rds")

# plot mortality curves ####

adf <- readRDS("alive_df.rds")

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
      y_breaks <- seq(0.25, 1, 0.25)
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
      scale_y_continuous(name = "", limits = c(0.25, 1), breaks = y_breaks) + 
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
      y_breaks <- seq(0.25, 1, 0.25)
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
      scale_y_continuous(name = "", limits = c(0.25, 1), breaks = y_breaks) + 
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
      y_breaks <- seq(0.25, 1, 0.25)
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
      scale_y_continuous(name = "", limits = c(0.25, 1), breaks = y_breaks) + 
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

midTideL$bin <- ifelse(midTideL$pWave>0.3, 1, 0)

a1<-ggplot(data=midTideL, aes(x = timestep, y = energy, fill = pWave))+
  geom_tile()+
  geom_hline(yintercept = c(seq(0.5,eMax-0.5, 1)), alpha = 0.2)+
  geom_vline(xintercept = c(seq(0.5,tSteps-0.5, 1)), alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, tSteps, 20), 
                     limits = c(0,tSteps+1))+
  scale_y_continuous(breaks = seq(0,eMax, 10))+
  scale_fill_gradientn(name = "Probability of \nwaving", 
                       limits=c(0,1),colors = c(low = "white", medium = "goldenrod1", high = "goldenrod4"), 
                       breaks = c(0, 0.5, 1))+
  xlab("Time")+
  ylab("Energy level") +
  theme_classic(base_size = 40) + 
  theme(legend.text = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.key.height = unit(1.35, 'cm'),
        legend.key.width = unit(1, "cm"))
  #ggtitle("\"Baseline\"")
#a1
ggsave(plot = a1, "figures/q1.0/baseline_strategy.png", units = "px", height = 5200, width = 7980)

#plot strategy for game ####

brl2<-read.table("2) game/largebrstrat.txt", header=F, sep = ",")

eMax<-nrow(brl2)
tides<-(length(brl2))/(tSteps+1)

eLevels <- 0:(eMax-1)
brl2<-cbind(eLevels, brl2)

vDfLength <- paste("V",(length(brl2)-1),sep="")

dataLong2<-gather(data = brl2, timestep, pWave, V1:all_of(vDfLength),factor_key = FALSE)
timestepNew <- rep_len(rep(1:(tSteps+1), each = eMax), length.out = nrow(dataLong2))
dataLong2$timestep <- as.numeric(timestepNew)
names(dataLong2)<-c("energy","timestep","pWave")

dataLong2$tide <- rep(1:tides, each = (eMax*(tSteps+1)))

dataLong2 <- subset(dataLong2, !timestep %in% c(tSteps, tSteps+1))

dataLong2$pWave <- as.numeric(dataLong2$pWave)

midTideL2 <- subset(dataLong2, tide == (tides/2))

midTideL2$bin <- ifelse(midTideL2$pWave>0.3, 1, 0)

a2<-ggplot(data=midTideL2, aes(x = timestep, y = energy, fill = pWave))+
  geom_tile()+
  geom_hline(yintercept = c(seq(0.5,eMax-0.5, 1)), alpha = 0.2)+
  geom_vline(xintercept = c(seq(0.5,tSteps-0.5, 1)), alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, tSteps, 20), 
                     limits = c(0,tSteps+1))+
  scale_y_continuous(breaks = seq(0,eMax, 10))+
  scale_fill_gradientn(name = "Probability of \nwaving", 
                       limits=c(0,1),colors = c(low = "white", medium = "blue", high = "navyblue"), 
                       breaks = c(0, 0.5, 1))+
  xlab("Time")+
  ylab("Energy level") +
  theme_classic(base_size = 40) + 
  theme(legend.text = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.key.height = unit(1.35, 'cm'),
        legend.key.width = unit(1, "cm"))
  #ggtitle("\"Social game\"")
#a2
ggsave(plot = a2, "figures/q1.0/game_strategy.png", units = "px", height = 5200, width = 7980)


#plot prop waving for baseline and game ####

cols2 <- c("goldenrod1", "navyblue")

p1 <- ggplot(data = df[df$strategy == "baseline (no game, refrac)" | df$strategy == "game",],
             aes(x = time, y = wave, colour = factor(strategy))) + 
  geom_smooth(se = F, linewidth = 5, show.legend = F) + 
  scale_x_continuous(name = "Time") + 
  scale_y_continuous(name = "Proportion waving", limits = c(0, 0.05),
                     breaks = seq(0, 0.05, 0.01)) + 
  scale_colour_manual(name = "Strategy", values = cols2,
                      labels = c("Baseline", "Social game")) +
  theme_classic(base_size = 35) + 
  theme(legend.title = element_text(size = 30),
        legend.text = element_text(size = 25),
        legend.key.width = unit(1.1, "cm"))
#p1

#ggsave(plot = p1, "figures/q1.0/BL_game_waving.png", units = "px", height = 4320, width = 7980)

#plot energy for baseline and game ####

p2 <- ggplot(data = df[df$strategy == "baseline (no game, refrac)" | df$strategy == "game",],
             aes(x = time, y = energy, colour = factor(strategy))) + 
  geom_smooth(se = F, linewidth = 5, show.legend = T) + 
  scale_x_continuous(name = "Time") + 
  scale_y_continuous(name = "Energy level", limits = c(15, 30),
                     breaks = seq(15, 30, 5)) + 
  scale_colour_manual(name = "Strategy", values = cols2,
                      labels = c("Baseline", "Social game")) +
  theme_classic(base_size = 35) + 
  theme(legend.title = element_text(size = 30),
        legend.text = element_text(size = 25),
        legend.key.width = unit(1.1, "cm"))
#p2

#ggsave(plot = p2, "figures/q1.0/BL_game_energy.png", units = "px", height = 4320, width = 7980)

#plot grid ####

gridPlot <- ggarrange(p1, p2, ncol = 2, nrow = 1, 
                      labels = c("a)", "b)"), font.label = list(size = 50),
                      hjust = 0, widths = c(1, 1.25))

ggsave(plot = gridPlot, "figures/q1.0/grid.png", units = "px", height = 2400, width = 7980)

#plot grid for var_F ####
cols3 <- c("navyblue", "green4")

p3 <- ggplot(data = df[df$strategy == "game" | df$strategy == "changing pFemMin",],
             aes(x = time, y = wave, colour = factor(strategy))) + 
  geom_smooth(se = F, linewidth = 5, show.legend = F) + 
  scale_x_continuous(name = "Time") + 
  scale_y_continuous(name = "Proportion waving", limits = c(0, 0.05),
                     breaks = seq(0, 0.05, 0.01)) + 
  scale_colour_manual(values = cols3) +
  theme_classic(base_size = 35) + 
  theme(legend.title = element_text(size = 30),
        legend.text = element_text(size = 25),
        legend.key.width = unit(1.1, "cm"))

p4 <- ggplot(data = df[df$strategy == "game" | df$strategy == "changing pFemMin",],
             aes(x = time, y = energy, colour = factor(strategy))) + 
  geom_smooth(se = F, linewidth = 5, show.legend = T) + 
  scale_x_continuous(name = "Time") + 
  scale_y_continuous(name = "Energy level", limits = c(15, 30),
                     breaks = seq(15, 30, 5)) + 
  scale_colour_manual(name = "Strategy", values = cols3,
                      labels = c("Variable *f*","Social game")) +
  theme_classic(base_size = 35) + 
  theme(legend.title = element_text(size = 30),
        legend.text = element_markdown(size=25),
        legend.key.width = unit(1.1, "cm"))

gridPlot2 <- ggarrange(p3, p4, ncol = 2, nrow = 1, 
                      labels = c("a)", "b)"), font.label = list(size = 50),
                      hjust = 0, widths = c(1, 1.25))
gridPlot2

#colorBlindness::cvdPlot(gridPlot2)
ggsave(plot = gridPlot2, "figures/q1.0/grid_varf.png", units = "px", height = 2400, width = 7980)
