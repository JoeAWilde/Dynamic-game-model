library(tidyverse)
library(ggpubr)
library(ggplotify)
library(officer)
library(rvg)
library(ggtext)

setwd("E:/OneDrive - University of Exeter/Dynamic programming/0.6 C++/newDead/figures/new strategies for write up")

density <- 1001

phi_w_l <- seq(0, 1, length.out = density)

phi_w_s <- seq(0, 1, length.out = density)

f_0 <- 0.01
f_1 <- 1
chi <- 0.5
aa <- 0.75
zz <- 0.75
theta <- 0.25
rho <- 20

pm_l <- data.frame()
pm_s <- data.frame()

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = density, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for(r in 1:density) {
  for(c in 1:density) {
    
    num <- f_0 + ((chi*phi_w_l[r] + (1-aa)*(1-chi)*phi_w_s[c])^theta)*(f_1 - f_0)
    den <- 1 + rho*(chi*phi_w_l[r] + (1-aa)*(1-chi)*phi_w_s[c])
    
    pm_l[r, c] <- num / den
    
    pm_s[r, c] <- (num*(1-zz)) / den
    
  }
  setTxtProgressBar(pb, r)
}


pm_lL <- pivot_longer(data = pm_l, cols = names(pm_l), values_to = "p_m") %>%
  mutate(phi_w_l = rep_len(phi_w_l, nrow(.)),
         phi_w_s = rep(phi_w_s, each = density)) %>%
  select(-name)

pm_lL$pm_bin <- ifelse(pm_lL$p_m > 0 & pm_lL$p_m <= 0.05, 0, 
                       ifelse(pm_lL$p_m > 0.05 & pm_lL$p_m <= 0.1, 0.05, 
                              ifelse(pm_lL$p_m > 0.1 & pm_lL$p_m <= 0.15, 0.1, 
                                     ifelse(pm_lL$p_m > 0.15 & pm_lL$p_m <= 0.2, 0.15,
                                            ifelse(pm_lL$p_m > 0.2 & pm_lL$p_m <= 0.25, 0.2,
                                                   ifelse(pm_lL$p_m > 0.25 & pm_lL$p_m <= 0.3, 0.25, NA))))))

pm_sL <- pivot_longer(data = pm_s, cols = names(pm_s), values_to = "p_m") %>%
  mutate(phi_w_l = rep_len(phi_w_l, nrow(.)),
         phi_w_s = rep(phi_w_s, each = density)) %>%
  select(-name)

pm_sL$pm_bin <- ifelse(pm_sL$p_m > 0 & pm_sL$p_m <= 0.05, 0, 
                       ifelse(pm_sL$p_m > 0.05 & pm_sL$p_m <= 0.1, 0.05, 
                              ifelse(pm_sL$p_m > 0.1 & pm_sL$p_m <= 0.15, 0.1, 
                                     ifelse(pm_sL$p_m > 0.15 & pm_sL$p_m <= 0.2, 0.15,
                                            ifelse(pm_sL$p_m > 0.2 & pm_sL$p_m <= 0.25, 0.2,
                                                   ifelse(pm_sL$p_m > 0.25 & pm_sL$p_m <= 0.3, 0.25, NA))))))

pm_lL$size <- rep_len("z = large", nrow(pm_lL))
pm_sL$size <- rep_len("z = small", nrow(pm_sL))

df <- rbind(pm_lL, pm_sL)

x_lab <- expression(phi["w[l, t]"])
y_lab <- expression(phi["w[s, t]"])

p1 <- ggplot(data = df, aes(x = phi_w_l, y = phi_w_s, fill = factor(pm_bin))) + 
  geom_tile() + 
  scale_x_continuous(name = x_lab) + 
  scale_y_continuous(name = y_lab) +
  # scale_fill_manual(values = RColorBrewer::brewer.pal(9, "BuPu")[3:9],
  #                    
  #                   ) + 
  scale_fill_viridis_d(
                      name = "P(*m*)<sub>[*z*, *t*]</sub>",
                      labels = c("0.00 - 0.05", "0.05 - 0.10", "0.10 - 0.15", "0.15 - 0.20", "0.20 - 0.25", "0.25 - 0.30")) + 
  theme_classic(base_size = 55) + 
  theme(legend.title = element_markdown(),
        legend.key.height = unit(1.2, "cm"),
        legend.key.width = unit(1.2, "cm"),
        axis.title = element_markdown(size = 60)) +
  facet_wrap(vars(size))
p1

ggsave(filename = "p_m graph.png", p1, units = "px", height = 4320, width = 9980)

