library(tidyverse)
library(ggplot2)
library(devtools)
#devtools::install_github("JaseZiv/worldfootballR")
#install.packages("worldfootballR")
library(worldfootballR)
library(showtext)
# install.packages("imager")
# library(imager)

# stats <- read.csv(file.choose())

setwd("G:")
setwd("Data Science")
setwd("python")

stats<- read.csv("playtypestatsforpizza - Copy.csv")

index <- 1:11

stats$index <- 1:11

stats <- stats %>% 
  mutate(type = case_when(
    index %in% 1:12 ~ "Scoring"
  ))

stats$cool <- rbind("ff0000","D4022B","BF0340","7F0780","400ABF","000DFF","007295","00867F","00AE55","00D72B","00FF00")

stats$cool <- as.factor(stats$cool)

cooolor <- c("ff0000","00ffff","00ff00","f7931e","ffff00","55aaff","ff5500","aaff00","800ff","fa6269","aa55ff")

temp <- ((360/11)/2)
myAng <- seq(-temp, -360+temp, length.out = 11)
ang <- ifelse(myAng < -90, myAng+180, myAng)
ang <- ifelse(ang < -90, ang+180, ang)

# remove(temp, myAng, ang)

qplot(data = stats, x = index, y = AtlantaHawks)

stats$index <- as.numeric(stats$index)

theme_kevin <- theme(plot.background = element_rect(fill = "black"),
                     panel.grid = element_blank(),
                     panel.background = element_rect(fill = "black"),

                     axis.text.x = element_text(face = "bold", size = 24, family = "Montserrat", ang = ang),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     axis.text =  element_text(face = "bold", size = 26, colour = "white", margin = c(0,0,2,0)),

                     legend.background = element_rect(fill = "black"),
                     legend.text = element_text( color = "white"),

                     plot.title = element_text(color = "white", size = 36, hjust = .5, vjust = .5, face = "bold"),
                     plot.subtitle = element_text(color = "white", size = 24, hjust = .5, vjust = .5),
                     plot.margin = unit(c(1,0,0,0), "cm"))

data <- ggplot(data = stats, aes(x = reorder(Playtype,index), y = BostonCeltics, fill = Playtype, label = Playtype))

data + geom_bar(data = stats, 
                stat = "identity", 
                width = 1, 
                show.legend = FALSE) + 
  
  coord_polar() +
  
  geom_label(aes(label = BostonCeltics, 
                 fill = Playtype), 
             color = "white", family = "Montserrat", size = 6, show.legend = FALSE) +
  
  labs(title = "Boston Celtics Playtype Prefrences",
       subtitle = "Playtype Data for 2022/23 Season") +
  theme_kevin

ggsave("BostonCeltics.png", height = 1320, width = 1080 , dpi = "screen", units = c("px"))
