#Fishing regulations over time 
setwd("~/GitHub/california-data/recreational-fishery/regulations/fishing regs figure")
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

font.size = 13

regs = read_xlsx("CDFW Recreational Regulations.xlsx", sheet = "regs reformatted")
regs$Month = factor(regs$Month, levels = rev(regs$Month[1:12]))
regs$Region = factor(regs$Region, levels = unique(regs$Region))
regs$Status2 = ifelse(regs$Status == "Closed", "Closed", ifelse(regs$Depth_restrictions == "None", 
                    "Open, unrestricted", paste("Open,", tolower(regs$Depth_restrictions))))
regs$Status2=factor(regs$Status2,levels=c("Closed", "Open, unrestricted", "Open, max depth", "Open, min depth"))
regs$Midmonth_change[which(regs$Midmonth_change == "yes") - 1] = "next"

#re-arrange regions for plotting
neworder <- c("Northern", "Mendocino", "San Francisco", "Central", "Southern", "North-Central",  "Monterey South-Central", "Morro Bay South-Central")
regs <- arrange(transform(regs,
     Region = factor(Region, levels = neworder)), Region)


ggplot(regs, aes(x = Year, y = Month, fill = Status2)) +
  facet_wrap(Region ~ ., nrow = 5, dir = "v", strip.position = "right") +
  geom_tile() + 
  geom_text(aes(label = Depth), size = 3) +
  scale_fill_manual(values=c("#8856a7", "#e0ecf4", "#9ebcda", "gold")) + 
  geom_point(data = regs %>% filter(Midmonth_change == "next"), color = "black", 
             show.legend = F,
             position = position_nudge(y=-0.5), shape=25, size = 1.5) +
  theme_classic() +
  scale_y_discrete(expand = expansion(0)) +
  scale_x_continuous(expand = expansion(0), breaks = 2000:2023) +
  theme(panel.background = element_rect(color = "black", fill = NULL), 
        axis.text = element_text(color = "black", size = font.size), 
        axis.ticks = element_line(color = "black")) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  axis.title.x = element_text(size = font.size),
  axis.title.y = element_text(size = font.size),
  strip.text.y.right = element_text(size = font.size)) +
  labs(fill = "Status:") +
  theme(legend.position = "top", legend.text = element_text(size = font.size))

ggsave("fishing_regs.png", width = 12, height = 12)

# Caption for the figure.
The CDFW recreational season lenght and depth restriction for nearshore rockfish by month from 2000 to 2003. A triangle indicates a regulation change mid-month. The regions defined base on the following latitudes: Northern ($42^\circ 00^\prime$ N lat. to $40^\circ 10^\prime$ N lat.), Mendocino ($40^\circ 10^\prime$ N lat. to $38^\circ 57^\prime$ N lat.), San Francisco ($38^\circ 57^\prime$ N lat. to $37^\circ 11^\prime$ N lat.), Central ($37^\circ 11^\prime$ N lat. to $34^\circ 27^\prime$ N lat.), Southern ($34^\circ 27^\prime$ N lat. to US/Mexico border). Not all management areas have been consistently defined over time. The northern and southern management areas have remained the same. From 2001-2003 the Central management area was defined as 40^\circ 10^\prime$ N lat. to $34^\circ 27^\prime$ N lat. In 2004, the Central area was split into a North-Central and South-Central areas at $36^\circ 00^\prime$ N lat. In 2005, the regions from 
 $40^\circ 10^\prime$ N lat. to $34^\circ $27^\prime$ N lat. were redefined. The North-Central encompasses $40^\circ 10^\prime$ N lat. to $37^\circ 11^\prime$ N lat., Monterey South-Central from $37^\circ 11^\prime$ N lat. to $36^\circ ^\prime$ N lat., and Morro Bay South-Central from $36^\circ 00^\prime$ N lat. to $34^\circ 27^\prime$ N lat.