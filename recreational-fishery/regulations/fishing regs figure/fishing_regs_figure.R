#Fishing regulations over time 
setwd("~/GitHub/california-data/recreational-fishery/regulations/fishing regs figure")
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

regs=read_xlsx("CDFW Recreational Regulations.xlsx", sheet="regs reformatted")
regs$Month=factor(regs$Month, levels = rev(regs$Month[1:12]))
regs$Region=factor(regs$Region, levels = unique(regs$Region))
regs$Status2=ifelse(regs$Status=="Closed","Closed",
                    ifelse(regs$Depth_restrictions=="None","Open, unrestricted",paste("Open,", tolower(regs$Depth_restrictions))))
regs$Status2=factor(regs$Status2,levels=c("Closed","Open, unrestricted","Open, max depth","Open, min depth"))
regs$Midmonth_change[which(regs$Midmonth_change=="yes")-1]="next"

ggplot(regs, aes(x=Year, y=Month, fill=Status2)) +
  facet_wrap(Region~., nrow = 5, dir = "v", strip.position = "right") +
  geom_tile() + 
  geom_text(aes(label=Depth), size=2) + 
  scale_fill_manual(values=c("#8856a7","#e0ecf4","#9ebcda","gold")) + 
  geom_point(data=regs %>% filter(Midmonth_change=="next"), color="black", 
             show.legend = F,
             position = position_nudge(y=-0.5), shape=25, size=1) +
  theme_classic() + 
  scale_y_discrete(expand = expansion(0)) +
  scale_x_continuous(expand = expansion(0), breaks = 2000:2023) +
  theme(panel.background = element_rect(color="black", fill=NULL), 
        axis.text = element_text(color="black"), 
        axis.ticks = element_line(color="black")) +
  theme(strip.background = element_blank()) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
  labs(fill="Status:") +
  theme(legend.position = "top")
ggsave("fishing_regs.png", width = 12, height = 12)
