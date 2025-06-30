library(tidyverse)
library(here)

#Create a ggplot that is blank, except for the axes labeled as such: x axis = "Shell growth" and y axis = "Shell thickness"
ggplot() + 
  geom_blank() +
  geom_segment(aes(x = 0, y = 0, xend = 0.75, yend = 0.75), arrow = arrow(type = "open", length = unit(0.2, "inches")), color="#D55E00", linewidth=1) +
  annotate("text", x = -0.7, y = -0.6, label = "Aragonite saturation", hjust = 0, vjust = 0, angle=45, color = "black", size=5) +
  geom_segment(aes(x = 0, y = 0, xend = -0.75, yend = -0.75), arrow = arrow(type = "open", length = unit(0.2, "inches")), color="#56B4E9", linewidth=1) +
  annotate("text", x = 0.15, y = 0.25, label = "Temperature", hjust = 0, vjust = 0, angle=45, color = "black", size=5) +
  #geom_segment(aes(x = 0, y = 0, xend = 0.95, yend = 0), arrow = arrow(type = "open", length = unit(0.2, "inches")), color="#E69F00", linewidth=1) +
  geom_point(aes(x = 0, y = 0), size = 3, color = "black") +
  annotate("text", x = -0.98, y = -1, label = "Decreasing", hjust = 0.2, vjust = 0.5, color = "black", size=5) +
  annotate("text", x = 0.6, y = -1, label = "Increasing", hjust = 0, vjust = 0.5, color = "black", size=5) +
  annotate("text", x = -0.98, y = 0.9, label = "Increasing", hjust = 0.2, vjust = 0, color = "black", size=5) +
  
  geom_segment(aes(x = -0.5, y = -1, xend = 0.5, yend = -1), color="black", linetype="dashed") +
  geom_segment(aes(x = 0.5, y = -1, xend = 0.551, yend = -1), arrow = arrow(), color="black") +
  geom_segment(aes(x = -0.5, y = -1, xend = -0.5, yend = -1), arrow = arrow(), color="black") +
  
  geom_segment(aes(y = -0.85, x = -0.95, yend = 0.85, xend = -0.95), color="black", linetype="dashed") +
  geom_segment(aes(y = 0.85, x = -0.95, yend = 0.851, xend = -0.95), arrow = arrow(), color="black") +
  geom_segment(aes(y = -0.85, x = -0.95, yend = -0.851, xend = -0.95), arrow = arrow(), color="black") +
  scale_x_continuous(limits = c(-1, 1)) + 
  scale_y_continuous(limits = c(-1, 0.95)) + 
  labs(y="Shell growth", x="Shell thickness") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_text(size = 17))

#Save the plot as a .png file
ggsave(here("figures", "pfister-fig.png"), width = 5, height = 5, units = "in", dpi = 300)
       