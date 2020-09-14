library(ggplot2)
library(cowplot)

mid <- 5
start <- 0
end <- 10

sharp <- function(x){
  ifelse(x>=mid, 1, 0)
}

fuzzy <- function(x){
  ifelse(x>=mid, x/20+0.5, x/25)
}

x<-seq(start, end, by=0.01)
y <- sharp(x)
y2 <- fuzzy(x)
df<-data.frame(x, y, y2)
df[x==mid,2]<-NA

Sharp_plot <-ggplot(df, aes(x=x,y=y, group = factor(1))) +
  geom_line(aes(x,y),df[x>mid,],color="red", cex=2) +
  geom_line(aes(x,y),df[x<mid,],color="blue", cex=2) +
  geom_segment(aes(x = mid,xend=mid, y = 0,  yend = 1),linetype="dashed") +
  scale_x_continuous(breaks=seq(start,end,length.out = 3), labels = c(495, 500, 505))+
  scale_y_continuous(breaks=seq(0,1,length.out = 3))+
  labs( x = "scores", y = "Conditional Probability of Receiving Treatment")+
  annotate("text",x=2.5, y=0.2, label="Assigned to Control", color="blue", size=6)+
  annotate("text",x=7.5, y=0.8, label="Assigned to Treatment", color="red", size=6)+
  annotate("text",x=5.7, y=0.5, label="Cutoff", size=4)

Fuzzy_plot <- ggplot(df, aes(x=x,y=y2, group = factor(1))) +
  geom_line(aes(x,y2),df[x>mid,],color="red", cex=2) +
  geom_line(aes(x,y2),df[x<mid,],color="blue", cex=2) +
  geom_segment(aes(x = mid,xend=mid, y = 0,  yend = 1),linetype="dashed") +
  scale_x_continuous(breaks=seq(start,end,length.out = 3), labels = c(495, 500, 505))+
  scale_y_continuous(breaks=seq(0,1,length.out = 3))+
  labs( x = "scores", y = "Conditional Probability of Receiving Treatment")+
  annotate("text",x=2.5, y=0.3, label="Assigned to Control", color="blue", size=6)+
  annotate("text",x=7.5, y=0.7, label="Assigned to Treatment", color="red", size=6)+
  annotate("text",x=5.7, y=0.5, label="Cutoff", size=4)
title <- ggdraw() + 
  draw_label("Treatment Status", fontface = 'bold',size=16) +
  theme(plot.margin = margin(0, 0, 0, 20))

print(plot_grid(title, rel_heights = c(0.1, 1), ncol=1,
                plot_grid(Sharp_plot, Fuzzy_plot
                          , nrow = 1, labels = c("Sharp RD","Fuzzy RD"), label_fontface = "bold",hjust = -0.5, vjust = -0.25)
                ), width=21, height=9)


