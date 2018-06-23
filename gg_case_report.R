library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

BW_df = read.csv("BW_dataset.csv")
BW_df$date = as.Date(BW_df$date)
print(BW_df)

BW_df$date %>% min()
BW_df$date %>% max()

p_BW = ggplot() +
  geom_line(data = BW_df, aes(date, BW), color = "orange", size = 3, alpha = 0.7) +
  geom_point(data = BW_df, aes(date, BW), color = "orange", size = 3.5, alpha = 0.7) +
  labs(x = "", y= "BW", title = "") +
  theme_minimal() + theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_blank()) + theme(axis.text.y = element_text(size = 15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p_BW

p2 = ggplot(BW_df, aes(date, BW)) + 
  geom_line(color = "black", size = 3, alpha = 0.5) + 
  geom_point(color = "black", size = 3.5, alpha = 0.2) +
  geom_text(aes(label = BW), color="black", size = 6, check_overlap = T) +
  labs(x = "", y= "", title = "BW") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p2


ALT_df = read.csv("ALT_dataset.csv")
ALT_df$date = as.Date(ALT_df$date)
print(ALT_df)

p_ALT = ggplot() +
  geom_line(data = ALT_df, aes(date, ALT), color = "darkblue", size = 3, alpha = 0.7) +
  geom_point(data = ALT_df, aes(date, ALT), color = "darkblue", size = 3.5, alpha = 0.7) +
  labs(x = "", y= "ALT", title = "") +
  theme_minimal() + theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y = element_text(size = 15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p_ALT


ggplot() + geom_line(data = BW_df, aes(date, BW)) + xlim(min(ALT_df$date), max(ALT_df$date))

p3 =ggplot(ALT_df, aes(date, ALT)) + 
  geom_line(color = "black", size = 3, alpha = 0.5) + 
  geom_point(color = "black", size = 3.5, alpha = 0.2) +
  geom_text(aes(label = ALT), color="black", size = 6, check_overlap = T) +
  labs(x = "", y= "", title = "ALT") +
  theme_minimal() +
  theme(axis.text.y = element_blank()) + theme(axis.text.x = element_text(size = 15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p3

# drug_plot_sample

drug_name = c("linagliptin(Trazenta)", "sitagliptin(Januvia)", "glimepiride(Amaryl)", "tofoglifozin(Apleway)")
drug_name = c("Linagliptin", "Sitagliptin", "Glimepiride", "Tofoglifozin")
value = c(1,1,2,3)
start_date = c("2013-04-23", "2013-10-29", "2014-02-12", "2015-08-27")
end_date   = c("2013-10-28", "2015-08-26", "2018-06-20", "2018-06-20")
df = data.frame(drug_name, value, start_date, end_date)
df

df2 = tidyr::gather(df, key="category", value = "date", start_date, end_date)
df2$date = as.Date(df2$date)
df2
p_drug = ggplot(df2, aes(x=date, y=drug_name, colour = drug_name )) + geom_line(size=10, alpha = 0.7)
p_drug

p_drug2 = p_drug + xlab("") + ylab("") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank()) + theme(axis.text.y = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_colour_discrete(guide=FALSE) + 
  geom_text(aes(label = drug_name), color="black", size = 6, check_overlap = T)
p_drug2

p1 = ggplot(df2, aes(x=date, y=drug_name)) + 
  geom_line(size=10, alpha = 0.4) +
  labs(x = "", y= "", title = "Drug") +
  theme_minimal() + 
  # theme(axis.text.x = element_blank()) + theme(axis.text.y = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())  +
  scale_colour_discrete(guide=FALSE) + 
  geom_text(aes(label = drug_name), color="black", size = 6, check_overlap = T)
p1

grid.arrange(p1, p2, p3, layout_matrix = matrix(1:3, nrow=3))
matrix(1:3, nrow=3)

layout <- rbind(c(1, 1),
                c(2, 2),
                c(3, 3),
                c(3, 3))
layout

grid.arrange(p_drug2, p_BW, p_ALT,
             # tableGrob(AutoImune),
             # tableGrob(Endocrime),
             layout_matrix = layout)



