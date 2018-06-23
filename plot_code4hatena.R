# plot_code4hatena

library(RCurl)

url <- getURL("https://raw.githubusercontent.com/Algo1970/EHR_data/master/drug_dataset.csv")
Drug_df <- read.csv(text = url, header = TRUE)
Drug_df

Drug_df_long = tidyr::gather(Drug_df, key="category", value = "date", start_date, end_date)
Drug_df_long$date = as.Date(Drug_df_long$date)
Drug_df_long

url <- getURL("https://raw.githubusercontent.com/Algo1970/EHR_data/master/BW_dataset.csv")
BW_df <- read.csv(text = url, header = TRUE)
BW_df$date = as.Date(BW_df$date)
head(BW_df)

url <- getURL("https://raw.githubusercontent.com/Algo1970/EHR_data/master/ALT_dataset.csv")
ALT_df <- read.csv(text = url, header = TRUE)
ALT_df$date = as.Date(ALT_df$date)
head(ALT_df)

library(ggplot2)
library(gridExtra)
ggplot() + geom_line(data = BW_df, aes(date, BW))

# drug_plot
p1 = ggplot(Drug_df_long, aes(x=date, y=drug_name)) + geom_line()
p1
# BW_plot
p2 = ggplot(data = BW_df, aes(date, BW)) + geom_line()
p2
# ALT_plot
p3 = ggplot(data = ALT_df, aes(date, ALT)) + geom_line()
p3
# plotの配置
grid.arrange(p1, p2, p3, layout_matrix = matrix(1:3, nrow=3))

# 日付調整
library(dplyr)
Drug_min_date = Drug_df_long$date %>% min()
BW_min_date = BW_df$date %>% min()
ALT_min_date = ALT_df$date %>% min()
minDate = min(Drug_min_date, BW_min_date, ALT_min_date)

Drug_min_date = Drug_df_long$date %>% max()
BW_min_date = BW_df$date %>% max()
ALT_min_date = ALT_df$date %>% max()
maxDate = max(Drug_min_date, BW_min_date, ALT_min_date)

p1 = ggplot(Drug_df_long, aes(x=date, y=drug_name)) + 
  geom_line() + 
  xlim(minDate, maxDate) + 
  labs(y= "", title = "drug") +
  theme(axis.text.y = element_blank())
p2 = ggplot(data = BW_df, aes(date, BW)) + 
  geom_line() + 
  xlim(minDate, maxDate) + 
  labs(y= "", title = "BW") +
  theme(axis.text.y = element_blank())
p3 = ggplot(data = ALT_df, aes(date, ALT)) + 
  geom_line() + 
  xlim(minDate, maxDate) + 
  labs(y= "", title = "ALT") +
  theme(axis.text.y = element_blank())
grid.arrange(p1, p2, p3, layout_matrix = matrix(1:3, nrow=3))


# plot_gray
p1 = ggplot(Drug_df_long, aes(x=date, y=drug_name)) + 
  geom_line(size=10, alpha = 0.4) +
  geom_text(aes(label = drug_name), color="black", size = 6, check_overlap = T) +
  xlim(minDate, maxDate) + 
  labs(x = "", y= "", title = "Drug") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) 
p1
p2 = ggplot(data = BW_df, aes(date, BW)) + 
  geom_line(color = "black", size = 3, alpha = 0.3) + 
  geom_point(color = "black", size = 3.5, alpha = 0.2) +
  geom_text(aes(label = BW), color="black", size = 6, check_overlap = T) +
  xlim(minDate, maxDate) + 
  labs(x = "", y= "", title = "BW") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) 
p2

p3 = ggplot(data = ALT_df, aes(date, ALT)) + 
  geom_line(color = "black", size = 3, alpha = 0.3) + 
  geom_point(color = "black", size = 3.5, alpha = 0.2) +
  geom_text(aes(label = ALT), color="black", size = 6, check_overlap = T) +
  xlim(minDate, maxDate) + 
  labs(x = "", y= "", title = "ALT") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_blank()) 
p3
grid.arrange(p1, p2, p3, layout_matrix = matrix(1:3, nrow=3))

# plot_color
p1 = ggplot(Drug_df_long, aes(x=date, y=drug_name, colour = drug_name)) + 
  geom_line(size=10, alpha = 0.4) +
  geom_text(aes(label = drug_name), size = 6, check_overlap = T) +
  scale_colour_discrete(guide=FALSE) + 
  xlim(minDate, maxDate) + 
  labs(x = "", y= "", title = "Drug") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) 
p1
p2 = ggplot(data = BW_df, aes(date, BW)) + 
  geom_line(color = "orange", size = 3, alpha = 0.5) + 
  geom_point(color = "orange", size = 3.5, alpha = 0.2) +
  geom_text(aes(label = BW), color="black", size = 6, check_overlap = T) +
  xlim(minDate, maxDate) + 
  labs(x = "", y= "", title = "BW") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) 
p2

p3 = ggplot(data = ALT_df, aes(date, ALT)) + 
  geom_line(color = "darkblue", size = 3, alpha = 0.3) + 
  geom_point(color = "darkblue", size = 3.5, alpha = 0.2) +
  geom_text(aes(label = ALT), color="black", size = 6, check_overlap = T) +
  xlim(minDate, maxDate) + 
  labs(x = "", y= "", title = "ALT") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_blank()) 
p3
grid.arrange(p1, p2, p3, layout_matrix = matrix(1:3, nrow=3))

# 関数


