# ggplot_casereport

library(ggplot2)

# drug_plot_sample

drug_name = c("linagliptin(Trazenta)", "sitagliptin(Januvia)", "glimepiride(Amaryl)", "tofoglifozin(Apleway)")
# value = c(1,1,2,3)
value = c("DPP-4","DPP-4","SU","SGLT2")
start_date = c("2012-02-20", "2012-10-21", "2014-08-10", "2015-12-27")
end_date   = c("2012-10-20", "2015-12-20", "2018-06-20", "2018-06-20")
df = data.frame(drug_name, value, start_date, end_date)
# df = data.frame(drug_name, start_date, end_date)
df

df2 = tidyr::gather(df, key="category", value = "date", start_date, end_date)
df2$date = as.Date(df2$date)
df2

ggplot(df2, aes(x=date, y=drug_name, colour = value )) + geom_line(size=15, alpha = 0.7)

p_drug = ggplot(df2, aes(x=date, y=drug_name, colour = drug_name )) + geom_line(size=15, alpha = 0.7)
p_drug = ggplot(df2, aes(x=date, y=drug_name, colour = drug_name )) + geom_line(size=15, alpha = 0.7)
p_drug + scale_colour_discrete(guide=FALSE)
p_drug




gg_casereport = function(){}

