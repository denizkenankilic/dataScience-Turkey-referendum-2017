install.packages("magrittr")
library(magrittr)
library(plotly)

# Read all data from .csv files
# "eksik" data files have two missing cities
# "Secim_2017" data has results of 2017 election
Secim_25 <- read.csv(file="2016_Secim_25_Donem.csv", header=TRUE, sep=",")
Secim_26 <- read.csv(file="2016_Secim_26_Donem.csv", header=TRUE, sep=",")
Secim_25_eksik <- read.csv(file="2016_Secim_25_Donem_eksik.csv", header=TRUE, sep=",")
Secim_26_eksik <- read.csv(file="2016_Secim_26_Donem_eksik.csv", header=TRUE, sep=",")
Tweets_loc_perc <- read.csv(file="il_tweet_perc.csv", header=TRUE, sep=",")
Secim_2017 <- read.csv(file="referandum_sonuc.csv", header=TRUE, sep=",")
# Tw_vs_sec26 <- read.csv(file="Tw-7gun vs Secim 26.csv", header=TRUE, sep=",")
# Tw_hours <- read.csv(file="referandum.csv", header=TRUE, sep=",")
# Tweets_locations_perc <- read.csv(file="tweets_hours.csv", header=TRUE, sep=",")

# Analaysis for Secim_25
Secim_25[is.na(Secim_25)] <- 0 # Change NA parts via 0
summary(Secim_25)
names(Secim_25)
head(Secim_25$akp)

Secim_25["evet_sayi"] <- NA # Create new column
Secim_25["hayir_sayi"] <- NA # Create new column
Secim_25["evet_yuzde"] <- NA # Create new column
Secim_25["hayir_yuzde"] <- NA # Create new column
Secim_25$evet_sayi <- Secim_25$akp+(Secim_25$mhp/2) # approximate estimation for "evet"
Secim_25$hayir_sayi <- Secim_25$chp+(Secim_25$mhp/2)+Secim_25$hdp+Secim_25$saadet # approximate estimation for "hayir"
Secim_25$evet_yuzde <- 100*(Secim_25$evet_sayi/(Secim_25$evet_sayi+Secim_25$hayir_sayi)) 
Secim_25$hayir_yuzde <- 100*(Secim_25$hayir_sayi/(Secim_25$evet_sayi+Secim_25$hayir_sayi))

# Analaysis for Secim_26
Secim_26[is.na(Secim_26)] <- 0 # Change NA parts via 0
summary(Secim_26)
names(Secim_26)
head(Secim_26$akp)

Secim_26["evet_sayi"] <- NA # Create new column
Secim_26["hayir_sayi"] <- NA # Create new column
Secim_26["evet_yuzde"] <- NA # Create new column
Secim_26["hayir_yuzde"] <- NA # Create new column
Secim_26$evet_sayi <- Secim_26$akp+(Secim_26$mhp/2) # approximate estimation for "evet"
Secim_26$hayir_sayi <- Secim_26$chp+(Secim_26$mhp/2)+Secim_26$hdp+Secim_26$saadet # approximate estimation for "hayir"
Secim_26$evet_yuzde <- 100*(Secim_26$evet_sayi/(Secim_26$evet_sayi+Secim_26$hayir_sayi))
Secim_26$hayir_yuzde <- 100*(Secim_26$hayir_sayi/(Secim_26$evet_sayi+Secim_26$hayir_sayi))

# Analaysis for Secim_25_eksik whose 2 cities are missing
Secim_25_eksik[is.na(Secim_25_eksik)] <- 0 # Change NA parts via 0
summary(Secim_25_eksik)
names(Secim_25_eksik)
head(Secim_25_eksik$akp)

Secim_25_eksik["evet_sayi"] <- NA
Secim_25_eksik["hayir_sayi"] <- NA
Secim_25_eksik["evet_yuzde"] <- NA
Secim_25_eksik["hayir_yuzde"] <- NA
Secim_25_eksik$evet_sayi <- Secim_25_eksik$akp+(Secim_25_eksik$mhp/2)
Secim_25_eksik$hayir_sayi <- Secim_25_eksik$chp+(Secim_25_eksik$mhp/2)+Secim_25_eksik$hdp+Secim_25_eksik$saadet
Secim_25_eksik$evet_yuzde <- 100*(Secim_25_eksik$evet_sayi/(Secim_25_eksik$evet_sayi+Secim_25_eksik$hayir_sayi))
Secim_25_eksik$hayir_yuzde <- 100*(Secim_25_eksik$hayir_sayi/(Secim_25_eksik$evet_sayi+Secim_25_eksik$hayir_sayi))

# Analaysis for Secim_25_eksik whose 2 cities are missing
Secim_26_eksik[is.na(Secim_26_eksik)] <- 0 # Change NA parts via 0
summary(Secim_26_eksik)
names(Secim_26_eksik)
head(Secim_26_eksik$akp)

Secim_26_eksik["evet_sayi"] <- NA
Secim_26_eksik["hayir_sayi"] <- NA
Secim_26_eksik["evet_yuzde"] <- NA
Secim_26_eksik["hayir_yuzde"] <- NA
Secim_26_eksik$evet_sayi <- Secim_26_eksik$akp+(Secim_26_eksik$mhp/2)
Secim_26_eksik$hayir_sayi <- Secim_26_eksik$chp+(Secim_26_eksik$mhp/2)+Secim_26_eksik$hdp+Secim_26_eksik$saadet
Secim_26_eksik$evet_yuzde <- 100*(Secim_26_eksik$evet_sayi/(Secim_26_eksik$evet_sayi+Secim_26_eksik$hayir_sayi))
Secim_26_eksik$hayir_yuzde <- 100*(Secim_26_eksik$hayir_sayi/(Secim_26_eksik$evet_sayi+Secim_26_eksik$hayir_sayi)) 

# All "evet" and "hayir" data sets for all locations
# Tweeter data is hourly between GMT: Sat, 21 Jan 2017 21:00:00 GMT and 
# GMT: Wed, 08 Mar 2017 11:00:00 GMT
# Tweeter data is calculated as its volume during all time
all_perc_data <- data.frame(Secim_25_eksik$il, Secim_25_eksik$evet_yuzde, Secim_26_eksik$evet_yuzde, 
                            Tweets_loc_perc$evet_yuzde, Secim_25_eksik$hayir_yuzde, Secim_26_eksik$hayir_yuzde, 
                            Tweets_loc_perc$hayir_yuzde)
all_perc_data <- setNames(all_perc_data, c("iller","25.donem_evet","26.donem_evet","tweet_evet",
                                           "25.donem_hayir","26.donem_hayir","tweet_hayir"))
all_perc_data["hayir_yuzde_ref"] <- NA
all_perc_data["evet_yuzde_ref"] <- NA
all_perc_data$hayir_yuzde_ref <- Secim_2017$hayir_oran
all_perc_data$evet_yuzde_ref <- Secim_2017$evet_oran

difference_2017 <- abs(all_perc_data$`evet_yuzde_ref`-all_perc_data$tweet_evet)
sum(difference_2017)
difference_data_2017 <- data.frame(all_perc_data$iller, difference_2017)
dif_plot_17 <- plot_ly(difference_data_2017, x = ~reorder(all_perc_data.iller,-difference_2017), y = ~difference_2017, name='2017 dif')
dif_plot_17

# Plot of all "evet" and "hayir" data for 79 locations
p1 <- plot_ly(all_perc_data, x = ~all_perc_data$iller, y = ~all_perc_data$`25.donem_evet`, type = 'bar', name = '25.donem_evet') %>%
  add_trace(y = ~all_perc_data$`26.donem_evet`, name = '26.donem_evet') %>%
  add_trace(y = ~all_perc_data$tweet_evet, name = 'tweet_evet') %>%
  add_trace(y = ~all_perc_data$`25.donem_hayir`, name = '25.donem_hayir') %>%
  add_trace(y = ~all_perc_data$`26.donem_hayir`, name = '26.donem_hayir') %>%
  add_trace(y = ~all_perc_data$tweet_hayir, name = 'tweet_hayir') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p1

cor(all_perc_data$tweet_evet, all_perc_data$`25.donem_evet`)
cor(all_perc_data$tweet_evet, all_perc_data$`26.donem_evet`)
cor(all_perc_data$tweet_hayir, all_perc_data$`25.donem_hayir`)
cor(all_perc_data$tweet_hayir, all_perc_data$`26.donem_hayir`)

difference_evet <- abs(all_perc_data$`26.donem_evet`-all_perc_data$tweet_evet)
difference_evet_2 <- abs(all_perc_data$`25.donem_evet`-all_perc_data$tweet_evet)
difference_hayir <- abs(all_perc_data$`26.donem_hayir`-all_perc_data$tweet_hayir)
difference_data <- data.frame(all_perc_data$iller, difference_evet, difference_evet_2) 
sum(difference_evet)
sum(difference_evet_2)

dif_plot <- plot_ly(difference_data, x = ~reorder(all_perc_data.iller,-difference_evet), y = ~difference_evet, name='26. dif')
dif_plot
dif_plot2 <- plot_ly(difference_data, x = ~all_perc_data.iller, y = ~difference_evet, type = 'bar', name = '26. donem') %>%
  add_trace(y = ~difference_evet_2, name = '25. donem') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
dif_plot2
# 25th and 26th elections and their comparison by bar plot
# 
library(ggplot2)
install.packages("plotly")
library(plotly)
evet_compare_secim <- data.frame(Secim_25$il, Secim_25$evet_yuzde, Secim_26$evet_yuzde, Secim_25$hayir_yuzde, Secim_26$hayir_yuzde)
p2 <- plot_ly(evet_compare_secim, x = ~Secim_25$il, y = ~Secim_25$evet_yuzde, type = 'bar', name = 'Secim_25$evet_yuzde') %>%
  add_trace(y = ~Secim_26$evet_yuzde, name = 'Secim_26$evet_yuzde') %>%
  add_trace(y = ~Secim_25$hayir_yuzde, name = 'Secim_25$hayir_yuzde') %>%
  add_trace(y = ~Secim_26$hayir_yuzde, name = 'Secim_26$hayir_yuzde') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p2


p3 <- plot_ly(Secim_25, labels = ~Secim_25$il, values = ~Secim_25$nÃ.fus, type = 'pie') %>%
  layout(title = 'Secim Percentages',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p3


# some relevant works
akp_il<-data.frame(Secim_26$il,Secim_26$akp)
head(akp_il)
percentage_akp_adana <- (sum(Secim_26$nÃ.fus))

# Some correlation values
cor(Secim_26$akp, Secim_26$nÃ.fus)
cor(Secim_26$chp, Secim_26$nÃ.fus)
cor(Secim_26$mhp, Secim_26$nÃ.fus)
cor(Secim_26$hdp, Secim_26$nÃ.fus)
cor(Secim_26$saadet, Secim_26$nÃ.fus)

Tw_vs_sec26$SECIM_EVET <- as.factor(Tw_vs_sec26$SECIM_EVET)
Tw_vs_sec26$EVET <- as.factor(Tw_vs_sec26$EVET)
install.packages("ggplot2")
library(ggplot2)
#ggplot(Tw_vs_sec26, aes(x=EVET, y=SECIM_EVET)) +geom_boxplot()
sp<-ggplot(Tw_vs_sec26, aes(x=EVET, y=SECIM_EVET, color=loc)) + geom_point()
sp
ggplot(Tw_vs_sec26, aes(x=EVET, y=SECIM_EVET, color=loc)) + geom_point()
abline(lm(as.numeric(Tw_vs_sec26$EVET) ~ as.numeric(Tw_vs_sec26$SECIM_EVET)))
plot(Tw_vs_sec26$EVET, Tw_vs_sec26$SECIM_EVET, xlab="EVET",ylab="SECIM_EVET")
abline(glm(Tw_vs_sec26$SECIM_EVET ~ Tw_vs_sec26$EVET))
