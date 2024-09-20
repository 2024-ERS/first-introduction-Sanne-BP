# working with dates and times in R - lubridate and tidyquant packages
# analyzing Climate Change, including time series analysis

remove(list=ls())
library(tidyverse)  # contains lubridate
library(tidyquant)  # for calculating moving averages

#When downloading new libaries, use renv::snapshot() to save it to the lockfile

# database: (remove hashtag)
# browseURL("https://docs.google.com/spreadsheets/d/1yKedemxKYMiBd8nwzZI5GI06enIZKRUvmbbLJk10uAI/edit?gid=304223944")

# read the daily weather data from the KNMI station Lauwersoog - 1 jan 1991 - 31 sep 2024
dat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRYSmvT7qFqBPa-XIxFIzIXpZOYlWpY-MqyMVVwh_Q1nN7pzxSGaPKlWRhPfCtomR59bkLuOgaUFRa1/pub?gid=2037414920&single=true&output=csv", show_col_types = FALSE) %>%
     mutate(date=lubridate::ymd(YYYYMMDD),   # make variable of type Date from string
            day=lubridate::day(date), 
            month=lubridate::month(date),
            year=lubridate::year(date),
            metyear_dec=ifelse(month==12,year+1,year),  # calculate meteorological year, which starts at 1 December of prevbious year
            metyear_nov=ifelse(month %in% c(11,12),year+1,year),  # winter for Hellman index, which starts at 1 November of prevbious year
            RH=ifelse(RH==-1,0,RH/10),     # RH = Daily precipitation amount (in 0.1 mm) (-1 for <0.05 mm) -> convert to mm/day
            TG=TG/10) %>% # TG = Daily mean temperature in (0.1 degrees Celsius) -> convert to 0C
     select(date,metyear_nov,metyear_dec,year,month,day,RH,TG)
head(dat,n=10)
tail(dat,n=10) # show the last 10 records of the tibble

# aggregate the dates per month
dat_month<-dat %>% 
  dplyr::group_by(year,metyear_nov,metyear_dec,month) %>%
  summarize(totrain_mo_mm=sum(RH,na=T), #monthly total rainfall in mm
            avgtemp_mo_oC=mean(TG,na.rm=T)) %>%             #monthly average temperature in oC
  mutate(mdate=ymd(paste(year,"-",month,"-","15",sep="")))  # months characterized by their 15th day to keep them in time series format
tail(dat_month,20) # note the definition of the metyear_nov and metyear_dec variables, these months "belong" to the next year

# plot monthly average temperature with 5-year running average
dat_month |> ggplot(aes(x=mdate,y=avgtemp_mo_oC)) +
  geom_line() +
  tidyquant::geom_ma(ma_fun=SMA, n=61, col="red", linetype="solid")+ #SMA = simple moving average
  coord_x_date(xlim=c("2014-01-01", "2024-08-31")) + #zooming into a particular part of a graph
  ggtitle("Monthly Temperature at Lauwersoog") +
  theme(text=element_text(size=10))

# plot monthly rainfall with 1-year running average
# not yet working
dat_month2 <- dat_month |>
  dplyr::mutate(rain_SMA_12 = rollapply(totrain_mo_mm, width = 13, FUN = sum, fill=NA, 
                                   align="right")) #rollapply = moving average

dat_month2 |> ggplot() +
  geom_line(aes(x=mdate,y=totrain_mo_mm))+
  geom_line(aes(x=mdate,y=rain_SMA_12))+

##  
dat_month |> ggplot() +
  geom_line(aes(x=mdate,y=totrain_mo_mm)) +
  tidyquant::geom_ma(ma_fun=SMA, n=13, col="blue", linetype="solid")+
  coord_x_date(xlim=c("2014-01-01", "2024-08-31")) +
  ggtitle("Monthly Rainfall (mm) at Lauwersoog") +
  theme(text=element_text(size=10))

dat_month |> ggplot() +
  geom_line(aes(x = mdate, y = totrain_mo_mm)) +
  tidyquant::geom_ma(aes(x = mdate, y = totrain_mo_mm), 
                     ma_fun = SMA, n = 13, col = "blue", linetype = "solid") +
  coord_x_date(xlim = c(as.Date("2014-01-01"), as.Date("2024-08-31"))) +  
  ggtitle("Monthly Rainfall (mm) at Lauwersoog") +
  theme(text=element_text(size=10))

# additive time series decomposition of the monthly mean temperatures: what is seasonal and what is a longterm trend?
monthly_temps <- dat_month |>
  ungroup() |>
  dplyr::filter(year!=2024) |>
  dplyr::select(avgtemp_mo_oC)

plot(monthly_temps$avgtemp_mo_oC)
monthly_temps_ts <- ts(monthly_temps, frequency = 12, start = c(1992, 1))
plot(monthly_temps_ts)
monthly_temps_decomp <- decompose(monthly_temps_ts)
plot(monthly_temps_decomp)
  
# note the trend component is the same as the 12 month running average from the previous plot
# adjust the time series for the seasonal component, and plot it

  
# note again that really cold monthly temperatures (after seasonal correction) become rare, especially since since 2010
# while really warm months (after seasonal correction) become more common from 1991 -2016, while declining a bit again since
# to make the plots and do this in tidyverse style, you may want to explore the timetk package
# https://cran.r-project.org/web/packages/timetk/timetk.pdf


# average temperature of the coldest month per meteorological year
dat_metyear_dec <- dat_month |>
  dplyr::group_by(metyear_dec) |>
  summarise(minmonth=min(avgtemp_mo_oC, na.rm = T))
dat_metyear_dec

  # note that 2010 and 2011 were the last winters with a (on average) sub-zero month, 
# potentially relevant for cockle recruitment (crabs stay on mudflats eating spatfall)
dat_metyear_dec |>
  ggplot(aes(x=metyear_dec, y=minmonth)) +
  geom_line(linewidth=0.7) +
  geom_point(size=3)

# Hellmann index (sum of all below-zero daily average temperatures from 1 November of previous year until 31 march)
# see https://nl.wikipedia.org/wiki/Koudegetal
dat_Hellman <- dat |>
  dplyr::filter(month %in% c(11,12,1,2,3), TG<0) |>
  dplyr::group_by(metyear_nov) |>
  summarize(Hellman=sum(TG, na.rm = T))
tail(dat_Hellman, 10)

dat_Hellman |>
  ggplot(aes(x=metyear_nov, y=Hellman)) +
  geom_point()+
  geom_line()

# 1997 was the last "Elfstedentocht"
# 2014 -2024 (our transect study) is characterized by only warm winters

# Warmth index
library(quantreg)
# see for calculation https://www.knmi.nl/nederland-nu/klimatologie/lijsten/warmtegetallen
dat_WarmthIndex <- dat |>
  dplyr::filter(month %in% c(4:8), TG>18) |>
  dplyr::group_by(year) |>
  dplyr::summarize(WarmthIndex=sum(TG, na.rm = T))
tail(dat_WarmthIndex, 10)

dat_WarmthIndex |>
  ggplot(aes(x=year, y=WarmthIndex)) +
  geom_point(size=2)+
  #geom_line(linewidth=0.8) +
  geom_smooth(method="lm", formula=y~x) 

dat_WarmthIndex

#Now i want to have 10, 50 and 90% quantile regression added to the previous plot
dat_WarmthIndex |>
  ggplot(aes(x=year, y=WarmthIndex)) +
  geom_point(size=2)+
  geom_smooth(method="lm", formula=y~x, col="red") +
  geom_quantile(quantiles = c(0.1, 0.5, 0.9), col = "blue")


