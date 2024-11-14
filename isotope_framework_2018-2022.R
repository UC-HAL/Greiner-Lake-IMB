#Delete previous stored info
rm(list=ls(all=T))

#Set WD to the folder with all of your files
setwd("C:/Users/msleung/OneDrive - University of Calgary/Cambridge Bay/Isotope Framework")

lapply(c("weathercan", "ggplot2", "remotes", "GSODR", "dplyr", "lubridate", "tidyverse", "data.table", "zoo"), require, character.only = TRUE) #order of import matters

options(dplyr.width=Inf, tibble.print_min=50) #width changes the number of columns (set to infinity!), print changes the minimum number of rows when looking at the data within your console

# Downloading meteorological data ####

# stations_search(name = "Cambridge Bay", interval = "hour") #53512 or 54139 CB A, 32233 CB GSN

#wc.32233.daily <- weathercan::weather_dl(32233, start = "2018-01-01", end = "2022-12-31", 
#                                         interval = "day", quiet = TRUE) %>%
#  select(date, year, month, mean_temp, total_precip, snow_grnd) 

#write.csv(wc.day, file = "wc.day.csv")

#wc32233_hourly <- weather_dl(station_ids = 32233, start = "2018-01-01", end = "2022-12-31", interval = "hour", quiet = TRUE) %>% 
#  select(date, year, month, rel_hum, temp, temp_dew, pressure, wind_spd, snow_grnd) %>%
#  mutate(date = ymd(date)) %>%
#  na.omit(total_precip, rel_hum, temp, temp_dew, pressure, wind_spd, snow_grnd)

#write.csv(wc.hr, file = "wc.hr.csv")
#summary(wc.hr)

wc.day <- read.csv("wc32233_daily.csv", header = TRUE) %>% mutate(date = ymd(date))

summary(wc.day)
#NA's = mean_temp (3), total_precip (3), snow_grnd (158)

wc.hr <- read.csv("wc32233_hourly.csv", header = TRUE) %>% 
  mutate(date=ymd(ï..date)) %>% 
  select(-c(ï..date)) %>%
  group_by(date) %>%
  mutate(wind_spd = wind_spd * 0.2777778, #km/h to m/s
         h = mean(rel_hum, na.rm=TRUE)/100, #from % to decimal fraction
         #      temp = mean(temp, na.rm=TRUE), daily average better (see below)
         dew_temp = mean(temp_dew, na.rm=TRUE),
         #      precip = sum(precip_amt, na.RM=NULL), daily average better (see below)
         u = mean(wind_spd, na.rm=TRUE)) %>% #averaging hourly into daily
  filter(row_number()==1) %>% #==1 keeps the first row of the grouping, ==n() keeps the last row of the grouping
  ungroup() %>% #grouping can cause problems later on, best to avoid
  select(-c(rel_hum, temp_dew, precip_amt, wind_spd)) #remove hourly non-averaged variables 
  

summary(wc.hr)
#read raw hourly csv, check NA's --> pressure = 1, temp = 33, dew_temp = 21 NA's, wind spd = 7 NA's

# Comparing daily and hourly mean temperature and preciptation, use DAILY (mean_temp & total_precip) because NO NA values (21 and 9, respectively)
#day.vs.hr <- right_join(wc.hr, wc.day, by = c("date", "year")) %>%
#  select(date, year, precip, total_precip, temp, mean_temp)
#summary(day.vs.hr)

wc1822 <- right_join(wc.hr, wc.day, by = c("date", "year")) %>%
  select(date, year, total_precip, mean_temp, h, dew_temp, snow_grnd, u) 

#  filter(date >= "2018-05-01" & date <= "2018-09-30" |
#           date >= "2019-05-01" & date <= "2019-09-30" |
#           date >= "2020-05-01" & date <= "2020-09-30" |
#           date >= "2021-05-01" & date <= "2021-09-30")
  
summary(wc1821) 
#NA's - 54 u, 21 dew temp and temp (0), and 75 Ev, find from GSODR, export and manually add, then RE-RUN**
 
write.csv(wc1822,"wcgs1822.csv") #Manually added in missing/NA precip days from GSOD and save as wcgs1821_final.csv

library(GSODR)
gs1821 <- GSODR::get_GSOD(c(2018,2019,2020,2021), station = "719250-99999") %>% #Cambridge Bay GSN
  select(YEARMODA, MONTH, DAY, TEMP, DEWP, WDSP, PRCP, EA, ES, RH) %>% #Temp already in Celcius, precip in mm
  mutate(date = ymd(YEARMODA),
         h = (RH/100)) %>%  
  select(-c(YEARMODA, RH, DAY))

  filter(date >= "2018-05-01" & date <= "2018-09-30" |
           date >= "2019-05-01" & date <= "2019-09-30" |
           date >= "2020-05-01" & date <= "2020-09-30" |
           date >= "2021-05-01" & date <= "2021-09-30" ) 
#           date >= "2022-05-01" & date <= "2022-09-30")

#727503-99999 = Cambridge Municipality 1992-2005
#712880-99999 = CBay GSN 2004-2021
#719250/729250-99999 = CBay Airport (A) 1934-2021/1973-1977


#write.csv(gs1821, "gsod1821.csv") 

wcgs1822 <- read.csv("wcgs1822_final.csv", header = TRUE) %>% #_final has been manually changed outside of R
  mutate(date = ymd(DATE),
         ppt = precip,
         h = replace(h, h>0.88, 0.88), #setting max assumption of RH 0.88 to preserve dE turbulent equation
         ea = 6.108*exp((17.27*dew_temp)/(dew_temp+237.3)), #tried calculating ea with temp, yielded h > 1, use dew temp
         es = 6.108*exp((17.27*temp)/(temp+237.3)),
         fu = 2.9 + (2.1*u),
         Pa = 101.3*((temp+273.15-(0.0065*8))/(temp+273.15))^5.26, 
         Lv = 2501-(2.361*temp),
         Y = 1.63*(Pa/Lv),
         D = ((4098*(0.6108*exp((17.27*temp)/(temp+237.3))))/((temp+237.3)^2)), #SVP slope, capital Delta
         Ev = 0.0269*(1+((u/0.2777778)/16))*(es-ea)) %>% #Meyers 8m eq E=0.0269(1+(u/16))(es-ea) #(1.26/(1.26-1))*(Y/(D+Y))*fu*(es-ea)) %>% #Penman-Monteith, E = (a/a-1)*(Y/Delta+Y)*f(u)*(es-ea)
  select(-(c(precip, DATE)))

summary(wcgs1822)

write.csv(wcgs1822,"wcgs1822_finalcalc.csv")

wcgs_monthly <- wcgs1822 %>% group_by(month, year) %>% 
  mutate(m.u = mean(u, na.rm = T),
         m.snow = mean(snow_grnd, na.rm = T),
         evap = sum(Ev, na.rm = T),
         precip = sum(ppt, na.rm = T),
         m.temp = mean(temp, na.RM = TRUE),
         m.dewp = mean(dew_temp, na.RM = TRUE))%>%
  filter(row_number()==1) %>% 
  ungroup() %>%
  select(-c(ppt, h, ea, es, fu, Pa, Lv, Y, D, Ev))

summary(wcgs_monthly)

write.csv(wcgs_monthly,"wcgs_monthly.csv")

wcgs_annual <- wcgs_monthly %>% group_by(year) %>%
  mutate(u.kmh = mean(m.u, na.rm = T),
         snow.mmyr = mean(m.snow, na.rm = T),
         evap.mmyr = sum(evap, na.rm = T)*10, #cm/d to mm/yr
         ppt.mmyr = sum(precip, na.rm = T),
         temp.degC = mean(m.temp[!is.na(m.temp)]),
         dewp.degC = mean(m.dewp, na.RM = TRUE))%>%
  filter(row_number()==1) %>% 
  ungroup() %>%
  select(-c(m.temp, m.u, month, date, m.snow, m.dewp, precip))

write.csv(wcgs_annual,"wcgs_annual.csv")

# Isotope data analysis ####

#Finding mean observed values for precip (dP), lake (dL), outputs (dO), and inputs (dI)

iso1822 <- read.csv("iso1822.csv", header = TRUE) %>% mutate(date=ymd(ï..date)) %>% select(-c(ï..date))

summary(iso1822)

#Changing each type into it's own column
iso.rain<- iso1822 %>% filter(type == "Rain") %>%
  mutate(rn.18O = d18O,
         rn.2H = d2H) %>%
  select(-c(d18O, d2H, site, type))

iso.snow<- iso1822 %>% filter(type == "Snow") %>%
  mutate(sw.18O = d18O,
         sw.2H = d2H) %>%
  select(-c(d18O, d2H, site, type))

iso.lake<- iso1822 %>% filter(type == "Lake") %>%
  mutate(lk.18O = d18O,
         lk.2H = d2H) %>%
  select(-c(d18O, d2H, site, type))

iso.outputs<- iso1822 %>% filter (type == "River") %>%
  mutate(out.18O = d18O,
         out.2H = d2H) %>%
  select(-c(d18O, d2H, site, type))

iso.inputs<- iso1822 %>% filter(type == "Pond") %>%
  mutate(in.18O = d18O,
         in.2H = d2H) %>%
  select(-c(d18O, d2H, site, type))

#Don't know if there is a more efficient way to do this? 
in.outputs <- full_join(iso.outputs, iso.inputs, by =c("date", "year","new_site"))
io.lake <- full_join(in.outputs, iso.lake, by =c("date", "year","new_site"))
iol.rain <- full_join(io.lake, iso.rain, by =c("date", "year","new_site")) 
all.sources <- full_join(iol.rain, iso.snow, by =c("date", "year","new_site"))

write.csv(all.sources, "iso.all.sources.unsorted.csv")

#result should be 529 obs.
#summary(all.sources) #DIDNT WORK, KEPT MAKING DUPLICATE DATES, manually categorize

# iso.all%>%filter(is.na(dew_temp)) REMEMBER TO REMOVE ANY SAMPLES OUT OF WATERSHED TO AVOID MORE NA's

#Average days with multiple samples
iso.day.avg <- all.sources %>% group_by(date) %>%
  mutate(rn18O = mean(rn.18O, na.rm = TRUE),
         rn2H = mean(rn.2H, na.rm = TRUE),
         sw18O = mean(sw.18O, na.rm = TRUE),
         sw2H = mean(sw.2H, na.rm = TRUE),
         lk18O = mean(lk.18O, na.rm = TRUE),
         lk2H = mean(lk.2H, na.rm = TRUE),
         out18O = mean(out.18O, na.rm = TRUE),
         out2H = mean(out.2H, na.rm = TRUE),
         in18O = mean(in.18O, na.rm = TRUE),
         in2H = mean(in.2H, na.rm = TRUE)) %>%
  filter(row_number()==1) %>% #==1 keeps the first row of the grouping, ==n() keeps the last row of the grouping
  ungroup() %>%
  select (-c(rn.18O, rn.2H, sw.18O, sw.2H, lk.18O,lk.2H, out.18O, out.2H, in.18O, in.2H))

write.csv(iso.day.avg, "iso1822.by.source.csv")

#Add ppt and temp to compare and plot
#left_join(iso.day.avg, wcgs1821, by = c("date", "year"))

#Export to manual interpolate for NA days
#write.csv(iso.day.avg, "iso.daily.avg.csv") #so many NA's, move to monthly resolution

#Add in manually interpolated monthly precip

monthly.dLdPdIdO<-read.csv("dLPIO.monthly.csv", header = T) %>% mutate(year = ï..Year, month = Month) %>% select(-c(ï..Year, Month))

# Isotope endmember calculations ####
isof.1822 <- right_join(monthly.dLdPdIdO,wcgs1822, by = c("year","month")) %>%
  mutate(ap.18o = exp((-7.685/10^3+(6.7123/(temp+273.15))-
                        (1.6664*10^3/(temp+273.15)^2))+
                        (0.350410*10^6/(temp+273.15)^3)), #TAS edited brackets
         epp.18o = (ap.18o-1)*1000,
         epk.18o = 14.2*(1-h),
         ep.18o = epp.18o+epk.18o,
         m.18o = (h-(epp.18o/10^3))/(1-h+epk.18o/10^3),  #TAS modified
         ap.2H = exp((1158.8*((temp+273.15)^3/10^12))-
                       (1620.1*((temp+273.15)^2/10^9))+
                       (794.84*((temp+273.15)/10^6))-
                       (161.04/10^3)+
                       (2.9992*(10^6/(temp+273.15)^3))), #TAS edited brackets
         epp.2H = (ap.2H-1)*1000,
         epk.2H = 12.5*(1-h),
         ep.2H = epp.2H+epk.2H,
         m.2H = (h-(epp.2H/10^3))/(1-h+epk.2H/10^3),   #TAS modified
         dA.18o = (dP18O-epp.18o)/ap.18o, #TAS used equilibrium equation
         dA.2H = (dP2H-epp.2H)/ap.2H, #TAS used equilibrium equation
         dlim.18o = (h*dA.18o+epk.18o+epp.18o/ap.18o)/(h-(epk.18o+epp.18o/ap.18o)/10^3), #TAS edited brackets
         dlim.2H = (h*dA.2H+epk.2H+epp.2H/ap.2H)/(h-(epk.2H+epp.2H/ap.2H)/10^3),   #TAS edited brackets
         dS.18o = (dP18O+(m.18o*dlim.18o))*((1+m.18o)^-1), 
         dS.2H = (dP2H+(m.2H*dlim.2H))*((1+m.2H)^-1),  
         dE.18o = (((dL18O-epp.18o)/ap.18o)-(h*dA.18o)-epk.18o)/(1-h+epk.18o/10^3), #TAS: reviewed and OK, but make sure h is in decimal notation and dL is per mille 
         dE.2H = (((dL2H-epp.2H)/ap.2H)-(h*dA.2H)-epk.2H)/(1-h+epk.2H/10^3), #see comment above, where is dL2H/dL18o defined??
         x.18o = (dL18O-dI18O)/(dlim.18o-dL18O),
         x.2H = (dL2H-dI2H)/(dlim.2H-dL2H),
         y.18o = (dL18O-dI18O)/(dI18O-dE.18o),
         y.2H = (dL2H-dI2H)/(dI2H-dE.2H)) 

iso.endmembers <- isof.1822 %>%
  select(c(month, year, date, temp, dew_temp, Ev, ppt, m.18o, m.2H, dL18O, dL2H, dP18O, dP2H, dA.18o, dA.2H, dlim.18o, dlim.2H, dS.18o, dS.2H, dE.18o, dE.2H, x.18o, x.2H, y.18o, y.2H))

sLEL <- isof.1822 %>%
  mutate(sLEL =(((h*(dA.2H-dP2H)+(1+dP2H)*(epk.2H+(epp.2H/ap.2H)))/(h-epk.2H-(epp.2H/ap.2H)))/
           (((h*(dA.18o-dP18O)+(1+dP18O)*(epk.18o+(epp.18o/ap.18o)))/(h-epk.18o-(epp.18o/ap.18o)))))) %>%
  select(c(year, month, date, sLEL)) %>%
  group_by(year, month) %>%
  mutate(m.sLEL = mean(sLEL, na.rm = T)) %>%
  filter(row_number()==1) %>% 
  ungroup()

#Linear regression of LEL between flux weighted dE and dlim, indicating slope - S(LEL)
#Checks: *dlim > dL > din > dE > dA* (general) and dPw < dL < dP < dEw < dAw (high latitude) 

summary(iso.endmembers)

#Checks for any positive values
iso.endmembers%>%filter(dlim.18o > "0") #2018-07-03 (0.82), 07 (3.2)

iso.endmembers%>%filter(dE.18o > "0") #2018-05-22,23,25,26,28,29, 2022-05-05,06,14,17,19,25,27

#2018-05-22 = 1.569171
#2018-05-23 = 1.581949
#2018-05-25 = 3.869842 
#2018-05-26 = 4.169713
#2018-05-28 = 3.333708
#2018-05-29 = 4.546099

iso.endmembers%>%filter(dE.2H > "0") #Same as above, ranging from +15-69

#2018-05-05 = 15.27630
#2018-05-22 = 48.02330
#2018-05-23 = 48.19265
#2018-05-25 = 60.84310
#2018-05-26 = 64.86908
#2018-05-28 = 65.36882
#2018-05-29 = 69.92640
#2018-05-30 = 19.74846

iso.endmembers%>%filter(date==("2018-06-01")) 
#dlim > dL > din > dE > dA (general)
#dPw < dL < dP < dEw < dAw (high latitude)

write.csv(iso.endmembers, "isoframework.1822.daily.csv") #EXPORT AND REMOVE ALL POSITIVE VALUES
#Removed months 1-4, 10-12, 2022-09-22, Temp data NA
#Removed days with positive dlim or dE values


isof.annual <- read.csv("isoframework.1822.daily.final.csv", header = T) %>% group_by(month, year) %>% #Import modified file with no positive iso values
  mutate(dA_18O = mean(dA.18o, na.rm = T),
         dA_2H = mean(dA.2H, na.rm = T), 
         dlim_18O = mean(dlim.18o, na.rm = T),
         dlim_2H = mean(dlim.2H, na.rm = T),
         dS_18O = mean(dS.18o, na.rm = T),
         dS_2H = mean(dS.2H, na.rm = T),
         dE_18O = mean(dE.18o, na.rm = T),
         dE_2H = mean(dE.2H, na.rm = T),
         evap = sum(Ev, na.rm = T),
         precip = sum(ppt, na.rm = T),
         m.temp = mean(temp, na.RM = T), 
         m.dewp = mean(dew_temp, na.RM = T),
         mean.m.18O = mean(m.18o, na.RM =T), 
         mean.m.2H = mean(m.2H, na.RM =T),
         x.mean.18o = mean(x.18o, na.RM =T),
         x.mean.2h = mean(x.2h, na.RM=T),
         y.mean.18o = mean(y.18o, na.RM=T),
         y.mean.2h = mean(y.2h, na.RM=T)) %>% 
  filter(row_number()==1) %>% #==1 keeps the first row of the grouping, ==n() keeps the last row of the grouping
  ungroup() %>%
  select (c(month, year,dA_18O, dA_2H, dlim_18O, dlim_2H, dS_18O, dS_2H, dE_18O, dE_2H, 
            dL18O, dL2H, dP18O, dP2H, evap, precip, m.temp, m.dewp, mean.m.18O, mean.m.2H, x.mean.18o, x.mean.2h, y.mean.18o, y.mean.2h))

#write.csv(isof.annual,"isoframework.1822.annual.csv")

# Flux and amount weighting for precip, evap, and atmospheric moisture ####


#ppt.evap.2022 <- wcgs1822%>%filter(year == '2022') %>% 
#  summarize(sum.ppt= sum(ppt[!is.na(ppt)])) #[inside here] ignores NA values

wcgs5_9 <- wcgs_monthly %>% filter(month >= 5 & month <= 9) %>%
  group_by(year) %>%
  mutate(sum.ppt= sum(precip),
        sum.evap= sum(evap)) %>%
  filter(row_number()==1) %>%
  ungroup


write.csv(isof.annual,"isoframework.1822.annual.csv")

# Flux and amount weighting for precip, evap, and atmospheric moisture ####
#Use months 5-9 to match resolution of samples

summer.PEA <- isof.annual %>%
  group_by(month, year) %>%
  mutate(dPo_ppt = dP18O*precip,
         dPh_ppt = dP2H*precip,
         dEo_evap = dE_18O*evap,
         dEh_evap = dE_2H*evap,
         dAo_evap = dA_18O*evap,
         dAh_evap = dA_2H*evap) %>%
  ungroup () %>%
  select (c(month, year, evap, precip, dPo_ppt, dPh_ppt, dEo_evap, dEh_evap, dAo_evap, dAh_evap))

amt.flux.weight.PEA <- summer.PEA %>%
  group_by(year) %>%
  mutate(precip.sum = sum(precip, na.rm = T),
         evap.sum = sum(evap, na.rm = T),
         dPo_ppt.sum = sum(dPo_ppt, na.rm = T),
         dPh_ppt.sum = sum(dPh_ppt, na.rm = T),
         evap.sum = sum(evap, na.rm=T),
         dEo_evap.sum = sum(dEo_evap, na.rm=T),
         dEh_evap.sum = sum(dEh_evap, na.rm=T),
         dAo_evap.sum = sum(dAo_evap, na.rm=T),
         dAh_evap.sum = sum(dAh_evap, na.rm=T),
         dP18o.summer = dPo_ppt.sum/precip.sum, 
         dP2H.summer = dPh_ppt.sum/precip.sum,
         dE18o.summer = dEo_evap.sum/evap.sum,
         dE2H.summer = dEh_evap.sum/evap.sum,
         dA18o.summer = dAo_evap.sum/evap.sum,
         dA2H.summer = dEh_evap.sum/evap.sum) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(c(year,dP18o.summer, dP2H.summer, dE18o.summer, dE2H.summer, dA18o.summer, dA2H.summer, precip.sum, evap.sum))#YESSSSS 

write.csv(amt.flux.weight.PEA, "1822_amount_flux_weight_dPEA.csv")

#looking for annual m values

m.annual <- isof.1822 %>%
  group_by(year) %>%
  mutate(m.18O.mean = mean(m.18o[!is.na(m.18o)]),
         m.2H.mean = mean(m.2H[!is.na(m.2H)]), 
         precip.sum = sum(ppt[!is.na(ppt)]),
         evap.sum = sum(Ev[!is.na(Ev)])) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(c(year, m.18O.mean, m.2H.mean, precip.sum, evap.sum))

# Greiner Lake Depth averages ####

#Not sure if I need this anymore
#For barometric and altitude corrections

#RUN WC.HR BEFORE THE FOLLOWING LINES**#

#isolating water pressure values from the rest of the met data 
GL1820 <- readxl::read_excel("GL_depth.xlsx") %>%
  mutate(altitude_corr = (water_pressure_dbar*10)-((8/304.8)*3.386)) 

GL20n21 <- readxl::read_excel("GL_depth_aug20_21.xlsx") %>%
  mutate(altitude_corr = (water_pressure_dbar*10)-((8/304.8)*3.386))

GLdepth <- bind_rows(GL1820, GL20n21)

#Making a separate reformatted string for YMD without HMS
GL.dep <- format(as.POSIXct(GLdepth$time, format='%Y/%m/%d %H:%M:%S'), format = '%Y/%m/%d')

summary(GL.dep)

#Changing hourly to daily data
GL_depth <- bind_cols(GLdepth, GL.dep) %>% 
  mutate(date = ymd(...5)) %>%
  group_by(date) %>%
  mutate(waterp = mean(water_pressure_dbar, na.RM =T),
         dep = mean(depth, na.RM = T),
         alt = mean(altitude_corr)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(-c(time, ...5, depth, altitude_corr, water_pressure_dbar))

#Barometric correction
GLcorr <- right_join(wc.hr, GL_depth, by = "date") %>%
  mutate(barometric_corr = alt - pressure,
         corrected_depth = (barometric_corr/9.8064)+7.6) %>%
  select(-c(pressure)) 
### Make time series on the change in depth or for change in volume??****

# Finding depth on peak and baseflow dates for 2018-2021
# 2018 - P: 07/03 - BF:10/13
# 2019 - P: 07/10 - BF: 11/01
# 2020 - P: 07/20 - BF: 10/18
# 2021 - P: 07/27 - BF: 11/27

#Average depth by month
depth.monthly <- GLcorr %>%
  separate(date, c("y", "m", "d")) 
  group_by(y, m) %>% 
  mutate(m.depth = mean(corrected_depth, na.RM = T)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(-c(depth, water_pressure_dbar, altitude_corr)) 

summary(depth.monthly)

write.csv(GL_depth, "GL_depth_by_month.csv")

#Average annual depth, all 12 months

GL5_9 <- GLcorr %>%  
  filter(date >= "2018-05-01" & date <= "2018-09-30" |
           date >= "2019-05-01" & date <= "2019-09-30" |
           date >= "2020-05-01" & date <= "2020-09-30" |
           date >= "2021-05-01" & date <= "2021-09-30") %>%
  group_by(year) %>%
  summarize(mean_depth = mean(corrected_depth))

depth.annual <- GL_depth %>% 
  group_by(y) %>%
  mutate(f.depth = mean(m.depth, na.RM = T)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(-c(m,d,m.depth))

#Group new_sites for all years

site.coord.iso.longterm <- read.csv("iso1822.csv", header = TRUE) %>% 
  mutate(date=ymd(ï..date)) %>% select(-c(ï..date)) %>% 
  group_by(new_site) %>%
  mutate(m.d18o = mean(d18O, na.RM = T),
         m.d2h = mean(d2H, na.RM = T)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(-c(site, year, date, d18O, d2H))

#average by year
site.coord.iso.annual <- read.csv("iso1822.csv", header = TRUE) %>% 
  mutate(date=ymd(ï..date)) %>% select(-c(ï..date)) %>% 
  group_by(new_site, year) %>%
  mutate(m.d18o = mean(d18O, na.RM = T),
         m.d2h = mean(d2H, na.RM = T)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(-c(site, date, d18O, d2H, type))

#change each annual average to its own column
d2018.avg <- filter(site.coord.iso.annual, year=="2018") %>%
  select(-c(year)) %>%
  rename(d18o.2018 = m.d18o,
         d2h.2018 = m.d2h)
  
d2019.avg <- filter(site.coord.iso.annual, year=="2019") %>%
  select(-c(year)) %>%
  rename(d18o.2019 = m.d18o,
         d2h.2019 = m.d2h)

d2020.avg <- filter(site.coord.iso.annual, year=="2020") %>%
  select(-c(year)) %>%
  rename(d18o.2020 = m.d18o,
         d2h.2020 = m.d2h)

d2021.avg <- filter(site.coord.iso.annual, year=="2021") %>%
  select(-c(year)) %>%
  rename(d18o.2021 = m.d18o,
         d2h.2021 = m.d2h)

d2022.avg <- filter(site.coord.iso.annual, year=="2022") %>%
  select(-c(year)) %>%
  rename(d18o.2022 = m.d18o,
         d2h.2022 = m.d2h)

d.avg.list <- list(d2018.avg,d2019.avg,d2020.avg,d2021.avg,d2022.avg)

d.1822.avg <- d.avg.list %>% reduce(full_join, by="new_site")

#add in site coordinates and sample quantities together

combined.sites <- readxl::read_xlsx("combined_sites_all.xlsx") %>%
  rename(new_site= newsite)

#iso sigs over 5 year period
combined.iso.lt <- inner_join(combined.sites,site.coord.iso.longterm, by="new_site") %>%
  rename(site = new_site,
         d18o.lt = m.d18o,
         d2h.lt = m.d2h,
         lat = latitude,
         long = longtitude,
         total_qty = sitetotal) %>%
  select(-c(emdmember))

#iso sigs by year
combined.iso.annual <- full_join(combined.sites,d.1822.avg,by="new_site") %>%
  rename(site = new_site,
         lat = latitude,
         long = longtitude,
         totalqty = sitetotal)

write.csv(combined.iso.lt, "combined.coord.iso.longterm.csv")

write.csv(combined.iso.annual, "combined.coord.iso.annual.csv")


# LMWL <- delta^2,"H" = 7.76 * delta^18,"O" + 2.81???
# GMWL <- delta^2,"H" = 8.0 * delta^18,"O" + 10.0???

# Correlation and t tests for isotope endmembers ####

#Upload isof endmembers

iso.em.d18O <- read.csv("cb.iso.endmembers.d18o.csv",header = T) #this file is a manual combination of the weighted calculated values from above and the observed values
#R adds uppercase X in front of each year so variable does not begin with a number

#Spearman-rank correlation tests between long term and each year

scorr_LTO_18 <- cor.test(iso.em.d18O$X1822, iso.em.d18O$X2018,
         alternative = c("two.sided"),
         method = "spearman",
         exact = NULL, conf.level = 0.95, continuity = FALSE)

scorr_LTO_19 <- cor.test(iso.em.d18O$X1822, iso.em.d18O$X2019,
                  alternative = c("two.sided"),
                  method = "spearman",
                  exact = NULL, conf.level = 0.95, continuity = FALSE)

scorr_LTO_21 <- cor.test(iso.em.d18O$X1822, iso.em.d18O$X2021,
                  alternative = c("two.sided"),
                  method = "spearman",
                  exact = NULL, conf.level = 0.95, continuity = FALSE)

scorr_LTO_22 <- cor.test(iso.em.d18O$X1822, iso.em.d18O$X2022,
                  alternative = c("two.sided"),
                  method = "spearman",
                  exact = NULL, conf.level = 0.95, continuity = FALSE)
scorr_LTO_18
scorr_LTO_19
scorr_LTO_21
scorr_LTO_22

iso.em.d2H <- read.csv("cb.iso.endmembers.d2h.csv",header = T)

scorr_LTH_18 <- cor.test(iso.em.d2H$X1822, iso.em.d2H$X2018,
                         alternative = c("two.sided"),
                         method = "spearman",
                         exact = NULL, conf.level = 0.95, continuity = FALSE)

scorr_LTH_19 <- cor.test(iso.em.d2H$X1822, iso.em.d2H$X2019,
                   alternative = c("two.sided"),
                   method = "spearman",
                   exact = NULL, conf.level = 0.95, continuity = FALSE)

scorr_LTH_21 <- cor.test(iso.em.d2H$X1822, iso.em.d2H$X2021,
                   alternative = c("two.sided"),
                   method = "spearman",
                   exact = NULL, conf.level = 0.95, continuity = FALSE)

scorr_LTH_22 <- cor.test(iso.em.d2H$X1822, iso.em.d2H$X2022,
                   alternative = c("two.sided"),
                   method = "spearman",
                   exact = NULL, conf.level = 0.95, continuity = FALSE)

scorr_LTH_18
scorr_LTH_19
scorr_LTH_21
scorr_LTH_22

# Pearson correlation matrix ####
#Use the range of monthly values instead of seasonal endmembers

#dP
#values for all years, May-Oct
dP18O <-  read.csv("dP.18O.csv",header = T) %>%
  mutate(X2018=ï..2018) %>% select(-c(ï..2018)) 

dP2H <-  read.csv("dP.2H.csv",header = T) %>%
  mutate(X2018=ï..2018) %>% select(-c(ï..2018)) 

pcorr_P18O <- cor(dP18O, method = "pearson")
pcorr_P2H <- cor(dP2H, method = "pearson")

#dI
#21,22 jul-oct
dI18O <- read.csv("dI.18O.csv",header = T) %>%
  mutate(X2021=ï..2021) %>% select(-c(ï..2021)) 

dI2H <-  read.csv("dI.2H.csv",header = T) %>%
  mutate(X2021=ï..2021) %>% select(-c(ï..2021)) 

pcorr_I18O <- cor(dI18O, method = "pearson")
pcorr_I2H <- cor(dI2H, method = "pearson")

#dO
#18,19,21,22 jul-sept
dO18O.3mo <- read.csv("dO.18O.jul-sept.csv",header = T) %>%
  mutate(X2018=ï..2018) %>% select(-c(ï..2018)) 

dO2H.3mo <-  read.csv("dO.2H.jul-sept.csv",header = T) %>%
  mutate(X2018=ï..2018) %>% select(-c(ï..2018)) 

pcorr_O18O.3mo <- cor(dO18O.3mo, method = "pearson")
pcorr_O2H.3mo <- cor(dO2H.3mo, method = "pearson")


#ONE sample t-test between each year & LT
#Null hypothesis - the mean of 20__ is equal to the long term average
#Alternate hypothesis - the mean of 20__ is not equal to the long term average
iso.em <- read.csv("cb.iso.endmembers.final.csv",header = T) %>% 
  mutate(year = ï..Year) %>% select(-c(ï..Year))

#dP
#checking data distribution for normality
iso.p <- iso.em %>% filter(endmember == "dP")
shapiro.test(iso.p$d18O) #p>0.05 means normal = normal
shapiro.test(iso.p$d2H) #normal

#d18o
dP.18o.2018 <- iso.p %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dPO.2018 <- t.test(dP.18o.2018, mu=-19.07, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dPO.2018
dP.18o.2019 <- iso.p %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dPO.2019 <- t.test(dP.18o.2019, mu=-19.07, alternative = "two.sided")
tt.dPO.2019
dP.18o.2021 <- iso.p %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dPO.2021 <- t.test(dP.18o.2021, mu=-19.07, alternative = "two.sided")
tt.dPO.2021
dP.18o.2022 <- iso.p %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dPO.2022 <- t.test(dP.18o.2022, mu=-19.07, alternative = "two.sided")
tt.dPO.2022

#d2h
dP.2h.2018 <- iso.p %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dPH.2018 <- t.test(dP.2h.2018, mu=-147.5, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dPH.2018
dP.2h.2019 <- iso.p %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dPH.2019 <- t.test(dP.2h.2019, mu=-147.5, alternative = "two.sided")
tt.dPH.2019
dP.2h.2021 <- iso.p %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dPH.2021 <- t.test(dP.2h.2021, mu=-147.5, alternative = "two.sided")
tt.dPH.2021
dP.2h.2022 <- iso.p %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dPH.2022 <- t.test(dP.2h.2022, mu=-147.5, alternative = "two.sided")
tt.dPH.2022

#dL
iso.l <- iso.em %>% filter(endmember == "dL")
shapiro.test(iso.l$d18O) #normal 
shapiro.test(iso.l$d2H) #normal

dL.18o.2018 <- iso.l %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dLO.2018 <- t.test(dL.18o.2018, mu=-18.58, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dLO.2018
dL.18o.2019 <- iso.l %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dLO.2019 <- t.test(dL.18o.2019, mu=-18.58, alternative = "two.sided")
tt.dLO.2019
dL.18o.2021 <- iso.l %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dLO.2021 <- t.test(dL.18o.2021, mu=-18.58, alternative = "two.sided")
tt.dLO.2021
dL.18o.2022 <- iso.l %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dLO.2022 <- t.test(dL.18o.2022, mu=-18.58, alternative = "two.sided")
tt.dLO.2022

dL.2h.2018 <- iso.l %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dLH.2018 <- t.test(dL.2h.2018, mu=-148.4, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dLH.2018
dL.2h.2019 <- iso.l %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dLH.2019 <- t.test(dL.2h.2019, mu=-148.4, alternative = "two.sided")
tt.dLH.2019
dL.2h.2021 <- iso.l %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dLH.2021 <- t.test(dL.2h.2021, mu=-148.4, alternative = "two.sided")
tt.dLH.2021
dL.2h.2022 <- iso.l %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dLH.2022 <- t.test(dL.2h.2022, mu=-148.4, alternative = "two.sided")
tt.dLH.2022

#d*
iso.ast <- iso.em %>% filter(endmember == "d*")
shapiro.test(iso.ast$d18O) #normal 
shapiro.test(iso.ast$d2H) #normal

dast.18o.2018 <- iso.ast %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dastO.2018 <- t.test(dast.18o.2018, mu=-13.73, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dastO.2018
dast.18o.2019 <- iso.ast %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dastO.2019 <- t.test(dast.18o.2019, mu=-13.73, alternative = "two.sided")
tt.dastO.2019
dast.18o.2021 <- iso.ast %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dastO.2021 <- t.test(dast.18o.2021, mu=-13.73, alternative = "two.sided")
tt.dastO.2021
dast.18o.2022 <- iso.ast %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dastO.2022 <- t.test(dast.18o.2022, mu=-13.73, alternative = "two.sided")
tt.dastO.2022

dast.2h.2018 <- iso.ast %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dastH.2018 <- t.test(dast.2h.2018, mu=-126, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dastH.2018
dast.2h.2019 <- iso.ast %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dastH.2019 <- t.test(dast.2h.2019, mu=-126, alternative = "two.sided")
tt.dastH.2019
dast.2h.2021 <- iso.ast %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dastH.2021 <- t.test(dast.2h.2021, mu=-126, alternative = "two.sided")
tt.dastH.2021
dast.2h.2022 <- iso.ast %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dastH.2022 <- t.test(dast.2h.2022, mu=-126, alternative = "two.sided")
tt.dastH.2022

#dA
iso.A <- iso.em %>% filter(endmember == "dA")
shapiro.test(iso.A$d18O) #normal 
shapiro.test(iso.A$d2H) #normal

dA.18o.2018 <- iso.A %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dAO.2018 <- t.test(dA.18o.2018, mu=-29.07, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dAO.2018
dA.18o.2019 <- iso.A %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dAO.2019 <- t.test(dA.18o.2019, mu=-29.07, alternative = "two.sided")
tt.dAO.2019
dA.18o.2021 <- iso.A %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dAO.2021 <- t.test(dA.18o.2021, mu=-29.07, alternative = "two.sided")
tt.dAO.2021
dA.18o.2022 <- iso.A %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dAO.2022 <- t.test(dA.18o.2022, mu=-29.07, alternative = "two.sided")
tt.dAO.2022

dA.2h.2018 <- iso.A %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dAH.2018 <- t.test(dA.2h.2018, mu=-265.2, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dAH.2018
dA.2h.2019 <- iso.A %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dAH.2019 <- t.test(dA.2h.2019, mu=-265.2, alternative = "two.sided")
tt.dAH.2019
dA.2h.2021 <- iso.A %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dAH.2021 <- t.test(dA.2h.2021, mu=-265.2, alternative = "two.sided")
tt.dAH.2021
dA.2h.2022 <- iso.A %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dAH.2022 <- t.test(dA.2h.2022, mu=-265.2, alternative = "two.sided")
tt.dAH.2022

#dS
iso.S <- iso.em %>% filter(endmember == "dS")
shapiro.test(iso.S$d18O) #Y
shapiro.test(iso.S$d2H) #Y

dS.18o.2018 <- iso.S %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dSO.2018 <- t.test(dS.18o.2018, mu=-14.84, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dSO.2018
dS.18o.2019 <- iso.S %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dSO.2019 <- t.test(dS.18o.2019, mu=-14.84, alternative = "two.sided")
tt.dSO.2019
dS.18o.2021 <- iso.S %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dSO.2021 <- t.test(dS.18o.2021, mu=-14.84, alternative = "two.sided")
tt.dSO.2021
dS.18o.2022 <- iso.S %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dSO.2022 <- t.test(dS.18o.2022, mu=-14.84, alternative = "two.sided")
tt.dSO.2022

dS.2h.2018 <- iso.S %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dSH.2018 <- t.test(dS.2h.2018, mu=-131.1, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dSH.2018
dS.2h.2019 <- iso.S %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dSH.2019 <- t.test(dS.2h.2019, mu=-131.1, alternative = "two.sided")
tt.dSH.2019
dS.2h.2021 <- iso.S %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dSH.2021 <- t.test(dS.2h.2021, mu=-131.1, alternative = "two.sided")
tt.dSH.2021
dS.2h.2022 <- iso.S %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dSH.2022 <- t.test(dS.2h.2022, mu=-131.1, alternative = "two.sided")
tt.dSH.2022

#dE
iso.E <- iso.em %>% filter(endmember == "dE")
shapiro.test(iso.E$d18O) #normal 
shapiro.test(iso.E$d2H) #normal

dE.18o.2018 <- iso.E %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dEO.2018 <- t.test(dE.18o.2018, mu=-44.85, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dEO.2018
dE.18o.2019 <- iso.E %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dEO.2019 <- t.test(dE.18o.2019, mu=-44.85, alternative = "two.sided")
tt.dEO.2019
dE.18o.2021 <- iso.E %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dEO.2021 <- t.test(dE.18o.2021, mu=-44.85, alternative = "two.sided")
tt.dEO.2021
dE.18o.2022 <- iso.E %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dEO.2022 <- t.test(dE.18o.2022, mu=-44.85, alternative = "two.sided")
tt.dEO.2022

dE.2h.2018 <- iso.E %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dEH.2018 <- t.test(dE.2h.2018, mu=-265.2, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dEH.2018
dE.2h.2019 <- iso.E %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dEH.2019 <- t.test(dE.2h.2019, mu=-265.2, alternative = "two.sided")
tt.dEH.2019
dE.2h.2021 <- iso.E %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dEH.2021 <- t.test(dE.2h.2021, mu=-265.2, alternative = "two.sided")
tt.dEH.2021
dE.2h.2022 <- iso.E %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dEH.2022 <- t.test(dE.2h.2022, mu=-265.2, alternative = "two.sided")
tt.dEH.2022

#dI
iso.I <- iso.em %>% filter(endmember == "dI")
shapiro.test(iso.I$d18O) #normal 
shapiro.test(iso.I$d2H) #normal

dI.18o.2018 <- iso.I %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dIO.2018 <- t.test(dI.18o.2018, mu=-21.7, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dIO.2018
dI.18o.2019 <- iso.I %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dIO.2019 <- t.test(dI.18o.2019, mu=-21.7, alternative = "two.sided")
tt.dIO.2019
dI.18o.2021 <- iso.I %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dIO.2021 <- t.test(dI.18o.2021, mu=-21.7, alternative = "two.sided")
tt.dIO.2021
dI.18o.2022 <- iso.I %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dIO.2022 <- t.test(dI.18o.2022, mu=-21.7, alternative = "two.sided")
tt.dIO.2022

dI.2h.2018 <- iso.I %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dIH.2018 <- t.test(dI.2h.2018, mu=-164.2, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dIH.2018
dI.2h.2019 <- iso.I %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dIH.2019 <- t.test(dI.2h.2019, mu=-164.2, alternative = "two.sided")
tt.dIH.2019
dI.2h.2021 <- iso.I %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dIH.2021 <- t.test(dI.2h.2021, mu=-164.2, alternative = "two.sided")
tt.dIH.2021
dI.2h.2022 <- iso.I %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dIH.2022 <- t.test(dI.2h.2022, mu=-164.2, alternative = "two.sided")
tt.dIH.2022

#dO
iso.O <- iso.em %>% filter(endmember == "dO")
shapiro.test(iso.O$d18O) #normal 
shapiro.test(iso.O$d2H) #normal

dO.18o.2018 <- iso.O %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dOO.2018 <- t.test(dO.18o.2018, mu=-19.21, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dOO.2018
dO.18o.2019 <- iso.O %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dOO.2019 <- t.test(dO.18o.2019, mu=-19.21, alternative = "two.sided")
tt.dOO.2019
dO.18o.2021 <- iso.O %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dOO.2021 <- t.test(dO.18o.2021, mu=-19.21, alternative = "two.sided")
tt.dOO.2021
dO.18o.2022 <- iso.O %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dOO.2022 <- t.test(dO.18o.2022, mu=-19.21, alternative = "two.sided")
tt.dOO.2022

dO.2h.2018 <- iso.O %>% select(-c(d2H,endmember)) %>% filter(year == 2018)
tt.dOH.2018 <- t.test(dO.2h.2018, mu=-153.2, alternative = "two.sided")#mu taken from LT value, comparing each year to LT
tt.dOH.2018
dO.2h.2019 <- iso.O %>% select(-c(d2H,endmember)) %>% filter(year == 2019)
tt.dOH.2019 <- t.test(dO.2h.2019, mu=-153.2, alternative = "two.sided")
tt.dOH.2019
dO.2h.2021 <- iso.O %>% select(-c(d2H,endmember)) %>% filter(year == 2021)
tt.dOH.2021 <- t.test(dO.2h.2021, mu=-153.2, alternative = "two.sided")
tt.dOH.2021
dO.2h.2022 <- iso.O %>% select(-c(d2H,endmember)) %>% filter(year == 2022)
tt.dOH.2022 <- t.test(dO.2h.2022, mu=-153.2, alternative = "two.sided")
tt.dOH.2022




