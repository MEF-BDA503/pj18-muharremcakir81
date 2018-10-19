install.packages("tidyverse", repos = "https://cran.r-project.org")

library(tidyverse)
#calışacağımız datanın bulunduğu dırectory'ı set ediyoruz':
setwd("C:/Users/STUDENT/Desktop/Travel_Weather")

#Datayı yükluyoruz:
load("travel_weather.RData")

#datayı data frame' e dönüştürüyoruz.
travel_weather %>%
  tbl_df()


#data frame'in yapısını görüntülemek için:
str(travel_weather)
glimpse(travel_weather)

#select : istenilen kolonları seçmemizi sağlar:
travel_weather %>% select(year, month, day, Venice)


#belirli aralıktaki kolonları seçmek için:
#kolon isimlerini kullanarak:
travel_weather %>% select(Amsterdam:Venice)

#kolon indexlerini kullanarak:
travel_weather %>% select(4:7)

#belirtilen kolon dışındaki tüm kolonları seçmek için:
travel_weather %>% select(-Amsterdam,-Venice)

#Kolon ismini rename yapmak için:
travel_weather %>% rename('New York' = NYC)

#Filter 
#ayın ilk üç gününü filtrelemek için:
travel_weather %>%
  filter(day <= 3)

#temmuz ayında Amsterdam london'dan veya VEnice'den sıcak oldupu günleri bulmak için:
travel_weather %>%
  filter(month == 7 & (Amsterdam > London | Amsterdam >Venice))

#temmuz ayında Amsterdam london'dan veya VEnice'den sıcak oldupu günleri bulmak için:
travel_weather %>%
  filter(abs(Amsterdam - Venice) >= 12)

#arrange : sort
travel_weather %>%
  arrange(year)

#NYC increasing, Amsterdam decreasing 
travel_weather %>%
  arrange(NYC ,desc(Amsterdam ))

travel_weather %>%
  arrange(desc(year), desc(month), desc(day)  )  

#mutate: Mutate ile yeni bir kolon oluşturup bu kolun tüm datasetin kolonları ile birlikte görüntülememizi sağlıyor:
travel_weather %>%
  mutate(VAdiff = Venice  - Amsterdam)

#venice sıcaklığı amsterdam'dan büyük olan kayıtları filtrelemek için:
travel_weather %>%
  mutate(VwarmerA = Venice > Amsterdam) %>% filter(VwarmerA)

#transmute : mutate'den farkı select gibi istediğimiz kolonları seçmemize imkan sağlıyor. 
travel_weather %>%
  transmute(year,month,day,
            VwarmerA = ifelse(Venice > Amsterdam,"Warmer", "Colder"))


#summarise : 
travel_weather %>%
  summarise(Venice_mean=mean(Venice),NYC_mean = mean(NYC))

#group_by : belirli kolonlara göre group by yapmak için:

travel_weather %>%
  group_by(month) %>%
  summarise(Amsterdam_mean=mean(Amsterdam))

# Amsterdam'ın NYC'den daha sıcak olduğu günlerin sayısını yıl ve ay bazında grouplamak için:
travel_weather %>%
  group_by(year,month) %>%
  summarise(AwarmerN_n=sum(Amsterdam > NYC))

#London'ın sıcaklığını yıl ve ay bazında min,max, median değerlerini hesaplamak için:
travel_weather %>%
  group_by(year,month) %>%
  summarise(London_min=min(London),London_median=median(London),London_max=max(London))


#Lead and Lag: bir satırın bir altındaki ve bir üstündeki satırı ilgili satıra yazan 
travel_weather %>%
  transmute(year,month,day,Amsterdam,A_prev=lag(Amsterdam),A_next=lead(Amsterdam),
            A_prev_diff=Amsterdam-A_prev,A_next_diff=Amsterdam-A_next)

#Slice: istenilen satırların getirilmesini sağlar:
travel_weather %>%
  slice(1:3)


travel_weather %>%
  group_by(year) %>%
  slice(1:3)

#Gather and Spread : pivot, unpivot:

#Gather : Unpivot
travel_weather_long <-
  travel_weather %>%
  gather(key=City,value=Temperature,-year,-month,-day)

travel_weather_long


#Spread: Pivot

travel_weather_long %>%
  group_by(month,City) %>%
  summarise(temp_avg=round(mean(Temperature))) %>%
  #Now spread the months to the columns
  spread(month,temp_avg)


#_all and _at prefixes
#Method 1:
travel_weather %>%
  select(Amsterdam:Venice) %>%
  summarise_all(funs(round(mean(.))))

#Method 2:
travel_weather %>%
  summarise_at(vars(Amsterdam:Venice),funs(round(mean(.))))

#Method 3: vars: hangi sütunlarda uygulamasını istediğimizi belirtiyoruz. funs: hangi işlemi yapacağımızı belirtiyoruz. 
travel_weather %>%
  mutate_at(vars(Amsterdam,London,Venice),funs(diff_NYC=abs(NYC-.))) %>%
  select(-Amsterdam,-London,-Venice)

#Exercise 1: Return the dates which Amsterdam is strictly warmer than London but strictly colder than Venice
travel_weather %>%
  filter(Amsterdam > London & Amsterdam < Venice)

#Exercise 2: For each month of each year calculate the average difference between NYC 
#and Amsterdam for the days NYC is strictly warmer than Amsterdam, 
#rounded by 1 decimal. Arrange from the highest difference to the lowest.
travel_weather%>%
  filter(NYC > Amsterdam)  %>%
  mutate(VwarmerA = NYC - Amsterdam) %>%
  group_by(year,month) %>%
  summarise(temp_avg=round(mean(VwarmerA),1)) %>%
  arrange(desc(temp_avg) )

#Exercise 3:
#Return the warmest city and its temperature of each day.

travel_weather%>%
  gather(key=City,value=Temperature,-year,-month,-day) %>%
  arrange(desc(Temperature)) %>%
 group_by(year,month,day) %>%
   slice(1)
  
  







