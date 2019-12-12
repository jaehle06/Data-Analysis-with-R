#https://www.kaggle.com/chrisbow/e-commerce-eda-and-segmentation-with-r
library(readr)
library(dplyr)
library(ggplot2)
library(DataExplorer)

custData <- read_csv("EcommerceData.csv")

glimpse(custData)
#dim : dataframe의 길이를 관측할 때 사용하며, 행과 열의 개수를 모두 출력합니다.
dim(custData) 

options(repr.plot.width=8, repr.plot.height =3)
# DataExplorer 패키지로 결측치를 찾음
plot_missing(custData)
# 데이터의 사이즈를 확인하고 결측치를 plot으로 확인
# 만약 이 결측치를 제거하고 괜찮은 데이터 셋이 나오면 분석 고고
custData <- na.omit(custData)
dim(custData)

# 확인 해보니 400000 행의 데이터를 갖고 주무를 것이고
# 한가지 눈에 보이는 건 invoiceData변수임 왜냐면 Character 변수라서..
# 하지만 이걸 땡겨와서 시간 정보로 2변수를 만들 수 있음 날짜, 시간
# 월, 년, 시간

# invoicedate 의 날짜와 시간 component 분리 하자
custData$date <- sapply(custData$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
custData$time <- sapply(custData$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})
# 월, 년, 시간 변수 만들자
custData$month <- sapply(custData$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][1]})
custData$year <- sapply(custData$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][3]})
custData$hourOfDay <- sapply(custData$time, FUN = function(x) {strsplit(x, split = '[:]')[[1]][1]})
head(custData ,n=5)
str(custData)
# 그럼 invoice date 컬럼에서 이제 좀 쓸모있는 component 들을 뽑아 낸거 고
# 그리고 이 뽑아낸 년, 월 , 시간을 적절한 클래스로 변환 해줘야함 

custData$date <- as.Date(custData$date, '%m/%d/%Y')
head(custData)


# date변수를 date 클래스로 변환했기 때문에 lubridate 패키치의 wday 함수를 사용해서 
# 요일을 나타내는 새로운 변수를 만들수 있음
library(lubridate)
custData$dayOfWeek <- wday(custData$date, label = TRUE)

# 그리고 수량(quantity)이랑 단가(unit price)를 곱해서 컬럼을 한 개더 만들어야 함 

custData <- custData %>% mutate(lineTotal = Quantity * UnitPrice)
head(custData)

# 이제 괜찮은 데이터프레임이 나왔지만 
# 제품 및 고객 분류에 참여하기 전에 데이터 세트의 더 큰 기능 중 일부를 살펴 보겠습니다. 
# 먼저 적절한 variable를 factor로 바꾸자

custData$Country <- as.factor(custData$Country)
custData$month <- as.factor(custData$month)
custData$year <- as.factor(custData$year)
levels(custData$year) <- c(2010,2011)
custData$hourOfDay <- as.factor(custData$hourOfDay)
custData$dayOfWeek <- as.factor(custData$dayOfWeek)
#이제 데이터를 본격적으로 분석해보자
names(custData)
options(repr.plot.width = 8, repr.plot.height = 3)

custData %>%
  group_by(date) %>%
  summarise(revenue = sum(lineTotal)) %>%
  ggplot(aes(x = date, y = revenue)) + geom_line() + geom_smooth(method = 'auto', se = FALSE) + labs(x = 'Date', y = 'Revenue (£)', title = 'Revenue by Date')

# 위 그래프는 매출이 상승하고 있는 것처럼 보이나 취할만한 인사이트를 나타내진 못함
# 그러니까 좀더 데이터를 파보자

#요일별 분석
# lubridate 패키지를 통해서 각 날짜를 요일로 할당했음
# 일주일이 지날 수록 난 다른 마음가짐을 가지고 있긴한데 다른 사람들은 일지일이 지날 수록 더 많이 소비할 가능성이 있을까?
# 일요일 오후를 지나면? 금요일 오후에 직장에서? 아니면 월요일?

custData %>% 
  group_by(dayOfWeek) %>% 
  summarise(revenue = sum(lineTotal)) %>% 
  ggplot(aes(x=dayOfWeek, y=revenue)) +geom_col() +labs(x='Day of Week' , y='Revenue', title = 'Revenue by Day of Week')


# 막대 그래프를 확인 해보면 특정 요일에 매출이 창출되는걸 볼 수 있음
# 새로운 데이터 프레임을 만들어서 요일별로 좀 더 딥하게 보자

weekdaySummary <- custData %>% 
  group_by(date, dayOfWeek) %>% 
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>% 
  mutate(aveOrdVal = (round((revenue/transactions),2))) %>% 
  ungroup() #group 해제 왜? group_by 를 통해 발생할 수 있는 에러 방지
head(weekdaySummary, n= 10)


ggplot(weekdaySummary, aes(x = dayOfWeek, y = revenue)) + geom_boxplot() + labs(x = 'Day of the Week', y = 'Revenue', title = 'Revenue by Day of the Week')
ggplot(weekdaySummary, aes(x = dayOfWeek, y = transactions)) + geom_boxplot() + labs(x = 'Day of the Week', y = 'Number of Daily Transactions', title = 'Number of Transactions by Day of the Week')
ggplot(weekdaySummary, aes(x = dayOfWeek, y = aveOrdVal)) + geom_boxplot() + labs(x = 'Day of the Week', y = 'Average Order Value', title = 'Average Order Value by Day of the Week')


# 눈으로 봤을 때 요일별 매출의 차이가 있는 걸 확인 할 수 있음
# 그리고 이 차이는 AOV 보다 거래수에 따른 차이에 의해 야기 된것임
# 그러면 데이터가 어떻게 분포 되어 있는지 Density Plot 으로 그래프를 그려보자

ggplot(weekdaySummary, aes(transactions, fill = dayOfWeek)) + geom_density(alpha = 0.2)
# 분포에서 합리적인 양의 왜도가 나타남
# 그려면 통계적으로 유의한 차이를 보기 위해 non-parametric test 를 사용할 것임

kruskal.test(transactions ~ dayOfWeek, data = weekdaySummary)

#유의한 차이가 있는 것으로 보임
#that's quite a p-value we've got. 
#Using the kruskal function from the agricolae package, 
#we can look to see which days are significantly different from the others:

library(agricolae)
kruskal(weekdaySummary$transactions, weekdaySummary$dayOfWeek, console = TRUE) # 비모수 검정 크리스컬 왈리스 검정 

# 일요일이 낮고 목요일이 높음
# 평균 주문 값이 상대적으로 일정하기 때문에 이는 수익 차이로 이어집니다
# 일요일에는 거래 횟수가 적고 목요일에는 거래 수가 많으므로 
# 디지털 광고 지출에 대한 권장 사항을 제시 할 수 있습니다. 
# 우리는 이미 더 많은 거래를하고 있다는 것을 알기 때문에 일요일에 더 적은 돈을 써야합니까, 
# 사람들이 목요일에 더 살 준비가되었음을 암시 할 수 있습니까? 
# 가능하지만 다른 주요 메트릭을 모르면 말하기가 다소 어려울 수 있습니다
# 이 데이터는 통찰력을 보여 주지만, 실제로 실행 가능하도록하기 위해이 정보를 더 많은 정보와 결합하고자합니다. 

# 특히, 이러한 데이터를 웹 분석 데이터와 결합하면 큰 가치가 있습니다. 
# 이러한 데이터는 웹 트래픽 수치와 어떤 관련이 있습니까? 
# 목요일에 전환율이 변경되거나 트래픽이 더 많고 일요일에 더 적은 트래픽이 있습니까?
# 현재 광고 지출은 어떻습니까? 
# 회사는 이미 일요일에 더 적은 비용을 지출하고 목요일에 더 많이 지출합니까? 구매주기는 어떻습니까? 
# 고객이 상품 구매에 대한 생각에서 구매하는 데 얼마나 걸립니까? 
# 보통 이틀이면 화요일에 더 많이 광고해야합니까? 
# 구매 준비가 된 목요일에 지출을 계속 늘리고 고객이 프로세스의 '연구'단계에있는 동안 클릭에 대해 비용을 지불하도록해야합니까?
# 이러한 유형의 질문은 데이터 세트를 단독으로 보는 것보다는 데이터 세트를 뒷받침하는 
# 수직적, 비즈니스 모델 및 기타 요소와 결정을 이해하는 것이 중요하다는 것을 보여줍니다.


#Hour of day analysis
#In a similar way to our day-of-the-week analysis, 
#is there insight to be had from looking at the hours of the day?

custData %>%
  group_by(hourOfDay) %>%
  summarise(revenue = sum(lineTotal)) %>%
  ggplot(aes(x = hourOfDay, y = revenue)) + geom_col() + labs(x = 'Hour Of Day', y = 'Revenue (£)', title = 'Revenue by Hour Of Day')

custData %>%
  group_by(hourOfDay) %>%
  summarise(transactions = n_distinct(InvoiceNo)) %>%
  ggplot(aes(x = hourOfDay, y = transactions)) + geom_col() + labs(x = 'Hour Of Day', y = 'Number of Transactions', title = 'Transactions by Hour Of Day')


#여기에 무언가가있는 것처럼 보입니다. 
#오전부터 오후 중반에 더 많은 거래와 더 많은 수익을 얻었으며, 이른 저녁으로 빠르게 이어졌습니다. 
#몇 시간이 빠졌으므로 조사해야 할 다른 것이 있습니다. 
#이 기간 동안 거래가 실제로 없거나 다른 것이 있습니까?
#여기서 수행 할 분석 유형은 dayOfWeek로 수행 한 작업과 유사하므로 매일 추세가 있음을 알고이를 남겨두고 
#더 많은 정보가있을 경우 실행 가능한 통찰력이있을 수 있습니다.


#### 국가별 요약 ####
names(custData)


countrySummary <- custData %>%
  group_by(Country) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

head(countrySummary, n = 10)
unique(countrySummary$Country)


countrCustSummary <- custData %>% 
  group_by(Country) %>% 
  summarise(revenue = sum(lineTotal), customers = n_distinct(CustomerID)) %>% 
  mutate(avgCustVal = (round((revenue/customers),2))) %>% 
  ungroup() %>% 
  arrange(desc(revenue))

head(countrCustSummary, n = 10)

#수익 기여 측면에서 상위 5 개국을 살펴 보도록하겠습니다. 
# 영국 소매 업체라는 사실을 알고 영국을 배제 할 것이므로 
#영국의 성능을 향상시키는 것은 의심 할 여지없이 이미 레이더에 있습니다. 
#영국이 아닌 영국의 상위 5 개 수익을 살펴보면 가장 적은 수의 거래는 69 개 (오스트레일리아)이며, 
#데이터 세트의 기간을 감안할 때 여전히 일정한 수의 거래이므로 이러한 국가를 포함하는 것이 정당한 것으로 보입니다.



topFiveCountries <- custData %>%
  filter(Country == 'Netherlands' | Country == 'EIRE' | Country == 'Germany' | Country == 'France' | Country == 'Australia')
topFiveCountries

topFiveCountrySummary <- topFiveCountries %>%
  group_by(Country, date) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo), customers = n_distinct(CustomerID)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

head(topFiveCountrySummary)


ggplot(topFiveCountrySummary, aes(x = Country, y = revenue)) + geom_col() + labs(x = ' Country', y = 'Revenue (£)', title = 'Revenue by Country')
ggplot(topFiveCountrySummary, aes(x = date, y = revenue, colour = Country)) + geom_smooth(method = 'auto', se = FALSE) + labs(x = ' Country', y = 'Revenue (£)', title = 'Revenue by Country over Time')
ggplot(topFiveCountrySummary, aes(x = Country, y = aveOrdVal)) + geom_boxplot() + labs(x = ' Country', y = 'Average Order Value (£)', title = 'Average Order Value by Country') + scale_y_log10()
ggplot(topFiveCountrySummary, aes(x = Country, y = transactions)) + geom_boxplot() + labs(x = ' Country', y = 'Transactions', title = 'Number of Daily Transactions by Country')

# EIRE의 수익은 정기적으로 구매하고 평균 주문 가치가 좋은 3 명의 고객이 주도한 것으로 보이지만 최근에는 EIRE 매출이 감소하고 있습니다. 
# 적은 수의 고객과 높은 수익을 감안할 때 이러한 고객에 대한 맞춤형 이메일 또는 프로모션으로 충성도가 높아지고 다시 구매할 수 있습니다




