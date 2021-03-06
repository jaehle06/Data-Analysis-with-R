library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
order_products = read.csv('instacartData/order_products__prior.csv')
orders = read.csv('instacartData/orders.csv')
departments = read.csv('instacartData/departments.csv')
products = read.csv('instacartData/products.csv')
aisles = read.csv("instacartData/aisles.csv")

stat = 
  order_products %>% left_join(orders, by='order_id') %>% left_join(products, by="product_id") %>%
    left_join(departments, by = 'department_id') %>%
    group_by(department, order_hour_of_day) %>%
    summarise(n=n())

ggplot(stat, aes(x = order_hour_of_day, y = n)) +
  geom_bar(stat = 'identity') + facet_grid(~department)

glimpse(orders)

# Do some recording and convert character variables to factors
# factor 는 문자형 변수로 특정 수준 값을 가질 수 있는 데이터 타입을 의미
# ABO 혈액형을 나타내는 변수를 문자형 변수를 정의할 때 이 변수가 취할 수 있는 값은 A, B, AB, O 의 네 가지 값만을 가져야 할 것이고 다른 종류의 문자가 들어오면 에러
# 이런 종류의 데이터 타입을 요인형
# 이러한 종류의 데이터 타입을 요인형이라 함
# 요인형 벡터는 factor() 함수를 이용해 생성할 수 있음
blood.type <- factor(c("A", "A", "AB", "O", "O"), levels = c("A", "B", "AB", "O"))
# table 함수는 도수분포표를 작성해주는 함수 
table(blood.type)


orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))


## 언제 사람들이 주문하나? 대부분 8시 ~ 18시 사이
# hour of day
# aes란 aesthetics(미학요소)를 정의해서 변수와 그래프 플롯의 축 혹은 크기형태 색상매핑
# geom을 추가하면 그래프 플롯에 있는 데이터의 점,선, 막대. + 하고 사용
orders %>%
  ggplot(aes(x = order_hour_of_day)) +
  geom_histogram(stat="count", fill = "green")

## day of week
#There is a clear effect of day of the week. Most orders are on days 0 and 1. 
#Unfortunately there is no info regarding which values represent which day, but one would assume that this is the weekend.

orders %>%
  ggplot(aes (x=order_dow)) +
  geom_histogram(stat = "count", fill = "red")


# 언제 재주문 하나?
orders %>%
  ggplot(aes( x = days_since_prior_order)) +
  geom_histogram(stat = "count", fill = "blue")

# 얼마나 이전에 주문을 했나?
# We can see that there are always at least 3 prior orders.
orders %>% filter(eval_set == "prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color = "red", size=1) + geom_point(size=2, color="red")


# 얼마나 많이 상품을 살까? 주문에 얼마나 많은 상품을 담았을까?


order_products %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="red") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))


# 베스트 셀링 상품
# Let’s have a look which products are sold most often (top10). And the clear winner is: Bananas


tmp <- order_products %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 
kable(tmp)
# kable 함수를 쓸 때 knitr(니터)패키지를 설치하고 라이브러리로 불러와야함
# 함수 안에 표에 표시하기를 원하는 데이터명을 기재하기만 하면, 별도의 제목행 구분 등의 작업 없이도 간단히 표를 그릴 수 있다

tmp %>% 
  ggplot(aes(x=reorder(product_name, -count), y = count)) +
  geom_bar(stat="identity", fill = "red") +
  theme(axis.text.x=element_text(angle = 90, hjust = 1), axis.title.x = element_blank())

# geom_bar 에서는 stat 을 반드시 identity 라고 해줘야함
# identity는 y축 값의 높이를 데이터를 기반으로 정해줄 때 사용해줍니다. 
# 즉, stat='identity'는 y축의 높이를 데이터의 값으로 하는 bar그래프의 형태로 지정한다는 것입니다.



# How often do people order the same items again?
# 주문된 상품의 59%가 모두 재주문 된것들

tmp <-  order_products %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>% 
  mutate(proportion = count/sum(count))
kable(tmp)

#시각화

tmp %>% 
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity")


# 가장 자주 재주문 된 상품은?
tmp <-  order_products %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>% 
  filter(n > 40) %>% 
  top_n(10, wt = proportion_reordered) %>% 
  arrange(desc(proportion_reordered)) %>% 
  left_join(products, by = "product_id")
kable(tmp)

tmp <-order_products %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>% 
  filter(n>40) %>% 
  top_n(10,wt=proportion_reordered) %>% 
  arrange(desc(proportion_reordered)) %>% 
  left_join(products,by="product_id")

kable(tmp)

# 시각화
tmp %>% 
  ggplot(aes(x=reorder(product_name, -proportion_reordered), y = proportion_reordered)) +
  geom_bar(stat="identity", fill = "red") +
  theme(axis.text.x=element_text(angle=90, hjust=1), axis.title =  element_blank()) + coord_cartesian(ylim=c(0.85, 0.95))


# Which item do people put into the cart first?
# People seem to be quite certain about Multifold Towels and 
# if they buy them, put them into their cart first in 66% of the time.
# 먼저 summarise(n = n()) 함수입니다. group_by() 와 함께 chaining 해서 사용하는 예시입니다.
# pct = put cart first

tmp <- order_products %>% 
  group_by(product_id, add_to_cart_order) %>% 
  summarize(count = n()) %>% mutate(pct = count/sum(count)) %>% 
  filter(add_to_cart_order == 1, count>10) %>% 
  arrange(desc(pct)) %>% 
  left_join(products,by="product_id") %>% 
  select(product_name, pct, count) %>% 
  ungroup() %>% 
  top_n(10, wt=pct) # top(n=출력할 개수, wt = 지정변수)

kable(tmp)


# 시각화
# reorder(name, mtcars$mpg)라는 값을 x축에 넣었습니다. 
# 이건 name을 x축에 넣을 때 그 mpg가 작은 것부터 앞에 오도록 순서를 조정하라는 뜻입니다. 
# 만약 값이 큰 것부터 작은 것 순서로 정렬하게 싶을 때는 reorder(name, -mtcars$mpg)라고 마이너스(-)만 붙이면 됩니다.

tmp %>% 
  ggplot(aes(x=reorder(product_name, -pct), y = pct)) +
  geom_bar(stat = "identity", fill = "red") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1), axis.title = element_blank()) + coord_cartesian(ylim = c(0.4,0.7))


# 마지막 주문 시간과 재주문 사이의 관련 가능성
# This is interesting: 
# We can see that if people order again on the same day, 
# they order the same product more often. Whereas when 30 days have passed, 
# they tend to try out new things in their order.


order_products %>% 
  left_join(orders, by = "order_id") %>% 
  group_by(days_since_prior_order) %>% 
  summarize(mean_reorder = mean(reordered)) %>% 
  ggplot(aes(x=days_since_prior_order, y = mean_reorder)) +
  geom_bar(stat="identity", fill = "blue")

  
  
# Association between number of orders and probability of reordering
# Products with a high number of orders are naturally more likely to be reordered. 
# However, there seems to be a ceiling effect(천장효과).
# geom_smooth : 추세션 
# mean() 함수
# n=n() 는 n은 요약된 데이터에서 행의 수를 n에 할당 되는 것
order_products %>% 
  group_by(product_id) %>% 
  summarize(proportion_reorderd = mean(reordered), n=n()) %>% 
  ggplot(aes(x=n, y=proportion_reorderd)) +
  geom_point() +
  geom_smooth(color = "red") +
  coord_cartesian(xlim = c(0,2000))




#Organic vs Non-organic
#What is the percentage of orders that are organic vs. not organic?
# ifelse 주어진 테스트 값에 따라 참, 거짓을 반환
# ifelse(test, # 참, 거짓을 저장한 객체
        #yes, # test가 참일 때 선택할 값
        #no # test가 거짓일 때 선택할 값)
# products$product_name 의미는 products라는 데이터 프레임에 product_name 이라는 변수를 선택한다는 의미

products <- products %>% 
    mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), organic= as.factor(organic))
    
tmp <- order_products %>% 
  left_join(products, by="product_id") %>% 
  group_by(organic) %>% 
  summarize(count = n()) %>% 
  mutate(proportion = count/sum(count))
kable(tmp)


#시각화
tmp %>% 
  ggplot(aes(x=organic,y=count, fill=organic))+
  geom_bar(stat="identity")



# Reordering Organic vs Non-Organic
# People more often reorder organic products vs non-organic products.
tmp <- order_products %>%  left_join(products, by ="product_id") %>% group_by(organic) %>% summarize(mean_reordered = mean(reordered))
kable(tmp)
#시각화
tmp %>% 
  ggplot(aes(x=organic, y= mean_reordered, fill = organic)) +
  geom_bar(stat = "identity")

# Treemap 
library(data.table)
library(treemap)

tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(departments,by="department_id")
tmp <- tmp %>% left_join(aisles,by="aisle_id")

tmp2<-order_products %>% 
  group_by(product_id) %>% 
  summarize(count=n()) %>% 
  left_join(products,by="product_id") %>% 
  ungroup() %>% 
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(tmp, by = c("department_id", "aisle_id")) %>% 
  mutate(onesize = 1)

treemap(tmp2,index=c("department","aisle"),vSize="onesize",vColor="department",palette="Set3",title="",sortID="-sumcount", border.col="#FFFFFF",type="categorical", fontsize.legend = 0,bg.labels = "#FFFFFF")





