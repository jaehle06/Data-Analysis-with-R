library(readr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(methods)
library(extrafont)
theme_update(text=element_text(family="NanumGothic"))
ordr_pr <- read_csv("dataset/instacartData/order_products__prior.csv")
prods <- read_csv("dataset/instacartData/products.csv")

#데이터 준비
#orderid로 그룹핑해서 상품을 리스트로 쪼개기
order_baskets <- ordr_pr %>% 
  inner_join(prods, by = "product_id") %>% 
  group_by(order_id) %>% 
  summarise(basket = as.vector(list(product_name)))

#transaction 타입으로 변경 : 총 6개의 거래 정보가 정상적으로 변환
transactions <- as(order_baskets$basket,"transactions")
head(transactions)


# 장바구니 분석하기
# 먼저 데이터를 살펴보자

hist(size(transactions), breaks = 0:100, xaxt="n", ylim=c(0,10000), main = "Number of Items per basket", xlab = "#Items")
axis(1, at=seq(0,90,by=10), cex.axis=0.8)
mtext(paste("Total:", length(transactions), "baskets,", sum(size(transactions)), "items"))


# 어떤 아이템이 빈번하게 나타날까?
# support 기준을 0.02 : 아이템은 적어도 모든 장바구니에 2% 정도는 나와야한다는 의미

item_frequency <- itemFrequency(transactions, type ="a")
support <- 0.02
freq_items <- sort(item_frequency,decreasing = F)
freq_items <- freq_items[freq_items>support*length(transactions)]

par(mar=c(2,10,2,2)); options(scipen = 5)
barplot(freq_items, horiz=T, las=1, main = "Frequent Items", cex.names = .8, xlim=c(0,11000))
mtext(paste("support:",support), padj = .8)
abline(v=support*length(transactions), col="red")

#과일이랑 야채들이 많음 - 바나나, 딸기, 아보카도 등등
# Frequent Itemsets
# 빈발 아이템을 계산!, 크기가 2이상인 빈번한 항목 집합을 관찰할 가능성이 작은 것을 고려해 support 기준을 낮춤

support <- 0.008
itemsets <- apriori(transactions, parameter = list(target = "frequent itemsets", supp = support, minlen = 2),
                    control = list(verbose = FALSE))
par(mar=c(5,18,2,2) +.1)
sets_order_supp <- DATAFRAME(sort(itemsets, by = "support", decreasing = F))
barplot(sets_order_supp$support, names.arg = sets_order_supp$items, xlim = c(0,0.02), horiz=T, las=2, 
        cex.names = .8, main = "Frequent Itemsets")
mtext(paste("support:", support), padj =.8)
# 우선 support 임계값이 0.008인 경우 빈번한 쌍만 관찰하고 두번째로 바나나가 많음
# support 가 가장 높은 8쌍에는 바나나가 들어있음 
# 거의 모든 아이템들은 과일과 야채가 들어있음 
# 우유를 포함한 빈번하게 나타내는 쌍이 보임


#Association Rules
# 낮은 support 기준과 높은 confidence 를 사용해서 작은 항목에 대해서도 강력한 규칙을 생성

rules1 <- apriori(transactions, parameter = list(supp = 0.0001, conf = 0.6, maxlen=3), control=list(verbose = FALSE))
summary(quality(rules1))
plot(rules1)

#품목간에 강한 연관성을 나타내는 강한 향상도가 있는 규칙이 있다! 이러한 규칙을 자세히 살펴보자
inspect(sort(rules1, by="lift")[1:10])
inspect(sort(rules1, by="confidence")[1:10])
#이러한 규칙은 대개 함께 구입한 유사한 품목에 영향을 주는 것 같음
# 바나나를 포함하는 규칙은 없음
# 다음으로 support 를 높이고 confidence를 낮추어 좀 더 빈번한 품목 규칙을 보자

rules2 <- apriori(transactions, parameter = list(supp = 0.0001, conf = 0.4, maxlen=3), control=list(verbose = FALSE))
summary(quality(rules2))
plot(rules2)

inspect(sort(rules2, by="lift")[1:10])
inspect(sort(rules2, by="confidence")[1:10])

#support 더 올리고 confidence 더 내려보자
rules3 <- apriori(transactions, parameter = list(supp = 0.0005, conf = 0.1, maxlen=3), control=list(verbose = FALSE))
summary(quality(rules3))
plot(rules3)

inspect(sort(rules3, by="lift")[1:10])
inspect(sort(rules3, by="confidence")[1:10])



