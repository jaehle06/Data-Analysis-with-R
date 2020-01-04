
#킥스타터 데이터 분석 
require(tidyverse)
ksdata <- readr::read_csv("dataset/ks-projects-201801.csv")
str(ksdata)
head(ksdata)
fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

#가장 인기있는 프로젝트 유형
# 카테고리와 하위 카테고리의 두가지 수준을 기준으로 함
# 카테고리별 프로젝트 수
library(dplyr)
library(ggplot2)
library(tidyverse)

#visualization
options(repr.plot.width=6, repr.plot.height=8)
missing_data <- ksdata %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.1)+coord_flip()+ theme_gray()

#table
sapply(ksdata, function(x) sum(is.na(x)))
table(is.na(ksdata$name))

#usd pledged 에 대한 na값만 존재
#usd_pledged_real 을 사용할 것이므로 이 열을 제거하고 usd_pleded_real의 이름을 usd_pledged로 변경
#마찬가지로 use_goal_real을 사용하여 동일한 작업을 수행하고 usd_goal이라는 이름을 지정

ksdata <- ksdata[,-13]
names(ksdata)
colnames(ksdata)[13] <- "usd_pledged"
colnames(ksdata)[15] <- "usd_goal"

cat.freq <- ksdata %>%
  group_by(main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

ggplot(cat.freq, aes(x=reorder(main_category,-count), count, fill=count)) + geom_bar(stat = "identity") +
  ggtitle("projects by category") + xlab("project category") + ylab("Frequency") +
  geom_text(aes(label = count), vjust = -0.5) +#숫자 위로 조금 더 올리기 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size=12, face="bold"),
        axis.text.x = element_text(size = 12, angle = 90), legend.position = "null") +
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#film & video 가 가장 인기있는 프로젝트 범주에 속하고 가장 인기 없는 프로젝트는 dance
#하위 범주에 대해 동일한 작업 수행
#159개의 하위 범주라 너무 많아서 가장 많은 수의 프로젝트로 10개의 하위 카테고리 보자

subcat.freq <- ksdata %>% 
  group_by(category) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
subcat.freq$category <- factor(subcat.freq$category, levels = subcat.freq$category)

ggplot(head(subcat.freq,10), aes(category, count, fill=count)) +geom_bar(stat = "identity") +
  ggtitle("projects by subcategory") + xlab("project subcategory") + ylab("Frequency") +
  geom_text(aes(label=count), vjust= -0.5) + theme_dark() +
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=12, angle = 90), legend.position = "null") +
  scale_fill_gradient(low = "skyblue1", high = "royalblue4")
#product design 은 여기에서 가장 인기있는 하위 범주이며 design범주에 속함

#카테고리별 추이
#dates to correct format
ksdata$launch_date <- as.Date(ksdata$launched, format = "%Y-%m-%d")
ksdata$deadline_date <- as.Date(ksdata$deadline, format="%Y-%m-%d")
#adding month and year columns for the deadline and launch dates:
ksdata$launch_year <- substr(ksdata$launched,1,4)
ksdata$launch_mth <- substr(ksdata$launched,1,7)

ksdata$final_year <- substr(ksdata$deadline,1,4)
ksdata$final_mth <- substr(ksdata$deadline,1,7)


ggplot(data=ksdata, aes(x=launch_year)) +
  geom_bar(colour="black", fill=fillColor2) +
  ylab('Count') +
  facet_wrap(~main_category) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal()

#모금이 가장 많이 된 프로젝트?
# 이번엔 후원자의 관점에서 가장 인기 있는 프로젝트를 금액기준으로 정렬(상위 15)


str(ksdata)

library(knitr)
#kable 함수는 데이터를 표형식으로 보여줌
kable(head(ksdata[order(-ksdata$usd_pledged), c(2,3,13)], 15)) #kable 함수를 쓰기 위한 knitr 라이브러리 
kable(head(ksdata[order(-ksdata$backers), c(2,3,11)], 15))
#가장 일반적인 하위 카테고리는 Video Games인 것으로 보인다. 이제는 각 카테고리에 대해 기부된 금액을 집계하여 총 금액을 확인할 것이다


#카테고리별 모금 총액
option("scipen" = 100)

pledged.tot <- ksdata %>%
  group_by(main_category) %>%
  summarize(total=sum(usd_pledged)) %>%
  arrange(desc(total))

pledged.tot$main_category <- factor(pledged.tot$main_category, levels=pledged.tot$main_category)

ggplot(pledged.tot, aes(main_category, total/1000000, fill=total)) + geom_bar(stat="identity") + 
  ggtitle("Total Amount Pledged by Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD millions)") + 
  geom_text(aes(label=paste0("$", round(total/1000000,1))), vjust=-0.5) + theme_gray() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

# 이것만 보면 영화 영상, tech game 은 지금까지 가장 높은 수익을 높이는 범주
# 후원자 수 (객단가) 를 고려하는 것이 중요하므로 각 카테고리에 대한 후원자 당 보장되는 평균 금액을 봐야함
# 각 카테고리에 대해 기부 된 총액을 각 카테고리에 대한 후원자 수로 나누어 계산


#1인당 평균 후원금액
pledged.avg <- ksdata %>% 
  group_by(main_category) %>% 
  summarise(pledged = sum(usd_pledged), backers = sum(backers)) %>% 
  mutate(avg = pledged/backers)
pledged.avg$main_category <- factor(pledged.avg$main_category, levels = pledged.avg$main_category)

ggplot(pledged.avg, aes(reorder(main_category, -avg), avg, filli = avg)) +geom_bar(stat = "identity") +
  ggtitle("average amount pledged per backer") + xlab("project category") + ylab("amount pledge(USD)") +
  geom_text(aes(label=paste0("$", round(avg,2))), vjust= -0.5) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size=12, face="bold"),
        axis.text.x = element_text(size = 12, angle = 90), legend.position = "null") +
  scale_fill_gradient(low="skyblue1", high="royalblue4")


#boxplot을 사용하여 개별 프로젝트에 대해 약속된 금액의 분배를 검토
# 이상치가 많은(자금 조달이 거의 없거나 엄청나게 많은) 프로젝트들로 인해 박스 플롯이 맨 아래에 삐걱 거리는 것처럼 보일 것

ggplot(ksdata, aes(main_category, usd_pledged, fill=main_category)) + geom_boxplot() + ggtitle("Amount Pledged vs. Project Category") +
  xlab("project Category") + ylab("Amount Pledged(usd)") +
  theme(plot.title = element_text(size=15, face = "bold", hjust= 0.5),
        axis.title = element_text(size=12, face = "bold"), 
        axis.text = element_text(size = 12, angle = 90), legend.position = "null") +
  coord_cartesian(ylim = c(0,50000))

#얼마나 많은 자금이 필요하나?
# 목표 금액이 가장 높은 프로젝트
kable(head(ksdata[order(-ksdata$usd_goal), c(2,3,14,10)], 15))
 
# 일시 중단된 프로젝트 하나를 제외한 다른 모든 프로젝트의 목표가 너무 높게 설정되어 아이디어에 비해 비합리적인 것으로 보인다. 
# 그러면 성공적으로 자금을 지원받은 프로젝트 중 상위 15개를 살펴보겠다.


#성공한 프로젝트 중 목표 금액이 가장 높은 프로젝트
goal.tops <- ksdata[ksdata$state=="successful",]
kable(head(goal.tops[order(-goal.tops$usd_goal), c(2,3,14,10)],15))

#여기에 나열된 일부 프로젝트는 최고 모금 프로젝트 목록(상위 15개)에도 있었다. 
#여기에 나열된 가장 일반적인 하위 카테고리는 ’Video Games’과 ’Narrative Film’이다. 
#각 카테고리의 평균 프로젝트 목표 금액을 살펴보겠다.
#아마도 나중에 어떤 프로젝트 유형이 성공 또는 실패했는지에 대한 통찰력을 줄 것이다

# 각 카테고리의 평균 프로젝트 목표 금액
goal.avg <- ksdata %>% 
  group_by(main_category) %>% 
  summarize(goal=sum(usd_goal), projects = n()) %>% 
  mutate(avg=goal/projects) %>% 
  arrange(desc(avg))

goal.avg$main_category <- factor(goal.avg$main_category,levels = goal.avg$main_category)

ggplot(goal.avg, aes(main_category, avg, fill=avg)) + geom_bar(stat="identity") + 
  ggtitle("Average Project Goal") + xlab("Project Category") + ylab("Project Goal (USD)") + 
  geom_text(aes(label=paste0("$", round(avg,0))), vjust=-0.5) + theme_classic() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

# ‘Technology’, ‘Journalism’, ‘Film & Video’는 평균 목표금액이 가장 높다. 
# 반대로 ’Dance’, ‘Crafts’, ’Photography’는 하위 그룹에 속한다. 
# 여기에서 박스 플롯을 사용하여 개별 프로젝트에 대한 프로젝트 목표 금액의 분포를 확인할 것이다.

ggplot(ksdata, aes(main_category, usd_goal, fill = main_category)) + geom_boxplot() +
  ggtitle("Project Goal vs. Project Category") + xlab("Project Category") + ylab("Project Goal(USD)") +
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x=element_text(size=12, angle = 90), 
        legend.position = "null") +
  coord_cartesian(ylim=c(0,60000))

# ‘Technology’는 엄청나게 높은 4 분위수 및 중간 값을 가지고 있다. ’Technology’에 비해 높지는 않지만, 
# ’Design’과 ’Food’ 또한 상대적으로 높은 4 분위수와 중앙값을 가지고 있다. 
# 이 두 카테고리의 평균 프로젝트 목표는 ‘Journalism’ 및 ’Film & Video’보다 낮았지만 중간 및 상위 분위 값이 높았으므로 
# 목표 금액이 적은 많은 프로젝트가 있어야하며 후자는 많은 (높은 금액의)이상치를 가지거나, 둘다이여야 한다.

# 서로 다른 종류의 프로젝트에 대해 모금액과 목표 금액을 살펴본 결과 각각의 분포가 서로 어떻게 비교되는지 확인해보겠다. 
# 자금 조달이 거의 없는 프로젝트가 많기 때문에(즉, 오른쪽으로 꼬리가 긴 분포를 가진 그래프) 위해 
# 두 변수에 로그 변환을 사용하여 분포를 시각화할 것이다.

usd.amounts <- gather(ksdata, type, amount, usd_pledged, usd_goal, factor_key = T)

ggplot(usd.amounts, aes(amount, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  ggtitle("Distribution of USD pledged vs. USD Goal") + 
  xlab("USD") +
  ylab("Frequency") +
  scale_fill_discrete("Type", labels = c("USD Pledged", "USD GOAL"))

ggplot(usd.amounts, aes(log(amount+1), fill=type)) + 
  geom_histogram(alpha=0.5, position="identity") + 
  ggtitle("Distribution of log(USD Pledged) vs. log(USD Goal)") + xlab("log(USD + 1)") + 
  ylab("Frequency") + scale_fill_discrete("Type", labels=c("USD Pledged", "USD Goal"))


# 목표금액 정규분포의 형태를 보이고 있다. 
# 반면에 모금액은 다봉분포(bimodal distribution, 서로 다른 두 개의 최빈값을 갖는 연속확률분포) 형태를 보인다. 
# 모금액은 목표 금액 분포보다 왼쪽에 위치하고 있으며, 많은 프로젝트가 필요한 자금을 조달받지 못했음을 보여주고 있다.


#목표 달성률이 가장 높았던 프로젝트
ksdata$ratio <- ksdata$usd_pledged/ksdata$usd_goal
kable(head(ksdata[order(-ksdata$ratio), c(2,3,13,14,15)], 15))
#대부분의 프로젝트 목표가 1 달러이다. 최소 목표가 1,000 달러 이상인 프로젝트만 살펴 보겠다.


goal.min <- ksdata[ksdata$usd_goal>= 1000,]
kable(head(goal.min[order(-goal.min$ratio), c(2,3,13,14,15)],15))
#가장 많은 후원금을 받았던 프로젝트가 여기에서도 보인다. 
#달성률이 높은 프로젝트에서 자주 보이는 카테고리는 ’Tabletop Games’와 ’Product Design’이다.


#어떤 유형의 프로젝트가 성공을 했고 실패??
# 프로젝트 상태별 분석

state.freq <- ksdata %>% 
  group_by(state) %>%  
  summarize(count = n()) %>% 
  arrange(desc(count))

state.freq$state <- factor(state.freq$state, levels=state.freq$state)



ggplot(state.freq, aes(state, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Status") + xlab("Project Status") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.5) + theme_classic() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#
# 실패한 프로젝트가 성공한 프로젝트보다 많다. 
# 대부분의 프로젝트가 빛을 보지 못하는 것 같다. 
# 프로젝트를 서로 다른 두 개의 카테고리로 그룹화 할 수 있는데, 
# “완료”된 프로젝트 (마감일에 도달한 프로젝트, 즉 성공적인 프로젝트 및 실패한 프로젝트)와 
# “완료되지 못한”프로젝트 (마감일에 도달하지 않은 프로젝트, 
#                즉 아직 진행중인 프로젝트나 취소 또는 일시 중지된 프로젝트)이다. 
# 이 작업을 수행하고 각 그룹의 프로젝트 상태 비율을 살펴 보겠다.

state.grp <- ksdata %>% 
  filter(state != "undefined") %>% 
  mutate(grp = ifelse(state %in% c("successful", "failed"), "complete", "incomplete")) %>% 
  group_by(grp,state) %>% 
  summarize(count=n()) %>% 
  mutate(pct = count/sum(count)) %>% 
  arrange(grp,desc(state))


state.grp$state <- factor(state.grp$state, level = state.grp$state)

ggplot(state.grp, aes(grp, pct, fill=state)) +geom_bar(stat="identity") +
  ggtitle("Project Status by Completion") + xlab("Projection Completion") + ylab("Percentage") +
  scale_x_discrete(label=c("complete", "incomplete"))+
  scale_fill_brewer(name="Project Status",
                    labels = c("Successful", "Failed", "Suspended" , "Live", "Cancelled"),
                    palette="Set1") +
  geom_text(aes(label=paste0(round(pct*100,1), "%")), position=position_stack(vjust=0.5),
            colour="white", size = 5) + theme_dark()+
  theme(plot.title = element_text(hjust=0.5), axis.title = element_text(size=12, face = "bold"),
        axis.text.x=element_text(size=12), legend.position = "bottom",
        legend.title = element_text(size =12, face="bold"))

# 완료된(마감일에 도달한) 프로젝트의 약 60%가 펀딩에 실패하고, 약 40% 정도가 성공했다는 것을 알 수 있다.
# 완료되지 못한 (아직 진행중이거나, 취소되었거나 일시 중지된) 프로젝트의 약 90%가 취소되었다.




#카테고리별 프로젝트 성공 실패 여부
state.pct <- ksdata %>% 
  filter(state %in% c("successful", "failed")) %>% 
  group_by(main_category, state) %>% 
  summarize(count=n()) %>% 
  mutate(pct = count/sum(count)) %>% 
  arrange(desc(state),pct)

state.pct$main_category <- factor(state.pct$main_category,levels = state.pct$main_category[1:(nrow(state.pct)/2)])#15개만 보자

ggplot(state.pct, aes(main_category, pct, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Project Category") + 
  xlab("Project Category") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) + 
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="white", size=5) + theme_dark() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold")) + coord_flip()


# ‘Dance’, ‘Theater’, ‘Comics’은 가장 높은 성공률을 보이며, ’Technology’, ‘Journalism’, ’Crafts’가 가장 성공률이 낮다. 
# ’Dance’와 ’Comics’은 모금액에서 높은 중앙값을 보였었고, 목표액에서는 낮은 중간값을 보였었다.
# ‘Technology’, ‘Journalism’, ’Crafts’은 모금액의 중간값이 낮았고, ’Technology’는 목표액의 중앙값이 높았다. 
# 일반적으로 모금액이 높고, 목표금액이 낮을 수록 성공할 확률이 높다. 
# 흥미롭게도 낮은 목표액의 중간 값을 가진 ’Crafts’는 목표액의 중간값이 낮았음에도 불구하고 성공률이 낮았다.
# 사람들이 전반적으로 이 카테고리에 관심이 많지 않다는 것을 알 수 있었다


# 프로젝트 기간이 성공에 영향을 줄까?
# 킥 스타터의 최대 프로젝트 기간은 60일
# 그리고 프로젝트를 30일 이하로 설정할 것을 권장
# 그들의 추론은 30일 이내에 자금 지원을 받지 못한 프로젝트도 마감 기한 까지 자금을 조달하지 못할 것이라고 말하고 있음
#프로젝트 마감일과 프로젝트 시작일의 차이를 계산한 다음
# 전체 일수로 나누어 각 프로젝트의 기간을 알 수 있음
# 프로젝트의 성공률을 일단위로 계산해보자
library(lubridate)
ksdata$length <- interval(ymd_hms(ksdata$launched), ymd(ksdata$deadline)) %/% days(1)

length.pct <- ksdata %>% 
  filter(state %in% c("successful", "failed"), length <= 61) %>% 
  group_by(length, state) %>% 
  summarize(count=n()) %>% 
  mutate(pct=count/sum(count))

ggplot(length.pct[length.pct$state == "successful",], aes(length, pct)) +
  geom_point(colour = "royalblue4", size = 2.5) + ggtitle("Success Rate vs. Project Length") +
  xlab("Project Length(Days)") + ylab("Success Rate(%)") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60)) +geom_vline(xintercept = 30, colour = "red") +
  theme_light()+
  theme(plot.title=element_text(hjust = 0.5), axis.title = element_text(size = 12, face = "bold"))

# 전반적으로 30일을 초과하는 프로젝트는 30일 미만의 프로젝트보다 성공률이 낮으므로 킥스타터의 말은 일리가 있다. 
# 실제로 30일을 초과하는 프로젝트의 경우 성공률과 프로젝트의 기간 간에 역 선형관계(음의 선형관계)를 보이고 있다
# (30일 이전 상향 추세선, 30일 이후 하향 추세선). 
# 프로젝트 길이에 따른 프로젝트 분포를 살펴보겠다.


ggplot(ksdata[ksdata$length <= 61,], aes(length)) + geom_density(colour = "royalblue4", size = 1) +
  ggtitle("Distribution of Projects by Campaign Length") + xlab("Project Length(days)")+
  ylab("Density(%)") + scale_x_continuous(breaks = c(0,10,20,30,40,50,60)) +
  geom_vline(xintercept = 30, colour = "red") + theme_light() +
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12, face = "bold"))
#대다수의 프로젝트가 30일 이내로 설정되어 있는 것처럼 보인다.

#연도별 프로젝트 현황
year.freq <- ksdata %>% 
  filter(year(launched)!= "1970") %>% 
  group_by(year=year(launched)) %>% 
  summarize(count=n())
#http://ds.sumeun.org/?p=975
ggplot(year.freq, aes(year,count, fill=count))+ geom_bar(stat = "identity")+
  ggtitle("Number of Projects by Launch Year") + xlab("Year") + ylab("Frequency") +
  scale_x_discrete(limits = c(2009:2018)) +
  geom_text(aes(label = paste0(count)), vjust = -0.5) + theme_light()+
  theme(plot.title=element_text(hjust = 0.5), axis.title = element_text(size =12, face = "bold"),
        axis.text.x = element_text(size = 12), legend.position = "null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")#연속형 값일 때 
#2015년은 프로젝트가 가장 많은 해였다. 2016년과 2017년의 프로젝트 수가 그 전(2014, 2015년)에 비해 프로젝트 수가 점차 감소하고 있는 것으로 보인다.

#연도별 프로젝트 성공/실패율
state.pct2 <- ksdata %>% 
  filter(year(launched) != 1970, state %in% c("successful", "failed")) %>% 
  group_by(year=year(launched), state) %>% 
  summarize(count=n()) %>% 
  mutate(pct=count/sum(count)) %>% 
  arrange(desc(state))

ggplot(state.pct2, aes(year,pct, fill= state)) +geom_bar(stat = "identity") +
  ggtitle("Success vs. Failure Rate by Year Launched") +
  xlab("year") +ylab("percentage") + scale_x_discrete(limits = c(2009:2017)) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(name = "Project Status", breaks=c("Success", "Failure"), labels=c("Success", "Failure")) +
  geom_text(aes(label=paste0(round(pct*100), "%")), position=position_stack(vjust=0.5),#paste0은 나열된 원소 사이에 공백없이 출력
            colour="white", size=5) + theme_light()+
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12,face="bold"),
        axis.text.x = element_text(size=8), legend.position = "bottom",
        legend.title =element_text(size=12, face="bold"))

# 흥미롭게도 2014년과 2015년은 프로젝트 성공률이 낮았음
# 어쩌면 많은 사람들이 당시에 크라우드 펀딩에 뛰어드려고 시도했지만 성공하지 못했나봄
# 년도를 기준으로 프로젝트 유형간에 차이가 있는지 살펴보자


#히트맵은 각 카테고리의 유형 및 연도의 프로젝트 수를 보여준다

cat.year <- ksdata %>% 
  filter(!year(launched) %in% c("1970","2018")) %>% 
  group_by(main_category, year=year(launched)) %>% 
  summarize(count=n())
cat.year
cat.year2 <- t(matrix(cat.year$count, nrow=9))
cat.year2

colnames(cat.year2) <- c("2009","2010","2011","2012","2013","2014","2015","2016","2017")
rownames(cat.year2) <- levels(ksdata$main_category)


library(gplots)
#히트맵은 여러개 패키지 있는데 이전 heatmap() 보다 확장성이 늘어난 히트맵 함수 
heatmap.2(cat.year2, dendrogram = "row", Colv=F, trace = "none" , margin=c(10,10))
#행(또는 열)덴드로그램만 표시하려면 Colv=FALSE or Rowv = FALSE를 설정하고 dendogram 매개변수 조정
#dendogram : 가까운 것 끼리 비슷한 패턴끼리 연결되는 선이 있는데 이를 덴드로그램이라고 함
#trace 옵션의 default 값은 column 임

#히트맵에서 중요한 점은 2015년에 technology, film&video와 music가 인기가 매우 많았음을 알 수 있었음


#국가별 프로젝트 현황은?
# 국가별 프로젝트 성공률과 모금 총액과 성공률 그리고 지리적 히트맵으로 시각화 할 것임


ks5 <- ksdata %>%
  group_by(country, state) %>% 
  select(country, state, goal,pledged, backers)%>%
  summarise(freq=n()) %>%
  mutate(categoryPerc = freq/sum(freq)) %>%
  filter(state == "successful") %>%
  arrange(desc(categoryPerc))
  
ks55 <- head(ks5, 10)

p5 <- ggplot(ks55, aes(x = reorder(country, -categoryPerc), y = round(categoryPerc*100, digits = 1))) +
  geom_bar(stat = "identity", fill = fillColor2) + theme(axis.text.x = element_text(angle=60, hjust=1)) + geom_text(aes(label=round(categoryPerc*100, digits = 1)), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_light()

p5+labs(y="Success %", x="Country", title="Top 10 Country by Successful project %")
  
  
# 킥스타터는 글로벌 플랫폼이다. 
# 미국이 아닌 여러 나라의 메이커들도 자신만의 프로젝트를 시작하기 위한 자금을 모금하기 위해 진행한다. 
# 위의 차트에서 볼 수 있듯이 미국은 프로젝트 성공률이 가장 높은 나라지만 다른 나라들 또한 크게 뒤떨어져 있지 않다.
# 미국에서 시작된 프로젝트의 성공률은 37.4%이며, 영국에서 시작된 프로젝트의 성공률은 35.8%이다. 
# 아시아 지역에서는 홍콩의 프로젝트 성공률이 35%로 영국과 비슷한 양상을 보이고 있으며, 싱가포르 (32.1%)는 상위 5위에 머무르고 있다. 
# 유럽에서는 영국 다음으로 덴마크 (32.3%), 뉴질랜드 (31%), 프랑스 (30.9%), 룩셈부르크 (30.6%), 스웨덴 (29%)이 뒤를 이었다.



#국가별 프로젝트 모금 현황
names(ksdata)
ks6 <- ksdata %>%
  group_by(country, state) %>% 
  select(country, state, goal, pledged, backers)%>%
  summarise(sumpledge=sum(pledged)) %>% 
  filter(state == "successful") %>% 
  arrange(desc(sumpledge))

ks66 <- head(ks6, 10)

p6 <- ggplot(ks66, aes(x = reorder(country, -sumpledge), y = round(sumpledge/1000000, digits = 0))) +
  geom_bar(stat = "identity", fill = fillColor2) + theme(axis.text.x = element_text(angle=60, hjust=1)) + geom_text(aes(label=round(sumpledge/1000000, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_light()

p6 + labs(y="Money raised (USD Million)", x="Country", title="Top 10 Country by Money raised (in USD Million)")

# 각국의 프로젝트 모금액을 비교해 보면, 미국은 나머지 모든 국가를 합친 것보다 월등히 높다는 것을 알 수 있다.
# 2018년 1월까지 총 31억 2000만 달러의 자금이 조달되었으며, 
# 그 중에서 미국은 25억 3700만 달러를 모았다
# (한화로 2조 8607억원 수준으로 이는 81.3%에 달한다). 
# 자금 조달 측면에서 영국은 1억 3800만 달러(한화로 1,557억) 수준이고, 
# 미국을 제외한 국가(영국 포함)의 총합은 5억 8300만 달러(한화 6,578억) 수준이다.


#미국과 미국 아닌 국가들의 프로젝트 성공률 비교

# 킥스타터는 전세계적으로 사용되고 있기 때문에, 
# 카테고리 성공 여부는 프로젝트가 미국 또는 미국 이외의 지역에서 시작되었는지에 따라 결정된다. 
# 이를 위해 데이터를 정리하고 분류하여 미국과 미국이 아닌 지역에서 성공률로 나누었다
# (아래의 차트는 미국 및 미국 외 국가의 카테고리 성공율을 비교한 것이다). 
# 전반적으로 미국의 성공률이 조금 더 높다


ks7 <- ksdata %>%
  filter(country %in% c('US')) %>%
  group_by(main_category, state) %>%
  select(main_category, state, goal, pledged, backers) %>%
  summarise(freq=n()) %>%
  mutate(us = freq/sum(freq)) %>%
  filter(state == "successful") %>%
  arrange(desc(us))

us_success <- select(ks7, main_category, us)


ks8 <- ksdata %>%
  filter(!country %in% c('US')) %>%
  group_by(main_category, state) %>%
  select(main_category, state, goal, pledged, backers) %>%
  summarise(freq=n()) %>%
  mutate(non_us = freq/sum(freq)) %>%
  filter(state == "successful") %>%
  arrange(desc(non_us))

non_us_success <- select(ks8, main_category, non_us)


p7 <- ggplot(ks7, aes(x = reorder(main_category, -us), y = round(us*100, digits = 0))) +
  geom_bar(stat = "identity", fill = fillColor2) + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  geom_text(aes(label=round(us*100, digits = 0)), position=position_dodge(width=0.3), vjust=-0.25) + 
  labs(y="Success %", x="Main Category", title="USA - Successful project %") + 
  theme_light()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))



p8 <- ggplot(ks8, aes(x = reorder(main_category, -non_us), y = round(non_us*100, digits = 0))) +
  geom_bar(stat = "identity", fill = fillColor2) + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  geom_text(aes(label=round(non_us*100, digits = 0)), position=position_dodge(width=0.3), vjust=-0.25) + 
  labs(y="Success %", x="Main Category", title="Non USA - Successful project %") + 
  theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(ggpubr)
ggarrange(p7, p8, ncol = 2)


# 그러면 미국과 미국이 아닌 국가에서의 성공률이 데이터를 통계적으로 진짜 차이가 있을까?
# 독립성 검정 (피어슨 카이제곱)

# 귀무가설 : 두개의 범주형 변수 간에는 연관성이 없고 행 변수와 열 변수는 모집단에서 독립적이며 , 카테고리의 성공에 영향을 미친다
# 대립가설 : 두개의 범주형 변수 간에는 연관성이 없고 행 변수와 열 변수는 모집단에서 독립적이며 , 카테고리의 성공에 영향을 미치지 않는다

compare_main <- left_join(us_success,non_us_success, by = c("main_category" = "main_category"))
compare_main

chisq.test(data.frame(compare_main$us, compare_main$non_us))
#결과의 P값이 1에 가깝기 때문에 귀무가설 채택 -> 
# 즉  카테고리의 성공에 영향을 준다고(성공에 차이가 있다고) 말할 수 있다. 
# 모든 범주에서 미국 내 성공확률은 미국을 제외한 국가의 성공률보다 높다. 
# 따라서 미국에서 프로젝트를 시작하는 것이 성공률을 더 높인다고 말할 수 있다.


#분산 분석 (Anova Test) - 2개 이상 집단간 차이를 규명 : t test 의 확장 
# 킥스타터에서 ‘Games’와 ’Design’ 카테고리는 가장 많은 모금액($741, $734 millon)이 형성, 
# 이 카테고리에 의해 모금된 금액이 통계적으로 차이가 있는지 아니면 비슷한지 확인해보겠다. 
# 분석을 위해 상위 1,000개의 프로젝트를 선택하고 Anova 분석을 시행

ks9 <- ksdata %>% 
  filter(main_category %in% c('Games')) %>% 
  select(pledged) %>% 
  arrange(desc(pledged))

ks10 <- ksdata %>%
  filter(main_category %in% c('Design')) %>%
  select(pledged) %>%
  arrange(desc(pledged))

ks9 <- head(ks9, 1000)
ks10 <- head(ks10, 1000)

sprintf("Mean amount pledged on Design Category: %f", mean(ks9$pledged))
sprintf("Mean amount pledged on Games Category: %f", mean(ks10$pledged))


## [1] "Mean amount pledged on Games Category: 387663.683480"
#ks11 <- round(ks9[sample(1:nrow(ks9), 1000, replace = FALSE), ], digits = -3)
#ks12 <- round(ks10[sample(1:nrow(ks10), 1000, replace = FALSE), ], digits = -3)
ks9 <- round(ks9, digits= -3)
ks10 <- round(ks10, digits = -3)
scores = data.frame(ks9, ks10)
#boxplot(scores)
scores = stack(scores)
resultaov <- aov(values ~ ind, data=scores, var.equal = T) #(종속변수) ~ (독립변수)
summary(resultaov)

# P값(유의확률)은 0.77로 유의수준인 0.05보다 높으므로 귀무가설을 채택할 수 있으며, 
# 두 카테고리 모두 통계적으로 같은 금액의 평균값을 가진 1,000개의 프로젝트가 있다고 말할 수 있다.
# 국가별 차이 시각화 
countries.freq <- ksdata %>% 
  filter(country!='N,0"') %>% 
  group_by(country) %>% 
  summarize(count=n())

library(rworldmap)
countries.match <- joinCountryData2Map(countries.freq, joinCode="ISO2", nameJoinColumn="country")

mapCountryData(countries.match, nameColumnToPlot="count", 
               mapTitle="Number of Projects by Country", catMethod="logFixedWidth", 
               colourPalette="heat")

# 킥스타터 모금액과 펀딩에 참여한 사람 수간의 관계 
# 킥스타터의 프로젝트는 펀딩에 참여한 사람들이 금전적으로 지원을 해주는 것이기 때문에 
# 후원자의 수에 따라 프로젝트 성공 및 금액의 여부가 결정된다. 아래의 그림은 후원자의 수와 모금액의 선형관계를 보여주고 있다. 
# 모금액과 후원자 수의 관계를 이해하기 위해 다음 모델을 사용하여 회귀 분석을 실시했다.

library(lattice)
#Pledgedamount=A0+B1∗backers
xyplot(ksdata$pledged~ksdata$backers, data = ksdata, xlab= "Number of backers", ylab= "Amount pledged (USD)", panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, ...)
})

#선형회귀모델의 가정

resultslm <- lm(ksdata$pledged~ksdata$backers, data = ksdata)
summary(resultslm)
#회귀 분석의 결과와 회귀 분석의 가정을 만족시키는 것을 보여주는 차트는 아래에 표시해두었다.
par(mfrow=c(2,2))
plot(resultslm)

#후원자의 p값(유의확률)은 0.01 미만으로 이는 후원자의 수가 모금 총액에 상당한 
#영향을 미친다는 것을 의미한다. 선형회귀 모델의 식은 아래와 같다.
#후원자의 기울기는 75.6이므로 각 후원자가 킥스타터에 평균적으로 $ 75.6에 기여했다고 가정할 수 있다. 
#따라서 더 많은 수의 후원자를 유치할 수 있는 프로젝트가 더 많은 돈을 모으게 됨으로써 성공적으로 진행될 것이다.
#Pledgedamount=1698.9+75.6∗backers


#prediction









# 날짜를 활용하면 좋은 점은 : 기간에 확보할 수 있는 외부데이터 활용하면 좋음 -> 카테고리들에 대한 이 당시 사회적인식 같은거 그 때 핫한
# 시기때는 더 지지 많고 이미지가 않좋다면 누가 지지하것어? , 뉴스 , SNS 이용해서 키워드가 감정분석을 통해서 새로운 변수 추가해도 될듯 
# 네이버 랩 키워드에 대한 상승 하락 추세. 
# 이 카테고리가지고 조언을 해줄 때 : 너가 지금 이카테고리이고 목표금액은 이렇고 이러면 목표 pledged, backeer 를 n 정도 확보해야하고
# 근데 이 기간에 이건 하락 추세니까 좀 있다가 하는게 좋을 것이다. 
# 마케팅 매체를 어떤 거 이용할 때 좋더라 이런것도 조언을 해주는거지
# name : 에서도 성공과 실패 차이가 있을 지? -> 네이밍 할 때 의문문으로 하는게 좋을 지 같은 거 제시를 해줘도 좋음






  
