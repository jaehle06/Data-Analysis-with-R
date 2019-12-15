# 광고 성과(A/B테스트 결과) 분석 

ad <- c("ad_A", "ad_B")
install <- c(124,141)
click <- c(7056,9678)
CTR <- c(1.76, 1.46)

# 샘플 광고 성과 데이터 프레임 생성(원래 아래 코드로 구글 애널리틱스 데이터 load 해서 분석)
df <- data.frame(ad,install,click,CTR)

#귀무가설 : 광고1, 광고2의 성과에는 차이가 없다.
#대립가설 : 광고1, 광고2의 성과에는 차이가 있다.


#prp.test 를 활용한 성공 혹은 실패(범주형 변수)로 구성된 광고 데이터 분석
#광고를 클릭했냐? 안했느냐(이항분포)로 보기 때문에 prop.test적용가능
prop.test(install,click, conf.level = 0.95)

# 결과
# p-value가 0.05 이하라면 귀무가설을 기각 
# 해당 데이터에서는 0.1403 이므로 귀무가설 채택
# 유의수준 0.05 이하에서 광고1과 광고2의 전환율에는 차이가 없다는 결론 도출 가능



# 참고로 구글 애널리틱스 API 로 R로 LOAD해서 위에 처럼 위의 예제 처럼 광고AB 테스트성과를 분석
sessionsA <- ga$getData(ID, start.date = as.Date("2019-10-31"),
                        end.date = as.Date("2019-11-31"),
                        metrics = "ga:entrances",
                        dimensions = "ga:landingPagePath",
                        filters="ga:landingPagePath==/www.wingeat.com")


conversionsA <- ga$getData(ID, start.date = as.Date("2019-10-31"),
                           end.date = as.Date("2019-11-31"),
                           metrics = "ga:goal5Completions",
                           dimensions = "ga:landingPagePath",
                           filters="ga:landingPagePath==/www.wingeat.com")

sessionsB <- ga$getData(ID, start.date = as.Date("2019-10-31"),
                        end.date = as.Date("2019-11-31"),
                        metrics = "ga:entrances",
                        dimensions = "ga:landingPagePath",
                        filters="ga:landingPagePath==/www.wingeat.com")

conversionsB <- ga$getData(ID, start.date = as.Date("2019-10-31"),
                           end.date = as.Date("2019-11-31"),
                           metrics = "ga:goal5Completions",
                           dimensions = "ga:landingPagePath",
                           filters="ga:landingPagePath==/www.wingeat.com")






