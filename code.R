# 혹시 설치를 안하셨다면
#install.packages("ggthemes")
#install.packages("gridExtra")

library(tidyverse)
library(ggthemes)
library(gcookbook)
#-------------------------------------------------------
# some tips
#-------------------------------------------------------

p <- ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point()
p + annotate("text", x=3, y=48, label = "Group 1") +
  annotate("text", x=4.5, y=66, label = "한글은 family 설정", family = "NanumGothic")





#-------------------------------------------------------
# 실습1 : 어종별 어획량 차트 함수 (line plots)
#-------------------------------------------------------
df <- read_csv("data/fish.csv")

# charts function
chart <- function(x){
  b <- df %>% 
    filter(category == x) %>% 
    group_by(years) %>% 
    summarise(total = sum(value))
  
  ggplot(b, aes(x=years, y=total, group=1, label=years)) +
    geom_line(stat = "identity", size = 1.5, color = "#76A665") +
    geom_smooth(method = "loess", color = "#FFDD5C", size = 1.5) +
    geom_point(color = "#76A665", size = 1.5) +
    ggtitle(paste0(x, " 어획량")) +
    scale_y_continuous(labels = scales::comma, limits = c(0, 150000)) +
    labs(x="연도", y="어획량", caption = "통계청") +
    theme_fivethirtyeight(base_family = "NanumGothic")
}

c1 <- chart("고등어")
c2 <- chart("갈치")
c3 <- chart("꽁치")
c4 <- chart("강달이류")
c5 <- chart("살오징어(오징어)")
c6 <- chart("정어리")
c7 <- chart("미역")
c8 <- chart("대게")

gridExtra::grid.arrange(c1, c2, c3, c4, ncol = 2)


#-------------------------------------------------------
# 실습2 : 방향성 있는 scatter plot
# https://news.joins.com/article/22922649
#-------------------------------------------------------

# 데이터 가져오기
df_baby <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTghhk4SVfIQuMtk2Rwz1QkYamVuDu4g1Wgtn7iYYNZ5laB8Yc9B7y2xWa6NNV_pcfbnZkPaYYYvoFl/pub?gid=0&single=true&output=csv")

# tidy 데이터 만들기
df_baby <- df_baby %>% 
  gather("years", "rate", 2:3)

# 2012, 2017 출생률 평균값
mean_2012 <- 9.6
mean_2017 <- 7

# 고액 출산금 줬지만 출생률 떨어져... 차트 만들어보기
ggplot(df_baby, aes(x = reorder(location, -rate), y = rate, color = years)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = mean_2017, ymax = mean_2012, alpha = 0.005, color = "#F0F0F0") +
  geom_point(size = 2) +
  theme_base(base_family = "NanumGothic") +
  theme_woons() +
  scale_x_discrete(labels = abbreviate) +
  ylim(0, 15) +
  geom_path(aes(group = location), color = "grey80", 
            arrow = arrow(ends = "last", length = unit(0.25, "cm"))) +
  geom_hline(yintercept = mean_2012, linetype = "dashed", color = "#F57771") +
  geom_hline(yintercept = mean_2017, linetype = "dashed", color = "#1FBFC3") +
  labs(title = "고액 출산금 줬지만 출생률 떨어져", 
       caption = "출처 : 각 지자체", 
       x = "", 
       y = "")


#-------------------------------------------------------
# 실습3 : 마포구 건물통합정보 Bar Plots
#-------------------------------------------------------
df_building <- read_csv("data/building.csv")
head(df_building)

df_building %>%
  group_by(type) %>% 
  summarise(mean = mean(n, na.rm = T))

mean_wood <- 14.3
mean_brick <- 208
mean_steel <- 246 

ggplot(df_building, aes(x = year, y = n, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_fivethirtyeight(base_family = "NanumGothic") +
  labs(title = "마포구 건물구조정보 분포 현황",
       subtitle = "건물 승인년도로 살펴본 마포구 건물들", 
       caption = "출처 : GIS 건물통합정보") +
  coord_flip() +
  geom_hline(yintercept = mean_wood, color = "#1BB940", linetype = "dashed") +
  geom_text(aes(1925, mean_wood, label = mean_wood, hjust = -0.1), color = "#1BB940") + 
  geom_hline(yintercept = mean_brick, color = "#F57670", linetype = "dashed") +
  geom_text(aes(1970, mean_brick, label = mean_brick, hjust = -0.2), color = "#F57670") + 
  geom_hline(yintercept = mean_steel, color = "#649EFC", linetype = "dashed") +
  geom_text(aes(2002, mean_steel, label = mean_steel, hjust = -0.4), color = "#649EFC")
