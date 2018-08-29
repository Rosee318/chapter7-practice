df <- read_csv("fish.csv")


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
    scale_y_continuous(labels = scales::comma) +
    labs(x="연도", y="어획량", caption = "통계청")
}

c1 <- chart("고등어")
c2 <- chart("갈치")
c3 <- chart("꽁치")
c4 <- chart("강달이류")
c5 <- chart("살오징어(오징어)")
c6 <- chart("정어리")
c7 <- chart("미역")
c8 <- chart("대게")

#install.packages("gridExtra")
gridExtra::grid.arrange(c1, c2, c3, c4, ncol = 2)
