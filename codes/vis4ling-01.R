# Title: Data visualization for linguists (1)
# Author: Sumin Park
# Date: 2022-11-23

# 필요한 패키지 가져오기
packages <- c("ggplot2", "ggpubr", "extrafont", "showtextdb", "curl")
for (package in packages) {
  if (package %in% rownames(installed.packages())) {
    do.call('library', list(package))
  }
  else {
    install.packages(package)
    do.call("library", list(package))
  }
} 

# 한글 글꼴 설치하기
font_install(source_han_sans("KR"))

# random state 고정하기
library(stats)
set.seed(0)

# 설정: 여성/남성/논바이너리, 20대/50대로 구성된 300명의 연구참여자.
N <- 300
genders <- c("Female", "Male", "Non-binary")
ages <- c("20s", "50s")

# 가상의 데이터 만들기 시작
## 설명변수: 300명에게 임의로 성별과 연령대 값을 부여한다.
x.gender <- factor(sample(genders, N, replace=TRUE, prob=c(4.5,4.5,1)))
x.age <- factor(sample(ages, N, replace=TRUE, prob=c(6,4)))

## 각 변수의 값들을 하나의 데이터프레임으로 묶어 준다.
data.all <- data.frame(age=x.age, gender=x.gender, f0=y.f0)

## 반응변수: F0 값을 각 성별에서 보고된 확률분포에서 추출한다.
## 여성-남성-논바이너리 순서로 F0 값을 추출하기 위해 데이터프레임을 정렬해 준다.
data.all <- data.all[order(data.all$gender), ]
gender.count <- table(data.all$gender)
## 성별 F0값 평균, 표준편차 출처: https://www.researchgate.net/figure/Means-and-standard-deviations-for-average-F0-in-Hz_tbl2_345025992
gender.mean <- c(198.5, 119.2, 144.3)
gender.sd <- c(18.3, 18.6, 36.7)
names(gender.mean) <- names(gender.sd) <- genders
y.f0 <- numeric()
for (gender in genders) {
  y.f0 <- c(y.f0, rnorm(gender.count[gender], gender.mean[gender], gender.sd[gender]))
}

## 반응변수 F0의 값을 데이터프레임에 추가해 준다.
data.all$f0 <- y.f0

## 참여자마다 고유한 아이디를 부여한다.
row.names(data.all) <- factor(paste0("S", formatC(1:N, width=3, flag="0")))

# 만들어진 데이터프레임을 확인해 보고 싶으면 아래 코드를 실행한다.
# head(data.all)
# summary(data.all)

# 가상의 데이터 만들기 끝

# 그림 그리기 시작

# 전체 설정: 글꼴과 글자 크기 정하기
theme_set(theme_grey(base_family='Source Han Sans KR', base_size=10))

# 1. 막대그래프: 실험참여자의 성별/연령별 분포
max.count <- max(table(data.all[c('age', 'gender')]))
p.bar <- ggplot(data.all, aes(x=age, fill=gender))
p.bar <- p.bar + geom_bar(position="dodge", color="black")
p.bar <- p.bar + scale_y_continuous(breaks=10*c(0:ceiling(max.count/10)))
p.bar <- p.bar + ggtitle("1. 막대그래프: x=범주형, y=개수 \n [예시] 연령대별·성별 실험참여자 수")
p.bar <- p.bar + theme(plot.title = element_text(size = 11, face = "bold"))

# 2. 히스토그램: 설명변수를 고려하지 않은 F0 값의 분포
f0.breaks <- 50*c(floor(min(data.all$f0)/50):ceiling(max(data.all$f0)/50))
p.hist <- ggplot(data.all, aes(x=f0))
p.hist <- p.hist + geom_histogram(binwidth=10, color="black", fill="#E69F00")
p.hist <- p.hist + scale_x_continuous(breaks=f0.breaks)
p.hist <- p.hist + ggtitle("2. 히스토그램: x=수치형, y=개수 \n [예시] 기본주파수(F0) 분포 \n [주의] x의 평균과 표준편차만 제시하는 것은 부족함!")
p.hist <- p.hist + xlab('F0 (Hz)')
p.hist <- p.hist + theme(plot.title = element_text(size = 11, face = "bold"))

# 3. 상자수염그림: 연령별/성별에 따른 F0 값의 분포
p.box <- ggplot(data.all, aes(x=age, y=f0, fill=gender))
p.box <- p.box + geom_boxplot()
p.box <- p.box + ggtitle("3. 상자수염그림: x=범주형, y=연속형 \n[예시] 연령대별·성별 F0 분포 \n[주의] 막대그래프 안 됨!")
p.box <- p.box + ylab('F0 (Hz)')
p.box <- p.box + theme(plot.title = element_text(size = 11, face = "bold"))

# 데이터 추가: 여성 화자의 F0 값들에 한하여 대응하는 VOT 값을 설정한다.
# F0 값을 (1) 190 미만, (2) 190 이상 220 이하, (3) 220 초과 세 구간으로 분할하여
# 각 구간에서 F0값과 적당히 상관관계를 가지도록 VOT 값을 추출한다.
# 세 개 구간은 대략 한국어 자음 평음/경음/격음 대립에 대응하도록 설정되었다.
# https://www.semanticscholar.org/paper/Correlation-between-VOT-and-F0-in-the-perception-of-Kim/752658ac2f7c022753d0c721499122e554f8771e/figure/0
get.a <- function (x) (ifelse(x<190, -0.2, ifelse(x<220, -0.325, -0.1)))
get.b <- function (x) (ifelse(x<190, 110, ifelse(x<220, 95, 120)))
get.y <- function (x) (get.a(x)*x + get.b(x) + rnorm(length(x),sd=10))
data.f <- subset(data.all, gender=='Female')
data.f$vot <- get.y(data.f$f0)

# 4. 산점도: F0와 VOT의 분포 (두 변수가 모두 수치형인 경우)
vot.breaks <- 20*c(0:ceiling(max(data.f$vot)/20))
f0f.breaks <- 20*c(floor(min(data.f$f0)/20):ceiling(max(data.f$f0)/20))
p.scatter <- ggplot(data.f, aes(x=jitter(f0, amount=10), y=vot, color=gender))
p.scatter <- p.scatter + geom_point(fill="white")
p.scatter <- p.scatter + scale_y_continuous(breaks=vot.breaks)
p.scatter <- p.scatter + scale_x_continuous(breaks=f0f.breaks)
p.scatter <- p.scatter + ggtitle("4. 산점도: x=수치형, y=연속형 \n[예시] 여성 화자의 F0와 VOT의 분포")
p.scatter <- p.scatter + xlab('F0 (Hz)')
p.scatter <- p.scatter + ylab('VOT (millisecond)')
p.scatter <- p.scatter + theme(plot.title = element_text(size = 11, face = "bold"))

# 종합: 네 개 그림을 한 화면에 모아서 그리기
figure <- ggarrange(p.bar, p.hist, p.box, p.scatter, ncol=1, nrow=4)
# 제목 설정
title <- expression(atop(bold("언어학자를 위한 그림 선택 기준 v.0.1"), scriptstyle("만든 사람: 박수민(https://github.com/suparklingmin)")))
annotate_figure(figure, top=text_grob(title, size=14, family="Source Han Sans KR", face="bold"))

# 그림 그리기 끝
