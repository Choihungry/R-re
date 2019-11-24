## 응용프로그래밍 R 분석코드

### 인터넷 이용률 2017
```{r}
install.packages("csv")
install.packages("RColorBrewer")
library(csv)
library(RColorBrewer)
##choose.file()에서 인터넷 이용율2017 선택
int2017 <- read.csv(choose.files())

int2017
str(int2017)
barplot(int2017$이용률, names = int2017$연도, ylim=c(0,100),col="skyblue")
title(main = "인터넷 이용률 2017")
```
### 개인정보 중요성 인식 2017
```{r}
#install.packages("RColorBrewer")
library(RColorBrewer)
cols <- brewer.pal(8, "Accent")

# 엑셀파일 자료를 가지고 메트릭스 형태로 설정
int17 <- matrix(c(0,0,0.4,0.2,5.4,
                  3.2,44.3,36.3,49.9,60.3), nrow = 2)
#rowname 설정
rownames(int17) <- c("정보보호","개인정보보호")
#colname 설정
colnames(int17) <- c("전혀중요하지않다", "중요하지않은편이다",
                     "보통이다","중요한편이다","매우중요하다")
int17
int17_p <- prop.table(int17, margin = 1) * 100
int17_p
nint17 <- ncol(int17)
nint17
b2 <- barplot(t(int17_p), width = 0.42, xlim = c(0,2), space = 0.45, col = cols[6:1], yaxt = "n")
#텍스트 입력
text(x = 0.4, y=76, labels=49)
text(x = 1.0025, y=69.4, labels=62.5)
text(x = 0.4, y=28, labels=45.1)
text(x = 1.0023, y=21, labels=34.6)
#legend, title 입력
legend("topright", inset = 0.0000001, fill = cols[6:1], legend = colnames(int17), bty = "n")
title(main = "정보보호 중요성 인식 2017")
```  
### 개인정보 심각성 인식 2016
```{r}
library(RColorBrewer)
cols <- brewer.pal(8, "Accent")
# 엑셀파일 자료를 가지고 메트릭스 형태로 설정
Na2 <- matrix(c(2.3,1.4,2.1,1.9,13.2,13.7,19.2,17.1
,54.5,42.5,45.4,40.8,30,42.4,33.3,40.2), nrow = 4)
#rowname 설정
rownames(Na2) <- c("악성코드 감염","개인정보 유출/사생활 침해",
"신용,직불카드 사기/불법결제","피싱/파밍/스미싱")
#colname 설정
colnames(Na2) <- c("s&vs","n","b","vb")
Na2
Na2_p <- prop.table(Na2, margin = 1) * 100
Na2_p
na2 <- ncol(Na2)
na2
a3 <- barplot(t(Na2_p),width = 0.5,xlim = c(0,3),space = 0.1, col = cols[0:6], yaxt = "n")
pos <- function(x){
  cumsum(x) - x / 2
}
#텍스트 입력후 legend ,title 설정
y_txt2 <- apply(Na2_p, MARGIN = 1, pos)
text(x = rep(a3, each = 4),y = y_txt2,labels = t(Na2),cex = 0.8)
legend("topright",inset = 0.01,fill = cols[0:5],legend = colnames(Na2))
title(main = "정보보호 심각성 인식2016")
```
### 개인정보 심각성 인식 2017
```{r}
library(RColorBrewer)
cols <- brewer.pal(8, "Accent")
# 엑셀파일 자료를 가지고 메트릭스 형태로 설정
ra <- matrix(c(2.2,1.7,3.6,3.7,8,15.1,14,16.1,14.9,21.3,46.6,38.2
,40.8,40.7,42.2,36,46,39.5,40.8,28.5), nrow = 5)
#rowname 설정
rownames(ra) <- c("악성코드 감염","개인정보 유출/사생활 침해","신용,
직불카드 사기/불법결제","피싱/파밍/스미싱","랜섬웨어 감염")
#colname 설정
colnames(ra) <- c("작다","보통이다","크다","매우 크다")
ra
ra_p <- prop.table(ra, margin = 1) * 100
ra_p
grp <- barplot(t(ra_p),width = 0.5,xlim = c(0,4),space = 0.1, col = cols[0:5], yaxt = "n")
pos <- function(x){
  cumsum(x) - x / 2
}
#텍스트 입력후 legend ,title 설정
y_txt <- apply(ra_p, MARGIN = 1, pos)
text(x = rep(grp, each = 4),y = y_txt, labels = t(ra))
legend("topright",inset = 0.01,fill = cols[0:5],legend=colnames(ra))
title(main = "정보보호 심각성 인식2017")
```
### 업데이트 주기 2017
```{r}
install.packages("csv")
install.packages("RColorBrewer")
library(RColorBrewer)
library(csv)
#csv파일 불러오기
dat <- read.csv(file ="C:/Users/info/Desktop/업데이트주기2017.csv",header = TRUE)
#dat rowname 설정
rownames(dat) <- c("10대","20대","30대","40대","50대")
#dat에 구조를 확인
str(dat)
#barplot을 그리기 위해 데이터프레임 구조를 행렬구조로 바꿈
dat2 <- as.matrix(dat)
dat2
#barplot 그리기
dat2_b <- barplot(t(dat2),horiz = T,width = 20,space = 0.1,axes = FALSE,ylim = c(0,100)
,col = brewer.pal(8, "Accent"),las = 1,legend = colnames(dat2))
title(main="2017 보안 업데이트 주기 조사 응답")
```
### 피해경로 2016, 2017
```{r}
P2016 <- read.csv(file.choose())
View(P2016)
library(ggplot2)
par(mar=c(5,10,5,5))
p1 <- ggplot(P2016, aes(fill=백분율, y=백분율, x=유형))
p1 + geom_bar(position="dodge", stat="identity")+coord_flip()
+ labs(y="피해경험 백분율", x="", title="개인정보 침해사고 유형 복수응답", cex=5.5)
+ggtitle("2016 개인정보 침해사고 유형- 복수응답")
+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "darkblue"))
+geom_text(aes(label=백분율), vjust=0.2, color="white", size=5.5)+ theme_minimal()+theme_dark()
#---------------------------------------
P2017 <- read.csv(file.choose())
View(P2017)
par(mar=c(5,10,5,5))
p2 <- ggplot(P2017, aes(fill = 백분율, y=백분율, x=유형))

p2+geom_bar(position="dodge", stat="identity")+coord_flip()
+ labs(y="피해경험 백분율", x="", title="개인정보 침해사고 유형 복수응답", cex=5.5)
+ggtitle("2017 개인정보 침해사고 유형- 복수응답")
+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "darkblue"))
+geom_text(aes(label=백분율), vjust=0.2, color="white", size=5.5)+ theme_minimal()+theme_dark()
```
