# install.packages(c('data.table', 'dplyr', 'ggplot2'))

require(data.table)
require(dplyr)
require(ggplot2)
data = read.csv('./Google Drive/Research/StatConsult/Soomgo/2005/정지연/dataR.csv', 
                header = T, encoding = 'euc-kr')
iconv(colnames(data), 'cp949', 'utf-8')
data[, `:=` (
  `사회적 제약` = Smd > mean(Smd),
  `상실의 고통` = (Lmd > mean(Lmd)) * 2 + .5,
  `심리적 성장` = Gtotal,
  `복합 애도` = Ctotalrr,
  `상실의 충격` = (충격Lmd > mean(충격Lmd)) * 2 + .5
)]
data <- data[FILTER < 2,]

# 조절변수 요인화
data = data[, `:=` (
  `사회적 제약` = factor(
    `사회적 제약`,
    levels = c(F, T),
    labels = c('사회적 제약 = 저', '사회적 제약 = 고')
  )
  # , `상실의 고통` = factor(
  #   `상실의 고통`,
  #   levels = c(0, 1),
  #   labels = c('상실의 고통 = 저', '상실의 고통 = 고')
  # )
)]


# 처치별 종속변수 평균점수
ggdata1 = data[, .(`avg. 심리적 성장` = mean(`심리적 성장`)), # 1
              by = .(`사회적 제약`, `상실의 고통`)][order(`사회적 제약`, `상실의 고통`)]
ggdata2 = data[, .(`avg. 심리적 성장` = mean(`심리적 성장`)), # 2
                by = .(`사회적 제약`, `상실의 충격`)][order(`사회적 제약`, `상실의 충격`)] 
ggdata3 = data[, .(`avg. 복합 애도` = mean(`복합 애도`)), # 2
                by = .(`사회적 제약`, `상실의 충격`)][order(`사회적 제약`, `상실의 충격`)] 

B1 <- ggdata1[, diff(`avg. 심리적 성장`), by=`사회적 제약`]
B2 <- ggdata2[, diff(`avg. 심리적 성장`), by=`사회적 제약`]
B3 <- ggdata3[, diff(`avg. 복합 애도`), by=`사회적 제약`]

# 조절효과 시각화
gg1 <- ggplot(ggdata1, aes(x = `상실의 고통`, y = `avg. 심리적 성장`)) +
  geom_line(aes(color = `사회적 제약`), lwd = 1) + 
  geom_text(x=1, y=39.5, label = paste('B = ', round(B1$V1[2], 2), sep = '')) +
  geom_text(x=1, y=36.8, label = paste('B = ', round(B1$V1[1], 2), sep = '')) +
  scale_x_discrete(
    limits = c(.5, 2.5),
    breaks = c(.5, 2.5),
    , labels = c('저', '고')
  ) + theme_bw()

gg2 <- ggplot(ggdata2, aes(x = `상실의 충격`, y = `avg. 심리적 성장`)) +
  geom_line(aes(color = `사회적 제약`), lwd = 1) + 
  geom_text(x=1, y=38.1, label = paste('B = ', round(B2$V1[2], 2), sep = '')) +
  geom_text(x=1, y=36.2, label = paste('B = ', round(B2$V1[1], 2), sep = '')) +
  scale_x_discrete(
    limits = c(.5, 2.5),
    breaks = c(.5, 2.5),
    , labels = c('저', '고')
  ) + theme_bw()


gg3 <- ggplot(ggdata3, aes(x = `상실의 충격`, y = `avg. 복합 애도`)) +
  geom_line(aes(color = `사회적 제약`), lwd = 1) + 
  geom_text(x=1, y=85, label = paste('B = ', round(B3$V1[2], 2), sep = '')) +
  geom_text(x=1, y=65, label = paste('B = ', round(B3$V1[1], 2), sep = '')) +
  scale_x_discrete(
    limits = c(.5, 2.5),
    breaks = c(.5, 2.5),
    , labels = c('저', '고')
  ) + theme_bw()

ggsave('./GoogleDrive/Research/StatConsult/Soomgo/2005/정지연/figs/fig1.eps', gg1, width = 15, height = 10, units = 'cm')
ggsave('./GoogleDrive/Research/StatConsult/Soomgo/2005/정지연/figs/fig1.png', gg1, width = 15, height = 10, units = 'cm')
ggsave('./GoogleDrive/Research/StatConsult/Soomgo/2005/정지연/figs/fig2.eps', gg2, width = 15, height = 10, units = 'cm')
ggsave('./GoogleDrive/Research/StatConsult/Soomgo/2005/정지연/figs/fig2.png', gg2, width = 15, height = 10, units = 'cm')
ggsave('./GoogleDrive/Research/StatConsult/Soomgo/2005/정지연/figs/fig3.eps', gg3, width = 15, height = 10, units = 'cm')
ggsave('./GoogleDrive/Research/StatConsult/Soomgo/2005/정지연/figs/fig3.png', gg3, width = 15, height = 10, units = 'cm')