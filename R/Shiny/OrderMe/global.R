library(DT)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(lubridate)
library(xlsx)
library(data.table)
library(tidyr)

idv <<- 9999:1
cur.idv <<- NULL

now2 <- function() {
  as_datetime(Sys.time()) + hours(9)
}

pop <- function(v, i = length(v)) {
  res <- v[i]
  assign(deparse(substitute(v)), v[-i], envir = .GlobalEnv)
  res
}

vals <<- reactiveValues()

vals$proto <- data.table(
  `삭제` = character(),
  `시간` = Date(),
  `멤버` = character(),
  `메뉴` = character(),
  `빵/샐러드` = character(),
  `치즈` = character(),
  `야채` = character(),
  `소스` = character(),
  `음료` = character(),
  `추가` = character()
)

brdorsal <- list('빵' = '빵', '샐러드' = '샐러드')

breads <- list(
  "화이트" = '화이트',
  '하티' = '하티',
  '파마산오레가노' = '파마산오레가노',
  '위트' = '위트',
  '허니오트' = '허니오트',
  '플랫' = '플랫'
)

bread.cms <- list("15cm" = '15cm',
                  "30cm" = '30cm')

cheeses <- list('아메리칸' = '아메리칸',
                '슈레드' = '슈레드',
                '모차렐라' = '모차렐라')

veges <- list(
  '양상추' = '양상추',
  '토마토' = '토마토',
  '오이' = '오이',
  '피망' = '피망',
  '양파' = '양파',
  '피클' = '피클',
  '올리브' = '올리브',
  '할라피뇨' = '할라피뇨',
  '아보카도' = '아보카도'
)

sauces <- list(
  '렌치' = '렌치',
  '마요네즈' = '마요네즈',
  '스윗 어니언' = '스윗 어니언',
  '허니 머스타드' = '허니 머스타드',
  '스윗 칠리' = '스윗 칠리',
  '핫 칠리' = '핫 칠리',
  '사우스웨스트 치폴레' = '사우스웨스트 치폴레',
  '머스타드' = '머스타드',
  '홀스래디쉬' = '홀스래디쉬',
  '올리브 오일' = '올리브 오일',
  '레드와인식초' = '레드와인식초',
  '소금' = '소금',
  '후추' = '후추',
  '스모크 바비큐' = '스모크 바비큐'
)

bevs <- list(
  '아이스 아메리카노' = '아이스 아메리카노',
  '따뜻한 아메리카노' = '따뜻한 아메리카노',
  '탄산' = '탄산',
  'X' = 'X'
)

addons <- list(
  '미트 추가' = '미트 추가',
  '베이컨 비츠' = '베이컨 비츠',
  '쉬림프 더블업' = '쉬림프 더블업',
  '에그마요' = '에그마요',
  '오믈렛' = '오믈렛',
  '아보카도' = '아보카도',
  '베이컨' = '베이컨',
  '페퍼로니' = '페퍼로니',
  '치즈 추가' = '치즈 추가'
)

rs <- function(l, llen = length(l)) {
  sample(l, sample(1:llen, 1)) %>% unlist(use.names = F)
}