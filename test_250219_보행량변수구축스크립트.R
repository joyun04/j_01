

################ 보행량 예측 ################
############## install/libraray ######

# 로드할 패키지 리스트
packages <- c("data.table", "tidyr", "readxl", "dplyr", "leaflet","sf", "ggthemes","leaflet","writexl")

load_packages <- function(pkg_list) {
  for (pkg in pkg_list) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)             # 패키지가 설치되어 있지 않으면 설치
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
  message("모든 패키지가 성공적으로 로드되었습니다!")
}

load_packages(packages)

##########.#############기타~3D로 뽑아보기 
##################### 1022 (기본준비 ) ##### 
f_coun <- read_excel("D:/D드라이브/JY/기타/기타_공간데이터/건축물대장/00.원시데이터/국토교통부_건축물대장_표제부+(2024년+03월)/표제부_2024.03_서울시.xlsx") #지점별 유동인구수

f_coun <- read_excel("D:/D드라이브/JY/연구/01.보행량예측/16.2410/02.유동인구/유동인구조사_2015_dataset_v3.xlsx") #지점별 유동인구수

sf_df <- st_read("D:/D드라이브/JY/연구/01.보행량예측/14.현행화/16.중구/중구_POINT최종.shp")## 격자 배경 불러오기

# str(f_coun)

sf_df <- st_as_sf(f_coun, coords = c("x", "y"), crs = 4326)  # x,y 붙여가지고 와가지고욤~~~ geometry로 변환만 하겠습니다 
## shp째로 가져와야될듯 왜냐하면 지오코딩한거 4326인데, 기존에 제공은 5179로 되어있는ㄷ;
getwd()
setwd("E:/D드라이브/JY/연구/01.보행량예측/16.2410/02.유동인구")
sf_df <- st_read("D:/D드라이브/JY/연구/01.보행량예측/16.2410/02.유동인구/유동인구_2015_fff.shp")

map <- sf_df %>% st_transform(4326)%>% leaflet() %>% addTiles() %>% addCircleMarkers(popup = ~code,radius = 2,color = "black") ## 지도에 표시 ㅎㅎㅎ 

##버퍼 제작 
buffer <- st_buffer(sf_df, dist = 50)
buffer_f <- st_transform(buffer, crs = 4326)

map2 <- map %>%
  addPolygons(data = buffer_f, fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2)%>% addCircleMarkers(popup = ~code,radius = 2,color = "red")


# f_df <- split(buffer_f,buffer_f$code

str(buffer_f)
f_info.sf.buffer2 <- split(buffer_f,buffer_f$code)

result.grid<- lapply(buffer_f, st_intersection,y=df_sample)
print(st_crs(pw_dataset))   # buffer_f의 좌표계 확인
print(st_crs(df_sample))   # buffer_f의 좌표계 확인
str(buffer_f)       
f_info.sf.buffer3 <- st_transform(f_info.sf.buffer2, crs = 4326)
intersection <- st_intersection(buffer_f, pw_dataset_z)
plot(df_sample)
############################## 

class(buffer_f)    # buffer_f의 클래스 확인
class(df_sample)

################################# 제1장 사회인구적 특성 
buffer_list <- split(buffer_f, buffer_f$group)
intersection_results <- lapply(buffer_list, st_intersection, y = df_sample)

################## 격자 데이터 붙이기 위해서

grid <- st_read("D:/D드라이브/JY/연구/01.보행량예측/16.2410/01.인구종사자수/서울시_100m_v2.shp")## 격자 배경 불러오기
# grid <- st_read("D:/D드라이브/JY/연구/01.보행량예측/16.2410/01.인구종사자수/서울시_100m_v2.shp")## 격자 배경 불러오기

## 인구수 종사자수 붙여야돼용 


shp_file_3857 <- st_transform(grid, crs = 4326)

st_crs(grid)
plot(grid)
pw_map <- shp_file_3857 %>% leaflet() %>% addTiles() ## 지도에 표시 ㅎㅎㅎ

pw_map <- grid %>% leaflet() %>% addTiles() ## 지도에 표시 ㅎㅎㅎ

#### 지도그려놓은건데 필요하면 돌려보세요#################
pw_map <- shp_file_3857 %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    color = "blue",            # 폴리곤 경계선 색상
    weight = 1,                # 경계선 두께
    opacity = 0.8,             # 경계선 투명도
    fillColor = "lightblue",   # 폴리곤 내부 채우기 색상
    fillOpacity = 0.5,         # 내부 채우기 투명도
    popup = ~paste("ID:", GRID_100M_)  # 각 격자 셀의 팝업 정보 표시
  )

pw_map <- df_sample %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    color = "blue",            # 폴리곤 경계선 색상
    weight = 1,                # 경계선 두께
    opacity = 0.8,             # 경계선 투명도
    fillColor = "lightblue",   # 폴리곤 내부 채우기 색상
    fillOpacity = 0.5,         # 내부 채우기 투명도
    popup = ~paste("ID:", GRID_100M_)  # 각 격자 셀의 팝업 정보 표시
  )


##############################

## 인구수 ❤️

text_pop <- fread("D:/D드라이브/JY/기타/기타_공간데이터/인구수_격자/2015/_census_reqdoc_1720570802873/2015년_인구_다사_100M.txt",sep = "^")

result_pop <- aggregate(V4 ~ V2, data = text_pop, FUN = sum) #인구수 합

pw_dataset <- left_join(grid,result_pop, by = c("GRID_100M_" = "V2"))

pw_dataset$V4[is.na(pw_dataset$V4)] <- 0 ##격제내에 구멍뚫린거 있어서 눌값처리 

str(pw_dataset)

# pw_dataset_z <- st_transform(pw_dataset, crs = 4326)

result <- st_join(buffer_f, pw_dataset_z, join = st_intersects)

# str(result)
result_pop22222 <- aggregate( V4 ~ code, data = result, FUN = sum) #인구수 합쳐야댕 

result_pop22222 <- result_pop22222 %>% rename("pop"="total")

buffer_f22 <- left_join(buffer_f, result_pop22222, by = c("code" = "code"))

## 종사자수 ❤️

text_work <- fread("D:/D드라이브/JY/기타/기타_공간데이터/인구수_격자/2015/_census_reqdoc_1720570802873/2015년_종사자_다사_100M.txt",sep = "^")

text_work$V4 <- as.numeric(text_work$V4)

result_work <- aggregate(V4 ~ V2, data = text_pop, FUN = sum) #인구수 합쳐야댕 

pw_dataset_w <- left_join(grid,result_work, by = c("GRID_100M_" = "V2"))

pw_dataset_w$V4[is.na(pw_dataset_w$V4)] <- 0 ##격제내에 구멍뚫린거 있어서 눌값처리 

dataset_w <- st_transform(pw_dataset_w, crs = 4326)

result_wo <- st_join(buffer_o, dataset_w, join = st_intersects)

result_wor <- aggregate(V4 ~ code.x, data = result_wo, FUN = sum) #인구수 합쳐야댕 
head(result_wor)

result_wor <- result_wor %>% rename("wor"="V4")
result_wor <- result_wor %>% rename("code"="code.x")

head(buffer_f3)

buffer_f3 <- left_join(buffer_f22, result_wor, by = c("code" = "code"))


## 차선수(ktdb 꺼 씀)❤️
w_road <- st_read("D:/D드라이브/JY/기타/기타_공간데이터/2016-TM-KA-MR-LLV2 도로망(2015년 기준)_170929/02링크/ad0022.shp")## 격자 배경 불러오기
str(w_oad)

w_oad <- w_road %>% select(c(LINK_ID,WIDTH))  

w_oad <- st_transform(w_oad, crs = 4326)

w_oad_re <- st_join(buffer_o, w_oad, join = st_intersects)

str(w_oad_re)

w_oad_re$WIDTH <- as.numeric(w_oad_re$WIDTH)

w_oad_re$WIDTH[is.na(w_oad_re$WIDTH)] <- 0 ##격제내에 구멍뚫린거 있어서 눌값처리 

width_Ro <- aggregate(WIDTH ~ code, data = w_oad_re, FUN = mean) #인구수 합쳐야댕 

buffer_f3 <- left_join(buffer_f3, width_Ro, by = c("code" = "code"))

## 도로율❤️


road <- st_read("D:/D드라이브/JY/기타/기타_공간데이터/새주소/2019/11000/TL_SPRD_RW_m.shp")## 격자 배경 불러오기
st_crs(road) <- 5179

road_f <- st_transform(road, crs = 4326)

## 좌표계에 문제있는거 같아서 map으로 확인 한 번 하기#######
map2 <- map %>%
  addPolygons(data = road_f, fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2)

########

######## 기타 주석 ###################
# print("유효성 확인:") ##도형수정해야되는거같은디 

# print(st_is_valid(road_f))

# 유효하지 않으면 수정👌
# polygon_valid <- st_make_valid(road_f)
# road_ff <- st_join(buffer_o, polygon_valid, join = st_intersects)
# clipped_roads <- st_intersection(road_f, buffer_o)
sf_use_s2(FALSE) ## 유효하지않은 ㅁ도형 그냥넘어가기 gis 도형수정했음 🎉 

#############################################

clipped_roads <- st_intersection(road_f, buffer_o) #💕

road_ff <- st_transform(clipped_roads, crs = 5179)

road_f_projected <- road_ff %>%
  mutate(area = as.numeric(st_area(road_ff)))

road_d <- aggregate(area ~ code, data = road_f_projected, FUN = sum) #인구수 합쳐야댕 

buffer_f3 <- left_join(buffer_f3, road_d, by = c("code" = "code"))

buffer_f3 <- buffer_f3 %>% rename("d_area"="area.x")

buffer_f3 <- buffer_f3 %>% select(-area.y)

str(road_f_projected)

## 경사도 ❤️

slope <- st_read("D:/D드라이브/JY/연구/01.보행량예측/14.현행화/04.훈련2/01.설명변수/07.slope/경사도.shp")## 격자 배경 불러오기

slope_f <- st_transform(road, crs = 4326)

str(slope_f)

buffer_oo <- st_transform(buffer_o, crs = 5179)
slope_ff <- st_transform(slope_f, crs = 5179)

slope_ree <- st_join(buffer_oo, slope_ff, join = st_intersects)
head(slope_ree)
slope <- aggregate(RW_SN ~ code, data = slope_ree, FUN = mean) #인구수 합쳐야댕 

head(slope)

slope <- slope %>% rename("slope"="RW_SN")

buffer_f3 <- left_join(buffer_f3, slope, by = c("code" = "code")) #💕

str(buffer_f3)

## 건축규모 ❤️
##### 건축물 대장 서울시만 뽑아보자! 
## 📢 표제부 2021.12

setwd("D:/D드라이브/JY/기타/기타_공간데이터/건축물대장/00.원시데이터/국토교통부_건축물대장_표제부(2015년+6월)")

DATA <- fread("mart_djy_03.txt")

for (col in 1:ncol(DATA)) {
  DATA[[col]] <- iconv(x = DATA[[col]], from = "EUC-KR", to = "UTF-8")
} #인코딩 잘 안됐으면 재인코딩, 

DATA$new <- substr(DATA$V9, 1, 2) #서울시만뽑으려고 시도코드생성중
DATA_seolu2 <- DATA %>% filter (DATA$new =="11") #서울시
str(DATA_seolu2)

## PNU 만들기👍

DATA_seolu2$V12 <- as.numeric(DATA_seolu2$V12)
DATA_seolu2$V13 <- as.numeric(DATA_seolu2$V13)
DATA_seolu2 <- DATA_seolu2 %>% mutate(bun=sprintf("%04d",V12)) 
DATA_seolu2 <- DATA_seolu2 %>% mutate(ji=sprintf("%04d",V13)) 
DATA_seolu2$V11 <- ifelse(DATA_seolu2$V11 == "1", "2", DATA_seolu2$V11)
DATA_seolu2$V11 <- ifelse(DATA_seolu2$V11 == "0", "1", DATA_seolu2$V11)
DATA_seolu2 <- DATA_seolu2 %>% unite(PNU, V9,V10, V11,bun,ji,sep="",remove = FALSE) 


str(DATA_seolu2)

DATA_seolu2[c("V28", "V31", "V46")] <- lapply(DATA_seolu2[c("V28", "V31", "V46")], as.numeric)

DATA_seolu2[c("V28", "V31", "V46")][is.na(DATA_seolu2[c("V28", "V31", "V46")])] <- 0



re_area<- aggregate(V28 ~ PNU, data = DATA_seolu2, FUN = sum) #대지면적(v28)
re_area <- re_area %>% rename("re_area"="V28") 
str(re_area)
f_area <- aggregate(V31 ~ PNU, data = DATA_seolu2, FUN = sum) #연면적(V31)
f_area <- f_area %>% rename("f_area"="V31")

floor <- aggregate(V46 ~ PNU, data = DATA_seolu2, FUN = median) #평균층수(v46)
##층수의 경우, mean값으로 할시 뭔가 이상함 ㅎㅎ; 값에 오류가...그냥 중앙값으로 하겠삼! 
floor <- floor %>% rename("floor"="V46")


### 버퍼에 지적도를 통해 pnu를 붙여보자 

map2 ## map 확인용 

ground<- st_read("D:/D드라이브/JY/기타/기타_공간데이터/지적도/AL_11_D002_20151224/2015_지적도_도형수정+5181.shp")## 격자 배경 불러오기

ground2 <- st_transform(ground, crs = 5179)


bu_gro <- st_join(buffer_oo, ground2, join = st_intersects)

bu_gro <- bu_gro %>% rename("PNU"="A1") ## 이름 바꿔주기 


ground_b <- left_join(bu_gro, re_area, by = c("PNU" = "PNU")) ## 대지면적
ground_b <- left_join(ground_b, f_area, by = c("PNU" = "PNU")) ## 연면적
ground_b <- left_join(ground_b, floor, by = c("PNU" = "PNU")) ## 평균면적
# str(ground_b)
# ground_b <- ground_b %>% select(-floor.x)
# ground_b <- ground_b %>% select(-floor.y)

ground_b$re_area[is.na(ground_b$re_area)] <- 0 ##격제내에 구멍뚫린거 있어서 눌값처리 
ground_b$f_area[is.na(ground_b$f_area)] <- 0 ##격제내에 구멍뚫린거 있어서 눌값처리 
ground_b$floor[is.na(ground_b$floor)] <- 0 ##격제내에 구멍뚫린거 있어서 눌값처리 

str(ground_b)

### aggregate를 몇번이나해야됨;

re_area_b <- aggregate(re_area ~ code, data = ground_b, FUN = sum) #대지면적

f_area_b <- aggregate(f_area ~ code, data = ground_b, FUN = sum) #연면적

floor_b <- aggregate(floor ~ code, data = ground_b, FUN = median) #평균면적


buffer_f3 <- left_join(buffer_f3, re_area_b, by = c("code" = "code"))
buffer_f3 <- left_join(buffer_f3, f_area_b, by = c("code" = "code"))
buffer_f3 <- left_join(buffer_f3, floor_b, by = c("code" = "code"))

buffer_f3 <- buffer_f3 %>%
  left_join(re_area_b, by = "code") %>%
  left_join(f_area_b, by = "code") %>%
  left_join(floor_b, by = "code")


str(buffer_f3)


## 건축용도 ❤️

## 건축용도 개많은데 단독주택부터 해봄 ㅎㅎ 


# DATA_seolu2$V37 <- as.numeric(DATA_seolu2$V37) ##v37~39
DATA_seolu2$cat_us <- substr(DATA_seolu2$V37, 1,2) ## 대분류 코드로 떼어보겠습니다~ 
library(dplyr)

result <- DATA_seolu2 %>%
  dplyr::group_by(PNU,cat_us) %>%
  dplyr::summarise(sum_value = sum(V31)) ## 용도코드별로 연면적 합산  

result$cat_us <- as.character(result$cat_us)

## 💕 단독주택 v1 

re_01 <- result %>% filter(cat_us=="01")

re_01 <- re_01 %>% rename("v1"="sum_value")
re_01 <- re_01 %>% select(-cat_us) ## 요상한게 붙어있는걸요 
re_01 <- re_01 %>% select(c(PNU,v1))

## 이거 지적도에붙인거라 지오메트리 붙어서 계산이안됨 어이없음

re_01 <- st_drop_geometry(re_01)

str(ground_01)

ground_01 <- left_join(ground_b,re_01,by="PNU")


ground_01$v1[is.na(ground_01$v1)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

ground_01_f <- aggregate(v1 ~ code, data = ground_01, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, ground_01_f, by = c("code" = "code"))


str(re_01)

## 💕 공동주택 v2 
## 공동주택 면적 편차가 너무커서 부속건축물은 빼고자 합니다~ 

# str(DATA_seolu2)
DATA_seolu3 <- DATA_seolu2 %>% filter(DATA_seolu2$V26=="주건축물") ## 주건축물만 뽑기

str(DATA_seolu3)

DATA_seolu3$cat_us <- substr(DATA_seolu3$V37, 1,2) ## 대분류 코드로 떼어보겠습니다~ 
# library(dplyr)

result2 <- DATA_seolu3 %>%
  dplyr::group_by(PNU,cat_us) %>%
  dplyr::summarise(sum_value = sum(V31)) ## 용도코드별로 연면적 합산  

re_02 <- result2 %>% filter(cat_us=="02")

re_02 <- re_02 %>% rename("v2"="sum_value")
re_02 <- re_02 %>% select(-cat_us) ## 요상한게 붙어있는걸요 
re_02 <- re_02 %>% select(c(PNU,v2))

re_02 <- st_drop_geometry(re_02)

str(re_02)
# str(re_01)##3 또그리게이트 

str(ground_02)##3 또그리게이트

ground_02 <- left_join(ground_b,re_02,by="PNU")


ground_02$v2[is.na(ground_02$v2)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

ground_02_f <- aggregate(v2 ~ code, data = ground_02, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, ground_02_f, by = c("code" = "code"))


## 💕 근생시설 v3 (03,04)

re_03 <- result %>% filter(cat_us %in% c("03","04"))

re_03 <- re_03 %>% rename("v3"="sum_value")
re_03 <- re_03 %>% select(-cat_us) ## 요상한게 붙어있는걸요 
re_03 <- re_03 %>% select(c(PNU,v3))

## 이거 지적도에붙인거라 지오메트리 붙어서 계산이안됨 어이없음
# library(sf)

re_03 <- st_drop_geometry(re_03)

ground_03 <- left_join(ground_b,re_03,by="PNU")


ground_03$v3[is.na(ground_03$v3)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

ground_03_f <- aggregate(v3 ~ code, data = ground_03, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, ground_03_f, by = c("code" = "code"))

## 💕 판매영업시설 v4 (07)
## 공동주택 면적 편차가 너무커서 부속건축물은 빼고자 합니다~ 
re_04 <- result %>% filter(cat_us =="07")

re_04 <- re_04 %>% rename("v4"="sum_value")
re_04 <- re_04 %>% select(-cat_us) ## 요상한게 붙어있는걸요 

## 이거 지적도에붙인거라 지오메트리 붙어서 계산이안됨 어이없음

re_04 <- st_drop_geometry(re_04)

ground_04 <- left_join(ground_b,re_04,by="PNU")


ground_04$v4[is.na(ground_04$v4)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

ground_04_f <- aggregate(v4 ~ code, data = ground_04, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, ground_04_f, by = c("code" = "code"))


## 💕 업무시설 v5 (14)

re_05 <- result %>% filter(cat_us =="14")

re_05 <- re_05 %>% rename("v5"="sum_value")

re_05 <- st_drop_geometry(re_05)

ground_05 <- left_join(ground_b,re_05,by="PNU")


ground_05$v5[is.na(ground_05$v5)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

ground_05_f <- aggregate(v5 ~ code, data = ground_05, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, ground_05_f, by = c("code" = "code"))

## 💕 교육시설 v6 (10) 

re_06 <- result %>% filter(cat_us =="10")

re_06 <- re_06 %>% rename("v6"="sum_value")

re_06 <- st_drop_geometry(re_06)

ground_06 <- left_join(ground_b,re_06,by="PNU")


ground_06$v6[is.na(ground_06$v6)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

ground_v6_f <- aggregate(v6 ~ code, data = ground_06, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, ground_v6_f, by = c("code" = "code"))

## 💕 의료시설 v7 (09) 


re_07 <- result %>% filter(cat_us =="09")

re_07 <- re_07 %>% rename("v7"="sum_value")

re_07 <- st_drop_geometry(re_07)

ground_07 <- left_join(ground_b,re_07,by="PNU")


ground_07$v7[is.na(ground_07$v7)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

ground_07_f <- aggregate(v7 ~ code, data = ground_07, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, ground_07_f, by = c("code" = "code"))


## 💕 문화 및 집회시설설 v8 (05) 


re_08 <- result %>% filter(cat_us =="05")

re_08 <- re_08 %>% rename("v8"="sum_value")

re_08 <- st_drop_geometry(re_08)

ground_08 <- left_join(ground_b,re_08,by="PNU")


ground_08$v8[is.na(ground_08$v8)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

ground_08_f <- aggregate(v8 ~ code, data = ground_08, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, ground_08_f, by = c("code" = "code"))

## 층별개요 보행활성화 용도  ❤️
## 층별개요 불러오기~ 

##층별개요를 통해 용적계산용 연면적을 산출하고자 함 

setwd("D:/D드라이브/JY/기타/기타_공간데이터/건축물대장/00.원시데이터/국토교통부_건축물대장_층별개요(2015년+6월)")
getwd()

DATA <- fread("MART_DJY_04.txt") #층별개요 2015년은 대문자더라; 

for (col in 1:ncol(DATA)) {
  DATA[[col]] <- iconv(x = DATA[[col]], from = "EUC-KR", to = "UTF-8")
} #인코딩 잘 안됐으면 재인코딩, 

DATA$new <- substr(DATA$V5, 1, 2) #서울시만뽑으려고 시도코드생성중 층별

DATA_seoluf <- DATA %>% filter (DATA$new =="11") #층별개요 서울시

#02. pnu작업 (지번)
DATA_seoluf$V8 <- as.numeric(DATA_seoluf$V8)
DATA_seoluf$V9 <- as.numeric(DATA_seoluf$V9)
DATA_seoluf <- DATA_seoluf %>% mutate(번=sprintf("%04d",V8))
DATA_seoluf <- DATA_seoluf %>% mutate(지=sprintf("%04d",V9))

#03. pnu작업 (대지)

DATA_1 <- DATA_seoluf %>% filter(V7 == "0" | V7 == "1")

# 대지 1,2 변환 
DATA_1$V7 <- ifelse(DATA_1$V7 == "1", "2", DATA_1$V7)

DATA_1$V7 <- ifelse(DATA_1$V7 == "0", "1", DATA_1$V7)
# b<- table(DATA_1$V15) 
# data.table(b) 

DATA_pnu3 <- DATA_1 %>%unite(PNU,V5 ,V6,V7,번,지,sep="")

## 아파트 연면적을 구해야함 
## 아파트의 경우, 지상층만 뽑아서 면적을 다더하는 방식으로 진행함 👌

up_floor <- DATA_pnu3 %>% filter(V20=="지상")

DATA_apt <- up_floor %>% filter(V26=="02001")

DATA_apt$V29 <- as.numeric(DATA_apt$V29)

DATA_apt_F <- aggregate(V29 ~ PNU, data = DATA_apt, FUN = sum) #평균면적

ground_09 <- left_join(ground_b,DATA_apt_F,by="PNU")

str(ground_09)

ground_09$V29[is.na(ground_09$V29)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 


ground_09_f <- aggregate(V29 ~ code, data = ground_09, FUN = sum) #평균면적

ground_09_f <- ground_09_f %>% rename("v9"="V29")

buffer_f3 <- left_join(buffer_f3, ground_09_f, by = c("code" = "code"))


## 지상 지하층을 어떻게 구분할까? = v20 👌

## 지상지하로 구분하고 1층을 빼는 방법으로 가보겠삼 

up_floor <- DATA_pnu3 %>% filter(V20=="지상")

## 1층만 빼내는 방법이 뭘까? = v21 👌
##숫자가 나을듯??? 

# A<-table(up_floor$V22)
# data.table(A)

# B<-table(up_floor$V21) ## 0층 오류가 많아서 1층으로 해주세요!!!! 
# data.table(B)
str(up_floor)

up_1f <- up_floor %>% filter(V21=="1") ## 1층만 추출 

up_1f$cat_us <- substr(up_1f$V26, 1,2) ## 대분류 코드로 떼어보겠습니다~ 

## 💕 보행활성화 용도 
### 원하는 보행활성화 용도만 빼와볼게용?


up_1f <- up_1f %>%
  mutate(V27 = case_when(
    V27 == "일반음식점" ~ 1,
    V27  %in% c("수퍼마켓", "소매점") ~ 2,
    V27  %in% c("휴게음식점", "제과점") ~ 3,
    TRUE ~ 4  # 그 외 값은 NA로 설정
  ))

up_1fre <- up_1f %>% select(c(PNU,V27))

ground_upf1 <- left_join(ground_b,up_1fre,by="PNU")

upf1 <- aggregate(V27 ~ code, data = ground_upf1, FUN = min) #평균면적

upf1 <- upf1 %>% rename("on_floor"="V27")

buffer_f3 <- left_join(buffer_f3, upf1, by = c("code" = "code"))


buffer_f3$on_floor[is.na(buffer_f3$on_floor)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 


## 💕 교통시설 

### 교통시설은 과거년도는 따로 전처리가 필요해서 gis내에서 확인한다음에 전처리했삼


# 버스 승하차 인원수 👌


################## 02.자동으로 파일읽오보리기~

# 1. CSV 파일이 있는 폴더 경로 설정
folder_path <- "D:/D드라이브/JY/기타/기타_공간데이터/교통변수/01.버스/BUS_STATION_BOARDING_MONTH_2015"


# 2. 파일 읽기 시 인코딩 설정 (예: UTF-8)
data_list <- lapply(file_list, function(file) {
  read.csv(file, fileEncoding = "EUC-KR")
})

# 3. 파일들을 데이터프레임으로 읽기
data_list <- lapply(file_list, read.csv)

# 4. (선택) 모든 데이터를 하나의 데이터프레임으로 합치기
combined_data <- do.call(rbind, data_list)

# 확인
str(combined_data)

## 승하차 다더해서 12월로나노 (월평균)

combined_data$승차총승객수 <- as.numeric(combined_data$승차총승객수)
combined_data$하차총승객수 <- as.numeric(combined_data$하차총승객수)


mtcars <- combined_data %>%
  mutate(sum = 승차총승객수+하차총승객수)
str(mtcars_bus)

mtcars_bus <- aggregate(sum ~ 버스정류장ARS번호, data = mtcars, FUN = sum) 

mtcars_bus <- mtcars_bus %>% mutate(mean = sum/12)

### 버퍼에 버스정류장 좌표를통해 code를 붙여보자 

map2 ## map 확인용 

bus<- st_read("D:/D드라이브/JY/기타/기타_공간데이터/교통변수/01.버스/v3euc버스정류장위치_2019_연계좌표정보.shp")## 문자로 읽혀야될거같은디 ㅠㅠ

str(bus)
bus2 <- st_transform(bus, crs = 5179)
buffer_oo2 <- st_transform(buffer_oo, crs = 5179)

bus_gro <- st_join(buffer_oo2, bus2, join = st_intersects)

bus_gro_f <- left_join(bus_gro, mtcars_bus, by = c("STTN_NO.정" = "버스정류장ARS번호"))

str(mtcars_bus) 

bus_gro_ff <- aggregate(sum ~ code, data = bus_gro_f, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, bus_gro_ff, by = c("code" = "code"))

buffer_f3$sum[is.na(buffer_f3$sum)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

buffer_f3$sum <- buffer_f3$sum/12
buffer_f3 <- buffer_f3 %>% rename("bus_t"="sum") ## 이름 바꿔주기 


##### 버스 역 수 👌


bus_gro <- st_join(buffer_oo2, bus2, join = st_intersects)

str(bus_gro)

bus_gro$bus <- 1

bus_gro_F <- aggregate(bus ~ code, data = bus_gro, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, bus_gro_F, by = c("code" = "code"))


# 버스 노선수 👌
## 노선의 경우 한달것만 불러와서 확인해야지 안그럼 더블카운팅 될듯 
## 하루만골라서해볼게용~~~
# library(data.table)
# setwd("D:/D드라이브/JY/기타/기타_공간데이터/교통변수/01.버스/BUS_STATION_BOARDING_MONTH_2015")


combined_data1 <- do.call(rbind, data_list[1])
bus_data1 <-combined_data1 %>% filter(사용일자=="20150120")
# str(mtcars)

bus_data1$bus_l <- 1

mtcars_i <- aggregate(bus_l ~ 버스정류장ARS번호, data = bus_data1, FUN = sum) #평균면적

mtcars_ii <- left_join(bus_gro, mtcars_i, by = c("STTN_NO.정" = "버스정류장ARS번호"))
# str(mtcars_ii)

mtcars_ii$bus_l[is.na(mtcars_ii$bus_l)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

mtcars_iif <- aggregate(bus_l ~ code, data = mtcars_ii, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, mtcars_iif, by = c("code" = "code"))

# mtcars_ii <- mtcars_i%>% filter(bus_l != ) 어차피 안붙어서 ~ 지울필요없을듯? 



# 지하철을 해보자! ㅎㅎ 👌


train <- st_read("D:/D드라이브/JY/기타/기타_공간데이터/교통변수/02.지하철/지하철역및승하차_2015년버전.shp")## 따로 전처리 했습니다~


## 지하철 역 수 및 승하차 인원 
str(train_gro)

train$train <-1

train2 <- st_transform(train, crs = 5179)

train2_buf <- st_buffer(train2, dist = 250)

train_gro <- st_join(buffer_oo2, train2_buf, join = st_intersects)

## 승하차
train_gro$train_t[is.na(train_gro$train_t)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

train_ff <- aggregate(train_t ~ code, data = train_gro, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, train_ff, by = c("code" = "code"))

str(mtcars_bus) 

## 역수
train_gro$train[is.na(train_gro$train)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

train_수_ff <- aggregate(train ~ code, data = train_gro, FUN = sum) #평균면적

buffer_f3 <- left_join(buffer_f3, train_수_ff, by = c("code" = "code"))

##### 지도 확인용 지하철!! ######### 
train3 <- st_transform(train, crs = 4326)
train2_buf2 <- st_transform(train2_buf, crs = 4326)

map2 <- map %>% addPolygons(data = train2_buf2, fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2)%>% addCircleMarkers(popup = ~code,radius = 2,color = "red") %>% addPolygons(data = buffer_f, fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2)%>% addCircleMarkers(popup = ~code,radius = 2,color = "red")
##### ######### 

## 💕 cbd

CBD <- st_read("D:/D드라이브/JY/집중연구/☆향후연구/98.기타자료/03.유형/01.도면/3도심.shp")## 따로 전처리 했습니다~

CBD$CBD <-1

### 지도로 확인 점 ###### 

CBD2 <- st_transform(CBD, crs = 4326)
map2 <- map %>% addPolygons(data = CBD2, fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2)%>% addCircleMarkers(popup = ~code,radius = 2,color = "red") %>% addPolygons(data = buffer_f, fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2)%>% addCircleMarkers(popup = ~code,radius = 2,color = "red")
#####################

# str(CBD)

CBD3 <- st_transform(CBD, crs = 5179)

CBD3_gro <- st_join(buffer_oo2, CBD3, join = st_intersects)

CBD3_gro$CBD[is.na(CBD3_gro$CBD)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

CBD3_ff <- aggregate(CBD ~ code, data = CBD3_gro, FUN = max) #평균면적

buffer_f3 <- left_join(buffer_f3, CBD3_ff, by = c("code" = "code"))


## 💕 공원 

## 공원의 경우 shp 조각모음을해서 따로 지하철마냥 전처리했습니다 가져다 쓰겠습니다~ 

## 버퍼내 공원 면적 

park<- st_read("D:/D드라이브/JY/연구/01.보행량예측/14.현행화/05.훈련3/01.설명변수/06.park/park+area.shp")## 따로 전처리 했습니다~

park3 <- st_transform(park, crs = 5179)

park3_gro <- st_join(buffer_oo2, park3, join = st_intersects)

park3_gro0 <- st_intersection(park3, buffer_oo2)

park3_gro0$area_m2 <- st_area(park3_gro0) # 면적 (기본 단위는 제곱미터)

options(scipen = 999)  # 지수 표기법을 억제


pp_ff <- aggregate(area_m2 ~ code, data = park3_gro0, FUN = sum) #평균면적

# str(pp_ff)

pp_ff$area_m2 <- as.numeric(pp_ff$area_m2)

pp_ff <- pp_ff %>% rename("pp"="area_m2")

buffer_f3 <- left_join(buffer_f3, pp_ff, by = c("code" = "code"))

buffer_f3$pp[is.na(buffer_f3$pp)] <- 0 ##연면적내에 구멍뚫린거 있어서 눌값처리 

## 공원 위계정하기~

str(park3_gro0)
park3_gro0$area_m2 <- st_area(park3_gro0) # 면적 (기본 단위는 제곱미터)


# 데이터프레임 예제
park_parea <- park3_gro0 %>%
  mutate(category = case_when(
    area_p <= 1500 ~ 1,
    area_p > 1500 & area_p <= 10000 ~ 2, 
    area_p > 10000 & area_p <= 30000 ~ 3, 
    area_p > 30000 & area_p <= 100000 ~ 4, 
    area_p > 100000 & area_p <= 4000000 ~ 5,
    area_p > 4000000 ~6,
    TRUE ~ 0            # 그 외의 경우 NA  
  ))

str(buffer_f3)
park_parea_ffff <- aggregate(category ~ code, data = park_parea, FUN = max) #평균면적



buffer_f3 <- left_join(buffer_f3, park_parea_ffff, by = c("code" = "code"))
buffer_f3 <- buffer_f3 %>% rename("P_area"="category")
buffer_f3$P_area[is.na(buffer_f3$P_area)] <- 0 ##그 외의 경우 0 처리함 

### 지도로 확인 점 ###### 

park2 <- st_transform(park, crs = 4326)
map2 <- map %>% addPolygons(data = park2, fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2)%>% addCircleMarkers(popup = ~code,radius = 2,color = "red") %>% addPolygons(data = buffer_f, fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2)%>% addCircleMarkers(popup = ~code,radius = 2,color = "red")

park3_grochang2 <- st_transform(park3_grochang, crs = 4326)
map2 <- map %>% addPolygons(data = park3_grochang2, fillColor = "black", fillOpacity = 0.5, color = "blue", weight = 2)%>% addCircleMarkers(popup = ~code,radius = 2,color = "red") %>% addPolygons(data = buffer_f, fillColor = "pink", fillOpacity = 0, color = "blue", weight = 2)%>% addCircleMarkers(popup = ~code,radius = 2,color = "red")


#####################

# 좌표계에 문제있는거 같아서 map으로 확인 한 번 하기#######

bu_gro_map <- st_transform(bu_gro, crs = 4326)
ground_map <- st_transform(ground2, crs = 4326)

map2 <- map %>%
  addPolygons(data = bu_gro_map, fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2)

a <- ggplot(data = ground) +geom_sf() +theme_minimal() +labs(title = "Shapefile Map Visualization", subtitle = "Using R and ggplot2")


### 완성된 데이터셋 필요없는거 다 버려볼게용?ㅎㅎㅎ

str(buffer_f3)


o_set <- st_drop_geometry(buffer_f3)
buffer_data <- o_set %>% select(-c(path,layer,x,y))
buffer_data$d_area[is.na(buffer_data$d_area)] <- 0 ##도로율이랑 
buffer_data$slope[is.na(buffer_data$slope)] <- 0 ##경사도 영이어도 되낭 
str(o_set)

## 📢 1차 보행량 모델링 과정

##################### 필요한 패키지 설치#####

install.packages("lightgbm")
install.packages("mlbench")
library(lightgbm)
library(mlbench)
install.packages("mlbench")
library(dplyr)
install.packages("xgboost")
library(xgboost)
library(readxl)
install.packages("caret")
install.packages("lightgbm")
library(caret)
library(lightgbm)
set.seed(200)# 재현성 

############################

### 카테고리 변수만 팩터화시키고
## 표준화는 거리 빼고는 딱히 안해도될거같음 

str(data)
buffer_data$on_floor <- as.factor(buffer_data$on_floor) ## 팩터변환
buffer_data$CBD  <- as.factor(buffer_data$CBD) ## 팩터변환
buffer_data$P_area  <- as.factor(buffer_data$P_area) ## 팩터변환
buffer_data$pop <- scale(buffer_data$pop , center = FALSE)
str(buffer_data)
data <- buffer_data

data <- data[, -which(names(data) == "code")] ## 필드 code명 제거 


## 상수가되는 변수가 뭐가있을까?
data2 <- data %>% select(-c(CBD,bus_t,bus,bus_l,pop)) ## 버스변수가 ㅠㅠ
data2 <- data %>% select(-c(CBD,pop)) ## 버스변수가 ㅠㅠ

data2 <- data2[-c(259, 132, 974,133,151,31,112), ] ##이상치빼봄 ㅋ


str(data2)


### 모델링 코드를 짜보자 
train_data <- data2[, -1]  # 첫 번째 열 제외 (독립 변수)
train_labels <- data2[, 1] # 첫 번째 열 (종속 변수)
str(data)
# 데이터 매트릭스 형식으로 변환
# train_matrix <- as.matrix(train_data)##하지
# train_labels <- as.matrix(train_labels)##말아바바


##########이건 기본처럼 나누기 ################
# 훈련 데이터와 테스트 데이터를 7:3 비율로 나누기
set.seed(200)  # 결과 재현을 위해 시드 설정
train_index <- sample(1:nrow(data2), size = 0.7 * nrow(data2))  # 70% 훈련 데이터 인덱스
test_index <- setdiff(1:nrow(data2), train_index)  # 나머지 30%는 테스트 데이터
####################

### 계층적 나누기#########

library(caret)

# 종속 변수를 기준으로 데이터 분할 (계층적 샘플링)
set.seed(200) #👌
train_index <- createDataPartition(data2$total, p = 0.7, list = FALSE)  # 'target_variable'은 종속 변수명
# train_data <- data2[train_index, ]
# test_data <- data2[-train_index, ]
# train_data_split <- data2[train_index, ]
# test_data_split <- data2[-train_index, ]
# 훈련 데이터와 테스트 데이터 나누기
train_data_split <- train_data[train_index, ]
train_labels_split <- train_labels[train_index]

test_data_split <- train_data[test_index, ] ##
test_labels_split <- train_labels[test_index] ## 



test_data_split <- train_data[-train_index, ]
test_labels_split <- train_labels[-train_index]


# 훈련 데이터와 테스트 데이터 매트릭스 형식으로 변환
train_matrix_split <- as.matrix(train_data_split)
test_matrix_split <- as.matrix(test_data_split)

## 으로 변환
lgb_train <- lgb.Dataset(data = train_matrix_split, label = train_labels_split, free_raw_data = FALSE)
lgb_test <- lgb.Dataset(data = test_matrix_split, label = test_labels_split, free_raw_data = FALSE)  # 테스트 데이터셋

str(data2)
# 파라미터 설정
params <- list(
  objective = "regression",
  metric = "l2",
  num_leaves = 20,  # 너무 크면 과적합 위험, 적당히 설정
  learning_rate = 0.01,  # 적당히 설정
  feature_fraction = 0.9,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  verbose = 0
)

cv_results <- lgb.cv(
  params = params,
  data = lgb_train,
  nfold = 5,  # 5-폴드 교차검증
  nround = 200,  # 부스팅 라운드 수 (num_boost_round -> nround)
  early_stopping_rounds =50,  # 10 라운드 이상 성능 향상이 없으면 학습 종료
  verbose = 1  # 진행 상황 출력
)
# 교차 검증 결과 출력
print(cv_results)
best_nrounds <- cv_results$best_iter

# 교차 검증에서 최적의 부스팅 라운드 수
bst <- lgb.train(params = params,
                 data = lgb_train,
                 nrounds = best_nrounds,
                 valids = list(test = lgb_test),
                 early_stopping_rounds = 10)


importance <- lgb.importance(bst)

# 중요도 출력
print(importance)

# 중요도를 막대 그래프로 시각화
lgb.plot.importance(importance, top_n = 10, measure = "Gain")


# 테스트 데이터에 대한 예측값 생성
predicted_values <- predict(bst, test_matrix_split)

# RMSE 계산
rmse <- sqrt(mean((test_labels_split - predicted_values)^2))
print(paste("RMSE:", rmse))

# 원본 데이터에 테스트 데이터의 예측값 추가
data2$predicted_medv2 <- NA  # 초기화
data2$predicted_medv2[test_index] <- predicted_values



#################################### 나주에 버리셈 원본 코딩


params <- list(
  objective = "regression",
  metric = "l2",
  num_leaves = 20, #클수록과적합하지만복잡한모델에적합 100기줌 
  learning_rate = 0.05,#0.5
  feature_fraction = 0.9,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  verbose = 0
)

num_round <- 500 #❤️
bst <- lgb.train(params = params,
                 data = lgb_train,
                 nrounds = num_round,
                 valids = list(test = lgb_test),
                 early_stopping_rounds = 10)

# 테스트 데이터에 대한 예측값 생성
predicted_values <- predict(bst, test_matrix_split)

# RMSE 계산
rmse <- sqrt(mean((test_labels_split - predicted_values)^2))
print(paste("RMSE:", rmse))

# 원본 데이터에 테스트 데이터의 예측값 추가
data2$predicted_medv2 <- NA  # 초기화
data2$predicted_medv2[test_index] <- predicted_values


# 예측 결과 확인
print("원본 데이터에 예측값 추가 확인:")
head(data)  # 데이터 확인

importance <- lgb.importance(bst)



# 중요도 출력
print(importance)

# 중요도를 막대 그래프로 시각화
lgb.plot.importance(importance, top_n = 10, measure = "Gain")

# 테스트 데이터만 추출하여 실제 값과 예측값 비교
test_results <- data.frame(
  Actual = test_labels_split,
  Predicted = predicted_values
)

# 테스트 데이터의 예측 결과 출력
print("테스트 데이터 예측 결과:")
print(head(test_results))

# 원본 데이터 시각적으로 확인 (옵션)
# View(data)  # RStudio에서 사용 가능

View(data)
print(length(test_index))         # 테스트 데이터 인덱스 개수 확인
print(length(predicted_values))  
print(dim(test_matrix_split))  # 테스트 데이터셋의 차원
print(length(test_labels_split))  # 테스트 라벨의 길이

par(mfrow = c(1, 2))
hist(train_labels_split, main = "Train Labels", xlab = "Value", col = "blue")
hist(test_labels_split, main = "Test Labels", xlab = "Value", col = "red")



## 📢 차 보행량 모델링 과정

########

remove(DATA)
## 중간 저장 💕#######
remove(DATA)
getwd()
setwd("D:/D드라이브/JY/연구/01.보행량예측/16.2410")
# write_xlsx(DATA_seolu2,"표제부_2015.6_참고용f.xlsx")
save.image(file = "보행량예측_250108_f.RData")
# load("보행량예측_241218.RData")
load("보행량예측_250108_f.RData")

#############

##################데이터기 너무커서 함 줄여볼게여 ㅇㅇ 
remove(up_floor)
remove





#######################나중에 참고용 #####################################################

# pw_pop <- read_excel("D:/D드라이브/JY/연구/01.보행량예측/16.2410/01.인구종사자수/2015년_인구수+종사자_다사_100M_f.xlsx") # 인구수+종사자수

## 좌표계 재설정

pw_dataset_ff <- st_transform(pw_dataset, crs = 4326)
st_crs(pw_dataset_ff)
pw_map <- pw_dataset_ff %>% leaflet() %>% addTiles() ## 지도에 표시 ㅎㅎㅎ 

pw_map <- pw_dataset_ff %>% st_transform(4326)%>% leaflet() %>% addTiles() ## 지도에 표시 ㅎㅎㅎ 

# ls.files<- list.files("/share_folder/027_데이터연구실 공유폴더/sgis_new_data/",pattern = "다사_100M.shp",full.names = T)https://dev.geodikt.com/graphics/5e19cea1-e62e-4bb7-9ecc-d6727cc3364c.png
ls.files<- list.files("D:/D드라이브/JY/연구/01.보행량예측/16.2410/01.인구종사자수/_census_data_2022_4_bnd_oa_bnd_oa_11_2022_2022",pattern = "bnd_oa_11_2022_2022_4Q.shp",full.names = T)
new.grid <- st_read(ls.files)

# f_info.sf.buffer5179 <- split(f_info.sf.buffer5179,f_info.sf.buffer5179$조사지점코드)

st_crs(new.grid) <- 5179
f_info.sf %>% st_transform(4326) %>% leaflet() %>% addTiles() %>% addCircles() %>% addPolygons(data=f_info.sf.buffer %>% st_transform(4326))%>% addPolygons(data=new.grid %>% st_transform(4326))

result.grid<- lapply(f_info.sf.buffer5179, st_intersection,y=new.grid)


result.grid <- do.call(bind_rows,result.grid)
result.grid <- result.grid %>% st_drop_geometry()
result.grid <- left_join(result.grid,mysgis_data, by=c("" = ""))
result_summary <- result.grid %>% group_by(조사지점코드) %>% summarise(
  to_in_001 = sum(to_in_001),)

gather()

f_info.sf.buffer5179 <- split(f_info.sf.buffer5179,f_info.sf.buffer5179$조사지점코드)
st_crs(new.grid) <- 5179

############################# 격자 데이터셋 붙이기 

result.grid <- do.call(bind_rows,result.grid)
result.grid <- result.grid %>% st_drop_geometry()
new.grid.tmp <- new.grid %>% st_drop_geometry()

#new.grid.tmp$조사지점코드 <- sample(c("01-003","01-004","01-005","01-006"),replace = T,size = 824227)
#new.grid %>% st_drop_geometry() %>% mutate(조사지점코드 = "01-003" )

## 주택 붙이기 (격자)

# 주택 변수 전처리

dasa<- fread("/home/whdbscjstk/J_project/DATA/grid2/2015년_주택_다사_100M.txt",sep="^")
table(dasa$V3)
dasa<- dasa %>% spread(V3,V4)

new.grid.tmp <- left_join(new.grid.tmp,dasa %>% select(-V1), by=c("GRID_100M_" = "V2"))

##
result.grid.f <- left_join(result.grid,new.grid.tmp, by="GRID_100M_")

#new.grid.tmp %>% group_by(조사지점코드) %>% summarise(to_in_001)
## 왜 덧셈이 안되지?

result.grid.f$to_ho_001[is.na(result.grid.f$to_ho_001)] <- 0

주택값 <- result.grid.f %>% group_by(조사지점코드) %>% summarise(주택 = sum(to_ho_001)) #na값 0으로 치환해야 되더라 

###이런식으로 여러 값을 계산해서 각 고유지점 값에 붙여서 하나의 데이터 셋 만들기 

asdsga <- left_join(f_con.ver,주택값, by='조사지점코드')

## 인구 수, 종사자 수, 사업체 수 



####격자 내가 원하는 데이터 불러오기 


a_인구 <- fread("DATA/grid2/2015년_인구_다사_100M.txt", sep="^")
table(a_인구$V3)


## v3기준으로 나눠서 필요한것만 추출  (성별은 필요없을 거 같아염)

#인구 
head(a_인구)

pop <- a_인구 %>% spread(V3,V4) %>% select(-V1) %>% group_by(V2)

f_pop <- pop %>% select(to_in_001)


#종사자 

a_종사자 <- fread("DATA/grid2/2015년_종사자_다사_100M.txt", sep="^")
table(a_종사자$V3)

wor <- a_종사자 %>% spread(V3,V4) %>% select(-V1) %>% group_by(V2)


#사업체

bul <- fread("DATA/grid2/2015년_사업체_다사_100M.txt", sep="^")
table(bul$V3)

f_bul <- bul %>% spread(V3,V4) %>% select(-V1) %>% group_by(V2)


## 격자셋에 붙이기
library(dplyr)

result.grid.c <-left_join(result.grid,f_pop, by=c('GRID_100M_'='V2'))

result.grid.c$to_in_001[is.na(result.grid.c$to_in_001)] <- 0

인구값 <- result.grid.c %>% group_by(조사지점코드) %>% summarise(인구 = sum(to_in_001)) #na값 0으로 치환해야 되더라 

result.a <- left_join(asdsga,인구값, by='조사지점코드')

#

result.grid.c <-left_join(result.grid,wor, by=c('GRID_100M_'='V2'))

result.grid.c$to_em_020[is.na(result.grid.c$to_em_020)] <- 0

종사자수 <- result.grid.c %>% group_by(조사지점코드) %>% summarise(종사자수 = sum(to_em_020)) #na값 0으로 치환해야 되더라 

result.a <- left_join(result.a,종사자수, by='조사지점코드')

#


result.grid.c <-left_join(result.grid,f_bul, by=c('GRID_100M_'='V2'))

result.grid.c$to_fa_010[is.na(result.grid.c$to_fa_010)] <- 0

사업체수 <- result.grid.c %>% group_by(조사지점코드) %>% summarise(사업체수 = sum(to_fa_010)) #na값 0으로 치환해야 되더라 

result.a <- left_join(result.a,사업체수, by='조사지점코드')

# remove(result.grid.c)

## 

f_info.sf.buffer <- st_buffer(f_info.sf,500)

f_info.sf.buffer <- f_info.sf.buffer %>% st_transform(5179)

ST <- st_read(dsn="DATA//POINT/서울시_소매점.shp")

T.SF <- ST %>% st_transform(5179)

result.poi <- st_intersection(ST.SF,f_info.sf.buffer)

result.poi.f <- result.poi %>% mutate(total=1)

result.poi <-result.poi.f %>% group_by(조사지점코드) %>% summarise(value = sum(total))



result.poi.test <-result.a %>% left_join(result.poi,by='조사지점코드')













## 조사지점 (유동인구)

f_coun <- read_excel("E:/D드라이vv브/JY/연구/01.보행량예측/02.원시데이터/02.유동인구/0_유동인구_유동인구기본_2015.xlsx") #지점별 유동인구수
f_info <-read_excel("E:/D드라이브/JY/연구/01.보행량예측/02.원시데이터/02.유동인구/4_유동인구_조사지점정보_2015.xlsx") # 지점별 기타 공간정보

# f_coun <- read_excel("E:/D드라이브/JY/연구/01.보행량예측/16.2410/02.유동인구/유동인구조사_2015_dataset_v3.xlsx") #지점별 유동인구수

g
##구조확인

str(f_coun)
str(f_info)

## 필요한 열 추출 
coun_f <- f_coun %>% select(c(조사지점코드,유동인구수))

## 조사지점코드별로 합계, 이후에 일평균써야돼서 5로 나눔 ㅎㅎ 

coun_df <- aggregate(유동인구수 ~ 조사지점코드, data = coun_f, FUN = sum)

coun_df$유동인구수 <- (coun_df$유동인구수/5) 

# 데이터를 공간 데이터화 시켜서 작업하는 것이 훨씬 수월함 
f_info <- f_info %>% mutate(x = as.numeric(X좌표), y = as.numeric(Y좌표))

## x,y 좌표만 필요할거같으니 따로 선택해서가지고다니기

info_xy <- f_info %>% select(c(조사지점코드,x,y))

coun_df_xy <- left_join(coun_df,info_xy, by="조사지점코드")

# 없는 값은 지워주고 
tmp <- coun_df_xy %>% filter(!is.na(x))

# 좌표체계 변환 
f_info.sf <- st_as_sf(tmp,coords = c("x","y"),crs=5174)

###이건 확인용으로 하장? ㅎㅎ
# 서울시 shp를 받아서 하는 것보다 배경지도(leaflet)를 깔고 확인하는 것이 효율적 
f_info.sf %>% st_transform(4326) %>% leaflet() %>% addTiles() %>% addCircles()

#### 데이터 전처리*(1) ####

## 버퍼생성 
# remove(f_info.sf.buffer)
f_info.sf.buffer <- st_buffer(f_info.sf,50) 
st_crs(f_info.sf.buffer) <- 5174

## 버퍼와함께 띄워 보기 
f_info.sf %>% st_transform(4326) %>% leaflet() %>% addTiles() %>% addCircles() %>% addPolygons(data=f_info.sf.buffer %>% st_transform(4326))

#버퍼만든 데이터에서  조사지점만 걸러내기 
f_info.sf.buffer<- f_info.sf.buffer %>% select(조사지점코드)

##.각 버퍼를 (개별로) split 하기 ##? 근데 왜 리스트화시키느거징
f_info.sf.buffer2 <- split(f_info.sf.buffer,f_info.sf.buffer$조사지점코드)

##길이가 같은지 확인해야되고, 길이가 다르다면 고유값으로 남기기 위해 unique()
# f_info.sf.buffer$조사지점코드 %>% unique() %>% length()
# f_info.sf.buffer %>% nrow() ##먼말인지모를수도 참고 

rm(4326) %>% leaflet() %>% addTiles() %>% addCircles() %>% addPolygons(data=f_info.sf.buffer5179 %>% st_transform(4326))



result.poi <- st_intersection(f_info.sf,f_info.sf.buffer)




################################# 제0장 종속변수의 정규화 



# 주택 가격 데이터 추출
s_total <- coun_df_xy$유동인구수 # medv: 주택 가격(median value)

# 히스토그램 그리기
ggplot(data.frame(유동인구수 = s_total), aes(x = 유동인구수)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", aes(y = ..density..)) +
  geom_density(alpha = 0.2, fill = "orange") +
  labs(title = "보행량 (Histogram)", x = "보행량", y = "Density")

# Q-Q plot 그리기
ggplot(data.frame(유동인구수 = s_total), aes(sample = 유동인구수)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "보행량 (Q-Q Plot)", x = "이론적 정규분위수", y = "샘플 데이터 정규분위수")


ggplot(data.frame(s_total = s_total), aes(x = s_total)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", aes(y = ..density..)) +
  geom_density(alpha = 0.2, fill = "orange") +
  labs(title = "보행량", x = "보행량량 (mpg)", y = "Density")


log_data <- log(s_total)

# Q-Q plot 그리기
ggplot(data.frame(유동인구수 = log_data), aes(sample = 유동인구수)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "보행량 (Q-Q Plot)", x = "이론적 정규분위수", y = "샘플 데이터 정규분위수")

ggplot(data.frame(log_data = log_data), aes(x = log_data)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", aes(y = ..density..)) +
  geom_density(alpha = 0.2, fill = "orange") +
  labs(title = "보행량", x = "보행량량 (mpg)", y = "Density")


################################# 제1장 사회인구적 특성 

################## 격자 데이터 붙이기 위해서

# buffer <- split(f_info.buffer,f_info.buffer$조사지점코드)22
## 격자 배경 불러오기 

# ls.files<- list.files("/share_folder/027_데이터연구실 공유폴더/sgis_new_data/",pattern = "다사_100M.shp",full.names = T)https://dev.geodikt.com/graphics/5e19cea1-e62e-4bb7-9ecc-d6727cc3364c.png
ls.files<- list.files("E:/D드라이브/JY/연구/01.보행량예측/02.원시데이터/04.인구종사자수/_census_data_2022_4_bnd_oa_bnd_oa_11_2022_2022",pattern = "bnd_oa_11_2022_2022_4Q.shp",full.names = T)
new.grid <- st_read(ls.files)

# f_info.sf.buffer5179 <- split(f_info.sf.buffer5179,f_info.sf.buffer5179$조사지점코드)

st_crs(new.grid) <- 5179
f_info.sf %>% st_transform(4326) %>% leaflet() %>% addTiles() %>% addCircles() %>% addPolygons(data=f_info.sf.buffer %>% st_transform(4326))%>% addPolygons(data=new.grid %>% st_transform(4326))

result.grid<- lapply(f_info.sf.buffer5179, st_intersection,y=new.grid)


result.grid <- do.call(bind_rows,result.grid)
result.grid <- result.grid %>% st_drop_geometry()
result.grid <- left_join(result.grid,mysgis_data, by=c("grid어쩌구저쩌구" = "뭐는뭐다"))
result_summary <- result.grid %>% group_by(조사지점코드) %>% summarise(
  to_in_001 = sum(to_in_001),)

gather()

f_info.sf.buffer5179 <- split(f_info.sf.buffer5179,f_info.sf.buffer5179$조사지점코드)
st_crs(new.grid) <- 5179
result.grid<- lapply(f_info.sf.buffer5179,st_intersection,y=new.grid) # 왕오래걸림 

############################# 격자 데이터셋 붙이기 

result.grid <- do.call(bind_rows,result.grid)
result.grid <- result.grid %>% st_drop_geometry()
new.grid.tmp <- new.grid %>% st_drop_geometry()

#new.grid.tmp$조사지점코드 <- sample(c("01-003","01-004","01-005","01-006"),replace = T,size = 824227)
#new.grid %>% st_drop_geometry() %>% mutate(조사지점코드 = "01-003" )

## 주택 붙이기 (격자)

# 주택 변수 전처리

dasa<- fread("/home/whdbscjstk/J_project/DATA/grid2/2015년_주택_다사_100M.txt",sep="^")
table(dasa$V3)
dasa<- dasa %>% spread(V3,V4)

new.grid.tmp <- left_join(new.grid.tmp,dasa %>% select(-V1), by=c("GRID_100M_" = "V2"))

##
result.grid.f <- left_join(result.grid,new.grid.tmp, by="GRID_100M_")

#new.grid.tmp %>% group_by(조사지점코드) %>% summarise(to_in_001)
## 왜 덧셈이 안되지?

result.grid.f$to_ho_001[is.na(result.grid.f$to_ho_001)] <- 0

주택값 <- result.grid.f %>% group_by(조사지점코드) %>% summarise(주택 = sum(to_ho_001)) #na값 0으로 치환해야 되더라 

###이런식으로 여러 값을 계산해서 각 고유지점 값에 붙여서 하나의 데이터 셋 만들기 

asdsga <- left_join(f_con.ver,주택값, by='조사지점코드')

## 인구 수, 종사자 수, 사업체 수 



####격자 내가 원하는 데이터 불러오기 


a_인구 <- fread("DATA/grid2/2015년_인구_다사_100M.txt", sep="^")
table(a_인구$V3)


## v3기준으로 나눠서 필요한것만 추출  (성별은 필요없을 거 같아염)

#인구 
head(a_인구)

pop <- a_인구 %>% spread(V3,V4) %>% select(-V1) %>% group_by(V2)

f_pop <- pop %>% select(to_in_001)


#종사자 

a_종사자 <- fread("DATA/grid2/2015년_종사자_다사_100M.txt", sep="^")
table(a_종사자$V3)

wor <- a_종사자 %>% spread(V3,V4) %>% select(-V1) %>% group_by(V2)


#사업체

bul <- fread("DATA/grid2/2015년_사업체_다사_100M.txt", sep="^")
table(bul$V3)

f_bul <- bul %>% spread(V3,V4) %>% select(-V1) %>% group_by(V2)


## 격자셋에 붙이기
library(dplyr)

result.grid.c <-left_join(result.grid,f_pop, by=c('GRID_100M_'='V2'))

result.grid.c$to_in_001[is.na(result.grid.c$to_in_001)] <- 0

인구값 <- result.grid.c %>% group_by(조사지점코드) %>% summarise(인구 = sum(to_in_001)) #na값 0으로 치환해야 되더라 

result.a <- left_join(asdsga,인구값, by='조사지점코드')

#

result.grid.c <-left_join(result.grid,wor, by=c('GRID_100M_'='V2'))

result.grid.c$to_em_020[is.na(result.grid.c$to_em_020)] <- 0

종사자수 <- result.grid.c %>% group_by(조사지점코드) %>% summarise(종사자수 = sum(to_em_020)) #na값 0으로 치환해야 되더라 

result.a <- left_join(result.a,종사자수, by='조사지점코드')

#


result.grid.c <-left_join(result.grid,f_bul, by=c('GRID_100M_'='V2'))

result.grid.c$to_fa_010[is.na(result.grid.c$to_fa_010)] <- 0

사업체수 <- result.grid.c %>% group_by(조사지점코드) %>% summarise(사업체수 = sum(to_fa_010)) #na값 0으로 치환해야 되더라 

result.a <- left_join(result.a,사업체수, by='조사지점코드')

# remove(result.grid.c)

## 

f_info.sf.buffer <- st_buffer(f_info.sf,500)

f_info.sf.buffer <- f_info.sf.buffer %>% st_transform(5179)

ST <- st_read(dsn="DATA//POINT/서울시_소매점.shp")

T.SF <- ST %>% st_transform(5179)

result.poi <- st_intersection(ST.SF,f_info.sf.buffer)

result.poi.f <- result.poi %>% mutate(total=1)

result.poi <-result.poi.f %>% group_by(조사지점코드) %>% summarise(value = sum(total))



result.poi.test <-result.a %>% left_join(result.poi,by='조사지점코드')

##################################### point 자료 


도시공원 <- st_read(dsn="DATA//POINT/서울시_도시공원.shp")

par <- 도시공원 %>% st_transform(5179)

par.f <- st_intersection(par,f_info.sf.buffer)

result.poisd <- par.f %>% st_drop_geometry()

result.poi.f <- result.poisd %>% mutate(total=1)

result.poi <-result.poi.f %>% group_by(조사지점코드) %>% summarise(도시공원 = sum(total))

result.poi.test <- result.a %>% left_join(result.poi,by='조사지점코드')

#

의원 <- st_read(dsn="DATA/POINT/서울시_의원.shp")

T.SF <- 의원 %>% st_transform(5179)

result.poi <- st_intersection(T.SF,f_info.sf.buffer)

result.poisd <- result.poi %>% st_drop_geometry()

result.poi.f <- result.poisd %>% mutate(total=1)

result.poi <-result.poi.f %>% group_by(조사지점코드) %>% summarise(의원 = sum(total))

result.poi.test <- result.poi.test %>% left_join(result.poi,by='조사지점코드')

#


체육 <- st_read(dsn="DATA/POINT/서울시_체육시설.shp")

st_crs(체육) <- 4326

psy <- 체육 %>% st_transform(5174)

psy.z <- st_intersection(psy,f_info.sf.buffer)

result.poisd <- psy.z %>% st_drop_geometry()

result.poi.f <- result.poisd %>% mutate(total=1)

result.poi <-result.poi.f %>% group_by(조사지점코드) %>% summarise(체육시설 = sum(total))

result.poi.test <- result.poi.test %>% left_join(result.poi,by='조사지점코드')


# 나머지, 데이터 붙이기 (보도너비, 차선수)


et <- f_info %>% select(c(조사지점코드,보도너비, 차선수))


result.poi.test <- result.poi.test %>% left_join(et, by='조사지점코드')


fin <- result.poi.test

############# **거리를 못구하겠어욤



station <- st_read(dsn="DATA//POINT/서울시_철도역_point.shp")
plot(station)

#좌표확인
station_f <- station %>% st_transform(5179)
plot(station_f$geometry)
plot(f_info.sf.buffer$geometry)


result.station <- st_intersection(station_f,f_info.sf.buffer)
table(result.station$조사지점코드)
station_f <- station %>% st_transform(5179)

temp.sf<- st_distance(f_info.sf,체육 %>% st_transform(5174))

temp.sf[,1:400]
temp.dist<- data.table(체육시설최근린거리=apply(temp.sf,1,min,na.rm=T),조사지점코드 = f_info.sf$조사지점코드)

str(temp.sf)

plot(result.station)


## 예시 환경 데이터 

## 라이브러리 load
library(rpart)

install.packages("party")
library("party")

# Decision Tree 모델 생성
head(airquality)

## 결측지 제거함 
airq <- subset(airquality, !is.na(Ozone))
head(airq)
fin[is.na(fin)] <- 0
## 종속변수는 Ozone

airct <- ctree(Ozone ~., data = airq)
airct
plot(airct)

# 예측값 근데 평균으로 나올거 (분류가 아니기 때문에 )
head(predict(airct, data=airq))

# MSE（모델성능） 
mean((airq$Ozone - predict(airct))^2)


#####################################


#############

str(fin)
fin$보도너비 <- as.numeric(fin$보도너비) 
fin$차선수 <- as.numeric(fin$차선수)
finn$조사지점코드 <- as.numeric(finn$조사지점코드)
finn <- finn %>% select(-조사지점코드)
finn <- subset(fin, !is.na(유동인구수))

## train, test 데이터 나누기 
## train셋으로 의사결정나무 모델을 만들고 test셋을 모델에 입력하여 얼마나 결과를 잘 예측하는지
library(caret)

set.seed(1000)
# reproducability setting 7:3

intrain<-createDataPartition(y=finn$유동인구수, p=0.7, list=FALSE) 
train<-finn[intrain, ]
test<-finn[-intrain, ]

## 의사 결정 나무 생성 (근데 분류인거같아서 다시 생각해보고 작성할 것 )
install.packages("tree")
library(tree)
library(quantmod)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot()


prp(train, type = 2, extra = 8)


treemod<-tree(유동인구수~. , data=train)
plot(treemod)
text(treemod)

## 가지치ㅈ기ㅣ기ㅣ (과적합문제 해결)

cv.trees<-cv.tree(treemod,FUN=prune.misclass)

# for classification decision tree

printcp(cv.trees)


############ 왜안되는겨 

str(finn)
airct <- ctree(유동인구수 ~., data = finn)
airct
plot(airct)


head(predict(airct, data=finn))
predict(airct, data=finn, type = "node")

mean((finn$유동인구수 - predict(airct))^2)

#########################
############################################################################################################