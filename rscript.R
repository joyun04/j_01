

#### library 부착 함수 만들기####
## packages에 쓸 거 담아놓기
packages <- c("data.table", "tidyr", "readxl", "dplyr", "leaflet","sf", "ggthemes","leaflet","writexl")

load_packages <- function(packages) {0
  for(pkg in packages) {
    if(!require(pkg, character.only = TRUE)) {
      install.packages(pkg) # 패키지 설치 
      library(pka, character.only = TRUE)
    } else{
      library(pkg,character.only = TRUE)
    }
  }
  message("완료!")
}

load_packages(packages)
##################################

## 실측지점 및 버퍼 만들기 #####
# setwd("E:/D드라이브/JY/연구/01.보행량예측/16.2410/02.유동인구") ## 유동인구(Foot traffic)
# ft <- read_excel("D:/D드라이브/JY/연구/01.보행량예측/16.2410/02.유동인구/유동인구조사_2015_dataset_v3.xlsx") #지점별 유동인구수
# ft_g <- st_as_sf(ft, coords = c("x", "y"), crs = 4326)
getwd()
ft_g <- st_read("D:/D드라이브/JY/연구/01.보행량예측/16.2410/02.유동인구/유동인구_2015_fff.shp")
map <- ft_g %>% st_transform(4326)%>% leaflet() %>% addTiles() %>% addCircleMarkers(popup = ~code,radius = 2,color = "black") ## 지도에 표시

##버퍼 제작 
ft_b <- st_buffer(ft_g, dist = 50) #반경 50m
ft_bf <- st_transform(ft_b, crs = 5179) #


map2 <- map %>% addPolygons(data = st_transform(ft_b, crs = 4326), fillColor = "pink", fillOpacity = 0.5, color = "blue", weight = 2) %>% addCircleMarkers(popup = ~code, radius = 2, color = "red")

## git test 용 
png("data/map4.png", width = 700, height = 1000)
dev.off()

########

fread()
##안녕하세요
##반가워용 


png("data/map5.png", width = 700, height = 1000)
dev.off()

######################참고 ########
# f_df <- split(buffer_f,buffer_f$code
# ft_sp <- split(ft_b,ft_b$code)
# result.grid<- lapply(buffer_f, st_intersection,y=df_sample)
# f_info.sf.buffer3 <- st_transform(f_info.sf.buffer2, crs = 4326)
# intersection <- st_intersection(buffer_f, pw_dataset_z)
# plot(df_sample)
# buffer_list <- split(buffer_f, buffer_f$group)
# intersection_results <- lapply(buffer_list, st_intersection, y = df_sample)


############################## 
############################## 제1장 사회인구적 특성 

# 00.격자 shp 불러오기
grid <- st_read("D:/D드라이브/JY/연구/01.보행량예측/16.2410/01.인구종사자수/서울시_100m_v2.shp")## 격자 # 5179 좌표

# 01.인구 : pop
# 02.종사자수 : wor

#### 지도확인용 #####
shp_file_3857 <- st_transform(grid, crs = 4326)
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
#######

# 01 pop
pop <- fread("D:/D드라이브/JY/기타/기타_공간데이터/인구수_격자/2015/_census_reqdoc_1720570802873/2015년_인구_다사_100M.txt",sep = "^")

pop <- aggregate(V4 ~ V2, data = pop, FUN = sum) #인구수 합

pop_g <- left_join(grid,pop, by = c("GRID_100M_" = "V2"))

pop_g$V4[is.na(pop_g$V4)] <- 0 ##격제내에 구멍뚫린거 있어서 눌값처리 

pop_g <- st_transform(pop_g, crs = 5179) #

# pw_dataset_z <- st_transform(pop_g, crs =4326)

pop_f <- st_join(ft_bf, pop_g, join = st_intersects)

str(pop_f)

pop_f <- aggregate(total ~ code, data = pop_f, FUN = sum) #인구수 합쳐야댕 

pop_f <- pop_f %>% rename("pop"="total")

ft_bf <- left_join(ft_bf, pop_f, by = c("code" = "code"))

ft_bf <- ft_bf %>% select(-c(path,layer))


# 02 wor

work <- fread("D:/D드라이브/JY/기타/기타_공간데이터/인구수_격자/2015/_census_reqdoc_1720570802873/2015년_종사자_다사_100M.txt",sep = "^")

str(work)

work$V4 <- as.numeric(work$V4)

work <- aggregate(V4 ~ V2, data = work, FUN = sum) #인구수 합쳐야댕 

work_g <- left_join(grid,work, by = c("GRID_100M_" = "V2"))

work_g$V4[is.na(work_g$V4)] <- 0 ##격제내에 구멍뚫린거 있어서 눌값처리 

# dataset_w <- st_transform(pw_dataset_w, crs = 4326)

work_f <- st_join(ft_bf, work_g, join = st_intersects)

work_f <- aggregate(V4 ~ code, data = work_f, FUN = sum) #인구수 합쳐야댕 

work_f <- work_f %>% rename("wor"="V4")

ft_bf <- left_join(ft_bf, work_f, by = c("code" = "code"))


## 03 도로폭 (width)

width <- st_read("D:/D드라이브/JY/기타/기타_공간데이터/2016-TM-KA-MR-LLV2 도로망(2015년 기준)_170929/02링크/ad0022.shp")## 도로 shp 

width <- width %>% select(c(LINK_ID,WIDTH))  

width <- st_transform(width, crs = 5179)

width_g <- st_join(ft_bf, width, join = st_intersects)

width_g$WIDTH <- as.numeric(width_g$WIDTH)

width_g$WIDTH[is.na(width_g$WIDTH)] <- 0 ##격제내에 구멍뚫린거 있어서 눌값처리 

width_f <- aggregate(WIDTH ~ code, data = width_g, FUN = mean) # 차폭은 평균으로 계산하고자 함  

ft_bf <- left_join(ft_bf, width_f, by = c("code" = "code"))

