library(dplyr)
library(lubridate)
library(ggplot2)

#目前 CD 完成for loop DD & 死亡檔待確定檔案數量再修改
#讀取檔案(cd,dd,死因)

setwd("C:/Users/End User/Documents/模擬資料/模擬資料CSV/Health-01/01西醫")
data1 <- list.files() #確認路徑以及檔案資料並合併在一個清單之中
lenf1 <- length(list.files())

for(i in 1:lenf1){

  setwd("C:/Users/End User/Documents/模擬資料/模擬資料CSV/Health-01/01西醫")
  cd <- lapply(data1[i], read.csv)
# cd <- read.csv("C:/Users/End User/Documents/模擬資料/模擬資料CSV/Health-01/01西醫/opdte10301_10.csv", 
#                header = TRUE, sep = ",")
  
  setwd("C:/Users/End User/Documents/模擬資料/模擬資料CSV/Health-02")
  data2 <- list.files() #確認路徑以及檔案資料並合併在一個清單之中
  lenf2 <- length(list.files())
  dd <- lapply(data2[i], read.csv)
# dd <- read.csv("C:/Users/End User/Documents/模擬資料/模擬資料CSV/Health-02/ipdte103.csv",
#               header = TRUE, sep = ",")
  
  setwd("C:/Users/End User/Documents/模擬資料/模擬資料CSV/Health-30")
  data3 <- list.files() #確認路徑以及檔案資料並合併在一個清單之中
  lenf3 <- length(list.files())
  death <- lapply(data3[i], read.csv)
#death <- read.csv("C:/Users/End User/Documents/模擬資料/模擬資料CSV/Health-30/multi103.csv",
#                  header = TRUE, sep = ",")
  
  cd <- data.frame(cd)
  dd <- data.frame(dd)
  death <- data.frame(death)

#篩取出都還活著的人
  all <- left_join(cd, dd, by = "ID")
  death <- death %>% mutate(d = 4)
  all <- left_join(all, death, by = "ID")
  all$d[is.na(all$d)] <- 0
  all <- all %>% filter(d != 4)

#新增一欄age-----------------------------------------------------------
  age <- as.character(all$AGE.x)
  age <- gsub("0-14天", 0.3, age) #將0-14天取代成0.3  
  age <- gsub("15-28天", 0.6, age) #將15-28天取代成0.6
  age <- gsub("29天-未滿1歲", 0.9, age) #將29天-未滿1歲取代成0.9
  age <- gsub("85歲以上", 86, age) #將85歲以上取代成86
  age <- strsplit(age ,"歲") 
  age <- as.numeric(age) #切割完的資料轉換形式
  c_data <- all %>% mutate(age)#新增整理新的年齡變數

#age 挑選(老人)---------------------------------------------------------
#挑選同一人最小歲數
  older <- filter(c_data, age >= 65)
  count_older <- older %>% group_by(ID) %>% summarise()
  min_age <- older %>% group_by(ID) %>% summarise(new_age = min(age))
  data_clean <- left_join(min_age, older, by = "ID")

#篩選乾淨資料所需變數----------------------------------------------
  vars <- c("ID", "ICD9CM_1.x", "ICD9CM_2.x", "ICD9CM_3.x",
          "ICD9CM_1.y", "ICD9CM_2.y", "ICD9CM_3.y", "ICD9CM_4", "ICD9CM_5")


#select一個變數內容須+上all_of()，不然有時候會有辨識問題
  ICD <- data_clean %>% select(all_of(vars))

#有V開頭編號取代為NA,NA可隨狀況替換             
  ICD$ICD9CM_1.x <- gsub("^V" , NA, ICD$ICD9CM_1.x) 
  ICD$ICD9CM_2.x <- gsub("^V" , NA, ICD$ICD9CM_2.x) 
  ICD$ICD9CM_3.x <- gsub("^V" , NA, ICD$ICD9CM_3.x) 
  ICD$ICD9CM_1.y <- gsub("^V" , NA, ICD$ICD9CM_1.y) 
  ICD$ICD9CM_2.y <- gsub("^V" , NA, ICD$ICD9CM_2.y) 
  ICD$ICD9CM_3.y <- gsub("^V" , NA, ICD$ICD9CM_3.y)
  ICD$ICD9CM_4 <- gsub("^V" , NA, ICD$ICD9CM_4) 
  ICD$ICD9CM_5 <- gsub("^V" , NA, ICD$ICD9CM_5) 

#有E開頭編號取代為NA,NA可隨狀況替換             
  ICD$ICD9CM_1.x <- gsub("^E" , NA, ICD$ICD9CM_1.x) 
  ICD$ICD9CM_2.x <- gsub("^E" , NA, ICD$ICD9CM_2.x) 
  ICD$ICD9CM_3.x <- gsub("^E" , NA, ICD$ICD9CM_3.x) 
  ICD$ICD9CM_1.y <- gsub("^E" , NA, ICD$ICD9CM_1.y) 
  ICD$ICD9CM_2.y <- gsub("^E" , NA, ICD$ICD9CM_2.y) 
  ICD$ICD9CM_3.y <- gsub("^E" , NA, ICD$ICD9CM_3.y)
  ICD$ICD9CM_4 <- gsub("^E" , NA, ICD$ICD9CM_4) 
  ICD$ICD9CM_5 <- gsub("^E" , NA, ICD$ICD9CM_5) 

#取出前3碼(ICD9判別方式)
  ICD$ICD9CM_1.x <- substring(ICD$ICD9CM_1.x, 1, 3)
  ICD$ICD9CM_2.x <- substring(ICD$ICD9CM_2.x, 1, 3)
  ICD$ICD9CM_3.x <- substring(ICD$ICD9CM_3.x, 1, 3)
  ICD$ICD9CM_1.y <- substring(ICD$ICD9CM_1.y, 1, 3)
  ICD$ICD9CM_2.y <- substring(ICD$ICD9CM_2.y, 1, 3)
  ICD$ICD9CM_3.y <- substring(ICD$ICD9CM_3.y, 1, 3)
  ICD$ICD9CM_4 <- substring(ICD$ICD9CM_4, 1, 3)
  ICD$ICD9CM_5  <- substring(ICD$ICD9CM_5, 1, 3)
#calculate frailty about CD(Outpatient)--------------------

  frailty_icd9_1 <- ICD %>% select(ID, ICD9CM_1.x) %>% 
    filter(ICD9CM_1.x == "285"|ICD9CM_1.x == "290"|ICD9CM_1.x == "332"|ICD9CM_1.x == "374"|
           ICD9CM_1.x == "386"|ICD9CM_1.x == "402"|ICD9CM_1.x == "414"|ICD9CM_1.x == "427"|
           ICD9CM_1.x == "428"|ICD9CM_1.x == "434"|ICD9CM_1.x == "437"|ICD9CM_1.x == "438"|
           ICD9CM_1.x == "486"|ICD9CM_1.x == "491"|ICD9CM_1.x == "493"|ICD9CM_1.x == "496"|
           ICD9CM_1.x == "530"|ICD9CM_1.x == "531"|ICD9CM_1.x == "532"|ICD9CM_1.x == "536"|
           ICD9CM_1.x == "558"|ICD9CM_1.x == "564"|ICD9CM_1.x == "585"|ICD9CM_1.x == "599"|
           ICD9CM_1.x == "600"|ICD9CM_1.x == "682"|ICD9CM_1.x == "692"|ICD9CM_1.x == "698"|
           ICD9CM_1.x == "733"|ICD9CM_1.x == "780"|ICD9CM_1.x == "785"|ICD9CM_1.x == "788")


  frailty_icd9_2 <- ICD %>% select(ID, ICD9CM_2.x) %>% 
    filter(ICD9CM_2.x == "285"|ICD9CM_2.x == "290"|ICD9CM_2.x == "332"|ICD9CM_2.x == "374"|
           ICD9CM_2.x == "386"|ICD9CM_2.x == "402"|ICD9CM_2.x == "414"|ICD9CM_2.x == "427"|
           ICD9CM_2.x == "428"|ICD9CM_2.x == "434"|ICD9CM_2.x == "437"|ICD9CM_2.x == "438"|
           ICD9CM_2.x == "486"|ICD9CM_2.x == "491"|ICD9CM_2.x == "493"|ICD9CM_2.x == "496"|
           ICD9CM_2.x == "530"|ICD9CM_2.x == "531"|ICD9CM_2.x == "532"|ICD9CM_2.x == "536"|
           ICD9CM_2.x == "558"|ICD9CM_2.x == "564"|ICD9CM_2.x == "585"|ICD9CM_2.x == "599"|
           ICD9CM_2.x == "600"|ICD9CM_2.x == "682"|ICD9CM_2.x == "692"|ICD9CM_2.x == "698"|
           ICD9CM_2.x == "733"|ICD9CM_2.x == "780"|ICD9CM_2.x == "785"|ICD9CM_2.x == "788")


  frailty_icd9_3 <- ICD %>% select(ID, ICD9CM_3.x) %>% 
    filter(ICD9CM_3.x == "285"|ICD9CM_3.x == "290"|ICD9CM_3.x == "332"|ICD9CM_3.x == "374"|
           ICD9CM_3.x == "386"|ICD9CM_3.x == "402"|ICD9CM_3.x == "414"|ICD9CM_3.x == "427"|
           ICD9CM_3.x == "428"|ICD9CM_3.x == "434"|ICD9CM_3.x == "437"|ICD9CM_3.x == "438"|
           ICD9CM_3.x == "486"|ICD9CM_3.x == "491"|ICD9CM_3.x == "493"|ICD9CM_3.x == "496"|
           ICD9CM_3.x == "530"|ICD9CM_3.x == "531"|ICD9CM_3.x == "532"|ICD9CM_3.x == "536"|
           ICD9CM_3.x == "558"|ICD9CM_3.x == "564"|ICD9CM_3.x == "585"|ICD9CM_3.x == "599"|
           ICD9CM_3.x == "600"|ICD9CM_3.x == "682"|ICD9CM_3.x == "692"|ICD9CM_3.x == "698"|
           ICD9CM_3.x == "733"|ICD9CM_3.x == "780"|ICD9CM_3.x == "785"|ICD9CM_3.x == "788")


  colnames(frailty_icd9_1) <- c("ID", "icd9")
  colnames(frailty_icd9_2) <- c("ID", "icd9")
  colnames(frailty_icd9_3) <- c("ID", "icd9")
  all_icd <- rbind(frailty_icd9_1, frailty_icd9_2)
  all_icd <- rbind(all_icd, frailty_icd9_3)
  all_icd <- all_icd %>% group_by(ID, icd9) %>% summarise(n = n()) %>% 
    filter(n >= 3)
  all_icd <- as.data.frame(all_icd[,-3]) 

#calculate frailty about DD----------------------------------------------

  frailty_icd9_4 <- ICD %>% select(ID, ICD9CM_1.y) %>% 
    filter(ICD9CM_1.y == "285"|ICD9CM_1.y == "290"|ICD9CM_1.y == "332"|ICD9CM_1.y == "374"|
           ICD9CM_1.y == "386"|ICD9CM_1.y == "402"|ICD9CM_1.y == "414"|ICD9CM_1.y == "427"|
           ICD9CM_1.y == "428"|ICD9CM_1.y == "434"|ICD9CM_1.y == "437"|ICD9CM_1.y == "438"|
           ICD9CM_1.y == "486"|ICD9CM_1.y == "491"|ICD9CM_1.y == "493"|ICD9CM_1.y == "496"|
           ICD9CM_1.y == "530"|ICD9CM_1.y == "531"|ICD9CM_1.y == "532"|ICD9CM_1.y == "536"|
           ICD9CM_1.y == "558"|ICD9CM_1.y == "564"|ICD9CM_1.y == "585"|ICD9CM_1.y == "599"|
           ICD9CM_1.y == "600"|ICD9CM_1.y == "682"|ICD9CM_1.y == "692"|ICD9CM_1.y == "698"|
           ICD9CM_1.y == "733"|ICD9CM_1.y == "780"|ICD9CM_1.y == "785"|ICD9CM_1.y == "788")

  frailty_icd9_5 <- ICD %>% select(ID, ICD9CM_2.y) %>% 
    filter(ICD9CM_2.y == "285"|ICD9CM_2.y == "290"|ICD9CM_2.y == "332"|ICD9CM_2.y == "374"|
           ICD9CM_2.y == "386"|ICD9CM_2.y == "402"|ICD9CM_2.y == "414"|ICD9CM_2.y == "427"|
           ICD9CM_2.y == "428"|ICD9CM_2.y == "434"|ICD9CM_2.y == "437"|ICD9CM_2.y == "438"|
           ICD9CM_2.y == "486"|ICD9CM_2.y == "491"|ICD9CM_2.y == "493"|ICD9CM_2.y == "496"|
           ICD9CM_2.y == "530"|ICD9CM_2.y == "531"|ICD9CM_2.y == "532"|ICD9CM_2.y == "536"|
           ICD9CM_2.y == "558"|ICD9CM_2.y == "564"|ICD9CM_2.y == "585"|ICD9CM_2.y == "599"|
           ICD9CM_2.y == "600"|ICD9CM_2.y == "682"|ICD9CM_2.y == "692"|ICD9CM_2.y == "698"|
           ICD9CM_2.y == "733"|ICD9CM_2.y == "780"|ICD9CM_2.y == "785"|ICD9CM_2.y == "788")

  frailty_icd9_6 <- ICD %>% select(ID, ICD9CM_3.y) %>% 
    filter(ICD9CM_3.y == "285"|ICD9CM_3.y == "290"|ICD9CM_3.y == "332"|ICD9CM_3.y == "374"|
           ICD9CM_3.y == "386"|ICD9CM_3.y == "402"|ICD9CM_3.y == "414"|ICD9CM_3.y == "427"|
           ICD9CM_3.y == "428"|ICD9CM_3.y == "434"|ICD9CM_3.y == "437"|ICD9CM_3.y == "438"|
           ICD9CM_3.y == "486"|ICD9CM_3.y == "491"|ICD9CM_3.y == "493"|ICD9CM_3.y == "496"|
           ICD9CM_3.y == "530"|ICD9CM_3.y == "531"|ICD9CM_3.y == "532"|ICD9CM_3.y == "536"|
           ICD9CM_3.y == "558"|ICD9CM_3.y == "564"|ICD9CM_3.y == "585"|ICD9CM_3.y == "599"|
           ICD9CM_3.y == "600"|ICD9CM_3.y == "682"|ICD9CM_3.y == "692"|ICD9CM_3.y == "698"|
           ICD9CM_3.y == "733"|ICD9CM_3.y == "780"|ICD9CM_3.y == "785"|ICD9CM_3.y == "788")

  frailty_icd9_7 <- ICD %>% select(ID, ICD9CM_4) %>% 
    filter(ICD9CM_4 == "285"|ICD9CM_4 == "290"|ICD9CM_4 == "332"|ICD9CM_4 == "374"|
           ICD9CM_4 == "386"|ICD9CM_4 == "402"|ICD9CM_4 == "414"|ICD9CM_4 == "427"|
           ICD9CM_4 == "428"|ICD9CM_4 == "434"|ICD9CM_4 == "437"|ICD9CM_4 == "438"|
           ICD9CM_4 == "486"|ICD9CM_4 == "491"|ICD9CM_4 == "493"|ICD9CM_4 == "496"|
           ICD9CM_4 == "530"|ICD9CM_4 == "531"|ICD9CM_4 == "532"|ICD9CM_4 == "536"|
           ICD9CM_4 == "558"|ICD9CM_4 == "564"|ICD9CM_4 == "585"|ICD9CM_4 == "599"|
           ICD9CM_4 == "600"|ICD9CM_4 == "682"|ICD9CM_4 == "692"|ICD9CM_4 == "698"|
           ICD9CM_4 == "733"|ICD9CM_4 == "780"|ICD9CM_4 == "785"|ICD9CM_4 == "788")


  frailty_icd9_8 <- ICD %>% select(ID, ICD9CM_5) %>% 
    filter(ICD9CM_5 == "285"|ICD9CM_5 == "290"|ICD9CM_5 == "332"|ICD9CM_5 == "374"|
           ICD9CM_5 == "386"|ICD9CM_5 == "402"|ICD9CM_5 == "414"|ICD9CM_5 == "427"|
           ICD9CM_5 == "428"|ICD9CM_5 == "434"|ICD9CM_5 == "437"|ICD9CM_5 == "438"|
           ICD9CM_5 == "486"|ICD9CM_5 == "491"|ICD9CM_5 == "493"|ICD9CM_5 == "496"|
           ICD9CM_5 == "530"|ICD9CM_5 == "531"|ICD9CM_5 == "532"|ICD9CM_5 == "536"|
           ICD9CM_5 == "558"|ICD9CM_5 == "564"|ICD9CM_5 == "585"|ICD9CM_5 == "599"|
           ICD9CM_5 == "600"|ICD9CM_5 == "682"|ICD9CM_5 == "692"|ICD9CM_5 == "698"|
           ICD9CM_5 == "733"|ICD9CM_5 == "780"|ICD9CM_5 == "785"|ICD9CM_5 == "788")

  colnames(frailty_icd9_4) <- c("ID", "icd9")
  colnames(frailty_icd9_5) <- c("ID", "icd9")
  colnames(frailty_icd9_6) <- c("ID", "icd9")
  colnames(frailty_icd9_7) <- c("ID", "icd9")
  colnames(frailty_icd9_8) <- c("ID", "icd9")

  all_icd1 <- rbind(frailty_icd9_4, frailty_icd9_5)
  all_icd1 <- rbind(all_icd1, frailty_icd9_6)
  all_icd1 <- rbind(all_icd1, frailty_icd9_7)
  all_icd1 <- rbind(all_icd1, frailty_icd9_8)

  all_icd2 <- rbind(all_icd, all_icd1) 

# calculate mFI to everyone(32)-----------------------------------
  icd93_a <- all_icd2 %>% group_by(ID, icd9) %>% summarise(n = n()) 

  icd93_b <- group_by(icd93_a, ID) %>% summarise(count = n()) 

  icd93_c <- mutate(icd93_b, mFI = icd93_b$count/32)

  a <- icd93_c %>% select(mFI)
  b <- as.data.frame(numeric(nrow(count_older) - nrow(icd93_c)))
  colnames(b) <- c("mFI")
  c <- rbind(a,b)
  icd93_mean <- mean(c$mFI) #mFI 平均值
  icd93_sd <- sd(c$mFI)     #mFI 標準差

#output summary------------------
  setwd("C:/Users/End User/Desktop/summary")
  sink(sprintf("summary_%s",data1[i]))
  print(summary(c$mFI))
  print(icd93_mean)
  print(icd93_sd)
  sink()
}


