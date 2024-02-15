library(RSQLite)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)
library(rjson)
library(jsonlite)
library(openxlsx)
library(tidyr)
library(magrittr)

#filename <- "/Users/sousekilyu/Documents/CBDB/latest.db"
filename <- '/Users/sousekilyu/Documents/Meta Data/CBDB/latest.db'
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

## Some operations
dbListTables(db)
df_BIOG_ADDR_DATA <- dbReadTable(db,"BIOG_ADDR_DATA")
df_BIOG_ADDR_CODES <- dbReadTable(db,"BIOG_ADDR_CODES")
df_BIOG_MAIN <- dbReadTable(db,"BIOG_MAIN")
df_OFFICE_CODES <- dbReadTable(db,"OFFICE_CODES")
df_ADDR_BELONGS_DATA <- dbReadTable(db,"ADDR_BELONGS_DATA")
df_POSTED_TO_OFFICE_DATA <- dbReadTable(db,"POSTED_TO_OFFICE_DATA")
df_OFFICE_CODES <- dbReadTable(db,"OFFICE_CODES")
df_OFFICE_CODE_TYPE_REL <- dbReadTable(db,"OFFICE_CODE_TYPE_REL")
df_OFFICE_TYPE_TREE <- dbReadTable(db,"OFFICE_TYPE_TREE")
df_NIAN_HAO <- dbReadTable(db,"NIAN_HAO")
df_DYNASTIES <- dbReadTable(db,"DYNASTIES")

df_EVENT_CODES <- dbReadTable(db,"EVENT_CODES")
df_EVENTS_DATA <- dbReadTable(db,"EVENTS_DATA")


load('/Users/sousekilyu/Documents/R/CBDB/data/df_ADDRESSES.rda')

df_ADDR_DATA <- df_BIOG_ADDR_DATA %>%
  filter(between(c_addr_type, 1, 8)) %>%
  select(c_personid,
         c_addr_id,
         c_addr_type) %>%
  group_by(c_personid) %>%
  arrange(c_addr_type) %>% 
  mutate(rn = row_number()) %>%
  ungroup() %>%
  filter(rn == 1) %>%
  left_join(df_ADDRESSES, by = c("c_addr_id" = "c_addr_id")) %>% 
  dplyr::select(c_personid,
         c_addr_id,
         c_addr_type,
         c_name_chn,
         x_coord,
         y_coord,
         n_province,
         n_city,
         n_district) %>%
  rename(address_name = c_name_chn) %>%
  unique()
df_ADDR_DATA$x_coord[df_ADDR_DATA$x_coord == 36.44774] <- 115.98869
df_ADDR_DATA$y_coord[df_ADDR_DATA$y_coord == 115.98869] <- 36.44774


## 数据清理 男女
df_BIOG_people <- df_BIOG_MAIN %>%
  left_join(df_ADDR_DATA, by = c("c_personid" = "c_personid")) %>%
  left_join(df_DYNASTIES, by = c("c_dy" = "c_dy")) 
df_BIOG_people$mingzi <- df_BIOG_people$c_mingzi_chn %>%
  str_replace_all('\\（', '(') %>%
  str_replace_all('\\）', ')') %>%
  str_replace_all('\\(.*\\)', '')
df_BIOG_people$is_name <- 1
df_BIOG_people$is_name[df_BIOG_people$mingzi == '某' |
                         df_BIOG_people$mingzi == '某某' |
                         df_BIOG_people$mingzi == '氏' |
                              is.na(df_BIOG_people$mingzi)] <- 0


## 数据清理
df_BIOG_people_s <- df_BIOG_people[,c('c_female', 'is_name', 'c_personid', 'c_dynasty_chn', 'c_start',
                                    'c_birthyear', 'c_deathyear', 'c_index_year',
                                    'c_name','c_name_chn', 'c_surname', 'c_surname_chn', 
                                    'mingzi','c_mingzi', 'c_mingzi_chn',
                                    'c_addr_id','c_addr_type','address_name',
                                    'x_coord','y_coord',
                                    'n_province',
                                    'n_city',
                                    'n_district'
                                    )]
df_BIOG_people_s$c_personid %<>% as.character()


df_des <- df_BIOG_people_s %>%
  group_by(
    c_female,
    is_name,
    c_dynasty_chn
  ) %>%
  summarise(freq = n())


## 筛选女性
df_BIOG_female <- df_BIOG_people_s[which(df_BIOG_people_s$c_female == 1),]
df_BIOG_male <- df_BIOG_people_s[which(df_BIOG_people_s$c_female == 0),]

save(df_BIOG_people_s, file = '/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/data/df_BIOG_people_s.rda')


# df_BIOG_hanzi <- df_BIOG_female %>%
#   filter(is_name == 1) %>%
#   select(c_dynasty_chn, mingzi) %>%
#   mutate(mingzi_list = strsplit(mingzi, '')) %>%
#   ungroup() %>%
#   select(c_dynasty_chn, mingzi_list) %>%
#   group_by(c_dynasty_chn) %>%
#   summarise(mingzi = unlist(mingzi_list)) %>%
#   ungroup() 
  
  

#write.csv(df_BIOG_hanzi, '/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/data/df_BIOG_hanzi_dy.csv')

#write.csv(df_BIOG_people_s, '/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/data/df_BIOG.csv')
#write.csv(df_BIOG_female, '/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/data/df_BIOG_female.csv')




