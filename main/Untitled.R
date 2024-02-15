library(RSQLite)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)
library(rjson)
library(jsonlite)
library(openxlsx)

filename <- "/Users/sousekilyu/Documents/CBDB/data/latest.db"
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

df_BIOG_MAIN_female <- df_BIOG_MAIN[which(df_BIOG_MAIN$c_female == 1),]

#df_ADDRESSES <- dbReadTable(db,"ADDRESSES")
df_ENTRY_DATA <- dbReadTable(db,"ENTRY_DATA")
df_ENTRY_CODES <- dbReadTable(db,"ENTRY_CODES")
df_ENTRY_TYPES <- dbReadTable(db,"ENTRY_TYPES")
df_ENTRY_CODE_TYPE_REL <- dbReadTable(db,"ENTRY_CODE_TYPE_REL")
df_DYNASTIES <- dbReadTable(db,"DYNASTIES")

# You can fetch all results:
sql_file <- "
SELECT a.c_personid,
       a.c_index_year,
       a.c_name_chn AS person_name,
       a.c_surname_chn AS person_surname,
       s.c_dynasty_chn AS dynasty_chn,
       a.c_birthyear AS birth_year,
       b.c_addr_id,
       b.c_addr_type,
       d.c_addr_desc,
       d.c_addr_desc_chn,
       c.c_name_chn AS addr_name,
       c.belongs1_Name,
       c.belongs2_Name,
       c.belongs3_Name,
       c.x_coord,
       c.y_coord,
       e.c_year AS entry_year,
       e.c_exam_rank AS exam_rank,
       e.c_age AS entry_age,
       e.c_entry_desc_chn AS entry_desc,
       e.c_nianhao_chn AS entry_nianhao,
       e.c_dynasty_chn AS entry_dynasty,
       case when o.c_personid IS NOT NULL then 1 else 0 end AS if_post_zaixiang
FROM BIOG_MAIN AS a
LEFT JOIN DYNASTIES AS s ON a.c_dy = s.c_dy
LEFT JOIN (-- 筛选地址并去重

           SELECT c_personid,
                  c_addr_id,
                  c_addr_type
           FROM
             (SELECT c_personid,
                     c_addr_id,
                     c_addr_type,
                     row_number() over(partition BY c_personid
                                       ORDER BY c_addr_type, c_notes) AS rn
              FROM BIOG_ADDR_DATA
              WHERE c_addr_type BETWEEN 1 AND 8)tmp
           WHERE rn = 1) AS b ON a.c_personid = b.c_personid
LEFT JOIN ADDRESSES AS c -- 地址归类
ON b.c_addr_id = c.c_addr_id
LEFT JOIN BIOG_ADDR_CODES AS d -- 地址名称
ON b.c_addr_type = d.c_addr_type
JOIN
  (SELECT *
   FROM
     (SELECT t1.c_personid,
             t1.c_year,
             t1.c_age,
             t1.c_entry_addr_id,
             t1.c_entry_code,
             t1.c_nianhao_id,
             g.c_nianhao_chn,
             g.c_dynasty_chn ,
             f.c_entry_desc_chn,
             t1.c_exam_rank,
             row_number() over(partition BY t1.c_personid
                               ORDER BY t1.c_year) AS rn
      FROM ENTRY_DATA t1
      JOIN ENTRY_CODES AS f -- 入仕途径编码
ON t1.c_entry_code = f.c_entry_code
      JOIN ENTRY_CODE_TYPE_REL AS r -- 入仕大类
ON f.c_entry_code = r.c_entry_code
      LEFT JOIN NIAN_HAO AS g -- 年号id和朝代
ON t1.c_nianhao_id = g.c_nianhao_id
      WHERE r.c_entry_type LIKE '%04%') tmp
   WHERE rn = 1) AS e -- 入仕途径
ON a.c_personid = e.c_personid
LEFT JOIN
  (SELECT t1.c_personid
   FROM POSTED_TO_OFFICE_DATA AS t1
   JOIN OFFICE_CODES AS t2 ON t1.c_office_id = t2.c_office_id
   WHERE t2.c_category_2 = '宰相門'
   GROUP BY t1.c_personid)o ON a.c_personid = o.c_personid
WHERE a.c_birthyear BETWEEN -211 AND 1911
GROUP BY a.c_personid,
         a.c_index_year,
         a.c_name_chn,
         a.c_surname_chn,
         a.c_birthyear,
         b.c_addr_id,
         b.c_addr_type,
         d.c_addr_desc,
         d.c_addr_desc_chn,
         c.c_name_chn,
         c.belongs1_Name,
         c.belongs2_Name,
         c.belongs3_Name,
         c.x_coord,
         c.y_coord,
         e.c_year,
         e.c_exam_rank,
         e.c_age,
         e.c_entry_desc_chn,
         e.c_nianhao_chn,
         e.c_dynasty_chn,
         case when o.c_personid IS NOT NULL then 1 else 0 end
"
res <- dbSendQuery(db, sql_file)
df_jinshi <- dbFetch(res)

df_jinshi$x_coord[df_jinshi$x_coord == 36.44774] <- 115.98869
df_jinshi$y_coord[df_jinshi$y_coord == 115.98869] <- 36.44774
df_jinshi$x_addr <- paste0(df_jinshi$x_coord,',',df_jinshi$y_coord)
df_jinshi <- left_join(df_jinshi, test_df, by = c("x_addr" = "x_addr"))

write.csv(df_jinshi, '/Users/sousekilyu/Documents/CBDB/data/coord.csv')

#write.xlsx(df_DYNASTIES, '/Users/sousekilyu/Documents/CBDB/data/df_DYNASTIES.xlsx')

#save(df_ADDRESSES, file = '/Users/sousekilyu/Documents/CBDB/data/df_ADDRESSES.rda')









# Clear the result
dbClearResult(res)

# Disconnect from the database
dbDisconnect(con)