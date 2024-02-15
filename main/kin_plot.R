## 社会关系
library(magrittr)
library(ggraph)
library(igraph)
library(tidyverse)
theme_set(theme_void())
library(showtext)
font.families()
showtext.auto()
font_add('PingFang', '/System/Library/Fonts/PingFang.ttc')

####
load('/Users/sousekilyu/Documents/R/CBDB_Women/data/df_BIOG_people_s.rda')
load('/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/data/df_zhao_all.rda')

##
df_zhao_all %<>%
  filter(c_name_chn != '杜氏(趙弘殷妻)')

## 宋朝皇室关系图
.edges <- df_zhao_all %>%
  select(c_personid, c_kin_id, type) %>%
  rename(from = c_personid,
         c_personid = c_kin_id) %>%
  left_join(df_BIOG_people_s) %>%
  select(from, c_personid, c_female, c_birthyear, type) %>%
  arrange(from, c_birthyear) %>%
  rename(to = c_personid)
.name <- unique(c(as.character(.edges$from), as.character(.edges$to)))
.vertices <- data.frame(
  c_personid = .name
) %>%
  left_join(df_BIOG_people_s) %>%
  select(c_personid, c_name_chn, c_birthyear, c_female) %>%
  rename(name = c_personid)

flareGraph_song <- graph_from_data_frame(.edges, vertices = .vertices)
V(flareGraph_song)$degree <- degree(flareGraph_song)

ggraph(flareGraph_song
       , layout = 'kk'  #网状图
       #,'igraph', algorithm = 'tree' #树状图
       #, circular = TRUE
) + 
  geom_edge_diagonal(aes(color = as.factor(c_female), linetype = as.factor(type)), alpha = .3) +
  geom_node_point(aes(color = as.factor(c_female)), size = 1.5, alpha = .5)+
  #coord_fixed() + 
  # geom_node_text(aes(label = c_name_chn
  #                    #, filter = c_name_chn %in% c('李世民（唐太宗）', '武曌（武則天）')
  #                    , filter = c_female == 1
  #                    ), color = 'black',
  #                size = 1) +
  geom_node_point(aes(filter = c_name_chn == '趙匡胤'
  ), color = "blue",
  size = 4,
  alpha = .6) +
  geom_node_point(aes(filter = c_name_chn == '趙炅'
  ), color = "blue",
  size = 4,
  alpha = .6) +
  geom_node_text(aes(filter = c_name_chn == '趙匡胤'
  ), color = "white",
  label = '趙匡胤',
  size = 15,
  alpha = .8) +
  geom_node_text(aes(filter = c_name_chn == '趙炅'
  ), color = "white",
  label = '趙炅',
  size = 15,
  alpha = .8) +
  scale_edge_color_manual(name = '性别' , 
                          values = c("0" = "#6187B6", "1" = "#F16E67"),
                          labels = c('男性', '女性')) +
  scale_color_manual(name = '性别' , 
                     values = c("0" = "#6187B6", "1" = "#F16E67"),
                     labels = c('男性', '女性')) +
  scale_edge_linetype_discrete(name = '关系类型',
                               labels = c('长晚辈关系', '婚姻关系')) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
  #ggforce::theme_no_axes()
ggsave('/Users/sousekilyu/Documents/R/CBDB_Women/plot/song_net.png',
       dpi = 300,
       height = 9, width = 12)

## 宋徽宗的延续

dt_song_ji <- getkin2DF(9008)
flareGraph_ji <- getdt2GRAPH(dt_song_ji)
V(flareGraph_ji)$degree <- degree(flareGraph_ji)

ggraph(flareGraph_ji, layout = 'kk') + 
  geom_edge_diagonal(aes(color = as.factor(c_female), linetype = as.factor(type)), alpha = .8) +
  geom_node_point(aes(color = as.factor(c_female)), size = 2, alpha = .8)+
  geom_node_point(aes(filter = c_name_chn == '趙佶'
  ), color = "blue",
  size = 4,
  alpha = .6) +
  geom_node_text(aes(label = str_replace_all(c_name_chn, "趙氏|\\(|\\)|\\（|\\）", ""),
    filter = (grepl('趙佶女', c_name_chn))
  ), color = "white",
  size = 15,
  alpha = .7) +
  scale_edge_color_manual(name = '性别' , 
                          values = c("0" = "#6187B6", "1" = "#F16E67"),
                          labels = c('男性', '女性')) +
  scale_color_manual(name = '性别' , 
                     values = c("0" = "#6187B6", "1" = "#F16E67"),
                     labels = c('男性', '女性')) +
  scale_edge_linetype_discrete(name = '关系类型',
                               labels = c('长晚辈关系', '婚姻关系')) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

ggsave('/Users/sousekilyu/Documents/R/CBDB_Women/plot/song_ji2.png',
       dpi = 300,
       height = 9, width = 12)

#####
## 以唐朝一支為例子
load('/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/data/df_li_all.rda')

.edges <- df_li_all %>%
  select(c_personid, c_kin_id, type) %>%
  rename(from = c_personid,
         c_personid = c_kin_id) %>%
  left_join(df_BIOG_people_s) %>%
  select(from, c_personid, c_female, c_birthyear, type) %>%
  arrange(from, c_birthyear) %>%
  rename(to = c_personid)
.name <- unique(c(as.character(.edges$from), as.character(.edges$to)))
.vertices <- data.frame(
  c_personid = .name
) %>%
  left_join(df_BIOG_people_s) %>%
  select(c_personid, c_name_chn, c_birthyear, c_female) %>%
  rename(name = c_personid)

flareGraph_tang <- graph_from_data_frame(.edges, vertices = .vertices)

ggraph(flareGraph_tang, layout = 'kk'
       #'igraph', algorithm = 'tree'
       #, circular = TRUE
) + 
  geom_edge_diagonal(aes(color = as.factor(c_female), linetype = as.factor(type)), alpha = .3) +
  geom_node_point(aes(color = as.factor(c_female)), size = 1.5, alpha = .5)+
  geom_node_point(aes(filter = c_name_chn == '武曌（武則天）'
  ), color = "red",
  size = 4,
  alpha = .6) +
  scale_edge_color_manual(name = '性别' , 
                          values = c("0" = "#6187B6", "1" = "#F16E67"),
                          labels = c('男性', '女性')) +
  scale_color_manual(name = '性别' , 
                     values = c("0" = "#6187B6", "1" = "#F16E67"),
                     labels = c('男性', '女性')) +
  scale_edge_linetype_discrete(name = '关系类型',
                               labels = c('长晚辈关系', '婚姻关系')) +
  theme(legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))
  #ggforce::theme_no_axes()
ggsave('/Users/sousekilyu/Documents/R/CBDB_Women/plot/tang_wzt.png',
       dpi = 300,
       height = 9, width = 12)

## 唐初
dt_tang_wzt <- getkin2DF(93663)
flareGraph_wu <- getdt2GRAPH(dt_tang_wzt)
V(flareGraph_wu)$degree <- degree(flareGraph_wu)

ggraph(flareGraph_wu, layout = 'kk') + 
  geom_edge_diagonal(aes(color = as.factor(c_female), linetype = as.factor(type)), alpha = .3) +
  geom_node_point(aes(color = as.factor(c_female)), size = 1.5, alpha = .5)+
  geom_node_point(aes(filter = c_name_chn == '武曌（武則天）'
                     ), color = "red",
                  size = 4,
                  alpha = .6) +
  geom_node_text(aes(filter = c_name_chn == '武曌（武則天）'
  ), color = "white",
  label = '武曌（武則天）',
  size = 25,
  alpha = .8) +
  scale_edge_color_manual(name = '性别' , 
                          values = c("0" = "#6187B6", "1" = "#F16E67"),
                          labels = c('男性', '女性')) +
  scale_color_manual(name = '性别' , 
                     values = c("0" = "#6187B6", "1" = "#F16E67"),
                     labels = c('男性', '女性')) +
  scale_edge_linetype_discrete(name = '关系类型',
                               labels = c('长晚辈关系', '婚姻关系')) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

ggsave('/Users/sousekilyu/Documents/R/CBDB_Women/plot/tang_wu2.png',
       dpi = 300,
       height = 9, width = 12)

####################
## 蘭陵蕭

load('/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/data/dt_xiao_all.rda')

.edges <- dt_xiao_all %>%
  select(c_personid, c_kin_id, type) %>%
  rename(from = c_personid,
         c_personid = c_kin_id) %>%
  left_join(df_BIOG_people_s) %>%
  select(from, c_personid, c_female, c_birthyear, type) %>%
  arrange(from, c_birthyear) %>%
  rename(to = c_personid)
.name <- unique(c(as.character(.edges$from), as.character(.edges$to)))
.vertices <- data.frame(
  c_personid = .name
) %>%
  left_join(df_BIOG_people_s) %>%
  select(c_personid, c_name_chn, c_birthyear, c_female) %>%
  rename(name = c_personid)

flareGraph_xiao <- graph_from_data_frame(.edges, vertices = .vertices)

ggraph(flareGraph_xiao
       #, layout = 'kk'
       ,'igraph', algorithm = 'tree'
       #, circular = TRUE
) + 
  geom_edge_diagonal(aes(color = as.factor(c_female), linetype = as.factor(type)), alpha = .3) +
  geom_node_point(aes(color = as.factor(c_female)), size = 1.5, alpha = .5)+
  #coord_fixed() + 
  geom_node_text(aes(label = c_name_chn
                     , filter = (c_female == 1 & !grepl('氏', c_name_chn))
                     #, filter = degree > 20
                     ), color = 'black',
                 size = 4) +
  scale_edge_color_manual(name = '性别' , 
                          values = c("0" = "#6187B6", "1" = "#F16E67"),
                          labels = c('男性', '女性')) +
  scale_color_manual(name = '性别' , 
                     values = c("0" = "#6187B6", "1" = "#F16E67"),
                     labels = c('男性', '女性')) +
  scale_edge_linetype_discrete(name = '关系类型',
                               labels = c('长晚辈关系', '婚姻关系')) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) +
  scale_y_reverse() 
  #ggforce::theme_no_axes()
ggsave('/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/plot/family_xiao.png',
       dpi = 300,
       height = 9, width = 12)
####################

## 王氏
load('/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/data/dt_wang_all.rda')

.edges <- dt_wang_all %>%
  select(c_personid, c_kin_id, type) %>%
  rename(from = c_personid,
         c_personid = c_kin_id) %>%
  left_join(df_BIOG_people_s) %>%
  select(from, c_personid, c_female, c_birthyear, type) %>%
  arrange(from, c_birthyear) %>%
  rename(to = c_personid)
.name <- unique(c(as.character(.edges$from), as.character(.edges$to)))
.vertices <- data.frame(
  c_personid = .name
) %>%
  left_join(df_BIOG_people_s) %>%
  select(c_personid, c_name_chn, c_birthyear, c_female) %>%
  rename(name = c_personid)

flareGraph_wang <- graph_from_data_frame(.edges, vertices = .vertices)
V(flareGraph_wang)$degree <- degree(flareGraph_wang)

ggraph(flareGraph_wang
       #, layout = 'kk'
       ,'igraph', algorithm = 'tree'
       #, circular = TRUE
) + 
  geom_edge_diagonal(aes(color = as.factor(c_female), linetype = as.factor(type)), alpha = .7, strength = 1, edge_width = 5) +
  geom_node_point(aes(color = as.factor(c_female)), size = 13)+
  #coord_fixed() + 
  #geom_node_text(aes(label = c_name_chn
                     #, filter = c_female == 1
                     #, filter = degree > 20
  #), color = 'white',
  #size = 15) +
  scale_edge_color_manual(name = '性别' , 
                          values = c("0" = "#6187B6", "1" = "#F16E67"),
                          labels = c('男性', '女性')) +
  scale_color_manual(name = '性别' , 
                     values = c("0" = "#6187B6", "1" = "#F16E67"),
                     labels = c('男性', '女性')) +
  scale_edge_linetype_discrete(name = '关系类型',
                               labels = c('长晚辈关系', '婚姻关系')) +
  theme(legend.position="none") +
  scale_y_reverse() 
ggsave('/Users/sousekilyu/Documents/R/CBDB_Women/plot/family_wang.png',
       dpi = 300,
       height = 100, width = 15, limitsize = FALSE)



## 瑯琊王氏
dt_llw <- getkin2DF(16682)
flareGraph_llw <- getdt2GRAPH(dt_llw)
V(flareGraph_llw)$degree <- degree(flareGraph_llw)

ggraph(flareGraph_llw
       ,'igraph', algorithm = 'tree'
       #, layout = 'kk'
       ) + 
  geom_edge_diagonal(aes(color = as.factor(c_female), linetype = as.factor(type)), alpha = .3) +
  geom_node_point(aes(color = as.factor(c_female)), size = 1.5, alpha = .5)+
  geom_node_text(aes(label = c_name_chn
                     #, filter = c_female == 1
                     #, filter = degree > 20
  ), color = 'black',
  size = 3) +
  scale_edge_color_manual(name = '性别' , 
                          values = c("0" = "#6187B6", "1" = "#F16E67"),
                          labels = c('男性', '女性')) +
  scale_color_manual(name = '性别' , 
                     values = c("0" = "#6187B6", "1" = "#F16E67"),
                     labels = c('男性', '女性')) +
  scale_edge_linetype_discrete(name = '关系类型',
                               labels = c('长晚辈关系', '婚姻关系')) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) +
  scale_y_reverse() 

ggsave('/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/plot/family_wang.png',
       dpi = 300,
       height = 9, width = 12)


## 瑯琊王氏
dt_zhu <- getkin2DF(9001)
flareGraph_zhu <- getdt2GRAPH(dt_zhu)
V(flareGraph_zhu)$degree <- degree(flareGraph_zhu)

ggraph(flareGraph_zhu
       #,'igraph', algorithm = 'tree'
       , layout = 'kk'
) + 
  geom_edge_diagonal(aes(color = as.factor(c_female), linetype = as.factor(type)), alpha = .3) +
  geom_node_point(aes(color = as.factor(c_female)), size = 1.5, alpha = .5)+
  geom_node_text(aes(label = c_name_chn
                     , filter = c_name_chn == '趙匡胤'
                     #, filter = degree > 20
  ), color = 'black',
  size = 5) +
  scale_edge_color_manual(name = '性别' , 
                          values = c("0" = "#6187B6", "1" = "#F16E67"),
                          labels = c('男性', '女性')) +
  scale_color_manual(name = '性别' , 
                     values = c("0" = "#6187B6", "1" = "#F16E67"),
                     labels = c('男性', '女性')) +
  scale_edge_linetype_discrete(name = '关系类型',
                               labels = c('长晚辈关系', '婚姻关系')) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) +
  scale_y_reverse() 

ggsave('/Users/sousekilyu/Library/Mobile Documents/com~apple~CloudDocs/R/Women/plot/dynasty_song.png',
       dpi = 300,
       height = 9, width = 12)


### 朱元璋
.edges <- df_zhu_all %>%
  select(c_personid, c_kin_id, type) %>%
  rename(from = c_personid,
         c_personid = c_kin_id) %>%
  left_join(df_BIOG_people_s) %>%
  select(from, c_personid, c_female, c_birthyear, type) %>%
  arrange(from, c_birthyear) %>%
  rename(to = c_personid)
.name <- unique(c(as.character(.edges$from), as.character(.edges$to)))
.vertices <- data.frame(
  c_personid = .name
) %>%
  left_join(df_BIOG_people_s) %>%
  select(c_personid, c_name_chn, c_birthyear, c_female) %>%
  rename(name = c_personid)

flareGraph_zhu <- graph_from_data_frame(.edges, vertices = .vertices)
V(flareGraph_zhu)$degree <- degree(flareGraph_zhu)

ggraph(flareGraph_zhu
       #, layout = 'kk'  #网状图
       ,'igraph', algorithm = 'tree' #树状图
       #, circular = TRUE
) + 
  geom_edge_diagonal(aes(color = as.factor(c_female), linetype = as.factor(type)), alpha = .3) +
  geom_node_point(aes(color = as.factor(c_female)), size = 1.5, alpha = .5)+
  #coord_fixed() + 
  # geom_node_text(aes(label = c_name_chn
  #                    #, filter = c_name_chn %in% c('李世民（唐太宗）', '武曌（武則天）')
  #                    , filter = c_female == 1
  #                    ), color = 'black',
  #                size = 1) +
  scale_edge_color_manual(name = '性别' , 
                          values = c("0" = "#6187B6", "1" = "#F16E67"),
                          labels = c('男性', '女性')) +
  scale_color_manual(name = '性别' , 
                     values = c("0" = "#6187B6", "1" = "#F16E67"),
                     labels = c('男性', '女性')) +
  scale_edge_linetype_discrete(name = '关系类型',
                               labels = c('长晚辈关系', '婚姻关系')) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
#ggforce::theme_no_axes()
ggsave('/Users/sousekilyu/Documents/R/CBDB_Women/plot/song_net.png',
       dpi = 300,
       height = 9, width = 12)
