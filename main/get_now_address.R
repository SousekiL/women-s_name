# 获取现代地名
get_new_addr = function(i)  {
  key = '33f617d7ea3f2ca315ac23c0fc9415ec'
  location = i                                   
  url = str_c('https://restapi.amap.com/v3/geocode/regeo?key=',
              key,
              '&location=',location,
              '&poitype=&radius=1000&extensions=base&batch=true&batch=false&roadlevel=1')         # 将参数拼接入API
  data = read_html(url, encoding='utf-8') %>% html_text()
  df = fromJSON(data)
  return(df)
}

null2na = function(i) {
  lapply(i, function(x) if(identical(x, character(0))) NA_character_ else x)
}

df_ADDRESSES$x_coord[df_ADDRESSES$x_coord == 36.44774] <- 115.98869
df_ADDRESSES$y_coord[df_ADDRESSES$y_coord == 115.98869] <- 36.44774

df_ADDRESSES$x_addr <- paste0(df_ADDRESSES$x_coord,',',df_ADDRESSES$y_coord)
x_addr_list <- unique(df_ADDRESSES$x_addr)
x_addr_list <- x_addr_list[x_addr_list != "NA,NA"]
x_addr_list <- x_addr_list[x_addr_list != "0,0"]

test_df <- data.frame()
for (i in seq(4560,length(x_addr_list),19)) {
  
  x_addr_list_s <- x_addr_list[i:(i+18)]
  x_addr_list_s <- x_addr_list_s[!is.na(x_addr_list_s)]
  x_addr_list_s <- c("116.111725,43.657055", x_addr_list_s)
  loc_list <- paste(x_addr_list_s, collapse = '|')
  
  df_addr <- get_new_addr(loc_list)
  
  test_df_tmp <- data.frame(
    x_addr = x_addr_list_s,
    n_province = unlist(null2na(df_addr$regeocode$addressComponent$province)),
    n_city = unlist(null2na(df_addr$regeocode$addressComponent$city)),
    n_district = unlist(null2na(df_addr$regeocode$addressComponent$district))
  )
  test_df <- rbind(test_df, test_df_tmp)
  print(i)
}
test_df <- unique(test_df)

df_ADDRESSES <- left_join(df_ADDRESSES, test_df, by = c("x_addr" = "x_addr"))
