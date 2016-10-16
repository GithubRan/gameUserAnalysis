#Config:

# 说明：函数es2df是格式转换函数，用于把从ElasticSearch中直接导入的数据转换为R软件中的数据框格式。
# 输入：a0702 a0705 之类的ES导入的列表套列表格式；输出：数据框 列名包括uid(user_id), ts(doc_timestamp), date, log_type
# 调用前需要加载R包“assertthat”。命令如：

install.packages("assertthat") 
library(assertthat)

es2df <- function (a)
{
  #从多层列表中提取时间戳和用户id。
  newts <- lapply(a, function(x) x$fields$doc_timestamp[[1]]);
  newid <- lapply(a, function(x) x$fields$`#user_id`[[1]]);
  log_type <- lapply(a, function(x) x$fields$log_type[[1]]);
  
  #检验数据好坏，剔除坏数据(必须先筛选newts再筛选newid，保证which下标有效)
#   if(all(nchar(newid) == 7)) 
#     {} 
#   else 
#   {
#     newts <- newts[-which(nchar(newid) != 7)];
#     newid <- newid[-which(nchar(newid) != 7)];
#   }
#   
  assert_that(all(nchar(newid) == 7))
  
  ##时间戳改造成日期和时间格式,转化成数据框
  newid <- do.call("c", newid)
  newts <- do.call("c", newts)
  log_type <- do.call("c", log_type)
  result <- data.frame(uid = newid, ts = newts, log_type = log_type); 
  result$ts <- paste(substr(result$ts, 1,10), substr(result$ts, 12,19));
  result$ts <- as.POSIXlt(result$ts, format = "%Y-%m-%d %H:%M:%S")
  result$date <- as.Date(substr(newts,1,10))
  
  #按照user_id排序，id内部按照时间排序
  result.ordered <- result[order(result$uid,result$ts),]
  return (result.ordered)
}

  