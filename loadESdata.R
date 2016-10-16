#调用前需要加载R包“elastic”。命令如：install.packages("elastic"); library(elastic)
#首先连接ssh -NL 19300:10.144.211.244:9200 root@115.28.191.120，然后执行下面命令
elastic::connect(es_base = "http://localhost", es_port = 19200); 

# 利用bool must 来连接两个queries
aggs <- '{"query":{"bool":{"must":[{"match":{"来源":"客户端"}} , {"range":{"doc_timestamp":{"from":"2015-07-03T00:00:00Z","to":"2015-07-03T23:59:59Z"}}}]}}}'
a0703 <- elastic::Search(index = "sevenga.ironcommander", body = aggs, size = 99999999)$hits$hits

q_bodys <- list(
  '{"query":{"bool":{"must":[{"match":{"来源":"客户端"}} , {"range":{"doc_timestamp":{"from":"2015-07-02T00:00:00Z","to":"2015-07-02T23:59:59Z"}}}]}}}',
  '{"query":{"bool":{"must":[{"match":{"来源":"客户端"}} , {"range":{"doc_timestamp":{"from":"2015-07-03T00:00:00Z","to":"2015-07-03T23:59:59Z"}}}]}}}')

ess <- lapply(q_bodys, FUN = function(x) elastic::Search(index = "sevenga.ironcommander", body = x, size = 999999999)$hits$hits)
dfs <- lapply(ess, es2df)
names(dfs) <- paste("a070", 2:7,sep = "")

################################################################################################################################################################################################
# bool query：（来源字段的值是客户端 OR 日志类型登陆游戏，退出游戏，充值获得金币，消耗道具，购买道具，派遣部队出征） AND （日期范围是list（某天））

## 首先满足了日志类型和来源的query

aggs <- '{"query":{"bool":{"must":{"range":{"doc_timestamp":{"from":"2015-07-01T00:00:00Z","to":"2015-07-31T23:59:59Z"}}},"should":[{"match":{"_type":"登陆游戏"}},{"match":{"_type":"退出游戏"}},{"match":{"_type":"充值获得金币"}},{"match":{"_type":"消耗道具"}},{"match":{"_type":"购买道具"}},{"match":{"_type":"派遣部队出征"}},{"match":{"来源":"客户端"}}], "minimum_should_match" : 1}}}'
Sys.time()
action.2015.07 <- elastic::Search(index = "sevenga.ironcommander", fields = c("user_id", "log_type", "doc_timestamp", "date_timestamp"), body = aggs, size = 99999999)$hits$hits
Sys.time()
save(action.2015.07, file = "action.2015.07.RData")

# 从月度数据拆分出每日数据
Sys.time()
for (i in 17:18){
  date <- as.Date(paste("2015-07-",i,sep = ""))
  esdate <- paste(date, "T00:00:00.000Z",sep = "")
  l <- length(action.2015.07) # l是7月所有记录的总条数
  name <- paste("action.2015.07.", i, sep = "")
  assign(name, lapply(c(1:l), FUN = function (x) subset(action.2015.07, action.2015.07[[x]]$fields$date_timestamp[[1]] == esdate)))
  save(temp, file = as.character(date)) 
}
Sys.time()


#试图将aggs分成多块，总是报错：
# must <- "{"range":{"doc_timestamp":{"from":"2015-07-03T00:00:00Z","to":"2015-07-03T23:59:59Z"}}}"
# should <- "[{"match":{"_type":"登陆游戏"}},{"match":{"_type":"退出游戏"}},{"match":{"_type":"充值获得金币"}},{"match":{"_type":"消耗道具"}},{"match":{"_type":"购买道具"}},{"match":{"_type":"派遣部队出征"}},{"match":{"来源":"客户端"}}]"
# aggs <- '{"query":{"bool":{"must":must,"should":should,"minimum_should_match" : 1}}}'
# a0703 <- elastic::Search(index = "sevenga.ironcommander", body = aggs, size = 50)$hits$hits

##然后尝试日期一次性查询多日：
#生成七月每天的起讫日期
dates <- as.Date(paste("2015-07-",c(1:31),sep = ""))
froms <- as.list(paste(dates, "T00:00:00Z",sep = ""))
tos <- as.list(paste(dates, "T23:59:59Z",sep = ""))
#doc_timestamps <- as.list(paste(""from":"))




