install.packages("elastic")
elastic::connect(es_base = "http://localhost", es_port = 19300);

#读取数据
a0705 <- elastic::Search(index = "sevenga.ironcommander", type = "登陆游戏", 
                     body = '{"query":{"range":{"doc_timestamp":{"from":"2015-07-05T00:00:00Z","to":"2015-07-05T23:59:59Z"}}}}', size = 99999999)$hits$hits;

#new method 用lapply匿名函数处理循环赋值问题；
newts <- lapply(a, function(x) x$fields$doc_timestamp[[1]]);
newid <- lapply(a, function(x) x$fields$'#user_id'[[1]]);
#检验数据好坏，剔除坏数据
factor <- nchar(newid)
table(factor)
newts <- newts[-which(nchar(newid)==4)]#必须先筛选newts再筛选newid，保证which下标有效。
newid <- newid[-which(nchar(newid)==4)]

##时间戳改造成日期和时间格式,转化成数据框
newid <- do.call(c, newid)
newts <- do.call(c, newts)#然而用newts <- c(newts)并没有起到转换的作用
result <- data.frame(id = newid, time = newts); 
result$time <- paste(substr(result$time, 1,10), substr(result$time, 12,19));
result$time <- as.POSIXlt(result$time, format = "%Y-%m-%d %H:%M:%S")
result$date <- as.Date(substr(newts,1,10))
head(result)
#按照user_id排序，id内部按照时间排序
result.ordered <- result[order(result$id,result$time),]
result.ordered[c(1:10,3000:3010,400000:400010),]

test <- result[which(result$date=="2015-07-12")]
head(test)
save(test, "userlife0712.RData")

#############################################以下是尝试历史，上面是当前有效代码##################################################################################
#use "scroll" to read pages;
aa <- elastic::Search(index = "sevenga.ironcommander", type = "登陆游戏", fields = c("#user_id","doc_timestamp"), 
                      size = 50, scroll = "1m")$hits$hits;
elastic::scroll(scroll_id = aa$'_scroll_id');
out <- list();
hits <- 1;
while(hits != 0){
  aa <- scroll(elastic::scroll_id = aa$'_scroll_id')
  hits <- length(aa$hits$hits)
  if(hits > 0)
    out <- c(out, aa$hits$hits)
}

#用循环把每个list中所需要的结果存储到一个数据框中。
#Error in id[i] = a[[i]]$fields$`#user_id`[[1]] : 更换参数长度为零
l <- length(a);
time=numeric(l);
id=numeric(l);
for(i in 1:l){
  id[i]=a[[i]]$fields$`#user_id`[[1]]
  time[i]=a[[i]]$fields$doc_timestamp[[1]] 
}
result=data.frame(id=id,time=time);
result[1:10]

#elastic::count(index = "sevenga.ironcommander", type = "player_es")
elastic::Search(index = "sevenga.ironcommander", type = "player_es", q = "user_id:1054209");

l <- length(a);
newts <- sapply(1:l, function(i) a$fields$doc_timestamp[[i]],simplify = TRUE);
newid <- sapply(1:l, function(i) a$fields$'#user_id'[[i]])