load("userlife0712.RData")
head(test)

#产生测试数据，并预览
test.ordered <- test[order(test$id,test$time),]
test.ordered[1:100,]
to <- data.frame(test.ordered)
to <- data.frame(date = to$date, id = to$id, time = to$time)
to[c(1:10,1000:1010,5000:5010,10000:10010,20000:20010),]

#产生first last 标签，判断两次行为的时间差额，与600秒比较，大于600则为开端或者结尾。
to$first <- c(600,to$time[2:length(to$time)]-to$time[1:(length(to$time)-1)])
to$last <- c(to$time[2:length(to$time)]-to$time[1:(length(to$time)-1)], 600)
#产生prior following 标签，0代表该条记录与上／下一条记录玩家相同，大于0代表该条记录必为会话开端或结尾。
to$prior <- c(1,to$id[2:length(to$time)]-to$id[1:(length(to$time)-1)])
to$follow <- c(to$id[2:length(to$time)]-to$id[1:(length(to$time)-1)], 1)

#筛选：prior大于0，或者first大于600的为开端；follow大于0或者last大于600的为结尾。
to.beginning <- subset(to, subset = (to$prior >= 1 |to$first > 600))
to.end <- subset(to, subset = (to$follow >= 1 |to$last > 600))


#检验通过后，生成userlife表格。
{if ((length(to.beginning) == length(to.end)) & (to.beginning$date == to.end$date) & (to.beginning$id == to.end$id))  
  to.userlife <- data.frame(date = to.beginning$date, user_id = to.beginning$id, first = substr(to.beginning$time,11,20), last = substr(to.end$time,11,20)) 
else 
  print("头尾个数不一致或者某条信息互相不对应")}
#添加序号position函数，输入0-1向量，比如to$prior，返回形如123112345612111231111234的向量。
to.userlife$position <- c(1:length(to.userlife))  
position = function(x)
{
  p <- rep(0,length(x))
  for (i in 1:length(x))  
  {if (x[i] == 1) p[i] <- 1 else p[i] <- p[i-1]+1}
  return (p)
}
  