#导入数据;
setwd("E:/览云精微实习/top10用户行为全记录")
u1106204 <- read.csv("1106204-行为全记录.csv", header = TRUE, as.is = TRUE)

#整理时间戳;
u1 <- subset(u1106204, select = c(doc_timestamp, log_type, user_id) )
head(u1)
u1$doc_timestamp.new <- paste(substr(u1$doc_timestamp, 1,10), substr(u1$doc_timestamp, 12,19));
u1$tz <- substr(u1$doc_timestamp, 20, 25)
u1$doc_timestamp.new <- strptime(u1$doc_timestamp.new[1:length(u1$doc_timestamp.new)], "%Y-%m-%d %H:%M:%S")
class(u1$doc_timestamp.new)

#按照时间排序;
u1 <- u1[order(u1$doc_timestamp.new),]
#观察用户行为类型分布，去掉异常;
table(u1$log_type)
names(table (u1$log_type)[1])
u1.1 <- subset(u1, u1$log_type != "")

#############################################################################
#导入数据;
setwd("E:/览云精微实习/top10用户行为全记录")
u1108397 <- read.csv("1108397-行为全记录.csv", header = TRUE, as.is = TRUE)

#整理时间戳;
u2 <- subset(u1108397, select = c(doc_timestamp, log_type, user_id) )
head(u2)
u2$doc_timestamp.new <- paste(substr(u2$doc_timestamp, 1,10), substr(u2$doc_timestamp, 12,19));
u2$tz <- substr(u2$doc_timestamp, 20, 25)
u2$doc_timestamp.new <- strptime(u2$doc_timestamp.new[1:length(u2$doc_timestamp.new)], "%Y-%m-%d %H:%M:%S")
class(u2$doc_timestamp.new)

#按照时间排序;
u2 <- u2[order(u1$doc_timestamp.new),]
#观察用户行为类型分布，去掉异常;
table(u2$log_type)
names(table (u2$log_type)[1])
u2.1 <- subset(u2, u2$log_type != "")

#####################################################################################
#把行为记录转化为cSPADE函数的输入格式：transaction格式
install.packages("arules") #加载转换为transaction所需要的包;
library(arules)
############################################  method 1 ##################################################################
u1.1$log_type <- as.factor(u1.1$log_type)
u1.df <- data.frame(item = u1.1$log_type)
u1.tr <- as(u1.df, "transactions")
transactionInfo(u1.tr)$sequenceID <- u1.1$user_id
transactionInfo(u1.tr)$eventID <- u1.1$doc_timestamp.new
inspect(u1.tr[1:10])

#应用cSPADE函数，挖掘序列中的频繁模式;
install.packages("arulesSequence")
library(arulesSequences)
u1.sp <- cspade(u1.tr)

#############################################  method 2  #########################################################

u1.df <- data.frame(sid = u1.1$user_id, eid = u1.1$doc_timestamp.new, item = u1.1$log_type)
u1.df$sid <- as.factor(u1.df$sid);
u1.df$eid <- as.factor(u1.df$eid)
u1.df$item <- as.factor(u1.df$item)
u1.tr <- as(u1.df, "transactions")
transactionInfo(u1.tr)$sequenceID <- u1.1$user_id
transactionInfo(u1.tr)$eventID <- u1.1$doc_timestamp.new
#应用cSPADE函数，挖掘序列中的频繁模式;
install.packages("arulesSequence")
library(arulesSequences)
cspade(u1.tr)

############################################# method 3  ########################################

u <- rbind(u1.1, u2.1)
u$log_type <- as.factor(u$log_type)
transactionInfo(u.tr)$sequenceID <- u$user_id
transactionInfo(u.tr)$eventID <- u$doc_timestamp.new
u.sp <- cspade(u.tr)
inspect(head(u.tr))
inspect(tail(u.tr))

################################## method 4 用户抽离，日期作为sid,时间作为eid ########################################

u1.1$date <- as.Date(u1.1$doc_timestamp.new, "%Y-%m-%d")
u1.1$time <- substr(u1.1$doc_timestamp.new, 12, 20)
u1.df <- data.frame(item = u1.1$log_type)
u1.tr <- as(u1.df, "transactions")
transactionInfo(u1.tr)$sequenceID <- u1.1$date
transactionInfo(u1.tr)$eventID <- rank(u1.1$time)
u1.sp <- cspade(u1.tr[1:50])
