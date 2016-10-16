#Config:

# 说明：函数df2uslf用于把数据框格式的用户id和行为时间，生成userlife数据框。
#       输入：数据框 列名包括uid(user_id), ts(doc_timestamp), date；输出：数据框 列名包括date, user_id, position, first, last, duration
#       调用前需要加载R包“assertthat”。命令如：install.packages("assertthat") library(assertthat)


df2uslf <- function (to)
{
  #为每次行动添加时间差标签，与上一次行动的时间差为delta_time1，与下一次的时间差为delta_time2.大于600秒代表会话开端或结尾。
  l <- length(to$ts)
  delta_time <- to$ts[2:l]-to$ts[1:(l-1)]
  delta_time <-  delta_time >= 600
  to$delta_time1 <- c(TRUE,delta_time)
  to$delta_time2 <- c(delta_time, TRUE)
  
  #为每次行动添加用户差异标签，与上一次行动的用户有变与否为newuser，与下一次行动的用户有变与否为lastuser。0代表相同，大于0代表会话开端或结尾。
  user_diff <- to$uid[2:length(to$ts)]-to$uid[1:(length(to$ts)-1)]
  user_diff <- user_diff > 0
  to$newuser <- c(TRUE,user_diff)
  to$lastuser <- c(user_diff, TRUE)
  
  #筛选：newuser大于0，或者delta_time1大于600的为开端；lastuser大于0或者delta_time2大于600的为结尾。
  to.beginning <- subset(to, subset = (to$newuser == TRUE |to$delta_time1 == TRUE))
  to.end <- subset(to, subset = (to$lastuser == TRUE |to$delta_time2  == TRUE))
  
  #断言检验：每组开头和结尾的id是否对号,对号则可以生成用户会话时长。
  assert_that(identical(to.beginning$uid, to.end$uid))
  #assert_that(all.equal())
  duration <- to.end$ts-to.beginning$ts
  
  #添加序号position函数，输入元素为零或正整数的向量，比如to$prior，返回形如123123412的向量。
  # method2 非零元素下标错位相减，得到每段用户记录的长度，分别生成后连接。
#   position2 = function(x)
#   {
#     w1 <- which(x != 0 )
#     w2 <- c(w1,length(x)+1)
#     p  <- unlist(lapply(w2[2:length(w2)]-w1, FUN = function(y) c(1:y)))
#     return (p)
#   }
  
  act <- function(x) {
    #if (x == TRUE) p =  1 ;
    #if (x == FALSE) p = act(which(x)-1) + 1;
    p <- x * 0 + 1;
    return p;
  }
  
  position = function(x)
  {
    p <- rep(0,length(x))
    for (i in 1:length(x))  
    {if (x[i] == TRUE) p[i] <- 1 else p[i] <- p[i-1]+1}
    return (p)
  }
  
  position3 = function(x)
  {
    i = 1
    p <- rep(0,length(x))
    foreach (i = 1:length(x))  %do%
    {if (x[i] == TRUE) p[i] <- 1 else p[i] <- p[i-1]+1}
    return (p)
  }
  
  #生成userlife表格。
  userlife <- data.frame(date = to.beginning$date, user_id = to.beginning$uid, position = position2(to.beginning$newuser), first = substr(to.beginning$ts, 11, 20), last = substr(to.end$ts, 11, 20), duration = duration)
  return (userlife)
}
