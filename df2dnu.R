# 输入：数据框格式 ,列名包含uid和date。若想得到准确的某日dnu，必须输入该日之前的所有天的记录。
# 输出：table格式，因子是日期，值是该日的dnu。

df2dnu <- function (x){
  
  dau <- unique(x$uid);
  
  # 排序
  dau <- dau[order(dau$uid, dau$date),];
  
  # 生成DNU
  uid <- dau$uid
  date <- dau$date
  uid1 <- c(0,uid[1:(length(uid)-1)]);
  tag <- uid - uid1;
  
  # 组成新的dataframe
  dau.tag <-cbind(dau, tag);
  
  # 得到46487次的DNU之和
  dnu.all <- subset(newdau, tag!=0, select=c(uid, date, tag));
  
  # 计算每天的DNU
  dnu.daily <- table(dnu$date)
  
  return(dnu.daily)
}


