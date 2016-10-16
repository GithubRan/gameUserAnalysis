# dfs2dau函数用于输入多日的数据框列表，返回每日的dau_num
# 输入：多个数据框组成的列表 每个数据框代表一天的记录，列名包括 uid  date 
# 输出： 

# df2dau函数是dfs2dau的内置函数，用于输入单日的数据框列表，返回当日dau_num
# 输入：一个数据框 代表同一天的记录，列名包括 uid  date 
# 输出： 列表格式，列名分别为dau_num和dau_uid，分别返回日活跃用户总数和所有日活跃用户id。

dfs2dau <- function(x){
  
  df2dau <- function (y){
    #assert_that(一个数据框内的每条记录的date都是一致的)
    result <- list(dau_num = length(unique(y$uid)), dau_uid = unique(y$uid))
    return(result)
  }
  
  daunums <- unlist(lapply(x,  FUN = function(z) (df2dau(z))$dau_num))
  
  return(daunums)
  
}

#