totals <- sqldf("select from_name, sum(likes_count) as likes, sum(comments_count) as comments,
                sum(shares_count) as shares, sum(love_count) as love, sum(haha_count) as haha,
                sum(wow_count) as wow, sum(sad_count) as sad, sum(angry_count) as angry
                from result_data group by from_name")


totalsbytype <- sqldf("select type, sum(likes_count) as likes, sum(comments_count) as comments,
                      sum(shares_count) as shares, sum(love_count) as love, sum(haha_count) as haha,
                      sum(wow_count) as wow, sum(sad_count) as sad, sum(angry_count) as angry
                      from result_data group by type")


totalsbyweekday <- sqldf("select weekday, sum(likes_count) as likes, sum(comments_count) as comments,
                         sum(shares_count) as shares, sum(love_count) as love, sum(haha_count) as haha,
                         sum(wow_count) as wow, sum(sad_count) as sad, sum(angry_count) as angry
                         from result_data group by weekday order by weekdayName")

totalsbyday <- sqldf("select day, weekday, sum(likes_count) as likes, sum(comments_count) as comments,
                     sum(shares_count) as shares, sum(love_count) as love, sum(haha_count) as haha,
                     sum(wow_count) as wow, sum(sad_count) as sad, sum(angry_count) as angry
                     from result_data group by day order by day")

totalsbypost<-sqldf("select count(id),day from result_data group by day order by day")