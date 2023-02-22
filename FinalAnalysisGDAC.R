require(readr)

#Read in data

cleandata <- read_csv("dfcleaned.csv")

cleandata1 <- cleandata[!(cleandata$keywords %in% c("auditions", "bait","betty","bigpapi","blizzard","bonus","bts","cheri","comeback","dontsneeze","dreamcometrue","emotional","gambling","gotme","gazette","howyoudoin","howyoudoing","indie","indy","jesse","lucas","realorfake","passion","stare","bonus","young","thankyou","vibes","tax","space","friendship","100","lvii","experts","lgbtq","award","website","tasty","chewy")), ]

#Group By Brand

brand_sums <- aggregate(cbind(cleandata1$retweet_count, cleandata1$reply_count, cleandata1$like_count, cleandata1$quote_count), by = list(cleandata1$brand), FUN = sum)

colnames(brand_sums) <- c("brand","retweet_sum", "reply_sum", "like_sum", "quote_sum")

print(brand_sums)

#Clean Brand Names 
brand_sums$brand <- sub("Paramount Plus: Stallone faces off", "Paramount Plus: Stallone Faces Off", brand_sums$brand)
brand_sums$brand <- sub("MARVEL- Guardians Galaxy", "MARVEL: Guardians Galaxy", brand_sums$brand)
brand_sums$brand <- sub("DraftKings: Kevin Hart-Free bet", "DraftKings: Kevin Hart - Free Bet", brand_sums$brand)
brand_sums$brand <- sub("Turbo Tax live2023", "TurboTax Live 2023", brand_sums$brand)
brand_sums$brand <- sub("Tubi", "Tubi: Who Touched the Remote!", brand_sums$brand)

#Scoring Model 1
#SUM

brand_sums$score1 <- rowSums(brand_sums[,c('retweet_sum','reply_sum','like_sum','quote_sum')])
brand_sums$score1 <- ifelse(is.na(brand_sums$score1), 0, brand_sums$score1)

#Scoring Model 2
#Weighted

brand_sums$score2 <- .40*(brand_sums$quote_sum) + .30*(brand_sums$reply_sum) + .20*(brand_sums$retweet_sum) + .10*(brand_sums$like_sum)
brand_sums$rank1 <- round(rank(-brand_sums$score2))

#Scoring Model 3
#Weighted by Survey Results

brand_sums$score3 <- .27*(brand_sums$quote_sum) + .25*(brand_sums$reply_sum) + .28*(brand_sums$retweet_sum) + .20*(brand_sums$like_sum)
brand_sums$final_rank <- round(rank(-brand_sums$score3))

#brand_sums <- brand_sums[-9, ]

top50 <- brand_sums[brand_sums$rank<=50,]
top10 <- brand_sums[brand_sums$rank<=10,]

#Percent difference for Top 50

first_val <- brand_sums$score3[brand_sums$final_rank==1]

brand_sums$pd <- ((brand_sums$score3 / first_val)-1)*100

first_value <- top50$score3[top50$final_rank==1]

top50$pd <- abs(((top50$score3 / first_value)-1)*100)

top50 <- top50[order(top50$final_rank),]


#Percent difference for Top 10

first_val <- brand_sums$score3[brand_sums$final_rank==1]

brand_sums$pd <- ((brand_sums$score3 / first_val)-1)*100

first_value <- top10$score3[top10$final_rank==1]

top10$pd <- abs(((top10$score3 / first_value)-1)*100)

top10 <- top10[order(top10$final_rank),]

#Change to % datatype
top50$pd <- sprintf("%.2f%%", top50$pd)
top10$pd <- sprintf("%.2f%%", top10$pd)

#Final Datasets
write.csv(top10,"top10_updated.csv", row.names = FALSE)
write.csv(top50,"top50_updated.csv", row.names = FALSE)

