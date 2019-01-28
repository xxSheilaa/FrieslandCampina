# merge all codes with products
Campina.SKu <- merge(code.2018.sample,UPPC.sku,by = "sku_id")
Campina.SKu <- Campina.SKu[,c("sku_id", "accountid","brand_id","prodgroups_id","category_id")]
head(Campina.SKu)
                         
freq.campina <- aggregate(accountid ~ category_id,data = Campina.SKu, FUN= length)
freq.campina <- merge(freq.campina,UPPC.categories, by = "category_id")


freq.campina1 <- aggregate(accountid ~ brand_id,data = Campina.SKu, FUN= length)
freq.campina1 <- merge(freq.campina1,UPPC.brands, by = "brand_id")
