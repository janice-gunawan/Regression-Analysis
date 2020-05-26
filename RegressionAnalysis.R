setwd("/Users/janicegunawan/Desktop/Marketing Analytics")
oj <- read.csv("oj.csv")
head(oj)
dim(oj)
summary(oj)
View(oj)

#drop the brand “minute.maid” from the data, name the new data “DT”
DT=oj[oj$brand != "minute.maid", ]

DT$trop <- cbind(ifelse(DT$brand == "tropicana",1,0))

View(DT)
summary(DT)

#average sales when trop=0 and trop=1
aggregate(sales~trop, data = DT, mean)

#average price when trop=0 and trop=1
aggregate(price~trop, data = DT, mean)

#% of time the product is featured when trop=0 and trop=1
aggregate(feat~trop, data = DT, mean)

#Regression (1) Sales = 𝛽0 +𝛽1 ⋅𝑡𝑟𝑜𝑝 + 𝑅𝑎𝑛𝑑𝑜𝑚 𝑁𝑜𝑖𝑠𝑒 
reg1 = lm(sales ~ trop, data = DT)
summary(reg1)

#Regression (2) Sales = 𝛽0 +𝛽1 ⋅𝑡𝑟𝑜𝑝 + 𝛽2 ⋅𝑃𝑟𝑖𝑐𝑒 + 𝑅𝑎𝑛𝑑𝑜𝑚 𝑁𝑜𝑖𝑠𝑒
reg2 = lm(sales ~ trop + price, data = DT)
summary(reg2)

plot(DT$price,DT$sales, main = "Price and sales", xlab = "Price", ylab = "Sales")
abline(lm(sales~price, data = DT), col = "blue")

#check price sensitivity for dominicks & tropicana
summary(lm(sales ~ price, data = DT[DT$trop==0,]))
summary(lm(sales ~ price, data = DT[DT$trop==1,]))

#Non-linear Regression Models
DT$trop_price = DT$trop * DT$price
reg3 = lm(sales ~ trop + price + trop_price, data = DT)
summary(reg3)
