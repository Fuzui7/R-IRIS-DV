# 鳶尾花資料集的視覺化
data(iris)
attributes(iris)
str(iris)
summary(iris)

attach(iris)
var = c(1:4)
# 算平均前，移除第5欄-花種，(因為花的"種類"不能拿來算平均)

colMeans(iris[,-5])

# correlation 相關性分析 cor(A,B)
# 看花萼長 與 花萼寬 有無正相關
cor(Sepal.Length, Sepal.Width)
# 花萼長與花萼寬 無正相關

#看花瓣長與花瓣寬 有無正相關
cor(Petal.Length, Petal.Width)
# 花瓣長與花瓣寬有高度正相關，達0.962

# pairwise成對比較
corr = cor(iris[var], use = "pairwise")
corr

# covariance 共異變數
cov(Petal.Length, Petal.Width)
covv = cov(iris[var], use = "pairwise")
covv

# Display various types of plots
plot(iris) #scatter matrix (散佈圖矩陣)
plot(Sepal.Length, Sepal.Width) #scatter plot (散佈圖)
plot(Petal.Length, Petal.Width)
# 散佈圖說明花瓣長寬正相關，與上面相關性分析一樣
plot(Species, Sepal.Length, main = "Distribution of Sepal.Length")

# 製作長條圖
num_species = with(iris, c(sum(iris[,5] == "setosa"), sum(iris[,5] == "versicolor"), sum(iris[,5] == "virginica")))
barplot(num_species, names.arg = c("setosa","versicolor","virginica"), xlab="species", ylab="number") 
# 製作圓餅圖 (pie chart)
pie(num_species, labels = c("setosa","versicolor","virginica")) #pie chart (圓餅圖)
pie(num_species, labels = c(sum(iris[,5] == "setosa"), sum(iris[,5] == "versicolor"), sum(iris[,5] == "virginica")))

# $ 符號 用作選取特定變數，這裡選取iris資料集當中"花的種類"
iris$Species
levels(iris$Species)

# 百分比+圓餅圖+種類敘述，使圖更直觀
# 從圖可知Setosa,Virginica,Versicolor各占1/3
percent = round(num_species/sum(num_species)*100)
label = paste(levels(iris$Species), percent,"%")
pie(num_species, label)

# 製作箱型圖boxplot
# 盒狀圖很重要，因為可以看出資料中的Outliers(離群值)

# 變數1~4的箱型圖
x = boxplot(iris[, 1:4], main="Three species") 
x$stats

# setosa
y = which(iris[, 5] == "setosa")
boxplot(iris[y, 1:4], main="setosa")
# versicolor
z = which(iris[, 5] == "versicolor")
boxplot(iris[z, 1:4], main="versicolor")
# virginica
xx = which(iris[, 5] == "virginica")
boxplot(iris[xx, 1:4], main = "virginica")

# 將圖做得更好看
# 補充:pch(plot character)，其對應符號0~B
plot(Sepal.Length[Species == "setosa"], Petal.Length[Species == "setosa"], pch = 1, col = "blue",
     xlim = c(3,8), ylim = c(0,9), 
     main = "scatter plot", xlab="SepalLen", ylab="PetalLen")
points(Sepal.Length[Species == "versicolor"], Petal.Length[Species == "versicolor"], pch = 2, col = "red")
points(Sepal.Length[Species == "virginica"], Petal.Length[Species == "virginica"], pch = 3, col = "green")
# 注標legend
legend(3, 9, c("setosa","versicolor","virginica"), col = c(1,2,3), pch = c(1,2,3))