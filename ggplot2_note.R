library(ggplot2)
set.seed(1410)
dsmall<-diamonds[sample(nrow(diamonds),100),]
dsmall

qplot(carat,price,data=diamonds)
qplot(log(carat),log(price),data=diamonds)

qplot(carat,price,data=dsmall,colour=color)
qplot(carat,price,data=dsmall,shape=cut)

qplot(carat,price,data=diamonds,alpha=I(1/10))
qplot(carat,price,data=diamonds,alpha=I(1/100))
qplot(carat,price,data=diamonds,alpha=I(1/200))

qplot(carat,price,data=dsmall,geom = c("point","smooth"))
qplot(carat,price,data=diamonds,geom = c("point","smooth"))

qplot(carat,price,data=dsmall,geom = c("point","smooth"),span=0.2)
qplot(carat,price,data=dsmall,geom = c("point","smooth"),span=1)

install.packages("mgcv")
library(mgcv)
qplot(carat,price,data = dsmall,geom=c("point","smooth"),method="gam",formula=y~s(x))
graphics.off()
qplot(carat,price,data = dsmall,geom=c("point","smooth"),method="gam",formula=y~s(x,bs="cs"))


library(splines)
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm")
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm",formula=y~ns(x,5))


qplot(color,price/carat,data=diamonds,geom = "jitter",alpha=I(1/5))
qplot(color,price/carat,data=diamonds,geom = "jitter",alpha=I(1/50))
qplot(color,price/carat,data=diamonds,geom = "jitter",alpha=I(1/200))


qplot(carat,data = diamonds,geom = "histogram")
qplot(carat,data = diamonds,geom = "density")


qplot(carat,data = diamonds,geom = "histogram",binwith=1,xlim = c(0,3))
qplot(carat,data = diamonds,geom = "histogram",binwith=0.1,xlim = c(0,3))
qplot(carat,data = diamonds,geom = "histogram",binwith=0.01,xlim = c(0,3))


qplot(carat,data = diamonds,geom = "density",colour=color)
qplot(carat,data=diamonds,geom = "histogram",fill=color)


qplot(color,data = diamonds,geom = "bar")
qplot(color,data = diamonds,geom = "bar",weight=carat)+scale_y_continuous("carat")


qplot(date,unemploy/pop,data=economics,geom = "line")
qplot(date,uempmed,data=economics,geom = "line")


#POSIXct:count time
#POSIXlt:list time
year<-function(x) as.POSIXlt(x)$year+1900
qplot(unemploy/pop,uempmed,data = economics,geom = c("point","path"))
qplot(unemploy/pop,uempmed,data = economics,geom = "path",colour=year(date))



qplot(carat,data = diamonds,facets = color~.,geom = "histogram",binwidth=0.1,xlim=c(0,3))
qplot(carat,..density..,data = diamonds,facets = color~.,geom = "histogram",binwidth=0.1,xlim=c(0,3))


qplot(
  carat,price,data=dsmall,
  xlab = "Price ($)",ylab = "Weight (carats)",
  main = "Price-weight relationship"
)

#frac()
qplot(
  carat,price/carat,data=dsmall,
  ylab = expression(frac(price,carat)),
  xlab = "Weight (carats)",
  main = "Small diamonds",
  xlim = c(.2,1)
)


#log="xy":log(x) log(y)
qplot(carat,price,data = dsmall,log = "xy")


#qplot()不是泛型泛型函数,ggplot()是



library(ggplot2)
qplot(displ,hwy,data = mpg,colour=factor(cyl))
qplot(displ,hwy,data = mpg,geom = c("point","smooth"),colour=factor(cyl))
graphics.off()



#facet panel
qplot(displ,hwy,data = mpg,facets = .~year)+geom_smooth()


# 1.Map variables to aesthetics
# 2.Facet datasets
# 3.Transform scales
# 4.Compute aesthetics
# 5.Train scales
# 6.Map cales
# 7.Render geoms


library(ggplot2)
p<-qplot(displ,hwy,data = mpg,colour=factor(cyl))
summary(p)

save(p,file="plot.rdata")
load("plot.rdata")
ggsave("plot.png",width = 5,height = 5)


#数据对象
p<-ggplot(diamonds,aes(carat,price,color=cut))
#图层
#layer(geom,geom_params,stat,stat_params,data,mapping,position)


p<-ggplot(diamonds,aes(x=carat))
###geom_params
p<-p+layer(
  geom="bar",
  params=list(fill="steelblue",binwidth=2),
  stat = "bin",
  position = "identity"
)


ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()
#===
graphics.off()
qplot(sleep_rem/sleep_total,awake,data = msleep)


qplot(sleep_rem/sleep_total,awake,data = msleep,geom = c("point","smooth"))
#===
ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()+geom_smooth()


p<-ggplot(msleep,aes(sleep_rem/sleep_total,awake))
summary(p)
p<-p+geom_point()
summary(p)

# 图层是普通的R对象，所以可以存储到变量里去，这有利于代码避繁就简。
# 例如，一组图形可以先用不同的数据来进行初始化，然后加上相同的图层，如果后面想改变图层，只需要修改一个地方即可。


p<-ggplot(mtcars,aes(mpg,wt,color=cyl))+geom_point()
p
mtcars<-transform(mtcars,mpg=mpg^2)
#用%+%来添加新的数据集以替代原来的数据集
p %+% mtcars
p


 
# 在更改数据集时，可以任意改变它的值和维度，但是如果将一个变量从离散型变成连续型或者从连续性变成离散型，那么也需要改变相应的默认标度。
# 在不使用分面的时候不必设定默认的数据集；分面是一个全局操作（作用于所有的图层），并且它需要一个定义了分面变量的默认数据集。如果没有给定默认的数据集，那么每个图层都要设定自己的数据集。
# 
# 数据是以副本而不是引用的形式存储到图形对象中的。
# 这样做有两个重要的好处：
# 1.如果数据改变了，绘图不会改变
# 2.ggplot2的对象都是自含型的，它们可以被存储（save()）到磁盘上，并且之后可以被直接加载（load()）运行

 
# aes()函数用来将数据变量映射到图形中，从而使变量成为可以被感知的图形属性
# aes(x=weight,y=height,colour=age)
# aes(x=weight,y=height,coulour=sqrt(age))
# 
# 每个aes()函数里额变量都必须包含于默认数据集或者图层数据集中，这是保证ggplot2对象都是自含型的重要方式之一，这样方便存储和重复使用。


p<-ggplot(mtcars)
summary(p)

p<-p+aes(wt,hp)
summary(p)


p<-ggplot(mtcars,aes(x=mpg,y=wt))
p+geom_point()

p+geom_point(aes(colour=factor(cyl)))
p+geom_point(aes(y=disp))



p<-ggplot(mtcars,aes(mpg,wt))
p+geom_point(colour="darkblue")
#!==
p+geom_point(aes(colour="darkblue"))


library(nlme)
p<-ggplot(Oxboys,aes(age,height,group=Subject))+geom_line()
graphics.off()
#已经将第一幅图存储到变量p中，可以在此基础上添加第二个图层，而不需要重新输入第一个图层的代码
p+geom_smooth(aes(group=Subject),method = "lm",se=F)



boysbox<-ggplot(Oxboys,aes(Occasion,height))+geom_boxplot()
graphics.off()
boysbox+geom_line(aes(group=Subject),colour="#3366FF")


ggplot(diamonds,aes(carat))+
  geom_histogram(aes(y=..density..),binwidth = 0.1)

#生成变量的名字必须要用..围起来。这样可以防止原数据集中的变量和生成变量重名时造成混淆，并且以后处理代码时，可以清晰的分辨出哪些变量是由统计变换生成的。
graphics.off()
#===
qplot(carat,..density..,data = diamonds,geom = "histogram",binwidth=0.1)


# 五种位置调整参数
# dodge:避免重复，并排放置
# fill:堆叠图形元素并将高度标准化为1
# identity：不做任何调整
# jitter:给点添加扰动避免重合
# stack:将图形元素堆叠起来












































