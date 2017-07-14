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




d<-ggplot(diamonds,aes(carat))+xlim(0,3)
d+stat_bin(aes(ymax=..count..),binwidth = 0.1,geom = "area")
d+stat_bin(aes(size=..density..),binwidth = 0.1,
           geom = "point",position = "identity")
d+stat_bin(aes(fill=..count..),y=1,binwidth = 0.1,
           geom = "tile",position = "identity")



require(nlme,quiet=TRUE,warn.conflicts = FALSE)
model<-lme(height~age,data = Oxboys,
           random = ~1+age|Subject)
oplot<-ggplot(Oxboys,aes(age,height,group=Subject))+geom_line()
oplot



age_grid<-seq(-1,1,length=10)
subjects<-unique(Oxboys$Subject)

preds<-expand.grid(age=age_grid,Subject=subjects)
preds$height<-predict(model,preds)

oplot+geom_line(data=preds,colour="#3366FF",size=0.4)



Oxboys$fitted<-predict(model)
Oxboys$resid<-with(Oxboys,fitted-height)

oplot %+% Oxboys+aes(y=resid)+geom_smooth(aes(group=1))
#残差不是随机分布的，模型有缺陷


model2<-update(model,height~age+I(age^2))
Oxboys$fitted2<-predict(model2)
Oxboys$resid2<-with(Oxboys,fitted2-height)

oplot %+% Oxboys+aes(y=resid2)+geom_smooth(aes(group=1))


#更新了数据集并且重新作了两次图却没有再次运行过oplot，这正是ggplot2图层功能所秉承的理念:使得反复拟合和评估模型变得轻松而自然



# geom_area():面积图，在普通线图的基础上，依y轴方向填充了下方面积的图形。对于分组数据，各组将按照依次堆积的方式绘制。
# geom_bar(stat="identity"):条形图，需要指定stat="dientity"，因为默认的统计变换将自动对“值”进行计数，而统计变换identity将保持数据不变。默认情况下，相同位置的多个条形图将依次向上堆积的方式绘制。
# geom_line():线条图，图形属性group决定了哪些观测是连接在一起的。geom_path与geom_line类似，但geom_path中的线条是根据它们在数据中出现的顺序进行连接的，而非从左至右进行连接。
# geom_point():散点图
# geom_ploygon():多边形，即填充后的路径。数据中的每一行代表了多边形的一个顶点。在绘图之前将多边形的顶点坐标数据和原始数据进行合并往往会更加方便。
# geom_text():可在指定点处添加标签。它是这些几何对象中唯一一个需要额外图形属性的：它需要指定label参数。可以通过设置可选的图形属性hjust和vjust来控制文本的横纵位置；此外，可以设置图形属性angle来控制文本的旋转。
# geom_tile():色深图(image plot)或水平图(level plot)，所有的瓦片(tile)构成了对平面的一个规则切分，且往往将fill图形属性映射至另一个变量。


df<-data.frame(
  x=c(3,1,5),
  y=c(2,4,6),
  label=c("a","b","c")
)

p<-ggplot(df,aes(x,y))+xlab(NULL)+ylab(NULL)
p
p+geom_point()+labs(title="geom_point")

p+geom_bar(stat = "identity")+labs(title="geom_bar(stat=\"identity\")")

p+geom_line()+labs(title="geom_line")
p+geom_area()+labs(title="geom_area")
p+geom_path()+labs(title="geom_path")
p+geom_text(aes(label=label))+labs(title="geom_text")
p+geom_tile()+labs(title="geom_tile")
p+geom_polygon()+labs(title="geom_polygon")


# 展示数据
# 有一些几何对象可以用于展示数据的分布，具体使用那种取决于分布的维度、分布是连续型或是离散型，以及我们感兴趣的是条件分布还是联合分布。
# 对于一维连续型分布，最重要的集合对象是直方图。为了找到一个表现力强的视图，多次测试组距的布局细节是必不可少的。可以改变组距宽度(binwidth)或者显式地精确指定切分位置(breaks)
# 有多种方式可以用来进行分布的跨组比较：同时绘制多个小的直方图，facets=.~var;使用频率多边形(frequency polygon)，geom="frepoly";或者使用条件密度图，position="fill"



depth_dist<-ggplot(diamonds,aes(depth))+xlim(58,68)
depth_dist+geom_histogram(aes(y=..density..),binwidth = 0.1)+facet_grid(cut ~ .)
depth_dist+geom_histogram(aes(fill=cut),binwidth = 0.1,position = "fill")
depth_dist+geom_freqpoly(aes(y=..density..,colour=cut),binwidth=0.1)



# 作为几何对象的直方图和频率多边形均使用了stat_bin统计变换。此统计变换生成了两个输出变量count和density。
# 变量count为默认值，因为它的可解释性更好。而变量density基本上相当于count除以count的总数，此变量在想要比较不同分布的形状而不是数据的绝对大小时更有用。
# 特别的，经常使用此变量来比较数据中不同大小子集的分布。
# 
# 和分布相关的许多几何对象都是以集合对象(geom)/统计变换(stat)的形式成对出现的。
# 这些几何对象中大多数的本质都是别名(alias):一个基本几何对象结合一个统计变换，即可绘制出想要的图形。
# 表面上看，箱线图(boxplot)似乎是一个例外，但在幕后实现上，geom_boxplot同样是使用基本的条、线和点组合而成的。
# 
# geom_boxplot=stat_boxplot+geom_boxplot:箱线图，即一个连续型变量针对一个类别型变量取条件所得的图形。
# 当类别型变量有许多独立的取值时，这种图形比较有用。
# 不过当类别型变量的取值很少时，直接研究分布的具体形状更佳。
# 箱线图也可对连续型变量取条件，前提是数据预先经过巧妙的封箱(binning)处理


library(plyr)
#类别型变量
qplot(cut,depth,data=diamonds,geom="boxplot")
#连续型变量
qplot(carat,depth,data=diamonds,geom = "boxplot",
      group=round_any(carat,0.1,floor),xlim = c(0,3))


#geom_jitter=position_jitter+geom_point:通过在离散型分布上添加随机噪声以避免遮盖绘制问题，这是一种较为粗糙的方法。

qplot(class,cty,data=mpg,geom = "jitter")
qplot(class,drv,data=mpg,geom = "jitter")


# geom_density=stat_density+geom_area:基于核平滑方法进行平滑后得到的频率多边形。
# 请仅在已知潜在的密度分布为平滑、连续且无边界的时候使用这种密度图，可以使用参数adjust来调整所得密度曲线的平滑程度


qplot(depth,data = diamonds,geom = "density",xlim = c(54,70))
qplot(depth,data = diamonds,geom = "density",xlim = c(54,70),
      fill=cut,alpha=I(0.2))


# 处理遮盖绘图问题
# 散点图是研究两个连续变量间关系的重要工具。但是当数据量很大时，这些点经常会出现重叠现象，从而掩盖真实的关系。
# 在极端情况下，甚至只能看到数据所在的大致范围，根据这种图形做出的任何结论都是值得怀疑的。
# 这种问题被成为遮盖绘制(overplotting)，对付的办法：
# 
# 小规模的遮盖绘制问题可以通过绘制更小的点加以缓解，或者使用中空的符号

df<-data.frame(x=rnorm(2000),y=rnorm(20000))
norm<-ggplot(df,aes(x,y))
norm+geom_point()
norm+geom_point(shape=1)#空心点
norm+geom_point(shape=".")#点的大小为像素级



# 对于更大的数据集产生的更为严重的遮盖绘制问题，可以使用α混合（调整透明度）让点呈现透明效果。
# 假设以比值形式指定α的值，则分母代表的是一个位置的颜色变为完全不透明时所需重叠点的数量。
# 在R中，可用的最小透明度为1/256，所以对于严重的遮盖绘制问题，这种方法的效果并不会太好


norm+geom_point(colour="black",alpha=1/3)
norm+geom_point(color="black",alpha=1/5)
norm+geom_point(color="black",alpha=1/10)
norm+geom_point(color="black",alpha=1/256)


# 如果数据存在一定的离散性，可以通过在点上增加随机扰动来减轻重叠。
# 特别是在与透明度一起使用时，这种方法很有效。
# 默认情况下，增加的扰动量是数据分辨率(resolution)的40%，这样可为数据中国的邻接区域留下一定的小间隙。


td<-ggplot(diamonds,aes(table,depth))+
  xlim(50,70)+ylim(50,70)
td+geom_point()
td+geom_jitter()
jit<-position_jitter(width = 0.5)
td+geom_jitter(position = jit)
td+geom_jitter(position = jit,colour="black",alpha=1/10)
td+geom_jitter(position = jit,colour="black",alpha=1/50)
td+geom_jitter(position = jit,colour="black",alpha=1/200)


# 将点分箱并统计每个箱中点的数量，然后通过某种方式可视化这个数量。
# 将图形划分为小的正方形箱可能会产生分散注意力的视觉假象。
# 使用bins和binwidth来控制箱的数量和大小


d<-ggplot(diamonds,aes(carat,price))+xlim(1,3)+
  theme(legend.position = "none")
d
d+stat_bin2d()
d+stat_bin2d(bins = 10)
d+stat_bin2d(binwidth = c(0.02,200))

install.packages("hexbin")
library(hexbin)
d+stat_binhex()
d+stat_binhex(bins=10)
d+stat_binhex(binwidth = c(0.02,200))


# 使用stat_density2d作二维密度估计，并将等高线添加到散点图中，或以着色瓦片(colored tiles)直接展示密度，或使用大小与分布密度成比例的点进行展示
d<-ggplot(diamonds,aes(carat,price))+xlim(1,3)+
  theme(legent.position="none")
d
#基于点和等高线的密度展示
d+geom_point()+geom_density2d()
#基于色深的密度暂时
d+stat_density2d(geom = "point",aes(size=..density..),
                 contour = F)+scale_size_area()
d+stat_density2d(geom = "tile",aes(fill=..density..),
                 contour = F)
last_plot()+scale_fill_gradient(limits=c(1e-5,8e-4))

#对付遮盖绘制问题的另一种方法是在图形上添加数据摘要，以指引人眼在茫茫数据中发现所寻模式的真实形状


#ggplot2暂不支持真正的三维曲面图

# 使用地图数据可能有两只原因：
# 1.为空间数据图形添加参考轮廓线
# 2.通过在不同的区域填充以构建等值线图(choropleth map)



# 添加地图边界可通过函数borders()完成
# 函数的前两个参数指定了要绘制的地图名map以及其中的具体区域region
# 其余的参数用于控制边界的外观:边界的颜色colour、线条的粗细size



library(maps)
data("us.cities")
#展示了美国(2006年1月)五十万人口以上的城市
big_cities<-subset(us.cities,pop>500000)
qplot(long,lat,data = big_cities)+borders("state",size=0.5)
#德克萨斯州的城市规划
tx_cities<-subset(us.cities,country.etc=="TX")
ggplot(tx_cities,aes(long,lat))+
  borders("county","texas",colour = "grey70")+
  geom_point(colour="black",alpha=0.5)



# 等值线图(choropleth map)则相对更难处理一些，自动化程度也没那么高
# 原因在于，要将数据中的标识符(identifier)同地图数据中的标识符完全匹配起来是有一定挑战性的。
# 关键在于，数据和地图数据中要有一列可以相互匹配


library(maps)
states<-map_data("state")
arrests<-USArrests
arrests
names(arrests)<-tolower(names(arrests))
arrests$region<-tolower(rownames(USArrests))


choro<-merge(states,arrests,by="region")
choro
#由于绘制多边形时涉及排序问题，且merge破坏了原始排序，故进行重新排序
choro<-choro[order(choro$order),]
head(choro)
#等值线图，各州人身伤害案件的数量
qplot(long,lat,data = choro,group=group,
      fill=assault,geom = "polygon")
#等值线图，人身伤害和谋杀类案件的比率
qplot(long,lat,data = choro,group=group,
      fill=assault/murder,geom = "polygon")



library(plyr)
ia<-map_data("county","iowa")
ia
mid_range<-function(x) mean(range(x,na.rm=TRUE))
centres<-ddply(ia,.(subregion),colwise(mid_range,.(lat,long)))
centres
?ddply
ggplot(ia,aes(long,lat))+
  geom_polygon(aes(group=group),
               fill=NA,colour="grey60")+
  geom_text(aes(label=subregion),data = centres,
            size=2,angle=45)


#不论是从模型所得或是从分布的假设而得，如果我们已经知道了一些关于数据中不确定性的信息，那么对这些信息加以展示通常是很重要的。
#在ggplot2中，共有四类几何对象可以用于这项工作，具体使用哪个取决于x的值是离散型还是连续型的，以及我们是否想要展示区间内的中间值，或是仅仅展示区间。



d<-subset(diamonds,carat<2.5 &
            rbinom(nrow(diamonds),1,0.2)==1)
head(d)

d$lcarat<-log10(d$carat)
d$lprice<-log10(d$price)

#剔除整体的线性趋势
detrend<-lm(lprice~lcarat,data=d)

d$lprice2<-resid(detrend)

mod<-lm(lprice2~lcarat*color,data=d)
head(mod)


install.packages("effects")
library(effects)
effectdf<-function(...){
  suppressWarnings(as.data.frame(effect(...)))
}
color<-effectdf("color",mod)
both1<-effectdf("lcarat:color",mod)
carat<-effectdf("lcarat",mod,default.levels=50)
both2<-effectdf("lcarat:color",mod,default.levels=3)


#进行数据变换以移除显而易见的效应
#对x轴和y轴的数据均以10为底的对数以剔除非线性性
qplot(lcarat,lprice,data = d,colour=color)
#剔除了主要的线性趋势
qplot(lcarat,lprice2,data = d,colour=color)

fplot<-ggplot(mapping = aes(y=fit,ymin=lower,ymax=upper))+
  ylim(range(both2$lower,both2$upper))  


#展示模型估计结果中变量color的不确定性
#color的边际效应
fplot %+% color+aes(x=color)+geom_point()+geom_errorbar()

#针对变量caret的不同水平（level），变量color的条件效应
#误差棒显示了95%的逐点置信区间
fplot %+% both2 +
    aes(x=color,colour=lcarat,group=interaction(color,lcarat))+
    geom_errorbar()+geom_line(aes(group=lcarat))+
    scale_colour_gradient()


#展示模型估计结果中变量carat的不确定性
#caret的边际效应
fplot %+% carat + aes(x=lcarat)+geom_smooth(stat="identity")

ends<-subset(both1,lcarat==max(lcarat))

#针对变量color的不同水平，变量caret的条件效应
#误差带显示了95%的逐点置信区间
fplot %+% both1+aes(x=lcarat,colour=color)+
  geom_smooth(stat="identity")+
  scale_colour_hue()+theme(legend.position = "none")+
  geom_text(aes(label=color,x=lcarat+0.02),ends)



#注意，在为这类图形添加题注时，需要细致地描述其中所含置信区间的本质，并说明观察置信区间之间的重叠是否有意义
#说明，当比较不同组时，如果区间没有重叠，则说明差异显著
#即，这些标准误是针对单组的均值的，还是针对不同组件均值之差的
#在计算和展示这些标准误时，multcomp包和multcompView包将非常有用，同时在多重比较中能正确地对自由度进行调整



#对于每个x的取值，计算对应y值的统计摘要是很有用的
#stat_summary()，使用ymin,y和ymax等图形属性，为汇总y的条件分布提供了一种灵活的方式



#参数fun.y，fun.ymin，fun.ymax能够接受简单的数值型摘要计算函数
#即该函数能够传入一个数值向量并返回一个数值型结果
#如mean()，median()，min()，max()



#fun.data可以支持更复杂的摘要计算函数
#也可以自己编写摘要计算函数：此函数应返回一个各元素有名称的向量作为输出



#添加图形注解
#在使用额外的标签注解图形时要记住的重要一点是：这些注解仅仅是额外的数据而已
#添加图形注解有两种基本的方式：逐个添加或批量添加

#逐个添加的方式适合少量的、图形属性多样化的注解
#需要添加多个具有类似属性的注解，将它们放到数据框中一并添加完成更有效


(unemp<-qplot(date,unemploy,data = economics,geom = "line",
              xlab = "",ylab = "No. unemployed (1000s)"))


graphics.off()
unemp<-qplot(date,unemploy,data = economics,geom = "line",
              xlab = "",ylab = "No. unemployed (1000s)")
unemp


presidential<-presidential[-(1:3),]
head(presidential)
head(economics)
yrng<-range(economics$unemploy)
xrng<-range(economics$date)
unemp+geom_vline(aes(xintercept=as.numeric(start)),data=presidential)


install.packages("scales")
library(scales)
library(ggplot2)
unemp+geom_rect(aes(NULL,NULL,xmin=start,xmax=end,
      fill=party),ymin=yrng[1],ymax=yrng[2],
      data = presidential,alpha=0.2)+scale_fill_manual(values = 
      c("blue","red"))


last_plot()+geom_text(aes(x=start,y=yrng[1],label=name),
      data=presidential,size=3,hjust=0,vjust=0)


caption<-paste(strwrap("Unemployment rates in the US has
    varied a lot over the years",40),collapse="\n")
unemp+geom_text(aes(x,y,label=caption),
  data=data.frame(x=xrng[2],y=yrng[2]),
  hjust=1,vjust=1,size=4)


highest<-subset(economics,unemploy==max(unemploy))
unemp+geom_point(data = highest,size=3,colour="red",
                 alpha=0.5)


#geom_text：可添加文字叙述或为点添加标签。对于多数图形，为所有观测都添加标签是无益的。然而，抽取部分观测添加标签可能会非常有用，往往希望标注出离群点或其它重要的点
#geom_vline,geom_hline：向图形添加垂直线或水平线
#geom_abline：向图形添加任意斜率和截距的直线
#geom_rect：可强调图形中感兴趣的矩形区域。有xmin、xmax、ymin、ymax几种图形属性
#geom_line,geom_path和geom_segment都可以添加直线。所有这些几何对象都有一个


#对于线和点这类简单的几何对象，可以根据点的数量调整图形属性size来改变点的大小

#无权重
qplot(percwhite,percbelowpoverty,data = midwest)
#以人口数量为权重
qplot(percwhite,percbelowpoverty,data = midwest,
      size=poptotal/1e6)+scale_size_area("Population\n(millions)",
      breaks=c(0.5,1,2,4))
#以面积为权重
qplot(percwhite,percbelowpoverty,data = midwest,size=area)+
      scale_size_area()


#对于更为复杂的、涉及到统计变换的情况，通过修改weight图形属性来表现权重
#这些权重将被传递给统计汇总计算函数
#在权重有意义的情况下，各种元素基本都支持权重的设定
#例如：各类平滑器、分位回归、箱线图、直方图、各类密度图
#无法直接看到这个权重变量，而且它也没有对应的图例，但它却会改变统计汇总的结果



lm_smooth<-geom_smooth(method = lm,size=1)
#未考虑权重的最优拟合曲线
qplot(percwhite,percbelowpoverty,data = midwest)+lm_smooth
#以人口数量作为权重的最优拟合曲线
qplot(percwhite,percbelowpoverty,data = midwest,
      weight=popdensity,size=popdensity)+lm_smooth



#不含权重信息的直方图，展示郡的数量
qplot(percbelowpoverty,data = midwest,binwidth=1)
#含权重信息的直方图，展示了人口数量
qplot(percbelowpoverty,data = midwest,weight=poptotal,
      binwidth=1)+ylab("population")



# 标度(scale)控制着数据到图形属性的映射
# 标度将数据转化为视觉上可以感知的东西：大小、颜色、位置、形状
# 标度也提供了读图时所使用的工具：坐标轴和图例（引导元素）
# 
# 更准确的说，每一种标度都是从数据空间的某个区域（标度的定义域）到图形属性空间的某个区域（标度的值域）的一个函数
#
# 每种标度的定义域都对应着提供给这个标度的变量的取值范围，此定义域可以是连续型或离散型、有序型或无序型
# 
# 执行标度的过程分为：变换、训练、映射
# 标度可以粗略分为四类：位置标度、颜色标度、手动离散型标度、同一型标度
# 
# 标度的另一个重要角色，是生成一个允许读者从图形属性空间到数据空间进行反向映射的引导元素，并从图中读出取值
# 
# 对于位置型图形属性，引导元素是坐标轴；对于所有其它图形属性来说，引导元素是图例
# 
# 与其它图形系统不同的是，无法直接控制坐标轴和图例的细节，不存在gglegend()或ggaxis()这种修改图例或坐标轴的函数
# 引导元素的外观皆由标度的参数控制


# 标度的工作原理
# 由于输入变量可能是离散型，也可能是连续型，所以标度的定义域要么是某些值组成的集合（以因子的形式存储，字符型因子或逻辑型因子），要么是一个实值区间（以长度为2的数值型向量的形式存储）
# 
# 标度的值域也可以是离散型或连续型的。
# 对于离散型标度，它的值域是输入值对应的图形属性值组成的一个向量。
# 对于连续型标度，它的值域是穿过某种更复杂空间的一条一维路径。
# 
# 将定义域映射到值域的过程：
# 1.变换：（仅针对连续型的定义域）例如对数据取对数或开根号
# 
# 2.训练：在一个仅有一个图层且仅呈现原始数据的图形中，这个学习过程包括确定某个（变换后的）连续型变量的最小值和最大值，或者是列出某个类别型变量的所有水平。
# 但是，标度的的定义域往往必须在多个面板中反映出横跨多个数据集的多个图层
# 
# 3.映射
# 
# 
# 在初始化整个图形和增加新图层时，默认的标度将被自动添加。
# 这意味着，如果在之后修改了底层数据或图形属性映射，变量类型和标度类型之间可能出现不匹配的情况


plot<-qplot(cty,hwy,data = mpg) 
plot

#这样做行不通是因为变量类型和默认标度不匹配
plot+aes(x=drv)

##更正默认标度后解决了问题
graphics.off()
plot+aes(x=drv)+scale_x_discrete()


# 如果要添加一个不同的标度或修改默认标度的某些特征，必须构造一个新的标度，然后使用+将其添加到图形上
# 所有的标度构建器（scale constructor）都有一套通用的命名方案
# 以scale_开头，接下来是图形属性的名称（colour_,shape_,x_）,最后以标度的名称结尾（gradient,hue,manual）


p<-qplot(sleep_total,sleep_cycle,data = msleep,colour=vore)
p

#显示添加默认标度
p+scale_colour_hue()

#修改默认标度的参数，这里改变了图例的外观
p+scale_colour_hue("What does\nit eat?",
  breaks=c("herbi","carni","omni",NA),
  labels=c("plants","meat","both","don't know"))

#使用一种不同的标度
p+scale_color_brewer(palette = "Set1")


# 标度可分为四组：
# 1.位置标度：用于将连续型、离散型和日期-时间型变量映射到绘图区域，以及构造对应的坐标轴
# 2.颜色标度：用于将连续型和离散型变量映射到颜色
# 3.手动标度：用于将离散型变量映射到我们选择的符号大小、线条类型、形状或颜色、以及创建对应的图例
# 4.同一型标度：用于直接将变量值绘制为图形属性，而不去映射它们


#通用参数
#1.name:设置坐标轴或图例上出现的标签，可以指定字符串（使用\n换行）或数学表达式，xlab(),ylab(),labs()

p<-qplot(cty,hwy,data = mpg,colour=displ)
p
p+scale_x_continuous("City mpg")
p+xlab("City mpg")
p+ylab("Highway mpg")
p+labs(x="City mpg",y="Highway",colour="Displacement")
#miles/gallon
p+xlab(expression(frac(miles,gallon)))


#2.limits:固定标度的定义域。连续型标度接受一个长度为2的数值型向量；离散型标度接受一个字符型向量。
#一旦设定了limits，数据将不再进行任何训练。
#限制定义域可以移除不想在图形上展示的数据，同时也可以保证要进行比较的多个图形中的绘制范围一致
#任何不在此标度定义域内的值均被丢弃：如果想要囊括图中的所有观测，其每个图形属性都必须位于每个标度的定义域中。
#丢弃过程发生在统计量的计算之前

#3.breaks,labels:控制着显示在坐标轴或图例上的值
#即坐标轴上应该显示那些刻度线的值，或一个连续型标度在一个图例中将被如何分段
#labels指定了应在断点处显示的标签
#若设置了labels，则必须同时指定breaks，只有这样，这两个参数才能被正确匹配
#breaks影响显示在坐标轴和图例上的元素，limits影响显示在图形上的元素


p<-qplot(cyl,wt,data = mtcars)
p
p+scale_x_continuous(breaks = c(5.5,6.5))
p+scale_x_continuous(limits = c(5.5,6.5))
p<-qplot(wt,cyl,data = mtcars,colour=cyl)
p
p+scale_colour_gradient(breaks=c(5.5,6.5))
p+scale_colour_gradient(limits=c(5.5,6.5))


#4.formatter:如果未指定任何标签，则将在每个断点处自动调用格式刷（formatter）来格式化生成标签
#对于连续型标度，可用的标签刷为：comma,percent,dollar,scientific
#对于离散型标度，则为abbreviate


#位置标度
#每幅图形一定拥有两个位置标度，一个指定水平位置（x标度），一个指定竖直位置（y标度）
#xlim(10,20):一个从10到20的连续型标度
#ylim(20,10):一个从20到10的反转后连续型标度
#xlim("a","b","c"):一个离散型标度
#xlim(as.Date(c("2008-05-01","2008-08-01"))):一个从2008年5月1日到8月1日的日期型标度

#在ggplot2中，为了保持与其它标度的一致性，任何在limits以外的数据都不会被绘制，也不会被包括在统计变换的过程中。
#这意味着，通过设置limits所得结果与在视觉上放大一块绘图区域所得的结果是不同的

#默认情况下，位置标度的limits会稍微超出数据的范围。这样就保证了数据与坐标轴不会发生重叠。
#可以用参数expand来控制溢出量，此参数是长度为2的数值型向量
#其中第一个元素给出的是乘式的溢出量，第二个参数给出的是加式的溢出量
#如果不想留任何多余的空间，就使用expand=c(0,0)


#连续型
#最常用的连续型位置标度是scale_x_continuous和scale_y_continuous，均将数据映射到x轴和y轴
#每个连续型标度均可接受一个trans参数，允许指定若干种线性或非线性的变换
#而每一种变换都是由“变换器”实现的，变换器描述了变换本身和对应的逆变换，以及如何去绘制标签

#变换通常被用来修改位置标度，所以对于x,y和z标度都是有简便写法的
#scale_x_log10()等价于scale_x_continuous(trans="log10")
#参数trans对任意的连续型标度均有效，包括颜色梯度，但简便写法仅针对位置标度存在



#对标度进行对数变换
qplot(log10(carat),log10(price),data = diamonds)
#对数据进行对数变换
#图形主体是完全相同的，但坐标轴上的标签是不同的
qplot(carat,price,data = diamonds)+scale_x_log10()+
  scale_y_log10()


#日期和时间
#日期和时间值基本上属于连续型，但在标注坐标轴时有着特殊的处理方式
#目前仅支持属于date类的日期值和属于POSIXct类的时间值
#如果是其他格式的，则需使用as.Date()或as.POSIXct()对其进行转换

#对于日期坐标轴，有三个参数可以用于控制其外观和刻度的位置：major、minor、format

#总体而言，此标度本身已经能够很好地选择默认值，不过如果想对其进行微调，细节如下：
#参数major和minor用以按照时间的单位,即年、月、周、日、时、分、秒来指定主要和次要断点的位置，并且允许以这些单位的倍数出现
#参数format指定了刻度标签的格式，"%d/%m/%y"

library(scales)
plot<-qplot(date,psavert,data = economics,geom = "line")+
  ylab("Personal savings rate")+
  geom_hline(yintercept = 0,colour="grey50")
plot
plot+scale_x_date(breaks = date_breaks("10 years"))
plot+scale_x_date(
  limits = as.Date(c("2004-01-01","2005-01-01")),
  labels = date_format("%Y-%m-%d")
)


#离散型
#离散型位置标度将输入中的各水平映射为整数。
#结果的顺序可用参数breaks进行控制，不想要的水平可以使用limits进行丢弃
#由于经常会在图形的非整点位置放置标签和标注，所以离散型位置标度可以接受连续型的值
#如果尚未调整breaks或limits，某个因子水平的所在位置的数值表示可以使用as.numeric()进行计算：以从1开始的整数表示


#颜色标度
#对连续型值有三种基于渐变的方法，对离散型值有两种方法

#hcl色彩空间的现代方案：色相(hue)、彩度(chroma)、明度(luminance)
#色相：是一个0和360之间的角度值，将一种色彩赋以“颜色”属性，如蓝、红、橙
#明度：指颜色的明暗程度。明度的高低，要看其接近白色或黑色的程度而定。0为黑，1为白
#彩度：指色彩的纯度。0是灰色，彩度的最大值随明度的变化而不同


#连续型
#根据颜色梯度中的色彩数量划分，共有三类连续型颜色梯度（即渐变色）：
#scale_colour_gradient()和scale_fill_gradient():双色梯度，顺序从低到高。参数low和high用以控制此梯度两端的颜色

#scale_colour_gradient2()和scale_fill_gradient2():三色梯度。参数low和high用以控制此梯度两端的颜色。顺序为低——中——高。
#中点的默认值为0，但也可以使用参数midpoint将其设置为任意值。这个参数

#scale_colour_gradientn()和scale_fill_gradientn():自定义的n色梯度。此标度需要赋给参数colours一个颜色向量。
#不加其它参数的话，这些颜色将依照数据的范围均匀地分布。如果需要让这些值不均匀地分布，则可以使用参数values
#如果rescale的取值为TRUE（默认值），则values应在0和1之间取值。
#如果rescale的取值为FALSE，则values应在数据范围内取值


f2d<-with(faithful,MASS::kde2d(eruptions,waiting,
  h=c(1,10),n=50))
df<-with(f2d,cbind(expand.grid(x,y),as.vector(z)))
names(df)<-c("eruptions","waiting","density")
#温泉喷发时间数据密度估计的三种配色方案
#默认的颜色梯度
erupt<-ggplot(df,aes(waiting,eruptions,fill=density))+
  geom_tile()+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))
erupt+scale_fill_gradient(limits=c(0,0.04))

#自定义的黑白梯度
erupt+scale_fill_gradient(limits=c(0,0.04),
  low = "white",high="black")

#中点设为密度均值的3点梯度
erupt+scale_fill_gradient2(limits=c(-0.04,0.04),
  midpoint = mean(df$density))



#install.packages("vcd")
library(vcd)
fill_gradn<-function(pal){
  scale_fill_gradientn(colours = pal(7),limits=c(0,0.04))
}

erupt+fill_gradn(rainbow)


#离散型
#离散型数据有两种颜色标度。一种可以自动选择颜色，另一种可以轻松地从手工甄选颜色集中选择颜色
#默认的配色方案，即scale_color_hue()，可通过沿着hcl色轮选取均匀分布的色相来生成颜色
#这种方案对多至约8种颜色时都能有较好的效果，但对于更多的颜色，要区分开不同颜色就变得比较困难了
#默认配色的另外一个缺点是，由于所有颜色都拥有相同的明度和彩度，当进行黑白打印时，就会成为几近相同的灰影

#除了这种基于计算的方案以外，另一种可选的方案是使用ColorBrewer配色
#这些手工甄选的颜色可在很多情境下良好的运作，尽管它更专注于地图，这些颜色也因此在展示较大的面积时表现更佳

#对于类别型数据中的点，关注的调色板是"Set1","Dark2"
#对面积而言，则是"Set2","Pastel1","Pastel2","Accent"
#使用RColorBrewer::display.brewer.all可列出所有的调色板


point<-qplot(brainwt,bodywt,data = msleep,log = "xy",
  colour=vore)
area<-qplot(log10(brainwt),data = msleep,fill=vore,
  binwidth=1)

#应用于点的三种调色板
point+scale_color_brewer(palette = "Set1")
point+scale_color_brewer(palette = "Set2")
point+scale_color_brewer(palette = "Pastel1")
#应用于条形的三种调色板
area+scale_fill_brewer(palette = "Set1")
area+scale_fill_brewer(palette = "Set2")
area+scale_fill_brewer(palette = "Pastel1")


#手动离散型标度
#离散型标度scale_linetype(),scale_size_discrete()和scale_shape()基本上没有选项（虽然对于形状标度，可以选择点是空心可填充的或是实心的）
#这些标度仅仅是按一定的顺序将因子的水平映射到一系列取值中

#如果想要定制这些标度，需要使用：scale_shape_manual(),scale_linetype_manual(),scale_colour_manual()

#手动型标度有一个重要参数values,可以用它来指定这个标度应该生成的值
#如果这个向量中的元素是有名称的，则它将自动匹配输入和输出的值，否则它将按照离散型变量中水平的先后次序进行匹配


plot<-qplot(brainwt,bodywt,data = msleep,log = "xy")

#自定义颜色标度
plot+aes(color=vore)+scale_color_manual(values = 
    c("red","orange","yellow","green","blue"))
colours<-c(carni="red","NA"="orange",insecti="yellow",
    herbi="green",omni="blue")
plot+aes(color=vore)+scale_color_manual(values = colours)

#自定义形状标度
plot+aes(shape=vore)+scale_shape_manual(values = c(1,2,6,0,23))



#在同一幅图上展示多个变量并显示一个有用的图例
#使用scale_colour_manual(),把线上色，然后添加一个图例说明哪种颜色对应着哪个变量即可
#这对于ggplot2不适用，因为图例是由标度负责的，但标度并不知道要为线条添加何种标度


huron<-data.frame(year=1875:1972,level=LakeHuron)
head(huron)
ggplot(huron,aes(year))+
  geom_line(aes(y=level-5),color="blue")+
  geom_line(aes(y=level+5),color="red")



#同一型标度
#当数据能被R中的绘图函数理解时，即数据空间和图形属性空间相同时，可以使用同一型标度(identity scale)

#图例和坐标轴
#坐标轴和坐标被共同称为引导元素，它们都是标度的逆函数：允许在图中读出观测并将其映射回原始值

#图例和坐标轴存在着天然的可比性：图例标题(legend title)和坐标轴名(axis label)是等价的，并且都由标度的名称参数(name)决定
#图例标示(legend key)和刻度标签(tick label)皆由标度的断点参数(break)决定

#要绘制图例，图形必须收集每一种图形属性的使用信息：为何种数据及为何种几何对象
#标度的断点(breaks)被用来确定图例标示的值，使用了对应图形属性的一系列集合对象则

#标度的断点(breaks)被用来确定图例标示的值，使用了对应图形属性的一系列集合对象则被用来确定如何绘制这些标示
#如果使用了点这个几何对象，那么将在图例中得到点；
#如果使用了线，那么将在图例中得到线

#ggplot2会尝试生成最小数量的、能够精确表达图中使用图形属性的图例
#当一个变量对应了多个图形属性时，ggplot2可以通过合并图例的办法来达到精简的目的

#name:控制着坐标轴名和图例标题，可为字符串或数学表达式

#breaks,labels:控制着哪些刻度标签出现在坐标轴上，以及哪些标示出现在图例上

#主题设置axis.*和legend.*控制着坐标轴和图例的整体外观

#内部网格线由主要断点和次要断点的参数控制。
#默认的，次要的网格线均匀地分布在原始的数据空间中：
#这就为对数——对数图形赋予了通用的行为，即主要网格是可乘的，而次要网格是可加的。
#使用minor_breaks覆盖掉次要网格线
#网格线的外观是panel.grid.major和panel.grid.minor两个主题设置来控制的

#图例的位置和对齐是使用主题设置legend.positio来控制的，其值可为
#right,left,top,bottom,none,或是一个表示位置的数值
#这个数值型位置由legend.justification给定的相对边角位置表示
#是一个长度为2的数值型向量：右上角c(1,1),左下角c(0,0)


#ggplot2提供两种分面类型:网格型(gacet_grid)和封装型(facet_wrap)
#网格分面生成的是一个2维面板网格，面板的行与列通过变量来定义
#封装分面则先生成一个1维的面板条块，然后再封装到2维中
#网格分面布局类似于基础图形中的coplot布局
#封装分面则类似于lattice的面板布局

#分面系统有两个基本参数：
#一个是分面变量的设置，另一个是指定分面的位置标度是全局还是局部


mpg2<-subset(mpg,cyl!=5 & drv %in% c("4","f"))
head(mpg2)
head(mpg2$drv)



#网格分面在2维网格中展示图形
#输入分面表达式时，需要设定哪些变量作为分面绘图的行，哪些变量作为列

#不进行分面：即不使用函数facet_grid或加上命令facet_null()
#此时，没有行或列被分面化，将得到一个单独的面板

#===qplot(cty,hwy,data=mpg2)
qplot(cty,hwy,data = mpg2)+facet_null()


#一行多列:".~a":纵坐标相同，有助于y位置的比较
qplot(cty,hwy,data = mpg2)+facet_grid(.~cyl)


#一列多行:"b~.",横坐标相同，利于x位置的比较，尤其是对数据分布的比较
qplot(cty,data = mpg2,geom = "histogram",binwidth=2)+
    facet_grid(cyl~.)


#多行多列:"a~b",通常将因子水平数目最大的变量按列排放，可以充分利用屏幕的宽高比
qplot(cty,hwy,data = mpg2)+facet_grid(drv~cyl)


#边际图
#切割图形就好比创建一个列联表
#列联表可展示每个单元的值以及边际和

#设定margins=TRUE可展示所有的边际图，或者如margins=c("sex","age"),列出要展示的边际图的变量名称
#可以使用grand_row或grand_col来分别生成所有行或列的边际图

p<-qplot(displ,hwy,data = mpg2)+
  geom_smooth(method = "lm",se=F)
p+facet_grid(cyl~drv)
#边际图：列联表
p+facet_grid(cyl~drv,margins = T)


#封装分面
#facet_warp并不是用两个或者更多的变量来生成一个2维网格
#而是生成一个长的面板条块（由任意数目的变量生成），然后将它封装在2维中
#在处理单个多水平变量时，这种处理方式非常有用，可以有效地利用空间来安放图形
#lattice中栅栏(Trellis)图形也采用这个方法


library(plyr)
movies$decade<-round_any(movies$year,10,floor)
qplot(rating,..density..,data = subset(movies,decade>1890),
    geom = "histogram",binwidth=0.5)+
    facet_wrap(~decade,ncol=6)


# 标度控制
# 对于两种分面，可以通过调整参数scales来控制面板的位置标度是相同（固定）还是允许变化（自由）
# 
# scales="fixed":x和y的标度在所有面板中都相同
# scales="free":x和y的标度在每个面板这都可以变化
# scales="free_x":x的标度可变，y的尺度固定
# scales="free_y":y的标度可变，x的尺度固定

p<-qplot(cty,hwy,data = mpg)
p+facet_wrap(~cyl)
p+facet_wrap(~cyl,scales="free")


# 固定标度可以在相同的基准上对子集进行比较，观察在哪些地方各子集有相似的总体模式
# 而自由标度可以发现更多细节，展示不同量纲的时间序列时非常有用

library(reshape2)
em<-melt(economics,id="date")
qplot(date,value,data = em,geom="line",group=variable)+
  facet_grid(variable~.,scale="free_y")


#使用网格分面时facet_grid有一个额外的限制:同列的面板必须有相同的x标度，同行的面板必须有相同的y标度
#facet_grid还有一个额外的参数space,值可为"free"或"fixed"
#当space设定为free时，每列(行)的宽度(高度)与该列(行)的标度范围成比例
#这将使得所有面板的标度比例相同:每个面板中的1cm都映射为相同的数据范围


mpg3<-within(mpg2,{
  model<-reorder(model,cty)
  manufacturer<-reorder(manufacturer,-cty)
})

models<-qplot(cty,model,data = mpg3)

models

models+facet_grid(manufacturer~.,scales = "free",
    space ="free" )+theme(strip.text.y = element_text())


#每种小汽车的城市油耗英里数的点图
#车类型按mpg均值排序

#使用scales="free_y"和space="free"，按生产商进行分面
#strip.text.y主题设置用来旋转分面标签

#ggplot2给每个面板都添加地图，缺失的分面变量按包含该分面变量所有的值来处理

#在分面图形中，每个组别都在单独的面板中，相隔较远，组间无重叠
#因此组与组之间重叠严重时，分面图形有一定的好处，不过这也会导致组间的细微差别难以发现
#使用图形属性区分各组时，各组将会离得很近甚至可能重叠，不过些微的差别容易被发现


xmaj<-c(0.3,0.5,1.3,5)
xmin<-as.vector(outer(1:10,10^c(-1,0)))
ymaj<-c(500,5000,10000)
ymin<-as.vector(outer(1:10,10^c(2,3,4)))
dplot<-ggplot(subset(diamonds,color %in% c("D","E","G","J")),
  aes(carat,price,color=color))+
  scale_x_log10(breaks=xmaj,labels=xmaj,minor=xmin)+
  scale_y_log10(breaks=ymaj,labels=ymaj,minor=ymin)+
  scale_color_hue(limits=levels(diamonds$color))+
  theme(legend.position = "none")
graphics.off()
dplot+geom_point()
dplot+geom_point()+facet_grid(.~color)

dplot+geom_smooth(method = lm,se=F,fullrange=T)

dplot+geom_smooth(method = lm,se=F,fullrange=T)+
  facet_grid(.~color)


#并列与分面
#分面可绘制出与图形并列类似的图形效果
#并列和分面绘制的区别在于标注方式：
#分面图形上方有颜色的标注，同时下面有切工的标注
#而并列图形在下方有颜色标注，切工却没有清晰标注出来


qplot(color,data = diamonds,geom = "bar",fill=cut,
  position = "dodge")

qplot(cut,data = diamonds,geom="bar",fill=cut)+
  facet_grid(.~color)+
  theme(axis.text.x = element_text(angle=90,hjust = 1,
    size = 8,colour = "grey50"))

#除标注方式外，当两个变量的因子水平几乎完全交叉，而部分变量组合缺失时，两种绘图方式也就会有所不同
#并且，并列图的用处不大，因为它只是对条形图做局部地分割，没有任何标签
#而分面就实用得多，它能控制分割方式是局部的(scales="free_x,space="free")还是局部的(scales="fixed")

mpg4<-subset(mpg,manufacturer %in%
  c("audi","volkswagen","jeep"))
mpg4$manufacturer<-as.character(mpg4$manufacturer)
mpg4$model<-as.character(mpg4$model)

base<-ggplot(mpg4,aes(fill=model))+
  geom_bar(position="dodge")+
  theme(legend.position = "none")


base+aes(x=model)+
  facet_grid(.~manufacturer)
last_plot()+facet_grid(.~manufacturer,
  scales = "free_x",space="free")
base+aes(x=manufacturer)


#总之，图形是选择分面还是并列，要视两变量间的关系而定：
#水平完全交叉：分面和并列基本等同

#水平几乎交叉:有相同标度的分面保证了所有的水平组合可见，即使有些是空的。
#当存在非结构性的缺失组合时，绘制分面图形非常有用

#水平无交叉(嵌套):标度自由的分面会对每个有较高水平的组别分配充足的作图空间，
#并对每个条目都进行标注

#连续型变量
#对连续型变量进行分面，首先需要将其变换为离散型，有三种转化方法：

#1.将数据分为n个长度相同的部分：用cut_interval(x,n=10)控制划分数目
#或用cut_interval(x,length=1)控制每个部分的长度。
#控制划分数目会容易些，但划分区间的端点值可能不太"美观"

#2.将数据划分为n个有相同数目点的部分：cut_number(x,n=10)
#这使得分面间进行对比会更容易(分面有相同数目的点)，但需要注意每个部分的标度范围是不同的


#面板长度为1
mpg2$disp_ww<-cut_interval(mpg2$displ,length = 1)
#每个面板等长
mpg2$disp_wn<-cut_interval(mpg2$displ,n=6)
#每个面板包含数目的点相同
mpg2$disp_nn<-cut_number(mpg2$displ,n=6)


plot<-qplot(cty,hwy,data = mpg2)+labs(x=NULL,y=NULL)
plot+facet_wrap(~disp_ww,nrow = 1)
plot+facet_wrap(~disp_wn,nrow = 1)
plot+facet_wrap(~disp_nn,nrow = 1)


#坐标系是将两种位置标度结合在一起组成的2维定位系统
#ggplot2包含了6种不同的坐标系，坐标系命名规则为coor_加上坐标系的名称

#坐标系有两大主要功能：
#1.将2个位置图形属性组合起来在图形中形成2维方位系统
#位置图形属性分别被称为x和y，但将它们称作位置1和位置2可能更合适
#因为图形位置属性的名字会随着坐标系的不同而不同

#2.配合分面，坐标系将绘出坐标轴和面板背景
#标度控制着坐标轴上出现的数值，并将数据映射到图形中的位置，然后通过坐标系将它们绘制出来
#


#变换
#与数据变换和标度变换不同，坐标轴变换将改变图形的几何形状：在极坐标系中，矩形变为圆环的一部分；
#在地图中，两点间的最短路径将不再是直线

#坐标系变换分为两步：
#首先，几何形状的参数变化只依据定位，而不是定位和维度
#但在一个非笛卡尔坐标系中，矩形则可能没有恒定的高度和宽度，那么如何去解析高度和宽度呢？
#解决方案就是仅使用基于定位的方式来标示图形
#获得矩形四个角的定位后，变换位置，然后将矩形转化成多边形

#将几何形状变为基于定位的表现形式后，下一步就是将每个位置转化到新的坐标系中
#对于点的转化，由于点的在任何坐标系中都是一个点，因此它的转化相对简单，但对于线和多边形就困难的多，这是因为直线在一个新的坐标系中并不一定是直线
#为解决这个问题，先假定坐标系之间的变换是连续的，即所有的极短的直线变换后仍是很短的直线
#这样，就可以将线和多边形切割为许多的线段后再进行变换，称为"分割再组合"(munching)


#统计量
#原来统计变换(stat)使用的统计方法都依赖坐标系的选择

#笛卡尔坐标系
#coord_cartesian,coord_equal,coord_flip,coord_trans，由于x和y的位置都是正交映射到图形的位置上，因此四种坐标系本质上仍是笛卡尔型的

#设置范围
#coord_cartesian有两个参数xlim和ylim
#与标度中范围参数不同之处：
#当设定标度范围时，任何超出范围的数据都会被删除；
#但当设定笛卡尔坐标系的范围时，使用的仍是所有的数据，只不过只展示一小片图形区域


#坐标系的范围设置vs标度的范围设置
#完整数据集
(p<-qplot(disp,wt,data = mtcars)+geom_smooth())

#x的标度范围设置为(325,400)
#标度的范围设置是对数据取子集，然后再重新拟合曲线
p+scale_x_continuous(limits = c(325,500))

#坐标系x轴范围设置为(325,400)
#坐标系的放缩就是图像的放缩
p+coord_cartesian(xlim = c(325,500))


(d<-ggplot(diamonds,aes(carat,price))+
    stat_bin2d(bins = 25,color="grey70")+
    theme(legend.position = "none"))
d+scale_x_continuous(limits = c(0,2))
d+coord_cartesian(xlim = c(0,2))


#坐标轴翻转
#大多数统计量和几何形状都假定只对x条件下的y值感兴趣，
#即大多数统计模型都假定x的值测量无误差

qplot(displ,cty,data = mpg)+geom_smooth()
qplot(cty,displ,data = mpg)+geom_smooth()
qplot(cty,displ,data = mpg)+geom_smooth()+coord_flip()



#变换
#与范围设置一样，在标度层面和坐标系层面上都可以进行数据变换
#coord_trans有x和y两个参数供坐标轴使用，都是字符串，被称作变换器
#标度层面的变换发生在统计量计算之前，且不会改变对象的几何形状
#但坐标系层面的变换却发生在统计量计算之后，会影响几何形状
#若两种变换一起使用，可先在变换的尺度上建模，然后再反演到变换前的图形以便于解释


qplot(carat,price,data = diamonds,log="xy")+
  geom_smooth(method = "lm")

library(scales)
last_plot()+coord_trans(x=exp_trans(10),y=exp_trans(10))

#相同标度
#coord_equal保证了x轴和y轴有相同的标度:x轴上和y轴上的1cm代表相同的数据波动范围，
#默认值设定的是1:1，可以通过修改参数ratio来更改两者的尺度比例

#利用极坐标可生成饼图、玫瑰图（源自条状图）和雷达图（源自直线几何对象）
#但由于角度在小的半径中比在大的半径中更难被感知，因此极坐标视觉感官性并不佳
#极坐标常被用于环型数据，特别是时间和方向数据
#参数theta决定了哪个变量被映射为角度（默认为x），哪个被映射为半径


##堆叠条状图
(pie<-ggplot(mtcars,aes(x=factor(1),fill=factor(cyl)))+
    geom_bar(width=1))

##饼图
pie+coord_polar(theta = "y")

##环形图
pie+coord_polar()


#主题
#主题系统控制着图形中的非数据元素外观，不会影响几何对象和标度等数据元素
#主题不能改变图形的感官性质，但可以使图形变得更具美感，满足整体一致性的要求

#主题的控制包括标题、坐标轴标签、图例标签等文字调整，以及网格线、背景、轴须的颜色搭配

#lattice和基础图形系统没有采用数据与非数据控制分离的方法，大部分函数都设定了许多参数来调整数据和非数据的外观，这很容易导致函数的复杂化
#ggplot2则采用了不同的策略：绘图时，首先确定数据如何展示，然后再用主题系统对细节进行渲染


#与ggplot2其它部分类似，主题也可以通过许多参数的控制使图形由粗糙变得美观：
#使用内置主题，图形每个元素的效果保持着视觉的一致性。
#默认主题为灰色背景、白色网格线，另一个为白色背景、灰色网格线

#修改内置主题的某些元素。每个主题都是由许多元素组成，
#都沿袭了内置主题中渲染元素的函数和参数
#通过调整和组合这些参数，如文本的大小和颜色、背景、网格线颜色、文本方向


#内置主题
#默认的theme_gray()使用淡灰色背景和白色网格线
#theme_bw()为白色背景和深灰色网格线

#两个主题都由唯一的参数base_size来控制基础字体的大小
#基础字体大小指的是轴标题的大小，图形标题比它大20%，轴须标签比它小20%

#全局性设置:theme_set(theme_grey())或theme_set(theme_bw())
#theme_set()返回先前的主题，可储存以备后用

#局部性设置:只改变单个图形的主题,qplot(...)+theme_grey()
#局部设置将会覆盖默认的全局性设置


hgram<-qplot(hwy,data = mpg,binwidth=1)
hgram

previous_theme<-theme_set(theme_grey())
hgram

hgram+previous_theme
graphics.off()

#永久性存储初始主题
theme_set(previous_theme)


#内置元素函数有四个基础类型：文本(text)、线条(lines)、矩形(rectangles)、空白(blank)
#element_text()绘制标签和标题，可控制字体的family\face\color\size\hjust\vjust\angle\lineheight


hgramt<-hgram+labs(title="This is a histogram")
hgramt
hgramt+theme(plot.title = element_text(size = 20))

hgramt+theme(plot.title = element_text(size = 20,colour = "red"))
hgramt+theme(plot.title = element_text(size = 20,hjust = 0))
hgramt+theme(plot.title = element_text(size = 20,face = "bold"))
hgramt+theme(plot.title = element_text(size = 20,angle = 180))


#绘制线条或线段，该元素函数可控制colour,size,linetype
hgram+theme(panel.grid.major = element_line(colour = "red"))
hgram+theme(panel.grid.major = element_line(size = 2))
hgram+theme(panel.grid.major = element_line(linetype = "dotted"))
hgram+theme(axis.line = element_line())
hgram+theme(axis.line = element_line(colour = "red"))
hgram+theme(axis.line = element_line(size = 0.5,linetype = "dashed"))


#element_rect()绘制主要供背景使用的矩形
#可以控制填充颜色(fill)和边界colour,size,linetype

hgram+theme(plot.background = element_rect(fill = "grey80",colour = NA))
hgram+theme(plot.background = element_rect(size=2))
hgram+theme(plot.background = element_rect(colour = "red"))
hgram+theme(panel.background = element_rect())
hgram+theme(panel.background = element_rect(colour = NA))
hgram+theme(panel.background = element_rect(linetype = "dotted"))


#element_blank()表示空主题，即对元素不分配相应的绘图空间
#该函数可删去不感兴趣的绘图空间
#使用colour=NA,fill=NA让某些元素不可见，可达到同样的效果，但仍占绘图空间

hgramt
last_plot()+theme(panel.grid.minor = element_blank())
last_plot()+theme(panel.grid.major = element_blank())
last_plot()+theme(panle.background = element_blank())
last_plot()+theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank())
last_plot()+theme(axis.line = element_line())



p<-qplot(mpg,wt,data = mtcars,colour=factor(cyl))
p

scale_color_discrete<-scale_color_brewer
p


#存储输出
#基本图形输出有两种类型：矢量型或光栅型















