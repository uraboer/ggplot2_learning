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














