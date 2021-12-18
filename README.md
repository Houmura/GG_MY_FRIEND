# GG_MY_FRIEND
<br> 

## 一个计算抽卡沉船概率的计算器

<br>


没有什么技术含量,虽然是针对碧蓝航线设计的，不过其他抽卡游戏（只要机制相同）都可以用。

<br>

## 预览图
![Image_text](https://github.com/Houmura/GG_MY_FRIEND/blob/master/image/ui-preview.PNG)
![Image_text](https://github.com/Houmura/GG_MY_FRIEND/blob/master/image/cli-preview.PNG)

<br>
<br> 

## 使用说明
* 如何启动？
>   + 带交互界面 (直接运行程序) 
>       + Windows 下直接双击运行，或在CMD直接调用(不带任何参数)
>       + Unix 下切换到程序目录，执行 `./AZCalculator` 命令即可 
> 
> <br>
> 
>   + 命令行模式 (命令行带参数运行) 

<br>
<br> 
<br>
<br> 

* 如何计算？
>   + 给定抽卡次数，计算抽齐毕业的概率(forward_mode，参数-f)，需要的参数有：
>       + 变量数(-v)：多少种卡片(目前仅支持最大5种)；
>       + 抽卡次数 (-n)：抽多少次卡；
>       + 每种卡片的获得概率(-ps)，单位是 % 
> 
> <br>
> 
>   + 给定毕业的概率，计算达到此概率所需要的抽卡次数(inversed_mode，参数-i)，需要的参数有：
>       + 变量数(-v)：多少种卡片(目前仅支持最大5种)；
>       + 毕业的概率 (-p)：单位是 %；
>       + 每种卡片的获得概率(-ps)，单位是 % 
>  <br>

<br>
<br>
<br>
<br> 

* 命令行参数说明
> * 给定抽卡次数，计算抽齐毕业的概率(forward_mode)： 
>  `AZCalculator -f -v [变量数] -n [抽卡次数] -ps [卡片1的掉率 [[卡片2的掉率] ...] ]`  
> <br>
> <br>
> 例如：  AZCalculator -f -v 2 -n 100 -ps 0.5 2.5  代表两张卡片，掉落概率分别为0.5% 和 2.5%，求抽 100 次就能抽齐的概率。
> <br> 
> <br>
> <br>
> <br>
> * 给定毕业的概率，计算达到此概率所需要的抽卡次数(inversed_mode)： 
>  `AZCalculator -i -v [变量数] -p [毕业的概率] -ps [卡片1的掉率 [[卡片2的掉率] ...] ]`  
> <br>
> <br>
> 例如：  AZCalculator -i -v 2 -p 95.0 -ps 0.5 2.5  代表两张卡片，掉落概率分别为0.5% 和 2.5%，求要以 95% 的概率抽齐，需要抽多少次。 

<br> 
<br>
<br>
<br>

* 从源码构建
> 本工具使用 CMake 进行构建，切换到 build 目录，执行 cmake .. 即可。由于字符编码问题，Windows下的源码使用GB2312编码，Unix下的源码使用UTF-8。此举是为了避免控制台输出中文乱码。
> 可以使用链接静态Fortran运行库的方法实现生成独立的可执行程序(Standalone 版本)。

<br> 
<br>
<br>
<br>

## 下载地址：
> Windows版本：https://github.com/Houmura/GG_MY_FRIEND/releases/download/2.0_Release/GG_MY_FRIEND_win_x64.zip
> Linux版本：https://github.com/Houmura/GG_MY_FRIEND/releases/download/2.0_Release/GG_MY_FRIEND_linux_x64.zip 

<br> 
<br>
<br>
<br>

# Credit:
使用了 fccf/option 的代码来处理命令行的参数

adapted from https://github.com/fccf/option 

<br>
<br>

2021.12.17
