# Use **Vi** Easily

## *Command Mode*

### 移动*光标*

`<number=1> <arrow-key>`<br>
向指定方向移动 $number$ 次

#### 行内

- `0`<br>
  到**行首**
- `$`<br>
  到**行尾**

#### 屏幕区域

- `H`<br>
  到**屏幕顶行**行首
- `M`<br>
  到**屏幕中央**一行的行首
- `L`<br>
  到**屏幕底端**一行的行首

#### 文件范围

- `<number> G`<br>
  到**文件**的第 $number$ 行
- `G`<br>
  到**文件**的**末行**
- `g g`<br>
  到**文件**的**首行** (相当于 `1 G`)

### *复制*, *删除*, *剪切/粘贴*

- `<number=1> x`<br>
  ***删除*右侧**的 $number$ 个字符
- `<number=1> X`<br>
  ***删除*左侧**的 $number$ 个字符

#### 行内

- `y 0`<br>
  ***复制***直至**行首**
- `y $`<br>
  ***复制***直至**行尾**


- `d 0`<br>
  ***剪切***直至**行首**
- `d $`<br>
  ***剪切***直至**行尾**

#### 多行

- `<number=1> y y`<br>
  向下**整行*复制***, 共 $number$ 行
- `<number=1> d d`<br>
  向下**整行*剪切***, 共 $number$ 行


- `p`<br>
  ***粘贴***为**光标上方行**
- `P`<br>
  ***粘贴***为**光标下方行**

`J`<br>
**联结**当前行与下一行

### 复用

- `u`<br>
  ***Undo***
- `C-r`<br>
  ***Redo***

`.`<br>
**重复**前一次操作

## *Insert*/*Replace* *Mode*

### *插入*

`i`

- `o`<br>
  在光标**下方**插入**新行**
- `O`<br>
  在光标**上方**插入**新行**

### *重写*

- `r`<br>
  **替换**光标处的字符
- `R`<br>
  **替换模式**

## *Command-Line Mode*

- `:set nu <RET>`
  显示行号
- `:set nonu <RET>`
  取消行号

### *保存*与*退出*

- `:w <RET>`
- `:w! <RET>`

- `:w <filename> <RET>`<br>
  **另存为**名为 *filename* 的文件
- `:r <filename> <RET>`<br>
  将名为 *filename* 的文件内容**读取**到当前行的后面


- `:q <RET>`
- `:q! <RET>`


- `:wq <RET>`
- `:wq! <RET>`

`:! <command> <RET>`<br>
暂时切出至命令行, 查看 `command` 执行的结果 (例如, `:! ls ~`)

`Z Z`<br>
若文件未被修改, 则直接退出; 否则, 先执行保存操作.

### *查找*与*替换*

查找:

- `/ <word> <RET>`
- `? <word> <RET>`
- `n`
- `N`

替换:

```
             到第 number_2 行
              / <number_2> \                        / g  \
:<number_1>, <              > s/<word_1>/<word_2>/ <      > <RET>
              \     $      /                        \ gc /
                到最后一行                         替换前询问
```

___

&copy; 2022 Shynur \<<one.last.kiss@outlook.com>\>. All rights reserved.
