
#|
  如果基于exp.3.025进行修改的话
  需要修改两个过程 assoc 与 insert-iter
  assoc 的检索过程
  insert-iter 中的新增过程
  chapter:2.3.3 关于 二叉树的描述, 首先key必须是可以排序的
  node ((keybase val) left right)
  null? node => #f
  key == keybase => val
  key < keybase 搜索 left
  key > keybase 搜索 right
  新增的的时候类似，但是现在的我需要使用set!处理
  但是在2.3.3里必然没有使用这种法是处理
  我重新找到了原来的过程
  使用递归的方式重建二叉树叶子
|#
