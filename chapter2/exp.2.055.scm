
(car ''asfdsfsdfsd)
quote
;; 首先明确 ' 是(quote的语法糖衣, 等价于
(car (quote (quote asfdsfsdfsd)))
;; 在符号表(quote asfdsfsdfsd)上使用car获得就是符号
quote


