
#|
 a or b 
 !(!a and !b)
 使用非门与门表示或门
 a -> !a -> and
 b -> !b -> and -> !and
|#
(define (or-gate a b output)
  (let ((!a-wire (make-wire))
        (!b-wire (make-wire))
        (and-wire (make-wire)))
    (inverter a !a-wire)
    (inverter b !b-wire)
    (and-gate !a-wire !b-wire and-wire)
    (inverter and-wire output)
    'ok))
#|
  思考的时候抓耳挠腮
  写的时候顺理成章
  同时也反映了信息流方式的优越性
  过程与程序有效贴合
|#


