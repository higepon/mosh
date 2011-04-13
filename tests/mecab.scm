(import (rnrs)
        (mosh test)
        (mosh control)
        (mosh ffi)
        (mecab))

(let1 m (mecab-new2 "")
  (test-false (pointer-null? m))
  (let* ([text (string->utf8 "僕はお腹がすいた")]
         [len (bytevector-length text)])
    (test-equal "僕\t名詞,代名詞,一般,*,*,*,僕,ボク,ボク\nは\t助詞,係助詞,*,*,*,*,は,ハ,ワ\nお腹\t名詞,一般,*,*,*,*,お腹,オナカ,オナカ\nが\t助詞,格助詞,一般,*,*,*,が,ガ,ガ\nすい\t動詞,自立,*,*,五段・カ行イ音便,連用タ接続,すく,スイ,スイ\nた\t助動詞,*,*,*,特殊・タ,基本形,た,タ,タ\nEOS\n"
                (mecab-sparse-tostr2 m text len))
    (let loop ([node (mecab-sparse-tonode2 m text len)]
               [surface* '()]
               [feature* '()])
      (cond
       [(pointer-null? node)
        (test-equal '("" "僕" "は" "お腹" "が" "すい" "た" "") (reverse surface*))
        (test-equal '(("BOS/EOS" #f #f #f #f #f #f #f #f)
                      ("名詞" "代名詞" "一般" #f #f #f "僕" "ボク" "ボク")
                      ("助詞" "係助詞" #f #f #f #f "は" "ハ" "ワ")
                      ("名詞" "一般" #f #f #f #f "お腹" "オナカ" "オナカ")
                      ("助詞" "格助詞" "一般" #f #f #f "が" "ガ" "ガ")
                      ("動詞" "自立" #f #f "五段・カ行イ音便" "連用タ接続" "すく" "スイ" "スイ")
                      ("助動詞" #f #f #f "特殊・タ" "基本形" "た" "タ" "タ")
                      ("BOS/EOS" #f #f #f #f #f #f #f #f))
                    (reverse feature*))]
       [else
        (loop (mecab-node-next node)
              (cons (mecab-node-surface node) surface*)
              (cons (mecab-node-feature node) feature*))]))
    (test-equal '("僕" "は" "お腹" "が" "すい" "た") (mecab-node-surface* (mecab-sparse-tonode2 m text len)))
    (mecab-destroy m)))

(test-results)
