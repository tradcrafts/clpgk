;; -*- coding: utf-8 -*-

(in-package :clap)
(common-header :xi)

;;;;;;;;;;;;;;;;;;;;;;  組み込みデータ型の定義 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Maybe
;; 定数nothingと、データコンストラクタjust が定義される

(define-data maybe nothing (just t))
(define-internal-data maybe# nothing# (just# t))



(\Xi+ (datatype <maybe>
                
                ________
                (@p nothing# []) : nothing\;
                
                X : A\;
                ======
                (@p just# X) : (just A)\;
                
                ;let A (newsym a)
                N : nothing\;
                ====
                N : (maybe A)\;
                
                J : (just A)\;
                ====
                J : (maybe A)\;)
      .)


                

(\Xi+ (define fromJust {(maybe A) --> A}
              (@p just# X) -> X
              _ -> (error "fromJust: nothing is not allowed"))

      (define just {A --> (just A)}
              X -> (@p just# X))

      (define isJust {(maybe A) --> boolean}
              (@p just# _) -> true
              _ -> false)

      (define isNothing {(maybe A) --> boolean}
              (@p nothing# []) -> true
              _ -> false)     

      (define fromMaybe {A --> (maybe A) --> A}
              A (@p nothing# []) -> A
              _ (@p just# A) -> A)

      (define maybeToList {(maybe A) --> [A]}
              (@p just# A) -> [A]
              _ -> [])

      (define listToMaybe {[A] --> (maybe A)}
              [] -> (@p nothing# [])
              [X\|_] -> (@p just# X))

      (define catMaybes {[(maybe A)] --> [A]}
              [] -> []
              [(@p just# X)\|Rest] -> [X\|(catMaybes Rest)]
              [_\|Rest] -> (catMaybes Rest))

      (define mapMaybe {(A --> (maybe B)) --> [A] --> [B]}
              F XS -> (catMaybes (map F XS)))
      
      (define maybe {B --> (A --> B) --> (maybe A) --> B}
              _ F (@p just# X) -> (F X)
              Default _ _ -> Default)
      .)



(\Xi+ (datatype <Functor/Maybe>
                X : (maybe A)\;
                ____
                X : (=> @Functor (maybe A))\;).)

(defmethod ~fmap (f (x nothing))
  (declare (ignore f))
  x)

(defmethod ~fmap (f (x just))
  (just (funcall f (xi:tuple-snd x))))
