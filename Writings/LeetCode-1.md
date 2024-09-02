> 在 $3 \times 3$ 的空格内, 用 $\{1, 2, ..., 9\}$ 分别填入空格内, 使每行数字组成的三位十进制数成为*完全平方数*.

```lisp
;; Common Lisp
(loop :for ls := (list 1) :then
      (if (and (zerop (mod (length ls) 3))
               (let ((num (+ (nth 0 ls) (* 10 (nth 1 ls)) (* 100 (nth 2 ls)))))
                 (/= num (expt (isqrt num) 2)))) ; 若不合要求
          (loop :do (let (next-number)           ; 则先回滚, 再剪枝, 再向后遍历
                      (mapcar #'(lambda (number)
                                  (when (> number (nth 0 ls))
                                    (setq next-number number)))
                              (sort (set-difference '(1 2 3 4 5 6 7 8 9) ls) #'>))
                      (if next-number
                          (return (cons next-number (cdr ls))) ; 完成剪枝, 向后遍历
                          (setq ls (cdr ls))))) ; 回滚, 继续剪枝
          (or (dolist (number '(1 2 3 4 5 6 7 8 9))    ; DFS
                (unless (equal ls (pushnew number ls)) ;
                  (return ls)))                        ;
              (return (nreverse ls)))))
```

求值结果:

```
(3 6 1 5 2 9 7 8 4)
```

$361 = 19^2, 529 = 23^2, 784 = 28^2.  $
