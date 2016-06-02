#! /usr/bin/env nix-shell
#! nix-shell -p racket -i bash

ERR=0
function report {
    if [[ "$1" -eq 0 ]]
    then
        echo "ok - $2"
        return 0
    else
        echo "not ok - $2"
        ERR=1
        return 1
    fi
}

function namesMatch {
    EXPECT=""

    NAMES=$(echo "$2" | racket rec_names.rkt)

    [[ "x$NAMES" = "x$3" ]]
    report "$?" "Got expected names from $1" ||
        echo -e "ID: $1\nDEF: $2\nEXPECT:\n$3\nNAMES:\n$NAMES\n\n" 1>&2
}

namesMatch "datatype" \
           "(declare-datatypes (a) ((list (nil) (cons (head a) (tail (list a))))))" \
           "list
nil
cons
head
tail"

namesMatch "function" \
           "(define-fun sort2 ((x Int) (y Int)) (list Int) (ite (<= x y) (cons x (cons y (as nil (list Int)))) (cons y (cons x (as nil (list Int))))))" \
           "sort2"

namesMatch "parameterised function" \
           "(define-fun (par (a) (zsplitAt ((x Int) (y (list a))) (Pair (list a) (list a)) (Pair2 (ztake x y) (zdrop x y)))))" \
           "zsplitAt"

namesMatch "recursive function" \
           "(define-fun-rec insert2 ((x Int) (y (list Int))) (list Int) (match y (case nil (cons x (as nil (list Int)))) (case (cons z xs) (ite (<= x z) (cons x y) (cons z (insert2 x xs))))))" \
           "insert2"

namesMatch "recursive parameterised function" \
           "(define-fun-rec (par (a) (ztake ((x Int) (y (list a))) (list a) (ite (= x 0) (as nil (list a)) (match y (case nil (as nil (list a))) (case (cons z xs) (cons z (ztake (- x 1) xs))))))))" \
           "ztake"

namesMatch "mutually recursive functions" \
           "(define-funs-rec ((stooge2sort2 ((x (list Int))) (list Int)) (stoogesort2 ((x (list Int))) (list Int)) (stooge2sort1 ((x (list Int))) (list Int))) ((match (zsplitAt (div (+ (* 2 (zlength x)) 1) 3) x) (case (Pair2 ys zs) (append (stoogesort2 ys) zs))) (match x (case nil (as nil (list Int))) (case (cons y z) (match z (case nil (cons y (as nil (list Int)))) (case (cons y2 x2) (match x2 (case nil (sort2 y y2)) (case (cons x3 x4) (stooge2sort2 (stooge2sort1 (stooge2sort2 x))))))))) (match (zsplitAt (div (zlength x) 3) x) (case (Pair2 ys zs) (append ys (stoogesort2 zs))))))" \
           "stooge2sort2
stoogesort2
stooge2sort1"

exit "$ERR"
