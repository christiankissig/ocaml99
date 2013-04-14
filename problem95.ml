(*# P95 (**) English number words
# 
# On financial documents, like cheques, numbers must sometimes be written in
# full words. Example: 175 must be written as one-seven-five. Write a predicate
# full_words/1 to print (non-negative) integer numbers in full words.*)

let rec full_words n =
        (* above a billion it's all repetition *)
        if n >= 1000000000 then "" else

        if n >= 1000000 
        then
                let x = n / 1000000
                and y = n mod 1000000 in
                Printf.sprintf "%s million %s" 
                (full_words x) 
                (if (y>0) then (full_words y) else "")
        else

        if n >= 1000
        then
                let x = n / 1000
                and y = n mod 1000 in
                Printf.sprintf "%s thousand %s" 
                (full_words x) 
                (if (y>0) then (full_words y) else "")
        else

        if n >= 100
        then
                let x = n / 100
                and y = n mod 100 in
                Printf.sprintf "%s hundred %s" 
                (full_words x) 
                (if (y>0) then (full_words y) else "")
        else

        let ties n =
                match n with
                2 -> "twenty"
                | 3 -> "thirty"
                | 4 -> "fourty"
                | 5 -> "fifty"
                | 6 -> "sixty"
                | 7 -> "seventy"
                | 8 -> "eighty"
                | 9 -> "ninety"
                | _ -> ""
        in

        if n >= 20
        then
                let x = n / 10
                and y = n mod 10 in
                Printf.sprintf "%s %s" 
                (ties x) 
                (if (y>0) then (full_words y) else "")
        else

        match n with
        0 -> "zero"
        | 1 -> "one"
        | 2 -> "two"
        | 3 -> "three"
        | 4 -> "four"
        | 5 -> "five"
        | 6 -> "six"
        | 7 -> "seven"
        | 8 -> "eight"
        | 9 -> "nine"
        | 10 -> "ten"
        | 11 -> "eleven"
        | 12 -> "twelve"
        | 13 -> "thirteen"
        | 14 -> "fourteen"
        | 15 -> "fifteen"
        | 16 -> "sixteen"
        | 17 -> "seventeen"
        | 18 -> "eighteen"
        | 19 -> "nineteen"
        | _ -> ""
;;

let n = 23904512 in
Printf.printf "%d : %s" n (full_words n);;
