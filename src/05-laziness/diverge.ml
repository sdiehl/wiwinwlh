let ignore x = 0;; 
let rec loop a = loop a;;

print_int (ignore (loop ()));
