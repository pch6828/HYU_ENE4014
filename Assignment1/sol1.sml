(*I will use some of these functions from the lecture*)

fun append(xs:int list, ys:int list) = 
	if null xs
	then ys
	else (hd xs)::append(tl xs, ys);

fun sum_list (xs: int list) = 
	if null xs
	then 0
	else hd(xs)+sum_list(tl(xs));

fun merge(xs:int list, ys:int list) =
	(if null xs 
	then ys
	else if null ys 
	then xs
	else if (hd xs) < (hd ys) 
	then (hd xs :: merge(tl xs, ys))
	else (hd ys :: merge(xs, tl ys)));

fun reverse(xs:int list) = 
	if null xs
	then xs
	else append(reverse(tl xs), (hd xs::[]));

fun sigma(a:int, b:int, f:(int->int)) = 
	if a>b
	then 0
	else f(a) + sigma(a+1, b, f);

fun digits(n:int) = 
	if n = 0
	then []
	else reverse((n mod 10)::reverse(digits(n div 10)));

fun additivePersistence(n:int) = 
	if n div 10 = 0
	then 0
	else 1+additivePersistence(sum_list(digits(n)));

fun digitalRoot(n:int) = 
	if n div 10 = 0
	then n
	else digitalRoot(sum_list(digits(n)));
