(*1. Simple Eval*)

datatype expr = NUM of int
		|PLUS of expr * expr
		|MINUS of expr * expr

datatype formula = TRUE
		   |FALSE
		   |NOT of formula
		   |ANDALSO of formula*formula
		   |ORELSE of formula*formula
		   |IMPLY of formula*formula
		   |LESS of expr*expr

fun cal e =
	case e of
		NUM i => i
		|PLUS(e1, e2) => cal(e1)+cal(e2)
		|MINUS(e1, e2) => cal(e1)-cal(e2)

fun eval f =
	case f of
		TRUE => true
		|FALSE => false
		|NOT f => not(eval(f))
		|ANDALSO(f1, f2) => (eval(f1)) andalso (eval(f2))
		|ORELSE(f1, f2) => (eval(f1)) orelse (eval(f2))
		|IMPLY(f1, f2) => (not(eval(f1))) orelse (eval(f2))
		|LESS(e1, e2) => cal(e1)<cal(e2)

		
(*2. Check MetroMap*)

type name = string

datatype metro = STATION of name
		|AREA of name*metro
		|CONNECT of metro*metro


fun checkMetro m = 
	let fun getridof(s:string, sl:string list) =
		if null sl
		then sl
		else if s = (hd sl)
		then getridof(s, tl sl)
		else
		(hd sl)::getridof(s, tl sl)

	    fun getrest m = 
		case m of
			STATION s => [s]
			|AREA(a, m1) => getridof(a, getrest m1)
			|CONNECT(m1, m2) => (getrest m1) @ (getrest m2)
	in
		null(getrest m)
	end

(*3. Lazy List*)
		
datatype 'a lazyList = nullList
		      |cons of 'a * (unit -> 'a lazyList)

fun seq(first, last) = 
	if first = last
	then cons(last, fn() => nullList)
	else cons(first, fn() => seq(first+1, last))

fun infseq(first) = 
	cons(first, fn() => infseq(first+1))

fun firstN(lazyListVal, n) =
	if n = 0
	then []
	else 
	case lazyListVal of
		nullList => []
		|cons(a, b) => a::firstN(b(), n-1)

fun Nth(lazyListVal, n) = 
	case lazyListVal of
		nullList => NONE
		|cons(a, b) => if n = 1 then SOME a else Nth(b(), n-1)	

fun filterMultiples(lazyListVal, n) = 
	case lazyListVal of
		nullList => nullList
		|cons(a, b) => if (a mod n) = 0 then filterMultiples(b(), n) else cons(a, fn() => filterMultiples(b(), n))
			       

fun primes() = 
	let
		fun sieve(cons(a, b)) = cons(a, fn() => sieve(filterMultiples(b(), a)))
	in
		sieve(infseq(2))
	end