datatype pattern = Wildcard 
		| Variable of string 
		| UnitP
		| ConstP of int
		| TupleP of pattern list
		| ConstructorP of string * pattern

datatype valu = Const of int
		| Unit
		| Tuple of valu list
		| Constructor of string * valu

(*1. check_pat*)

fun make_list p = 
	case p of
		Variable s => [s]
		|ConstructorP(s, pt) => make_list pt
		|TupleP pl => foldl(fn (p, acc) => acc @ make_list p) [] pl
		|_ => []	

fun is_set l = 
	case l of
		[] => true
		|x::xs => (not(List.exists(fn a => a = x) xs)) andalso is_set(xs)

fun check_pat p = 
	let 
		val pl = make_list p
	in
		is_set pl
	end

(*2. match*)

fun match(v, p) = 
	case p of
		Wildcard => SOME []
		|Variable s => SOME [(s, v)]
		|UnitP =>
			(case v of
				Unit => SOME []
				|_ => NONE)
		|ConstP n =>
			(case v of
				Const m => if n = m then SOME [] else NONE
				|_ => NONE)
		|TupleP pl =>
			let fun allmatch(vpl) = 
				(case vpl of
					[] => SOME []
					|(v, p)::xs => let val res1 = match(v, p); val res2 = allmatch(xs) in if (isSome res1) andalso (isSome res2) then SOME ((Option.valOf res1)@(Option.valOf res2)) else NONE end)
			in
				(case v of
					Tuple vl =>
						if (length vl) = (length pl)
						then allmatch(ListPair.zip(vl, pl))
						else NONE
					|_ => NONE)
			end
		|ConstructorP(s1, pt) =>
			(case v of
				Constructor(s2, vl) => if s1 = s2 then match(vl, pt) else NONE
				|_ => NONE)

(*3. Rock, Scissors, Paper*)

type name = string

datatype RSP = ROCK
		|SCISSORS
		|PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament = PLAYER of name * (RSP strategy ref)     
			| MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))

fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))

fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))

fun next(strategyRef) = 
	let 
		val Cons(rsp, func) = !strategyRef 
	in
		strategyRef := func();
		rsp
	end

fun RSP_game(PLAYER(n1, s1), PLAYER(n2, s2)) = 
	let
		val g = (next(s1), next(s2))
	in
		(case g of
			(ROCK, SCISSORS) => PLAYER(n1, s1)
			|(ROCK, PAPER) => PLAYER(n2, s2)
			|(SCISSORS, ROCK) => PLAYER(n2, s2)
			|(SCISSORS, PAPER) => PLAYER(n1, s1)
			|(PAPER, ROCK) => PLAYER(n1, s1)
			|(PAPER, SCISSORS) => PLAYER(n2, s2)
			|_ => RSP_game(PLAYER(n1, s1), PLAYER(n2, s2)))
	end

fun whosWinner(t) =
	case t of
		PLAYER (n, st) => PLAYER(n, st)
		|MATCH (t1, t2) => RSP_game(whosWinner(t1), whosWinner(t2))

val r = onlyOne(ROCK); 
val s = onlyOne(SCISSORS); 
val p = onlyOne(PAPER); 
val rp = alterTwo(ROCK, PAPER); 
val sr = alterTwo(SCISSORS, ROCK);
val ps = alterTwo(PAPER, SCISSORS); 
val srp = alterThree(SCISSORS, ROCK, PAPER);

val winner = whosWinner(MATCH(PLAYER("s", ref s), MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r))));