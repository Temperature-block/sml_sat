val boolToString = fn
    true => "true"
  | false => "false"

fun print_list lst =
    let
        fun to_string [] = ""
          | to_string [x] = Int.toString x  (* Convert the last element without a comma *)
          | to_string (x::xs) = Int.toString x ^ ", " ^ to_string xs  (* Convert and append *)
    in
        print (to_string  lst ^ "\n")  (* Print the resulting string followed by a newline *)
    end
fun print_string_list lst =
    let
        fun to_string [] = ""
          | to_string [x] = x  (* Convert the last element without a comma *)
          | to_string (x::xs) = x ^ ", " ^ to_string xs  (* Convert and append *)
    in
        print (to_string lst ^ "\n")  (* Print the resulting string followed by a newline *)
    end

fun print_ll [] = ()
  | print_ll (lst::lsts) = (print_string_list lst; print_ll lsts)

fun print_and_exit msg = 
  (print (msg ^ "exittt \n"); OS.Process.exit OS.Process.success)

fun stringToInt (s: string) =
    case Int.fromString s of
        SOME n => n
      | NONE => print_and_exit "Invalid integer string"

fun split s delim =
   let
      (* Helper function to find the index of the first occurrence of `delim` in the string *)
      fun find_delim str delim =
         let
            fun aux i =
               if i >= String.size str then NONE
               else if String.sub (str, i) = delim then SOME i
               else aux (i + 1)
         in
            aux 0
         end
      
      (* Auxiliary function for splitting the string *)
      fun aux str acc =
         case find_delim str delim of
             NONE => List.rev (str :: acc)  (* No delimiter found, add the remaining string *)
           | SOME i =>
               let
                  val before_ = String.substring (str, 0, i)  (* Extract substring before delimiter *)
                  val after = String.substring (str, i + 1, String.size str - (i + 1))  (* Extract substring after delimiter *)
               in
                  aux after (before_ :: acc)  (* Recursively call with the rest of the string *)
               end
   in
      aux s []  (* Initial call with an empty accumulator *)
   end

fun flt carr numclause numvars =
    if carr = "" then ([],numclause,numvars)
    else
let
    fun check_and_ret_arr arr xch nc nv =
        if xch = "p" andalso numclause+numvars < nc+nv then [] 
        else if xch = "c" then []
        else if (List.last arr) <> "0\n" then  print_and_exit ("Expected 0 at end Got " ^ (List.last arr) ^ boolToString(((List.last arr) <> "0")) ^ boolToString(("0" = "0")))
             else if List.exists (fn y => (Int.fromString y) = NONE ) arr then print_and_exit "NON digit string detected"
                  else arr

    val ncarr = (split carr #" ")
    val x = List.nth(ncarr,0)
    val (nclause,nvars) = if numclause < 0 andalso numvars < 0 andalso x = "p" then 
                             if List.length ncarr <> 4 then print_and_exit "check your p cnf"
                             else (fn (x,y) => if x<0 orelse y<0 then print_and_exit ">0 exp" else (x,y))((stringToInt (List.nth (ncarr,3))),(stringToInt (List.nth (ncarr,2))))
                          else
                            (numclause,numvars)

    val rarr = check_and_ret_arr ncarr x nclause nvars
in
    (rarr,nclause,nvars)
end
    
fun read_andfilter_file file track clause_num max_var=
let 
   fun check_and_ret trk cn vn = let
    val fltrd = List.filter (fn x => x <> []) trk
    val _ = print_ll trk
   val arr = if List.length fltrd <> cn then print_and_exit ("clause num inconsistent with pcnf " ^ (Int.toString (List.length fltrd))) 
              else List.map (fn sublist => List.take(sublist, List.length(sublist) - 1)) fltrd
    in (arr,clause_num,max_var,[]) end

   val line = TextIO.inputLine file;
   val _ = if line = NONE then TextIO.closeIn file else ();
in
case line of
  NONE => (check_and_ret track clause_num max_var)
   | SOME text => let 
  val (narr,clause,max) = flt text clause_num max_var
  val _ = print("\nrfil\n")
in 
  read_andfilter_file file (track @ [narr]) clause max end
end

fun slause_to_int arr = 
    case arr of
         [] => []
        |x::xs => [stringToInt x]@ (slause_to_int xs)

fun get_clause (arr,nclause,nvars,track) = 
    let 

    fun check ar =
        case ar of
        [] => ()
        | x::xs => if (abs x) > nvars then print_and_exit "More vars than necessary in clause" else check xs

    val y = List.nth(arr,0)
    val toint  = slause_to_int y
    val _ = check toint
    in
    case arr of
    [] => (track,nclause,nvars)
    | x::[] => ((track @ [toint]),nclause,nvars)
    | x::xs => get_clause(xs,nclause,nvars,(track @ [toint])) end


fun parse path =
   (* exit if it isnt well formed cnf file
   ignore edge cases for now later we check them and handle appropriately*)
let
   val clause = get_clause (read_andfilter_file (TextIO.openIn path) [] ~1 ~1);
in
   clause
end

fun solve (arr,nclause,nvars) = 
let
fun make_partial vars track =
    (* -1 to indicate variable hasnt been assigned any true or false 0 for false and 1 for true *)
    if vars = 0 then track else make_partial (vars - 1) (track @ [~1])

fun eval (clause : int list) (plist : int list) =
    (* -1 indicating that we cannot evaluate fully 0 for false and 1 for true *)
    let
    val _ = print_list clause
    val _ = print("debug\n")
    fun get_tvals num =
        case num of 
        0 => false
        | 1 => true
        | _ => print_and_exit "wrong"

    fun rev_tvals truthy =
        case truthy of
        false => 0
        | true => 1
        

    (* fun get_each cls = List.foldl (fn (x,acc) => acc @ [List.nth (plist, (abs (x)))]) [] cls *)
fun get_each cls =
    List.foldl (fn (x, acc) =>
        let
            val _ = print_list plist
            val _ = print ("finding" ^Int.toString x ^ "\n")
            val value = List.nth (plist, (abs (x) - 1))  (* Get the value from plist *)
            val _ = print (Int.toString value ^ "\n")  (* Print the value *)
        in
            acc @ [value]  (* Append the value to the accumulator *)
        end) [] cls
    
    fun aply ocls cstates = case (ocls,cstates) of
                           ([],[]) => false
                           | ([],_) => false
                           | (_,[]) => false
                           | (x::xs,y::ys) => if x<0 then not (get_tvals y) orelse (aply xs ys) else (get_tvals y) orelse (aply xs ys)

    val states = get_each clause
        val _ = print("geach")
    val check = if List.exists (fn x => x = ~1) states then ~1 else rev_tvals (aply clause states)
    val _ = print_list [check]
    in
    check end  

fun run_DPLL clauses plist = 
    let
    val _ = print("dpll\n")
fun plist_modif num plist_ =
    let
        (* Helper function to recursively traverse the list and modify the first ~1 *)
        fun modify (x::xs) = 
    if x = ~1 then 
        num::xs  (* Replace the first occurrence of ~1 and return the rest unchanged *)
    else 
        x::modify(xs)  (* Continue traversing the list and keep 'x' as is *)
  | modify xs = xs  (* If no ~1 is found, return the list as is *)

    in
        modify plist_
    end

    fun check_status arr = if List.exists (fn x => x = 0) arr then 0 else if List.exists (fn x => x = ~1) arr then ~1 else 1
      
    val _ = if List.exists (fn x => x = []) clauses then print_and_exit "UNSAT" else ()

    (* we should short circuit early as this isnt the most efficient but eh this is a prototype *)

    val states = List.foldl (fn (x,acc) => acc @ [(eval x plist)]) [] clauses

    (* if overall status is true and there are ~1 then pick next ~1 in plist to modify 
       if there is a 0 then return we can encode three states for our dpll
       ~1 to indicate case 1, 0 to indicate case 2,1 for case three for case three we can just print SAT 
       and call it a day forget SAT proof for now as its a prototype *)

    val check = check_status states
    val _ = print("iter\n")
    val overall = if check = ~1 then let val z = plist_modif 0 plist
                                         val on = plist_modif 1 plist
                     in 
                     if (run_DPLL clauses z) = 1 then 1
                     else (run_DPLL clauses on) end 
                  else check

    in
    overall
    end

val partial_list = make_partial nvars []
val _ = print_list partial_list
(*val _ = print_list arr*)
in
run_DPLL arr partial_list
end

fun solve_each args =
case args of
    [] => print("nothing more to solve\n")
    |x::xs => let val _ = print("test ")
                  val sol = solve (parse x)
                  val _ = if sol = 0 then print ("UNSAT") else print ("SAT") in solve_each xs end

fun main () = 
let
    val args = CommandLine.arguments() ;
val _ = solve_each args
    in
    ()
end
