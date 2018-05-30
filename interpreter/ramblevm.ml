(* Put your implementation in this file. *)

open Instr
open Disassembler
exception Error
exception HError
exception Halt  

type prog_ret = [ `Reg of value | `Halt of value ]

(* generate fresh location int *)
let fresh_loc = 
    let n = ref 0 in
    fun () -> (let temp = !n in n := !n + 1; temp)

let max_of_inst (i:instr):int = match i with
  | I_const (`L_Reg r,_) -> r
  | I_mov (`L_Reg r1, `L_Reg r2) -> if (r1 > r1) then r1 else r2
  | I_add (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_sub (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_mul (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_div (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_eq (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_lt (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_leq (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_is_int (`L_Reg r1, `L_Reg r2) -> if (r1 > r1) then r1 else r2
  | I_is_str (`L_Reg r1, `L_Reg r2) -> if (r1 > r1) then r1 else r2
  | I_is_tab (`L_Reg r1, `L_Reg r2) -> if (r1 > r1) then r1 else r2
  | I_if_zero (`L_Reg r,_) -> r
  | I_rd_glob (`L_Reg r,_) -> r
  | I_wr_glob (_,`L_Reg r) -> r
  | I_mk_tab (`L_Reg r) -> r
  | I_rd_tab (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_wr_tab (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_has_tab (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_call (`L_Reg r1,r2,r3) -> if (r1 > r2) then (if r1 > r3 then r1 else r3) else if (r2 > r3) then r2 else r3
  | I_ret (`L_Reg r) -> r
  | I_halt (`L_Reg r) -> r
  | _ -> raise Error
;;

let max_regs (p:prog) (f:string):int =
    let func = if (Hashtbl.mem p f) then (Hashtbl.find p f) else raise Error in
    let max = Array.fold_left (fun a i -> if ((max_of_inst i) > a) then (max_of_inst i) else a) (-1) func in
    max
;; 

let current_inst (p:prog) ((h, s):config):instr = 
    match s with
        | [] -> raise Error
        | ((f,pc,_)::_) -> let func = if (Hashtbl.mem p f) then (Hashtbl.find p f) else raise Error in
                           if (pc > Array.length func) then raise Error else (Array.get func pc)
;;

let run_inst (p:prog) ((h, ((id,pc,rs)::s1 as s)):config):config = match (current_inst p (h,s))  with
  | I_const (`L_Reg r, v) -> Hashtbl.replace rs r v;
        (h, (id,(pc+1),rs)::s1)
  | I_mov (`L_Reg r1, `L_Reg r2) -> (if (Hashtbl.mem rs r2) then (Hashtbl.replace rs r1 (Hashtbl.find rs r2)) else raise Error);
        (h, (id,(pc+1),rs)::s1)
  | I_add (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> 
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          (match v2,v3 with
            | (`L_Int a), (`L_Int b) -> Hashtbl.replace rs r1 (`L_Int (a + b));
            | _,_ -> raise Error
          );
        (h, (id,(pc+1),rs)::s1)
  | I_sub (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          (match v2,v3 with
            | (`L_Int a), (`L_Int b) -> Hashtbl.replace rs r1 (`L_Int (a - b));
            | _,_ -> raise Error
          );
        (h, (id,(pc+1),rs)::s1)
  | I_mul (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          (match v2,v3 with
            | (`L_Int a), (`L_Int b) -> Hashtbl.replace rs r1 (`L_Int (a * b));
            | _,_ -> raise Error
          );
        (h, (id,(pc+1),rs)::s1)
  | I_div (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          (match v2,v3 with
            | (`L_Int a), (`L_Int b) -> Hashtbl.replace rs r1 (`L_Int (a / b));
            | _,_ -> raise Error
          );
        (h, (id,(pc+1),rs)::s1)
  | I_eq (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> 
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          (match v2,v3 with
            | (`L_Int a), (`L_Int b) -> if (a = b) then (Hashtbl.replace rs r1 (`L_Int 1)) else (Hashtbl.replace rs r1 (`L_Int 0));
            | (`L_Str a), (`L_Str b) -> if (a = b) then (Hashtbl.replace rs r1 (`L_Int 1)) else (Hashtbl.replace rs r1 (`L_Int 0));
            | (`L_Id a), (`L_Id b) -> if (a = b) then (Hashtbl.replace rs r1 (`L_Int 1)) else (Hashtbl.replace rs r1 (`L_Int 0));
            | (`L_Loc a), (`L_Loc b) -> if (a = b) then (Hashtbl.replace rs r1 (`L_Int 1)) else (Hashtbl.replace rs r1 (`L_Int 0));
            | _,_ -> raise Error
          );
        (h, (id,(pc+1),rs)::s1)
  | I_lt (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> 
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          (match v2,v3 with
            | (`L_Int a), (`L_Int b) -> if (a < b) then (Hashtbl.replace rs r1 (`L_Int 1)) else (Hashtbl.replace rs r1 (`L_Int 0));
            | _,_ -> raise Error
          );
        (h, (id,(pc+1),rs)::s1)
  | I_leq (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          (match v2,v3 with
            | (`L_Int a), (`L_Int b) -> if (a <= b) then (Hashtbl.replace rs r1 (`L_Int 1)) else (Hashtbl.replace rs r1 (`L_Int 0));
            | _,_ -> raise Error
          );
        (h, (id,(pc+1),rs)::s1)
  | I_is_int (`L_Reg r1, `L_Reg r2) -> 
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          (match v2 with
            | (`L_Int _) -> (Hashtbl.replace rs r1 (`L_Int 1));
            | _ -> (Hashtbl.replace rs r1 (`L_Int 0));
          );
        (h, (id,(pc+1),rs)::s1)
  | I_is_str (`L_Reg r1, `L_Reg r2) -> 
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          (match v2 with
            | (`L_Str _) -> (Hashtbl.replace rs r1 (`L_Int 1));
            | _ -> (Hashtbl.replace rs r1 (`L_Int 0));
          );
        (h, (id,(pc+1),rs)::s1)
  | I_is_tab (`L_Reg r1, `L_Reg r2) ->
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          (match v2 with
            | (`L_Loc _) -> (Hashtbl.replace rs r1 (`L_Int 1));
            | _ -> (Hashtbl.replace rs r1 (`L_Int 0));
          );
        (h, (id,(pc+1),rs)::s1)
  | I_jmp n -> 
        (h, (id,(pc+1+n),rs)::s1)
  | I_if_zero (`L_Reg r,n) ->
          let v = (if (Hashtbl.mem rs r) then Hashtbl.find rs r else raise Error) in 
          (match v with
            | (`L_Int a) when (a = 0) -> (h, (id,(pc+1+n),rs)::s1)
            | _ -> (h, (id,(pc+1),rs)::s1)
          )     
  | I_rd_glob (`L_Reg r, ((`L_Id _) as id2)) ->
          let v = (if (Hashtbl.mem h id2) then Hashtbl.find h id2 else raise Error) in
          Hashtbl.replace rs r v;
        (h, (id,(pc+1),rs)::s1)
  | I_wr_glob (((`L_Id _) as id2),`L_Reg r) ->
          let v = (if (Hashtbl.mem rs r) then Hashtbl.find rs r else raise Error) in
          Hashtbl.replace h id2 v;
        (h, (id,(pc+1),rs)::s1)
  | I_mk_tab (`L_Reg r) ->
          let m = fresh_loc () in
          let t = Hashtbl.create 10 in
          Hashtbl.replace h (`L_Loc m) (`L_Tab t); 
          Hashtbl.replace rs r (`L_Loc m);
        (h, (id,(pc+1),rs)::s1)
  | I_rd_tab (`L_Reg r1, `L_Reg r2, `L_Reg r3) -> 
          let l2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          let v' = (if (Hashtbl.mem h l2) then
                (match (Hashtbl.find h l2) with
                    | `L_Tab t -> Hashtbl.find t v3
                    | _ -> raise Error
                ) else raise Error) in
          Hashtbl.replace rs r1 v';
        (h, (id,(pc+1),rs)::s1)
  | I_wr_tab (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
          let l1 = (if (Hashtbl.mem rs r1) then Hashtbl.find rs r1 else raise Error) in
          let v2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in 
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          (match (Hashtbl.find h l1) with
             | `L_Tab t ->  Hashtbl.replace t v2 v3
             | _ -> raise Error
          );
        (h, (id,(pc+1),rs)::s1)
  | I_has_tab (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
          let l2 = (if (Hashtbl.mem rs r2) then Hashtbl.find rs r2 else raise Error) in
          let v3 = (if (Hashtbl.mem rs r3) then Hashtbl.find rs r3 else raise Error) in 
          (if (Hashtbl.mem h l2) then
              (match (Hashtbl.find h l2) with
                | `L_Tab t -> if (Hashtbl.mem t v3) then Hashtbl.replace rs r1 (`L_Int 1) else
                 Hashtbl.replace rs r1 (`L_Int 0))
          else raise Error);
        (h, (id,(pc+1),rs)::s1)
  | I_call (`L_Reg r1,n1,n2) ->
          let new_regs = Hashtbl.create 10 in
          for i = n1 to n2 do
              Hashtbl.replace new_regs (i-n1) (Hashtbl.find rs i)
          done;
          if (n2 < n1) then Hashtbl.clear new_regs;
          let id2' = (if (Hashtbl.mem rs r1) then (Hashtbl.find rs r1) else raise Error) in
          let id2 = (match id2' with
                        | `L_Id name -> name 
                        | _ -> raise Error
          ) in

          if ((id2 = "print_string") && (not (Hashtbl.mem p id2))) then 
              if (n1 = n2) then
                (match (Hashtbl.find rs n1) with
                    | `L_Str s -> print_string s; (Hashtbl.replace rs n1 (`L_Str s)); (h, (id,(pc+1),rs)::s1)  
                    | _ -> raise Error
                )
              else raise Error
          else

          if ((id2 = "print_int") && (not (Hashtbl.mem p id2))) then 
              if (n1 = n2) then
                (match (Hashtbl.find rs n1) with
                    | `L_Int s -> print_int s; (Hashtbl.replace rs n1 (`L_Int s)); (h, (id,(pc+1),rs)::s1)  
                    | _ -> raise Error
                )
              else raise Error
          else

          if ((id2 = "to_s") && (not (Hashtbl.mem p id2))) then 
              if (n1 = n2) then
                (match (Hashtbl.find rs n1) with
                    | `L_Str s -> (Hashtbl.replace rs n1 (`L_Str s)); (h, (id,(pc+1),rs)::s1)  
                    | `L_Int s -> (Hashtbl.replace rs n1 (`L_Str (string_of_int s))); (h, (id,(pc+1),rs)::s1)  
                    | `L_Id s -> (Hashtbl.replace rs n1 (`L_Str ("Function<" ^ s ^ ">"))); (h, (id,(pc+1),rs)::s1)  
                    | _ -> raise Error
                )
              else raise Error
          else

          if ((id2 = "to_i") && (not (Hashtbl.mem p id2))) then 
              if (n1 = n2) then
                (match (Hashtbl.find rs n1) with
                    | `L_Int s -> (Hashtbl.replace rs n1 (`L_Int s)); (h, (id,(pc+1),rs)::s1)  
                    | `L_Str s -> (Hashtbl.replace rs n1 (`L_Int (int_of_string s))); (h, (id,(pc+1),rs)::s1)  
                    | _ -> raise Error
                )
              else raise Error
          else

          if ((id2 = "concat") && (not (Hashtbl.mem p id2))) then 
              if (n1 = (n2-1)) then
                (match (Hashtbl.find rs n1),(Hashtbl.find rs n2) with
                    | (`L_Str st1),(`L_Str st2) -> (Hashtbl.replace rs n1 (`L_Str (st1^st2))); (h, (id,(pc+1),rs)::s1)  
                    | _,_ -> raise Error
                )
              else raise Error
          else

          if ((id2 = "length") && (not (Hashtbl.mem p id2))) then 
              if (n1 = n2) then
                (match (Hashtbl.find rs n1) with
                    | `L_Str s -> (Hashtbl.replace rs n1 (`L_Int (String.length s))); (h, (id,(pc+1),rs)::s1)  
                    | _ -> raise Error
                )
              else raise Error
          else

          if ((id2 = "size") && (not (Hashtbl.mem p id2))) then 
              if (n1 = n2) then
                  (match (Hashtbl.find rs n1) with
                    | (`L_Loc _) as l ->
                        (match (Hashtbl.find h l) with
                            | `L_Tab t -> Hashtbl.replace rs n1 (`L_Int (Hashtbl.length t)); (h, (id,(pc+1),rs)::s1)
                            | _ -> raise Error)
                    | _ -> raise Error)  
              else raise Error
          else

          if ((id2 = "iter") && (not (Hashtbl.mem p id2))) then 
              let arg1 = n1 in
              let arg2 = (n1+1) in
              let arg3 = n2 in
              let l = match (Hashtbl.find rs arg1) with (`L_Loc a) -> (`L_Loc a) | _ -> raise Error in
              let name = (match (Hashtbl.find rs arg2) with (`L_Id s) -> s | _ -> raise Error) in

              if (arg3 = (arg1+2)) then
                  (match (Hashtbl.find h l) with
                    | `L_Tab t ->
                       let lst = Hashtbl.fold (fun k v a -> (k,v)::a) t [] in
                       let rec build_call_stack l stac = (match l with
                          | [] -> stac
                          | (k,v)::rest -> let args = Hashtbl.create 3 in
                                Hashtbl.replace args (0) k;
                                Hashtbl.replace args (1) v;
                                Hashtbl.replace args (2) (Hashtbl.find rs arg3);
                                Hashtbl.replace rs n1 (`L_Int 0);
                                build_call_stack rest ((name,0,args)::stac)
                          )
                       in
                       let new_stack = build_call_stack lst s in
                       (h, new_stack)
                     | _ -> raise Error)
              else raise Error
          else

        (h, (id2,0,new_regs)::(id,pc,rs)::s1)
  | I_ret (`L_Reg r) ->
          (match s1, (current_inst p (h,s1)) with
            | (id2,pc2,rs2)::s2, (I_call (_,n1,_)) -> 
                    let v = (if (Hashtbl.mem rs r) then Hashtbl.find rs r else raise Error) in 
                    Hashtbl.replace rs2 n1 v;
                    (h,(id2,(pc2+1),rs2)::s2)
              (*when iter is used *)
            | (id2,pc2,rs2)::s2,_ -> (h,s1)
            | _,_ -> raise Error 
          )
  | I_halt (`L_Reg r) -> raise Halt
  | _ -> raise Error
;; 

let running i (h,s) = match i,s with
    | (I_halt _),_ | (I_ret _),("main",_,_)::_ -> false
    | _ -> true
;;

let run_prog (p:prog):prog_ret = 
    let h = Hashtbl.create 10 in
    let rs = Hashtbl.create 10 in
    let s = ("main",0,rs)::[] in

    let rec loop (h,s) =
        (*dis_instr stdout (current_inst p (h,s)); print_string "\n";*)
        if (running (current_inst p (h,s)) (h,s)) then
            let (h1,s1) = run_inst p (h,s) in
            loop (h1,s1) 
        else
            let last = current_inst p (h,s) in
            match last with
                | I_ret (`L_Reg r) -> (`Reg (Hashtbl.find rs r))
                | I_halt (`L_Reg r) -> (`Halt (Hashtbl.find rs r))
    in
    loop (h,s) 
;;
