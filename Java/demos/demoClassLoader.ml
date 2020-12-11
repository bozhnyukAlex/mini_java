open Java_lib.Parser
open Opal
open Java_lib.Ast
open Java_lib.Interpreter
open Java_lib.Interpreter.Result

open Java_lib.Interpreter.ClassLoader (Java_lib.Interpreter.Result)

let print_list =
  Format.pp_print_list Format.pp_print_string Format.std_formatter

let print_field_table : (key_t, field_r) Hashtbl.t -> unit =
 fun ht ->
  print_string "[[";
  Hashtbl.iter
    (fun key elem -> Printf.printf "%s -> %s\n" key (show_field_r elem))
    ht;
  print_string "]]"

let print_method_table : (key_t, method_r) Hashtbl.t -> unit =
 fun ht ->
  print_string "[[";
  Hashtbl.iter
    (fun key elem -> Printf.printf "%s -> %s\n" key (show_method_r elem))
    ht;
  print_string "]]"

let print_constructor_table : (key_t, constructor_r) Hashtbl.t -> unit =
 fun ht ->
  print_string "[[";
  Hashtbl.iter
    (fun key elem -> Printf.printf "%s -> %s\n" key (show_constructor_r elem))
    ht;
  print_string "]]"

let print_string_option = function
  | Some s -> print_string s
  | None -> print_string ""

(* Скорее всего, вы скажете, что это плохо, но пока так... *)
let print_class_r : class_r -> unit =
 fun cr ->
  Printf.printf "{ this_key : %s; " cr.this_key;
  print_string "field_table : ";
  print_field_table cr.field_table;
  print_string "; method_table : ";
  print_method_table cr.method_table;
  print_string "; constructor_table : ";
  print_constructor_table cr.constructor_table;
  print_string "; children_keys : ";
  print_list cr.children_keys;
  Printf.printf "; is_abstract : %b; is_inheritable : %b; " cr.is_abstract
    cr.is_inheritable;
  print_string "parent_key : ";
  print_string_option cr.parent_key;
  print_string "} \n"

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main(String[] args) {
		Person p = new Person(80, 45);
		System.out.println(p.getWeight());
		
		Child ch = new Child(66, 20);
		ch.setCash(50);
		ch.giveEvenNumbers100();
	    
	}
}


class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w,a);
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }
    
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }

    
}
|})

let loading = c_table_add test_value >>= fun _ -> update_child_keys

let test_load =
  match loading with
  | Error m -> print_string m
  | Ok _ ->
      print_string "!!!";
      Hashtbl.iter
        (fun key elem ->
          Printf.printf "%s -> " key;
          print_class_r elem)
        class_table;
      print_string "]]\n"
