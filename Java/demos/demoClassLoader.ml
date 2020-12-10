open Java_lib.Parser
open Opal
open Java_lib.Ast
open Java_lib.Interpreter

open Java_lib.Interpreter.ClassLoader (Result)

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
    cr.is_abstract;
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
	    
	}
}

class Person {
    public int weight;
    public int age;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    
    
    public int getWeight() {
        return weight;
    }
    
    public int getAge() {
        return age;
    }
    
    public void setWeight(int w) {
        this.weight = w;
    }
    public void setAge(int a) {
        this.age = a;
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

let loading = load test_value

let test =
  match loading with
  | Error m -> print_string m
  | Ok _ ->
      print_string "[[";
      Hashtbl.iter
        (fun key elem ->
          Printf.printf "%s -> " key;
          print_class_r elem)
        class_table;
      print_string "]]\n"
