open Java_lib.Parser
open Opal
open Java_lib.Ast
open Java_lib.Interpreter
open Java_lib.Interpreter.Result

open Java_lib.Interpreter.ClassLoader (Java_lib.Interpreter.Result)

(* let print_list =
  Format.pp_print_list Format.pp_print_string Format.std_formatter *)

let rec print_list = function
  | [] -> print_string ""
  | x :: xs -> print_string (x ^ " ") |> fun _ -> print_list xs

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

let rec print_classes = function
  | [] -> print_string "\n"
  | x :: xs -> print_class_r x |> fun _ -> print_classes xs

let test_load t_val =
  match load t_val with
  | Error m ->
      print_string (m ^ "\n");
      Hashtbl.clear class_table
  | Ok _ ->
      print_string "[[";
      Hashtbl.iter
        (fun key elem ->
          Printf.printf "%s -> " key;
          print_class_r elem)
        class_table;
      print_string "]]\n";
      Hashtbl.clear class_table

let () =
  print_string "-------------------TESTING_INHERITANCE-------------------\n\n"

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
    
    public void giveEvenNumbers100() {
        for (int i = 0; i < 100; i++) {
            if (i % 2 == 0 && !(i % 2 == 1)) {
                System.out.println(i);
            }
            else {
                continue;
            }
        }
    }
    
}
|})

let () = test_load test_value

let () = print_string "-------------------SIMILAR_FIELDS-------------------\n\n"

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

class Person {
    public int weight;
    public int weight;
    
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
    
    public void giveEvenNumbers100() {
        for (int i = 0; i < 100; i++) {
            if (i % 2 == 0 && !(i % 2 == 1)) {
                System.out.println(i);
            }
            else {
                continue;
            }
        }
    }
    
}
|})

let () = test_load test_value

let () =
  print_string "-------------------SIMILAR_METHODS_ERROR-------------------\n\n"

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

class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    public void setWeight(int w) {
        this.weight = w;
    }
    
    public void setWeight(int a) {
  
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

let () = test_load test_value

let () =
  print_string
    "-------------------SIMILAR_CONSTRUCTOR_ERROR-------------------\n\n"

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

class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }

    public Person(int x, int y) {
        int z = z + y; 
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

let () = test_load test_value

let () =
  print_string "-------------------LACK_OF_SUPER_ERROR-------------------\n\n"

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

class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
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

let () = test_load test_value

let () =
  print_string "-------------------ABSTRACTNESS_ERRORS-------------------\n\n"

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main(String[] args) {
	    
	}
}

class Figure {
    abstract int accept(Visitor v);
}

class Circle extends Figure {
    public int radius;

    public Circle(int radius) {
        this.radius = radius;
    }

    public Circle() {
        this.radius = 1;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
    }

|})

let () = test_load test_value

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main(String[] args) {
	    
	}
}

abstract class Figure {
    int accept(Visitor v);
}

class Circle extends Figure {
    public int radius;

    public Circle(int radius) {
        this.radius = radius;
    }

    public Circle() {
        this.radius = 1;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
    }
}
|})

let () = test_load test_value

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main(String[] args) {
	    
	}
}

abstract class Figure {
    abstract int accept(Visitor v) {
        int x = 1 + 2;
    }
}

class Circle extends Figure {
    public int radius;

    public Circle(int radius) {
        this.radius = radius;
    }

    public Circle() {
        this.radius = 1;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
    }
}
|})

let () = test_load test_value

let test_value =
  Option.get
    (apply parser
       {| 
public class Main
{
	public static void main(String[] args) {
	    
	}
}

abstract class Figure {
    abstract int accept(Visitor v);
}

class Circle extends Figure {
    public int radius;

    public Circle(int radius) {
        this.radius = radius;
    }

    public Circle() {
        this.radius = 1;
    }

}
|})

let () = test_load test_value

let () =
  print_string
    "-------------------FINAL_MODIFIERS_ERRORS-------------------\n\n"

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

final class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        super(w, a);
        this.weight = w;
        this.age = a;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
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

let () = test_load test_value

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

class Person {
    public int age;
    public int weight;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    public int getWeight() {
        return weight;
    }
    
    final public int getAge() {
        return age;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w, a);
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

let () = test_load test_value

let () =
  print_string "-------------------@OVERRIDE_ERRORS-------------------\n\n"

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

class Person {
    public int age;
    public int weight;
    
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
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w, a);
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }

    @Override
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
}
|})

let () = test_load test_value
