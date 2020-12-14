open Java_lib.Parser
open Opal
open Java_lib.Ast
open Java_lib.Interpreter
open Java_lib.Interpreter.Result

open Java_lib.Interpreter.ClassLoader (Java_lib.Interpreter.Result)

let show_keys_list list = List.fold_left (fun s acc -> acc ^ " " ^ s) "" list

(* TODO: Это всё неплохо, но такое сложение строк будет тормозить независимо от того, OCaml это или С#. 
Тут надо какое-то буферизиванное печатанье в строку, возможно, 
с использование модуля Buffer (а может до кучи и модуля Format тоже)*)
let show_hashtbl ht show =
  match Hashtbl.length ht with
  | 0 -> "[[]]"
  | _ ->
      Hashtbl.fold (fun k v acc -> acc ^ k ^ " -> " ^ show v ^ "\n") ht "[["
      ^ "]]"

let show_field_table ht = show_hashtbl ht show_field_r

let show_method_table ht = show_hashtbl ht show_method_r

let show_constructor_table ht = show_hashtbl ht show_constructor_r

let show_string_option = function Some s -> s | None -> "None"

(* TODO: Ну здесь чуть меньше гавнокода, потому что concat вроде умеет заранее буффер выделять адекватной длины, 
но всё равно могло быть лучше. В конце исправите.*)
let show_class_r cr =
  let th_key = "{ this_key : " ^ cr.this_key ^ "; " in
  let f_table = "field_table : " ^ show_field_table cr.field_table ^ "; " in
  let m_table = "method_table : " ^ show_method_table cr.method_table ^ "; " in
  let c_table =
    "constructor_table : " ^ show_constructor_table cr.constructor_table ^ "; "
  in
  let ch_keys = "children_keys : " ^ show_keys_list cr.children_keys ^ "; " in
  let is_abstr = "is_abstract : " ^ Bool.to_string cr.is_abstract ^ "; " in
  let is_inh = "is_inheritable : " ^ Bool.to_string cr.is_inheritable ^ "; " in
  let par_key_o = "parent_key : " ^ show_string_option cr.parent_key ^ "}" in
  String.concat ""
    [ th_key; f_table; m_table; c_table; ch_keys; is_abstr; is_inh; par_key_o ]

let show_class_table ht = show_hashtbl ht show_class_r

let test_load t_val =
  match load t_val with
  | Error m ->
      print_string (m ^ "\n");
      Hashtbl.clear class_table
  | Ok _ ->
      print_string (show_class_table class_table ^ "\n");
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
