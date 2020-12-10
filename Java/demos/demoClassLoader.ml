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
public class Main {

    public static void main(String[] args) {
        Figure[] list = new Figure[] {new Circle(5), new Rectangle(2,4), new Triangle()};
        AreaVisitor areaVisitor = new AreaVisitor();
        PerimeterVisitor perimeterVisitor = new PerimeterVisitor();

        for (int i = 0; i < list.length; i++) {
            System.out.println(list[i].accept(areaVisitor));
        }
        for(int j = 0; j < list.length; j++) {
            System.out.println(list[j].accept(perimeterVisitor));
        }
    }
}

abstract class Figure {
    abstract int accept(Visitor v);
}

abstract class Visitor {
    abstract int visit(Circle circle);
    abstract int visit(Rectangle rectangle);
    abstract int visit(Triangle triangle);
}

class AreaVisitor extends Visitor {

    @Override
    int visit(Circle circle) {
        return 3 * circle.radius * circle.radius;
    }

    @Override
    int visit(Rectangle rectangle) {
        return rectangle.a * rectangle.b;
    }

    @Override
    int visit(Triangle triangle) {
        int p = (triangle.a + triangle.b + triangle.c) / 2;
        return p * (p - triangle.a) * (p - triangle.b) * (p - triangle.c);
    }
}

class PerimeterVisitor extends Visitor {

    @Override
    int visit(Circle circle) {
        return 2 * 3 * circle.radius;
    }

    @Override
    int visit(Rectangle rectangle) {
        return (rectangle.a + rectangle.b) * 2;
    }

    @Override
    int visit(Triangle triangle) {
        return triangle.a + triangle.b + triangle.c;
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

class Triangle extends Figure {
    public int a, b, c;

    public Triangle(int a, int b, int c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }
    public Triangle() {
        this.a = 1;
        this.b = 1;
        this.c = 1;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
    }
}

class Rectangle extends Figure {
    public int a, b;

    public Rectangle() {
        this.a = 1;
        this.b = 1;
    }

    public Rectangle(int a, int b) {
        this.a = a;
        this.b = b;
    }

    @Override
    int accept(Visitor v) {
        return v.visit(this);
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
