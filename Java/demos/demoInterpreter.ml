open Java_lib.Parser
open Opal
open Java_lib.Ast
open Java_lib.Interpreter
open Java_lib.Interpreter.Result

open Java_lib.Interpreter.ClassLoader (Java_lib.Interpreter.Result)

open Java_lib.Interpreter.Main (Java_lib.Interpreter.Result)

let test_interp test_val =
  match load test_val with
  | Error m ->
      print_string (m ^ "\n");
      Hashtbl.clear class_table
  | Ok load_table -> (
      match execute load_table with
      | Error m ->
          print_string (m ^ "\n");
          Hashtbl.clear class_table
      | Ok res_context ->
          print_string (show_context res_context ^ "\n\n");
          Hashtbl.clear class_table )

let () = print_string "------------------- FIRST TEST ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
        public class Main {

            public static void main() {
                int a = 1;
                int b = 2;
                int c = 3;
            }  
        }
        |})

let () = test_interp test_val

let () =
  print_string "------------------- LITTLE ARITHMETIC TEST ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
        
public class Main {

    public static void main() {
        int a = 1;
        int b = 2;
        int c = 3;
        int val1 = 1 + 2 + 3 + 4 + 5;
        int val2 = a + b;
        int val3 = a + 100;
        int val4 = 10 / 2;
        int val5 = 10 % 2;
        int val6 = (a + b) * 100; 
        a = a + 1;
        int val7 = (val1 * val2 + 4) / 2 + 100; 
    }  
}
        |})

let () = test_interp test_val

let () =
  print_string
    "------------------- PATTERN VISITOR TEST YES I'M CRAZY ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
        

public class Main {

    public static void main() {
        Circle c = new Circle(4);
        

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

let () = test_interp test_val
