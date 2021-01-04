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
    "------------------- SIMPLE METHOD CALL TEST ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
        
public class Main {

    public static void main() {
        Person person = new Person(25, "Bob");
        int res = person.sum(25, 100);
        int a1 = person.getAge();
        person.setAge(30);
        int a2 = person.getAge(); 
    }
}

class Person {
    int age;
    String name;

    public Person() {}

    public Person(int age, String name) {
        this.age = age;
        this.name = name;
    }

    public int sum(int a, int b) {
        return a + b;
    }

    public int getAge() {
        return this.age;
    }

    public void setAge(int age) {
        this.age = age;
    }

}
        |})

let () = test_interp test_val

let () =
  print_string
    "------------------- UPDATE OBJECT STATE IN MAIN TEST ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
        
public class Main {

    public static void main() {
        Person person = new Person(25, "Bob");
        Person p1, p2, p3;
        p1 = person;
        p2 = p1;
        p3 = p2;
        person.setAge(55);
        int res = p2.getAge(); 
    }
}

class Person {
    int age;
    String name;

    public Person() {}

    public Person(int age, String name) {
        this.age = age;
        this.name = name;
    }

    public int sum(int a, int b) {
        return a + b;
    }

    public int getAge() {
        return this.age;
    }

    public void setAge(int age) {
        this.age = age;
    }

}
        |})

let () = test_interp test_val

let () =
  print_string "------------------- CHILD WORKING TEST ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
        
public class Main {

    public static void main() {
        Person person = new Person(25, "Bob");
        Person childFirst = new Child(3, "Alice");
        Child childSecond = new Child(person);
        childSecond.setAge(20);
        childFirst.setAge(4);
        person.setAge(27);
    }
}

class Person {
    int age;
    String name;

    public Person() {}

    public Person(int age, String name) {
        this.age = age;
        this.name = name;
    }

    public int sum(int a, int b) {
        return a + b;
    }

    public int getAge() {
        return this.age;
    }

    public void setAge(int age) {
        this.age = age;
    }

}
class Child extends Person {

    public Person parent;

    public Child(int age, String name) {
        super(age, name);
        parent = new Person(40, "Flexer");
    }

    public Child(Person parent) {
        this.parent = parent;
    }

    public Child() {}

    public int getName() {
        return name;
    }

    public Person getParent() {
        return parent;
    }
}
        |})

let () = test_interp test_val

let () = print_string "------------------- SCOPE TEST ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
public class Main {
    public static void main() {
        int a = 2;
        int b = 3;
        int c = 4;
        if (c == 4) {
           int d = 7;
           a = a + 1;
           int e = 10;
           int g = 42;
        }
        b = 5;
        c = 6;
        a = 15;
        int i = 0;
        while (i < 3) {
      	   int m = 2;
      	   int n = 3;
      	   int z = 4;
      	   int f = 5;
      	   i = i + 1;
      	 }
      	a = 100;
      	b = 200;
        c = 300;
        for (int k = 0, p = 0; k < 6; k = k + 1) {
            int m = 2;
      	    int n = 3;
      	    int z = 4;
      	    int f = 5;
            p = p + 1;
        }
        a = 1000;
        b = 2000;
        c = 3000;
    }
}   

        |})

let () = test_interp test_val

let () =
  print_string
    "------------------- MANY CYCLES TEST + ARRAY SORTING ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|        
public class Main {
    public static void main() {
        int[] arr = new int[] {10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
        int n = 11;
        for (int i = 0; i < n - 1; i = i + 1) {
            for (int j = 0; j < n - i - 1; j = j + 1) {
                if (arr[j] > arr[j + 1]) {
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                }
            }
        }
    }
}
        |})

let () = test_interp test_val

let () =
  print_string
    "------------------- ARRAY SORT AS FUNCTION (CHECKING CHANGE OF ARRAY \
     STATE IN OTHER CONTEXT) ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
        
public class Main {
    public static void main() {
        int[] arr = new int[] {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
        BubbleSorter bubbleSorter = new BubbleSorter();
        bubbleSorter.sort(arr, 16);
    }
}

class BubbleSorter {
    public void sort(int[] arr, int n) {
        for (int i = 0; i < n - 1; i = i + 1) {
            for (int j = 0; j < n - i - 1; j = j + 1) {
                if (arr[j] > arr[j + 1]) {
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                }
            }
        }
    }
}
        |})

let () = test_interp test_val

let () =
  print_string
    "------------------- CHANGE OF OBJECT STATE IN OTHER CONTEXT \
     ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
        
public class Main {
    public static void main() {
        Person person = new Person(25, "Bob");
        Child child = new Child(person);
        child.setParentAge(30);
    }
}

class Person {
    int age;
    String name;

    public Person() {}

    public Person(int age, String name) {
        this.age = age;
        this.name = name;
    }
}

class Child extends Person {

    public Person parent;

    public Child(int age, String name) {
        super(age, name);
        parent = new Person(40, "Spike");
    }

    public Child(Person parent) {
        this.parent = parent;
    }

    public void setParentAge(int age) {
        Person p1 = parent; 
        p1.age = age;
    }
}
        |})

let () = test_interp test_val

let () =
  print_string "------------------- PATTERN VISITOR TEST ------------------\n"

let test_val =
  Option.get
    (apply parser
       {|
        

public class Main {

    public static void main() {
        Figure[] list = new Figure[] {new Circle(5), new Rectangle(2,4), new Triangle()};
        AreaVisitor areaVisitor = new AreaVisitor();
        PerimeterVisitor perimeterVisitor = new PerimeterVisitor();
        int[] resPerimeter = new int[3];
        int[] resArea = new int[3];
        for (int i = 0; i < 3; i = i + 1) {
            resPerimeter[i] = list[i].accept(areaVisitor);
        }
        for(int j = 0; j < 3; j = j + 1) {
            resArea[j] = list[j].accept(perimeterVisitor);
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

let visitor_test = test_interp test_val
