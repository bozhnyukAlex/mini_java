### An implementaion of Java-OOP mini-language

This is a homework for functional programming course.

License: LGPL

Author: Bozhnyuk Alexander, bozhnyuks@mail.ru

Замечания. 
    1) Ключевые слова не могут являться именами классов
    2) Метод main ничего в себя не принимает, является просто точкой входа программы. Метод main - единственный.
    3) В классе, где лежит метод Main, не может быть методов, конструкторов или полей. 

На данном этапе я сделал загрузку классов + класс Object. 

Features done:

- 1 Загрузка классов
- 2 Подготовлен тест про паттерн Visitor
- 3 Больше тестов на все
- 4 Класс Object
- 5 Функции печати с более лучшей сигнатурой

Класс Object: 
```
public class Object {
    public int equals(Object obj) {
        if (this == obj) return 1;
        else return 0;
    }
    
    public String toString() {
    	return "Object";
    }
}
```

Features in progress:

- 1 Интерпретация выражений, стейтментов и т д
- 2 
- 3 

Дальнейший этап интерпретации - научиться интерпретировать отдельный метод в контексте класса, и просто интерпретировать метод main. Там уже и будут интерпретации выражений, стейтментов и прочее. 


