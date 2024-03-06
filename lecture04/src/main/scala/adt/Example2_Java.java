package adt;

import java.util.List;

public class Example2_Java {
    //available since java 14 - March 2020 (!)
    interface Pet {
    }

    public record Dog(long id, String name) implements Pet {
    }

    public record Cat(long id, String name, String color) implements Pet {
    }


    Dog dog = new Dog(1, "Doggie");
    Cat cat = new Cat(2, "Cattie", "Red");

    List<Pet> pets = List.of(dog, cat);

}
