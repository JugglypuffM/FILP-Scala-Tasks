package adt;

import java.util.List;
import java.util.Objects;

public class Example1_Java {
    interface Pet {
    }

    public class Dog implements Pet {
        public final long id;
        public final String name;

        Dog(long id, String name) {
            this.id = id;
            this.name = name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Dog dog)) return false;
            return id == dog.id && Objects.equals(name, dog.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(id, name);
        }
    }

    public class Cat implements Pet {
        public final long id;
        public final String name;
        public final String color;

        Cat(long id, String name, String color) {
            this.id = id;
            this.name = name;
            this.color = color;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Cat cat)) return false;
            return id == cat.id && Objects.equals(name, cat.name) && Objects.equals(color, cat.color);
        }

        @Override
        public int hashCode() {
            return Objects.hash(id, name, color);
        }
    }

    Dog dog = new Dog(1, "Doggie");
    Cat cat = new Cat(2, "Cattie", "Black");

    List<Pet> pets = List.of(dog, cat);

    // both dog and cat are defined by their fields. so we can change classes for record classes
}
