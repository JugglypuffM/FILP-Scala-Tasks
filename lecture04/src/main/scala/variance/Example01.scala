package variance

object Example01 {
  trait Base
  trait Derived extends Base

  val producer1: Function1[Unit, Derived] = _ => new Derived {}
  val producer2: Function1[Unit, Base]    = producer1
//  val c: Function1[Unit, Derived] = b

//   ┌──────┐   ┌──────────────────────┐
//   │ Base │   │ Function[Unit, Base] │
//   └──▲───┘   └─────────────▲────────┘
//      │                     │
//  ┌───┴─────┐ ┌─────────────┴───────────┐
//  │ Derived │ │ Function[Unit, Derived] │
//  └─────────┘ └─────────────────────────┘

  val consumer1: Function1[Base, Unit]    = _ => ()
  val consumer2: Function1[Derived, Unit] = consumer1
//  val f: Function1[Base, Unit]    = e

//   ┌──────┐   ┌─────────────────────────┐
//   │ Base │   │ Function[Derived, Unit] │
//   └──▲───┘   └─────────────▲───────────┘
//      │                     │
//  ┌───┴─────┐ ┌─────────────┴────────┐
//  │ Derived │ │ Function[Base, Unit] │
//  └─────────┘ └──────────────────────┘
}
