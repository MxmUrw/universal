
trait Conv<B>
{
    fn conv(self) -> B;
}

trait MyBigTrait 
{
    type A;
    type B;
    type C;
}

trait Open<Z> : Sized
{
    fn run<A>(self, f: impl Fn(Self) -> A) -> A; 
}

trait MyBigTraitExt : MyBigTrait
{
    type X : Open<(Self::A, Self::B)> where Self::A: Conv<Self::B>;
}

struct M {}
impl MyBigTrait for M
{
    type A = u8;
    type B = u16;
    type C = u32;
}
impl MyBigTraitExt for M
{
    type X = ();
}
impl<Z> Open<Z> for ()
{
    fn run<A>(self, f: impl Fn(Self) -> A) -> A {
        todo!()
    }
}
impl Conv<u16> for u8 {
    fn conv(self) -> u16 {
        self as u16
    }
}

// pub fn myfun<X: MyBigTraitExt>(x: &'static X::X, a: X::A) -> X::B {
//     x.run()
// }
