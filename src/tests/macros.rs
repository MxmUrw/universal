
#![cfg(test)]

use std::process::Termination;

use crate::*;

use itertools::Either;

struct Test {
    a: u8
}

impl Test {
    fn new() -> Self {
        Test { a: 0 }
    }
}

define_many!
{
    [ myfun (x : u8) : Test
    = Test::new()
    ]

    // [ fun2 (u8 + u16) : u16
    // | Either::Left(a) => todo!()
    // ]

    // [ double for (A : Num). A A (v : i32) -> A
    // | x y => x * 2
    // ]
}



