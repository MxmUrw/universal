
use crate::macros::types::*;

////////////////////////////////////////
// helpers


#[macro_export]
macro_rules! mdo {
    (return $($rest:tt)+) => {
        crate::core::opure($($rest)+)
    };
    ($expr:expr) => {$expr};
    (let $var:ident = $expr:expr; $($expr2:tt)+ ) => {
        {
            let $var = $expr;   
            mdo!($($expr2)+)
        }
    };
    ($var:ident <= $expr:expr; $($expr2:tt)+) => {
        $expr.obind(move |$var| mdo!($($expr2)+))
    };
}


//-- definitions
#[macro_export]
macro_rules! define {

    ($fn_name:ident : $type:ty = $($tokens:tt)+) => {

        #[allow(unused)]
        fn $fn_name() -> $type {
            define_expr!($($tokens)+)
        }

    };
}

#[macro_export]
macro_rules! define2 {
    // ($fn_name:ident : $($tokens:tt)+) => {
    //     type_and_term!{define2callback, {args= {name=$fn_name} } [] $($tokens)+}
    // };

    ($callback:ident, $fn_name:ident [$($params:tt)*] for . $(($var:ident : $($type:tt)+))* : $($tokens:tt)+) => {
        type_and_term!{$callback, {args= {name=$fn_name} {direct_vars= $(($var : $($type)+))*} {params=$($params)*} } [] $($tokens)+}
    };

    ($callback:ident, $fn_name:ident [$($params:tt)*] for $param:ident $($rest:tt)+) => {
        define2!{$callback, $fn_name [ $($params)* $param, ] for $($rest)+}
    };

    ($callback:ident, $fn_name:ident [$($params:tt)*] for $param:lifetime $($rest:tt)+) => {
        define2!{$callback, $fn_name [ $($params)* $param, ] for $($rest)+}
    };

    ($callback:ident, $fn_name:ident [$($params:tt)*] for ($param_name:ident $($param_names:ident)* : $($param:tt)+) $($rest:tt)+) => {
        define2!{$callback, $fn_name [ $($params)* $param_name : $($param)+, ] for ($($param_names)* : $($param)+) $($rest)+}
    };

    ($callback:ident, $fn_name:ident [$($params:tt)*] for (: $($param:tt)+) $($rest:tt)+) => {
        define2!{$callback, $fn_name [ $($params)* ] for $($rest)+}
    };

    ($callback:ident, $fn_name:ident for $($rest:tt)+) => {
        define2!{$callback, $fn_name [] for $($rest)+}
    };

    ($callback:ident, $fn_name:ident $(($var:ident : $($type:tt)+))* : $($tokens:tt)+) => {
        type_and_term!{$callback, {args= {name=$fn_name} {direct_vars= $(($var : $($type)+))*} {params=} } [] $($tokens)+}
    };
}

#[macro_export]
macro_rules! declare2callback {

    ({name = $fn_name:ident} 
        {direct_vars= $(($var_name:ident : $($var_type:tt)+))*}
        {params = $($params:tt)*}
        {type = $($type:tt)+} 
        {expr = ()}) => {

        #[allow(unused)]
        fn $fn_name<$($params)*>
        (
            $(
                $var_name : make_type_top!($($var_type)+)
            ),*

        ) -> make_type_top!($($type)+);
    };
}


#[macro_export]
macro_rules! define2callback {

    ({name = $fn_name:ident} 
        {direct_vars= $(($var_name:ident : $($var_type:tt)+))*}
        {params = $($params:tt)*}
        {type = $($type:tt)+} 
        {expr = $($expr:tt)+}) => {

        #[allow(unused)]
        fn $fn_name<$($params)*>
        (
            $(
                $var_name : make_type_top!($($var_type)+)
            ),*

        ) -> make_type_top!($($type)+) {
            define_expr!($($expr)+)
        }

    };

    ({name = $fn_name:ident}
        {direct_vars= $(($var_name:ident : $($var_type:tt)+))*}
        {params = $($params:tt)*}
         {type = $($type:tt)+} 
         {exprs= $([$x:pat => $($expr:tt)+])* }
        ) 
        => {

        #[allow(unused)]
        fn $fn_name<$($params)*>
        (
            $(
                $var_name : make_type_top!($($var_type)+)
            ),*

        ) -> make_type_top!($($type)+) {
            move |a| match a
            {
                $($x => define_expr!($($expr)+)),*
            }
        }
    };
}

#[macro_export]
macro_rules! define_many {
    ($([ $($def:tt)+ ])*) => {
        $(
            $crate::define2!{define2callback, $($def)+}
        )*
        // call_many!{define, [] $($def)+}
    };
}

#[macro_export]
macro_rules! declare_many {
    ($([ $($def:tt)+ ])*) => {
        $(
            $crate::define2!{declare2callback, $($def)+}
        )*
        // call_many!{define, [] $($def)+}
    };
}


#[macro_export]
macro_rules! define_expr {
    ($x:ident -> $($expr:tt)+) => {
        move |$x| define_expr!($($expr)+)
    };
    (do $($expr:tt)+) => {
        mdo!($($expr)+)
    };
    ($expr:expr) => {
        $expr
    };
}

#[macro_export]
macro_rules! make_type {
    ($t1:tt -> $($t2:tt)+) => {
        impl Fn(make_type!($t1)) -> make_type!( $($t2)+ ) + 'static
    };
    ($t1:ident $t2:ident) => {
        $t1<$t2>
    };
    ($type:ty) => {
        $type
    };
}

#[macro_export]
macro_rules! make_type_top {
    // ($head:ident [$($arg:tt)*]) => {
    //     $head::of<make_type_top!($($arg)*)>
    // };

    // ($head:ident $($arg:tt)*) => {
    //     make_type_gen!({head = $head} {args=} $($arg)*)
    // }

    ($($arg:tt)*) => {
        eval_type!($($arg)*)
    }

}

#[macro_export]
macro_rules! make_type_gen {
    ({head=$head:ident} {args=$($arg:ident)*} -> $($rest:tt)+) => {
        impl Fn($head$(<$arg>),*) -> make_type_top!($($rest)+) + 'static + Copy
    };
    ({head=$head:ident} {args=$($arg:ident)*} [$($inside:tt)*] -> $($rest:tt)+) => {
        impl Fn($head$(<$arg>),*::of<make_type_top!($($inside)+)>) -> make_type_top!($($rest)+) + 'static + Copy
    };
    ({head=$head:ident} {args=$($arg:ident)*} $cur:ident $($rest:tt)*) => {
        make_type_gen!({head=$head} {args=$($arg)* $cur} $($rest)*)
    };
    ({head=$head:ident} {args=$($arg:ident)*} ) => {
        $head$(<$arg>),*
    };
    ($type:ty) => {
        $type
    };
}

#[macro_export]
macro_rules! type_and_term {

    ($callback:ident, {args=$({$($args:tt)+})+} [$($type:tt)*] | $x:pat => $($expr:tt)+) => {
        type_and_term!{$callback, {args=$({$($args)+})+} {type=$($type)*} {exprs=} [$x =>] $($expr)+ }
    };

    ($callback:ident,
        {args=$({$($args:tt)+})+}
        {type=$($type:tt)*}
        { exprs= $([$x:pat => $($expr:tt)+])* }
        [$cur_x:pat => $($cur_expr:tt)*]
        | $y:pat => $($tail:tt)+
    ) =>
    {
        type_and_term!{$callback, {args=$({$($args)+})+} {type=$($type)*} { exprs= $([$x => $($expr)+])* [$cur_x => $($cur_expr)+] } [$y =>] $($tail)+ }
    };

    ($callback:ident,
        {args=$({$($args:tt)+})+}
        {type=$($type:tt)*}
        { exprs= $([$x:pat => $($expr:tt)+])* }
        [$cur_x:pat => $($cur_expr:tt)+]
        ) =>
    {
        $callback!{$({$($args)+})+ {type=$($type)*} { exprs= $([$x => $($expr)+])* [$cur_x => $($cur_expr)+] } }
    };

    ($callback:ident,
        {args=$({$($args:tt)+})+}
        {type=$($type:tt)*}
        { exprs= $([$x:pat => $($expr:tt)+])* }
        [$cur_x:pat => $($cur_expr:tt)*]
        $head:tt $($tail:tt)*) =>
    {
        type_and_term!{$callback, {args=$({$($args)+})+} {type=$($type)*} { exprs= $([$x => $($expr)+])* } [$cur_x => $($cur_expr)* $head] $($tail)* }
    };

    ($callback:ident, {args=$({$($args:tt)+})+} [$($type:tt)*] = $($expr:tt)+) => {
        $callback!{$({$($args)+})+ {type=$($type)*} {expr= $($expr)+} }
    };

    // ($callback:ident, $({$($args:tt)+})+ {type=$($type:tt)*} [$x:pat $($expr:tt)+] | $x:pat => $($expr:tt)+) => {
    // };

    ($callback:ident, {args=$({$($args:tt)+})+} [$($type:tt)*] $type0:tt $($expr:tt)+) => {
        type_and_term!{$callback, {args=$({$($args)+})+} [$($type)* $type0] $($expr)+}
    };
}

// parses the left tokens, constructing a `match` term from them,
// and after that calls `callback` with the term.
// the inner terms are parsed with the `parse_inner_macro`
// macro_rules! parse_matches {
//     ({input = | $($pat:pat)* => $($expr:tt)+} $callback:ident, {args=$($args:tt)*}) => {
        
//     };

//     // Final case (input empty)
//     ({input = } {callback = $callback:ident} {args=$($args:tt)*} $({pattern = $pattern:pat} {result = $($term:tt)+})*) => {
//         $callback_macro!{$($args)* {result = match {
//             $( $pat => $($term)+  ),*  
//         }}}
//     };
// }

// macro_rules! parse_expr {
//     ({input = {$expr:expr} $($rest:tt)*} {callback = $callback:ident} {args = $($args:tt)*}) => {
//         $callback!{{input = $($rest)*} $($args)* {result = $expr}}
//     }
// }




macro_rules! macro_fun_impl
{
    // early return
    ({input = | [$($pat:tt)*]... => return[$($conclusion:tt)*] $($rest:tt)*} {name=$name:ident} {cases = $($cases:tt)*} ) =>
    {
        macro_fun_impl!
        {
            {input = $($rest)*}
            {name = $name}
            {cases = $($cases)* {  
                ({input = $($pat)* $$($$rest:tt)*} {callback = $$callback:ident} {args = $$($$args:tt)*}) =>
                {
                    $$callback!{{input = $$($$rest)*} $$($$args)* {result = $($conclusion)*}}
                }
            }}
        }
    };

    // calling a function
    ({input = | [$($pat:tt)*]... => call $call_fun:ident [$($call_args:tt)*] [$($append_tokens:tt)*] $($rest:tt)*} {name=$name:ident} {cases = $($cases:tt)*} ) =>
    {
        macro_fun_impl!
        {
            {input = $($rest)*}
            {name = $name}
            {cases = $($cases)* {  
                ({input = $($pat)* $$($$rest:tt)*} {callback = $$callback:ident} {args = $$($$args:tt)*} $$($$results:tt)*) =>
                {
                    $call_fun!{{input = $($call_args)* $$($$rest)*} {callback = $name} {args = { callback = $$callback } {args = $$($$args)*} $$($$results)* $($append_tokens)*}}
                }
            }}
        }
    };

    // calling a function (with results wrapping)
    ({input = | [$($pat:tt)*]... => call $call_fun:ident {wrap_results=$wrap_label:ident} [$($call_args:tt)*] [$($append_tokens:tt)*] $($rest:tt)*} {name=$name:ident} {cases = $($cases:tt)*} ) =>
    {
        macro_fun_impl!
        {
            {input = $($rest)*}
            {name = $name}
            {cases = $($cases)* {  
                ({input = $($pat)* $$($$rest:tt)*} {callback = $$callback:ident} {args = $$($$args:tt)*} $$($$results:tt)*) =>
                {
                    $call_fun!{{input = $($call_args)* $$($$rest)*} {callback = $name} {args = { callback = $$callback } {args = $$($$args)*} {$wrap_label = $$($$results)*} $($append_tokens)*}}
                }
            }}
        }
    };

    // calling a function (with results matching)
    ({input = | [$($pat:tt)*] [$($result_pat:tt)*] => call $call_fun:ident [$($call_args:tt)*] [$($append_tokens:tt)*] $($rest:tt)*} {name=$name:ident} {cases = $($cases:tt)*} ) =>
    {
        macro_fun_impl!
        {
            {input = $($rest)*}
            {name = $name}
            {cases = $($cases)* {  
                ({input = $($pat)* $$($$rest:tt)*} {callback = $$callback:ident} {args = $$($$args:tt)*} $($result_pat)*) =>
                {
                    $call_fun!{{input = $($call_args)* $$($$rest)*} {callback = $name} {args = { callback = $$callback } {args = $$($$args)*} $($append_tokens)*}}
                }
            }}
        }
    };

    // recursing
    ({input = | [$($pat:tt)*]... => continue [$($call_args:tt)*] [$($append_tokens:tt)*] $($rest:tt)*} {name=$name:ident} {cases = $($cases:tt)*} ) =>
    {
        macro_fun_impl!
        {
            {input = $($rest)*}
            {name = $name}
            {cases = $($cases)* {  
                ({input = $($pat)* $$($$rest:tt)*} {callback = $$callback:ident} {args = $$($$args:tt)*} $$($$results:tt)*) =>
                {
                    $name!{{input = $($call_args)* $$($$rest)*} {callback = $$callback} {args = $$($$args)*} $$($$results)* $($append_tokens)*}
                }
            }}
        }
    };

    // returning
    ({input = | do [$($pat:tt)*] [$($result_pat:tt)*] => return[$($conclusion:tt)*] $($rest:tt)*} {name=$name:ident} {cases = $($cases:tt)*} ) =>
    {
        macro_fun_impl!
        {
            {input = $($rest)*}
            {name = $name}
            {cases = $($cases)* {  
                ({input = $($pat)* $$($$rest:tt)*} {callback = $$callback:ident} {args = $$($$args:tt)*} $($result_pat)*) =>
                {
                    $$callback!{{input = $$($$rest)*} $$($$args)* {result = $($conclusion)*}}
                }
            }}
        }
    };

    // emitting
    ({input = | do [$($pat:tt)*] [$($result_pat:tt)*] => do[$($conclusion:tt)*] $($rest:tt)*} {name=$name:ident} {cases = $($cases:tt)*} ) =>
    {
        macro_fun_impl!
        {
            {input = $($rest)*}
            {name = $name}
            {cases = $($cases)* {  
                ({input = $($pat)* $$($$rest:tt)*} {callback = $$callback:ident} {args = $$($$args:tt)*} $($result_pat)*) =>
                {
                    $($conclusion)*
                }
            }}
        }
    };


    ({input = } {name=$name:ident} {cases = $({ $($case:tt)+ })*}) =>
    {
        macro_rules! $name
        {
            $(
                $($case)+
            );*
        }
    }
}

macro_rules! macro_fun {
    ($name:ident $($rest:tt)+ ) =>
    {
        macro_fun_impl!{{input = $($rest)+} {name=$name} {cases = }}
    };
}


macro_rules! extract_last_result_macro
{
    ( {$($front:tt)*} $($rest:tt)+) =>
    {
        extract_last_result_macro!{$($rest)*}
    };

    ( {result = $($result:tt)*}) =>
    {
        $($result)*   
    };
}

macro_rules! run_macro_fun {
    ($name:ident, $($input:tt)*) =>
    {
        $name!{{input = $($input)*} {callback = extract_last_result_macro} {args = }}
    };
}

macro_fun!
{
    parse_expr
    | [($expr:expr)]... => return[$expr]
    | [$tt:tt]... => return[$tt]
}

macro_fun!
{
    parse_matches
    | [| $($pat:pat)* => ]... => call parse_expr [] [{pat = $($pat),*}]
    | do [] [$({pat = $($pat:pat),*} {result = $($tm:tt)+} )* ] => return [$( (($($pat),*) => ($($tm)+)) )*]
}

macro_fun!
{
    parse_fun_and_emit
    | [$name:ident]... => call parse_fun_args [] [$name]
    | do [] [$name:ident
        {result = 
        {vars = $(($var:ident : $($var_ty:tt)+))*}
        {ty = $($ty:tt)+}
        {tm = $($tm:tt)+}
    }] => do
    [
        pub fn $name($($var : eval_type!{$($var_ty)+}),*) -> $($ty)+ {
            $($tm)+
        }
    ]
}

macro_fun!
{
    parse_fun_args
    |    [($var:ident $($var_tail:ident)* : $($ty:tt)*)]... => continue [($($var_tail)* : $($ty)*)] [($var : $($ty)*)]
    |    [(: $($ty:tt)*)]... => continue [] []
    |    [:]... => call parse_fun_ty [] [] 
    // TODO I have to do sth in case tm_type=matches in the below line,
    // in that case I want to call `generate_math_tm` to convert `tm` into a term with the match statement
    | do [] [$(($($var:tt)+))* {result= {tm_type= expr} {ty = $($ty:tt)+} {tm = $($tm:tt)+}}] => return
         [
            {vars = $(($($var)+))*}
            {ty = $($ty)+}
            {tm = $($tm)+}
         ]
    | [] [$(($var_name:ident : $($var_ty:tt)+))* {result= {tm_type= matches} {ty = $($ty:tt)+} {tm =  (($($head_match_pat:pat),*) => ($($head_match_tm:tt)+)) $($tail_match:tt)*}}]
        => call take_n [{counter = $(($head_match_pat))+} $(($var_name))*]
                    //    [$(($var_name : $($var_ty)+))* {result= {tm_type= matches} {ty = $($ty)+} {tm =  (($($head_match_pat),*) => ($($head_match_tm)+)) $($tail_match)+}}]
                    [$(($var_name : $($var_ty)+))* {previous= {tm_type=matches} {ty = $($ty)+} {tm =  (($($head_match_pat),*) => ($($head_match_tm)+)) $($tail_match)*}}]
    | do [] [$(($($var:tt)+))* {previous= {tm_type= matches} {ty = $($ty:tt)+} {tm = $( (($($match_pat:pat),*) => ($($match_tm:tt)+)) )*}} {result = $(($match_var_name:ident))*}] => return 
         [
            {vars = $(($($var)+))*}
            {ty = $($ty)+}
            {tm = match ( $($match_var_name),*, )
                {
                    $(
                        ($($match_pat),*,) => $($match_tm)+,
                    )*
                }
            }
         ] 
}

// to be called as `call take_n [{counter = COUNTER} INPUTS]` 
macro_fun!
{
    take_n
    | [{counter = ($($head:tt)*) $($tail:tt)*} ($($input:tt)*)]... => continue [{counter = $($tail)*}] [($($input)*)]
    | do [{counter = } $($rest:tt)*] [$($results:tt)*] => return [$($results)*]
}

// macro_fun!
// {
//     generate_match_tm
//     | [($($name:ident)*)]... => call generate_anonymous_names [$($name)*] []
//     | do [{tm = $( $($pats:pat)* => {$($tm:tt)+} )*} ] [ $($names:ident)* ] => return
//     [
//         match ($($names),*)
//         {
//             $(
//                 $pats => $($tm)+
//             ),*
//         }
//     ]
// }

macro_fun!
{
    parse_fun_ty
    | [=]... => call parse_expr {wrap_results=ty} [][{tm_type=expr}]
    | [|]... => call parse_matches {wrap_results=ty} [|][{tm_type=matches}]
    | [$t:tt]... => continue [][$t]
    | do [] [{ty = $($ty:tt)+} {tm_type=$tm_type:ident} {result = $($tm:tt)+}] => return[{tm_type=$tm_type} {ty = $($ty)+} {tm = $($tm)+}]
}

macro_rules! define {
    ($([$($function:tt)+])*) => {
        $(
            run_macro_fun!{parse_fun_and_emit, $($function)+}
        )*
    };
}


mod test
{
    #[test]
    fn test1()
    {
        assert_eq!(run_macro_fun!(parse_expr, (12u8)), 12);
        assert_eq!(hello(4), 8);
        assert_eq!(hello1(None), 0);
        assert_eq!(hello2(Some(3)), 3);
        assert_eq!(hello3(Some(1), Some(2)), 3);
    }

    define!
    {
        [ hello (a: u8) : u8 = (a + a)
        ]

        [ hello1 (x: Option u8) : u8
        | z => 0
        ]

        [ hello2 (x: Option u8) : u8
        | Some(a) => a
        | None => 0
        ]

        [ hello3 (x: Option u8) (y: Option u8) : u8
        | Some(a) Some(b) => (a + b)
        | _ _ => 0
        ]

        [ hello4 (x y: Option u8) : u8
        | Some(a) Some(b) => (a + b)
        | _ _ => 0
        ]
    }

    // run_macro_fun!{parse_fun_and_emit, hello (a: u8) : u8 = (a + a) }
    // run_macro_fun!{parse_fun_and_emit,
    //     hello1 (x: Option u8) : u8
    //     | z => 0
    // }
    // run_macro_fun!{parse_fun_and_emit,
    //     hello2 (x: Option u8) : u8
    //     | Some(a) => a
    //     | None => 0
    // }
    // run_macro_fun!{parse_fun_and_emit,
    //     hello3 (x: Option u8) (y: Option u8) : u8
    //     | Some(a) Some(b) => (a + b)
    //     | _ _ => 0
    // }
}

