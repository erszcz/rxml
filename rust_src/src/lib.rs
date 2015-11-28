extern crate libc;
#[macro_use] extern crate ruster_unsafe;

use libc::c_uint;
use ruster_unsafe::*;
use std::fmt::{ Debug, Error as FormatError, Formatter };
use std::mem::uninitialized;

/// Create NIF module data and init function.
nif_init!( b"rxml_native\0",
           Some(load),
           Some(reload),
           Some(upgrade),
           Some(unload),
           // test
           nif!(b"native_add\0", 2, native_add, ERL_NIF_DIRTY_JOB_IO_BOUND),
           // TODO: make parse_nif ERL_NIF_DIRTY_JOB_CPU_BOUND?
           nif!(b"parse_nif\0", 3, parse_nif),
           nif!(b"print_binary\0", 1, print_binary),
           nif!(b"test\0", 0, test),
           nif!(b"test_badarg\0", 0, test_badarg),
           nif!(b"test_badarity\0", 0, test_badarity),
           nif!(b"tuple\0", 0, tuple),
           nif!(b"tuple_add\0", 1, tuple_add, ERL_NIF_DIRTY_JOB_CPU_BOUND)

           // exml.erl
           //nif!(b"new_parser\0", 0, new_parser)
         );

static mut NIF_INTERNAL_ERROR: ERL_NIF_TERM = 0 as ERL_NIF_TERM;

mod atom {

    macro_rules! define { ($atom:ident) => {
        #[inline]
        pub fn $atom(env: *mut ::ruster_unsafe::ErlNifEnv) -> ::ruster_unsafe::ERL_NIF_TERM {
            unsafe {
                if let Ok (atom) = ::std::ffi::CString::new(stringify!($atom)) {
                    ::ruster_unsafe::enif_make_atom(env, atom.as_ptr() as *const u8)
                } else {
                    super::NIF_INTERNAL_ERROR
                }
            }
        }
    } }

    define!(badarg);
    define!(badarity);
    define!(error);
    define!(none);
    define!(ok);
    define!(unimplemented);

}

pub enum Error {
    BadArg (*mut ErlNifEnv),
    BadArity (*mut ErlNifEnv)
}

impl From<Error> for ERL_NIF_TERM {
    fn from(e: Error) -> ERL_NIF_TERM {
        let (env, reason) = match e {
            Error::BadArg(env) => (env, atom::badarg(env)),
            Error::BadArity(env) => (env, atom::badarity(env))
        };
        unsafe { enif_raise_exception(env, reason) }
    }
}

macro_rules! nif_try { ($expr:expr) => {
    match $expr {
        Ok (val) => val,
        Err (err) => return ::std::convert::From::from(err)
    }
} }

macro_rules! fail {
    ($expr:expr) => (
        return Err ($expr)
    )
}

/// Initialize global constants.
extern "C" fn load(env: *mut ErlNifEnv,
                   _priv_data: *mut *mut c_void,
                   _load_info: ERL_NIF_TERM)-> c_int {
    unsafe { NIF_INTERNAL_ERROR = enif_make_atom(env, b"error\0" as *const u8) }
    0
}

/// Does nothing, reports success
extern "C" fn reload(_env: *mut ErlNifEnv,
                     _priv_data: *mut *mut c_void,
                     _load_info: ERL_NIF_TERM) -> c_int { 0 }

/// Does nothing, reports success
extern "C" fn upgrade(_env: *mut ErlNifEnv,
                      _priv_data: *mut *mut c_void,
                      _old_priv_data: *mut *mut c_void,
                      _load_info: ERL_NIF_TERM) -> c_int { 0 }

/// Does nothing, reports success
extern "C" fn unload(_env: *mut ErlNifEnv,
                     _priv_data: *mut c_void) {}


/// Add two integers. `native_add(A,B) -> A+B.`
extern "C" fn native_add(env: *mut ErlNifEnv,
                         argc: c_int,
                         args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    unsafe {
        let mut a: c_int = uninitialized();
        let mut b: c_int = uninitialized();
        if argc == 2 &&
           0 != enif_get_int(env, *args, &mut a) &&
           0 != enif_get_int(env, *args.offset(1), &mut b) {
            enif_make_int(env, a+b)
         }
         else {
            enif_make_badarg(env)
         }
    }
}

extern "C"
fn test_badarg(env: *mut ErlNifEnv,
               argc: c_int,
               args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    unsafe {
        enif_raise_exception(env, atom::badarg(env))
    }
}

extern "C"
fn test_badarity(env: *mut ErlNifEnv,
                 argc: c_int,
                 args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    unsafe {
        enif_raise_exception(env, atom::badarity(env))
    }
}

/// Add integers provided in a 2-tuple. `tuple_add({A,B}) -> A+B.`
extern "C" fn tuple_add(env: *mut ErlNifEnv,
                        argc: c_int,
                        args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    unsafe {
        let mut a:c_int = uninitialized();
        let mut b:c_int = uninitialized();
        let mut size:c_int = uninitialized();
        let mut tup:*const ERL_NIF_TERM = uninitialized();
        if argc == 1 &&
           0 != enif_get_tuple(env, *args, &mut size, &mut tup) &&
           size == 2 &&
           0 != enif_get_int(env, *tup, &mut a) && 
           0 != enif_get_int(env, *tup.offset(1), &mut b) {
            enif_make_int(env, a+b)
        }
        else {
            enif_make_badarg(env)
        }
    }
}

fn unpack_binary(env: *mut ErlNifEnv, i: isize, args: *const ERL_NIF_TERM)
    -> Result<ErlNifBinary, Error>
{
    unsafe {
        let arg = *args.offset(i);
        let mut bin: ErlNifBinary = uninitialized();
        if !c_bool(enif_inspect_binary(env, arg, &mut bin)) {
            fail!(Error::BadArg(env))
        }
        Ok (bin)
    }
}

struct Binary(ErlNifBinary);

impl Debug for Binary {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        let &Binary(ref bin) = self;
        let s = unsafe { std::slice::from_raw_parts(bin.data, bin.size) };
        try!(formatter.write_str(&format!("ErlNifBinary{:?}", s)));
        Ok (())
    }
}

extern "C"
fn print_binary(env: *mut ErlNifEnv,
                argc: c_int,
                args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    assert!(argc == 1);
    let bin = nif_try!(unpack_binary(env, 0, args));
    println!("{:?}", Binary(bin));
    atom::ok(env)
}

extern "C"
fn test(env: *mut ErlNifEnv,
        argc: c_int,
        args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    assert!(argc == 0);
    atom::ok(env)
}

extern "C"
fn tuple(env: *mut ErlNifEnv,
         argc: c_int,
         args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    assert!(argc == 0);
    let arr = [atom::ok(env), atom::error(env)];
    unsafe {
        enif_make_tuple_from_array(env, arr.as_ptr(), arr.len() as c_uint)
    }
}

/// Create a new XML parser.
extern "C" fn new_parser(env: *mut ErlNifEnv,
                         argc: c_int,
                         args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    //let mut fake_parser: ErlNifBinary = uninitialized();
    //enif_alloc_binary(12, &fake_parser);
    //fake_parser
    //enif_make_binary(env, b"fake_parser\0" as *const u8)
    atom::ok(env)
}

extern "C"
fn parse_nif(env: *mut ErlNifEnv,
             argc: c_int,
             args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    assert!(argc == 3);
    atom::unimplemented(env)
}

#[allow(dead_code)]
fn atom(env: *mut ErlNifEnv, a: &[u8]) -> ERL_NIF_TERM {
    unsafe { enif_make_atom(env, a.as_ptr()) }
}

// Roughly inspired by:
// https://github.com/rust-lang/rust/blob/e3dfb2c45fa3a2da312fc2dbc36aa0c3a06319eb/src/libstd/sys/unix/mod.rs#L91-L98
fn c_bool(t: c_int) -> bool {
    if t != 0 { true } else { false }
}
