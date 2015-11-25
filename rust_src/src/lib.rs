#[macro_use]
extern crate ruster_unsafe;
use ruster_unsafe::*;
use std::mem::uninitialized;

/// Create NIF module data and init function.
nif_init!( b"rxml_native\0",
           Some(load),
           Some(reload),
           Some(upgrade),
           Some(unload),
           // test
           nif!(b"static_atom\0",  0, static_atom),
           nif!(b"native_add\0",   2, native_add, ERL_NIF_DIRTY_JOB_IO_BOUND),
           nif!(b"tuple_add\0",    1, tuple_add, ERL_NIF_DIRTY_JOB_CPU_BOUND),
           nif!(b"print_binary\0", 1, print_binary)
           // exml.erl
           nif!(b"new_parser\0", 0, new_parser) );

// test
static mut my_atom:ERL_NIF_TERM = 0 as ERL_NIF_TERM;

static mut XML_ELEMENT_START: ERL_NIF_TERM = 0 as ERL_NIF_TERM;
static mut XML_ELEMENT_END  : ERL_NIF_TERM = 0 as ERL_NIF_TERM;
static mut XML_CDATA        : ERL_NIF_TERM = 0 as ERL_NIF_TERM;
static mut OK               : ERL_NIF_TERM = 0 as ERL_NIF_TERM;
static mut NONE             : ERL_NIF_TERM = 0 as ERL_NIF_TERM;
static mut ERROR            : ERL_NIF_TERM = 0 as ERL_NIF_TERM;

/// Initialize global constants.
extern "C" fn load(env: *mut ErlNifEnv,
                   _priv_data: *mut *mut c_void,
                   _load_info: ERL_NIF_TERM)-> c_int {
    unsafe {
        my_atom           = enif_make_atom(env, b"static atom from Rust\0" as *const u8);
        XML_ELEMENT_START = enif_make_atom(env, b"xml_element_start\0" as *const u8);
        XML_ELEMENT_END   = enif_make_atom(env, b"xml_element_end\0" as *const u8);
        XML_CDATA         = enif_make_atom(env, b"xml_cdata\0" as *const u8);
        OK                = enif_make_atom(env, b"ok\0" as *const u8);
        NONE              = enif_make_atom(env, b"none\0" as *const u8);
        ERROR             = enif_make_atom(env, b"error\0" as *const u8);
        0
    }
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

/// Provide static atom that was initialized by `load()`
extern "C" fn static_atom(_env:*mut ErlNifEnv,
                          _argc: c_int,
                          _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    unsafe { my_atom }
}

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

extern "C"
fn print_binary(env: *mut ErlNifEnv,
                argc: c_int,
                args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    assert!(argc == 1);
    unsafe {
        let mut bin: ErlNifBinary = uninitialized();
        OK
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
    unsafe { OK }
}
