extern crate libc;
#[macro_use] extern crate ruster_unsafe;
extern crate xml;

use libc::c_uint;
use ruster_unsafe::*;
use std::fmt::{ Debug, Error as FormatError, Formatter };
use std::mem::uninitialized;

const LOAD_OK: c_int    = 0;
const LOAD_ERROR: c_int = 1;

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
           nif!(b"tuple_add\0", 1, tuple_add, ERL_NIF_DIRTY_JOB_CPU_BOUND),

           // exml_event:new_parser/0
           nif!(b"new_parser\0", 0, new_parser)
         );

static mut NIF_INTERNAL_ERROR: ERL_NIF_TERM = 0 as ERL_NIF_TERM;
static mut PARSER_RESOURCE: *mut ErlNifResourceType = 0 as *mut ErlNifResourceType;

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
    define!(badxml);
    define!(enif_call_failed);
    define!(error);
    define!(none);
    define!(ok);
    define!(unimplemented);
    define!(xml_cdata);
    define!(xml_element_end);
    define!(xml_element_start);

}

pub enum Error {
    BadArg (*mut ErlNifEnv),
    BadArity (*mut ErlNifEnv),
    BadXML (*mut ErlNifEnv),
    EnifCallFailed (*mut ErlNifEnv)
}

impl From<Error> for ERL_NIF_TERM {
    fn from(e: Error) -> ERL_NIF_TERM {
        let (env, reason) = match e {
            Error::BadArg(env) => (env, atom::badarg(env)),
            Error::BadArity(env) => (env, atom::badarity(env)),
            Error::BadXML(env) => (env, atom::badxml(env)),
            Error::EnifCallFailed(env) => (env, atom::enif_call_failed(env))
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
    unsafe {
        NIF_INTERNAL_ERROR = enif_make_atom(env, b"error\0" as *const u8);
        if !is_enif_ok(NIF_INTERNAL_ERROR as c_int)
            { return LOAD_ERROR }
        PARSER_RESOURCE = enif_open_resource_type(env,
                                                  0 as *const u8,
                                                  b"parser_resource\0" as *const u8,
                                                  Some (parser_dtor as ErlNifResourceDtor),
                                                  ErlNifResourceFlags::ERL_NIF_RT_CREATE,
                                                  0 as *mut ErlNifResourceFlags);
        if !is_enif_ok(PARSER_RESOURCE as c_int)
            { return LOAD_ERROR }
    }
    LOAD_OK
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

struct Binary {
    nif_binary: ErlNifBinary,
    allocated: bool
}

impl Binary {

    fn from_string(env: *mut ErlNifEnv, s: &str) -> Result<Binary, Error> {
        let bin = unsafe {
            let mut b: ErlNifBinary = uninitialized();
            if !is_enif_ok(enif_alloc_binary(s.len(), &mut b)) {
                fail!(Error::EnifCallFailed(env))
            }
            std::ptr::copy_nonoverlapping(s.as_ptr(), b.data as *mut u8, s.len());
            b
        };
        Ok (Binary { nif_binary: bin, allocated: true })
    }

    fn from_ith_arg(env: *mut ErlNifEnv, i: isize, args: *const ERL_NIF_TERM)
        -> Result<Binary, Error> { unsafe { Self::from_term(env, *args.offset(i)) } }

    fn from_term(env: *mut ErlNifEnv, arg: ERL_NIF_TERM) -> Result<Binary, Error> {
        unsafe {
            let mut bin: ErlNifBinary = uninitialized();
            if !is_enif_ok(enif_inspect_binary(env, arg, &mut bin)) {
                fail!(Error::BadArg(env))
            }
            Ok (Binary { nif_binary: bin, allocated: false })
        }
    }

    fn as_slice(&self) -> &[u8] {
        let &Binary { nif_binary: ref bin, .. } = self;
        unsafe { std::slice::from_raw_parts(bin.data, bin.size) }
    }

    fn to_term(mut self, env: *mut ErlNifEnv) -> Result<ERL_NIF_TERM, Error> {
        let term = unsafe {
            let t = enif_make_binary(env, &mut self.nif_binary);
            if !is_enif_ok(t as c_int) {
                fail!(Error::EnifCallFailed(env))
            }
            t
        };
        self.allocated = false;
        Ok (term)
    }

}

impl Drop for Binary {
    fn drop(&mut self) {
        if self.allocated {
            unsafe { enif_release_binary(&mut self.nif_binary) }
        }
    }
}

impl Debug for Binary {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        try!(formatter.write_str(&format!("ErlNifBinary{:?}", self.as_slice())));
        Ok (())
    }
}

extern "C"
fn print_binary(env: *mut ErlNifEnv,
                argc: c_int,
                args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    assert!(argc == 1);
    let bin = nif_try!(Binary::from_ith_arg(env, 0, args));
    println!("{:?}", bin);
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
    let parser = nif_try!(allocate_parser(env));
    parser
}

// TODO: this leaks memory!
//       I can't just leave the template like this - it will never be deallocated
fn allocate_parser(env: *mut ErlNifEnv) -> Result<ERL_NIF_TERM, Error> {
    let mut parser_template: Box<xml::Parser> = Box::new(xml::Parser::new());
    parser_template.feed_str("");
    unsafe {
        let size = std::mem::size_of::<xml::Parser>();
        let mut parser_p: *mut xml::Parser = enif_alloc_resource(PARSER_RESOURCE, size)
                                             as *mut xml::Parser;
        if !is_enif_ok(parser_p as c_int)
            { fail!(Error::EnifCallFailed(env)) }
        let parser_template_p: *mut xml::Parser = Box::into_raw(parser_template);
        std::ptr::copy_nonoverlapping(parser_template_p, parser_p, size);
        let parser_term = enif_make_resource(env, parser_p as *mut c_void);
        if !is_enif_ok(parser_term as c_int)
            { fail!(Error::EnifCallFailed(env)) }
        enif_release_resource(parser_p as *mut c_void);
        Ok (parser_term)
    }
}

extern "C" fn parser_dtor(_env: *mut ErlNifEnv, parser_p: *mut c_void) -> () {
    let parser = unsafe { Box::from_raw(parser_p as *mut xml::Parser) };
    // let the Drop destructor do the rest
}

// Throws exceptions on errors.
// -spec parse_nif(exml_event:c_parser(), binary(), integer()) -> Result when
//       Result :: {ok, list()}.
extern "C"
fn parse_nif(env: *mut ErlNifEnv,
             argc: c_int,
             args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    assert!(argc == 3);
    let bin = nif_try!(Binary::from_ith_arg(env, 1, args));
    let buf = nif_try!(std::str::from_utf8(bin.as_slice()).or(Err (Error::BadXML(env))));
    let mut parser = nif_try!(get_parser(env, 0, args));
    parser.feed_str(buf);
    let mut events: Vec<ERL_NIF_TERM> = vec![];
    while let Some (ev) = parser.next() {
        match ev {
            Ok (xml::Event::ElementStart (ref tag)) =>
                events.push(nif_try!(xml_element_start(env, tag))),
            Ok (xml::Event::ElementEnd (ref tag)) =>
                events.push(nif_try!(xml_element_end(env, tag))),
            Ok (xml::Event::Characters (ref chars)) =>
                events.push(nif_try!(xml_characters(env, chars))),
            Err (e) => {
                print!("{:?}\r\n", e);
                nif_try!(Err (Error::BadXML(env)))
            }
            _ => {}
        }
    }
    nif_try!(List(&events).to_term(env))
}

fn get_parser(env: *mut ErlNifEnv,
              i: isize,
              args: *const ERL_NIF_TERM) -> Result<&'static mut xml::Parser, Error> {
    unsafe {
        let mut parser: &'static mut xml::Parser = uninitialized();
        // TODO: there might be a more elegant way of doing this 2-step cast...
        let mut parser_mut_p: *mut c_void = parser as *mut xml::Parser as *mut c_void;
        let parser_mut_p2: *mut *mut c_void = &mut parser_mut_p as *mut *mut c_void;
        if !is_enif_ok((enif_get_resource(env, *args.offset(i), PARSER_RESOURCE, parser_mut_p2))) {
            fail!(Error::BadArg(env))
        }
        Ok (parser)
    }
}

fn xml_element_start(env: *mut ErlNifEnv, tag: &xml::StartTag) -> Result<ERL_NIF_TERM, Error> {
    let bname = if let Some (ref prefix) = tag.prefix {
        let bytes: Vec<u8> =
            prefix.bytes().chain(":".bytes()).chain(tag.name.bytes()).collect();
        let prefixed_name = try!(std::str::from_utf8(&bytes)
                                          .or(Err (Error::BadXML(env))));
        try!(Binary::from_string(env, prefixed_name)
                    .and_then(|b| b.to_term(env)))
    } else {
        try!(Binary::from_string(env, &tag.name)
                    .and_then(|b| b.to_term(env)))
    };
    let empty_list = try!(List(&[]).to_term(env));
    let attrs = try!(attribute_list(env, tag.attributes.iter()));
    let nss = try!(namespace_list(env, tag.attributes.iter()));
    Tuple(&[atom::xml_element_start(env), bname, nss, attrs]).to_term(env)
}

fn xml_element_end(env: *mut ErlNifEnv, tag: &xml::EndTag) -> Result<ERL_NIF_TERM, Error> {
    let bname = try!(Binary::from_string(env, &tag.name)
                            .and_then(|b| b.to_term(env)));
    Tuple(&[atom::xml_element_end(env), bname]).to_term(env)
}

fn xml_characters(env: *mut ErlNifEnv, chars: &str) -> Result<ERL_NIF_TERM, Error> {
    let cdata = try!(Binary::from_string(env, chars)
                            .and_then(|b| b.to_term(env)));
    Tuple(&[atom::xml_cdata(env), cdata]).to_term(env)
}

struct Tuple<'a>(&'a [ERL_NIF_TERM]);

impl<'a> Tuple<'a> {
    fn to_term(&self, env: *mut ErlNifEnv) -> Result<ERL_NIF_TERM, Error> {
        let &Tuple(ref arr) = self;
        unsafe {
            let tuple = enif_make_tuple_from_array(env, arr.as_ptr(),
                                                   arr.len() as c_uint);
            if !is_enif_ok(tuple as c_int) {
                fail!(Error::EnifCallFailed(env))
            }
            Ok (tuple)
        }
    }
}

struct List<'a>(&'a [ERL_NIF_TERM]);

impl<'a> List<'a> {
    fn to_term(&self, env: *mut ErlNifEnv) -> Result<ERL_NIF_TERM, Error> {
        let &List(ref arr) = self;
        unsafe {
            let list = enif_make_list_from_array(env, arr.as_ptr(),
                                                 arr.len() as c_uint);
            if !is_enif_ok(list as c_int) {
                fail!(Error::EnifCallFailed(env))
            }
            Ok (list)
        }
    }
}

type AttrIter<'a> = std::collections::hash_map::Iter<'a, (String, Option<String>), String>;

fn attribute_list(env: *mut ErlNifEnv,
                  attrs: AttrIter) -> Result<ERL_NIF_TERM, Error> {
    let l: Vec<ERL_NIF_TERM> = attrs
        .filter(|&(&(ref name, ref opt_ns), value)| {
            if let &Some(ref ns) = opt_ns {
                { false }
            } else if name == "xmlns"
                { false }
            else
                { true }
        })
        .map(|(&(ref name, _), value)| {
            let attr = [nif_try!(Binary::from_string(env, name)
                                        .and_then(|b| b.to_term(env))),
                        nif_try!(Binary::from_string(env, value)
                                        .and_then(|b| b.to_term(env)))];
            nif_try!(Tuple(&attr).to_term(env))
        })
        .collect();
    List(&l).to_term(env)
}

fn namespace_list(env: *mut ErlNifEnv,
                  attrs: AttrIter) -> Result<ERL_NIF_TERM, Error> {
    let l: Vec<ERL_NIF_TERM> = attrs
        .filter(|&(&(ref name, ref opt_ns), value)| {
            if let &Some(ref ns) = opt_ns {
                { true }
            } else if name == "xmlns"
                { true }
            else
                { false }
        })
        .map(|(&(ref name, ref opt_ns), value)| {
            if let &Some(ref ns) = opt_ns {
                if ns == "http://www.w3.org/2000/xmlns/" {
                    let ns = [nif_try!(Binary::from_string(env, value)
                                              .and_then(|b| b.to_term(env))),
                              nif_try!(Binary::from_string(env, name)
                                              .and_then(|b| b.to_term(env)))];
                    nif_try!(Tuple(&ns).to_term(env))
                } else {
                    // Assumption: there's only one namespace which can be used
                    // to define namespace prefixes and it's handled in the case
                    // above this one.
                    unreachable!()
                }
            } else  if name == "xmlns" {
                let ns = [nif_try!(Binary::from_string(env, value)
                                          .and_then(|b| b.to_term(env))),
                          atom::none(env)];
                nif_try!(Tuple(&ns).to_term(env))
            } else {
                // All cases except the handled ones should be filtered
                // out by the previous .filter() pass.
                unreachable!()
            }
        })
        .collect();
    List(&l).to_term(env)
}

fn indent(size: usize) -> String {
    const INDENT: &'static str = "    ";
    (0..size).map(|_| INDENT)
        .fold(String::with_capacity(size * INDENT.len()), |r, s| r + s)
}

#[allow(dead_code)]
fn atom(env: *mut ErlNifEnv, a: &[u8]) -> ERL_NIF_TERM {
    unsafe { enif_make_atom(env, a.as_ptr()) }
}

// Roughly inspired by:
// https://github.com/rust-lang/rust/blob/e3dfb2c45fa3a2da312fc2dbc36aa0c3a06319eb/src/libstd/sys/unix/mod.rs#L91-L98
fn is_enif_ok(t: c_int) -> bool {
    if t != 0 { true } else { false }
}
