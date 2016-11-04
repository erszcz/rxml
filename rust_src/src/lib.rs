#[macro_use] extern crate erlang_nif_sys;
extern crate libc;
extern crate xml;

use erlang_nif_sys::*;
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
           // exml_event:new_parser/0
           nif!(b"new_parser\0", 0,         new_parser),
           nif!(b"reset_parser\0", 1,       not_implemented),
           nif!(b"free_parser\0", 1,        not_implemented),
           // TODO: make parse_nif ERL_NIF_DIRTY_JOB_CPU_BOUND?
           nif!(b"parse_nif\0", 3,          parse_nif),
           nif!(b"escape_cdata_nif\0", 1,   not_implemented),
           nif!(b"unescape_cdata_nif\0", 1, not_implemented),
           nif!(b"escape_attr_nif\0", 1,    not_implemented),
           nif!(b"unescape_attr_nif\0", 1,  not_implemented)
         );

static mut NIF_INTERNAL_ERROR: ERL_NIF_TERM = 0 as ERL_NIF_TERM;
static mut PARSER_RESOURCE: *const ErlNifResourceType = 0 as *const ErlNifResourceType;

mod atom {

    macro_rules! define { ($atom:ident) => {
        #[inline]
        pub fn $atom(env: *mut ::erlang_nif_sys::ErlNifEnv) -> ::erlang_nif_sys::ERL_NIF_TERM {
            unsafe {
                if let Ok (atom) = ::std::ffi::CString::new(stringify!($atom)) {
                    ::erlang_nif_sys::enif_make_atom(env, atom.as_ptr() as *const u8)
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
    define!(none);
    define!(xml_cdata);
    define!(xml_element_end);
    define!(xml_element_start);
    define!(not_implemented);

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

extern "C" fn reload(_env: *mut ErlNifEnv,
                     _priv_data: *mut *mut c_void,
                     _load_info: ERL_NIF_TERM) -> c_int { 0 }

extern "C" fn upgrade(_env: *mut ErlNifEnv,
                      _priv_data: *mut *mut c_void,
                      _old_priv_data: *mut *mut c_void,
                      _load_info: ERL_NIF_TERM) -> c_int { 0 }

extern "C" fn unload(_env: *mut ErlNifEnv,
                     _priv_data: *mut c_void) {}

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

/// Create a new XML parser.
extern "C" fn new_parser(env: *mut ErlNifEnv,
                         argc: c_int,
                         _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    assert!(argc == 0);
    nif_try!(allocate_parser(env))
}

type ParserPointer = *const c_void;

fn allocate_parser(env: *mut ErlNifEnv) -> Result<ERL_NIF_TERM, Error> {
    let parser: Box<xml::Parser> = Box::new(xml::Parser::new());
    unsafe {
        let size = std::mem::size_of::<ParserPointer>();
        let mut parser_addr: *mut ParserPointer = enif_alloc_resource(PARSER_RESOURCE, size)
                                             as *mut ParserPointer;
        if !is_enif_ok(parser_addr as c_int)
            { fail!(Error::EnifCallFailed(env)) }
        let parser_p = Box::into_raw(parser);
        *parser_addr = parser_p as ParserPointer;
        let parser_addr_term = enif_make_resource(env, parser_addr as *mut c_void);
        if !is_enif_ok(parser_addr_term as c_int)
            { fail!(Error::EnifCallFailed(env)) }
        enif_release_resource(parser_addr as *mut c_void);
        Ok (parser_addr_term)
    }
}

extern "C" fn parser_dtor(_env: *mut ErlNifEnv, void_p: *mut c_void) -> () {
    let parser_p = void_p as ParserPointer;
    let _parser = unsafe { Box::from_raw(*(parser_p as *mut *mut xml::Parser)) };
    // let the Drop destructor (if any) do the rest
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
    let parser_guard = ();
    let mut parser = nif_try!(get_parser(env, 0, args, &parser_guard));
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

fn get_parser<'parser>(env: *mut ErlNifEnv,
                       i: isize,
                       args: *const ERL_NIF_TERM,
                       _guard: &'parser ()) -> Result<&'parser mut xml::Parser, Error> {
    unsafe {
        let paddr = &mut (0 as *const c_void) as *mut *const c_void;
        if !is_enif_ok( enif_get_resource(env, *args.offset(i), PARSER_RESOURCE, paddr) ) {
            fail!(Error::BadArg(env))
        }
        assert!(*paddr != 0 as *mut c_void, "enif_get_resource failed");
        let parser = &mut ***(paddr as *mut *mut *mut xml::Parser);
        Ok (parser)
    }
}

fn xml_element_start(env: *mut ErlNifEnv, tag: &xml::StartTag) -> Result<ERL_NIF_TERM, Error> {
    let bname = if let Some (ref prefix) = tag.prefix {
        let prefixed = try!(prefix_with(&tag.name, prefix).or(Err (Error::BadXML(env))));
        try!(Binary::from_string(env, &prefixed)
                    .and_then(|b| b.to_term(env)))
    } else {
        try!(Binary::from_string(env, &tag.name)
                    .and_then(|b| b.to_term(env)))
    };
    let attrs = try!(attribute_list(env, tag.attributes.iter()));
    let nss = try!(namespace_list(env, tag.attributes.iter()));
    Tuple(&[atom::xml_element_start(env), bname, nss, attrs]).to_term(env)
}

fn prefix_with(name: &str, prefix: &str) -> Result<String, ()> {
    let bytes: Vec<u8> =
        prefix.bytes().chain(":".bytes()).chain(name.bytes()).collect();
    std::string::String::from_utf8(bytes).or(Err (()))
}

fn xml_element_end(env: *mut ErlNifEnv, tag: &xml::EndTag) -> Result<ERL_NIF_TERM, Error> {
    let bname = if let Some (ref prefix) = tag.prefix {
        let prefixed = try!(prefix_with(&tag.name, prefix).or(Err (Error::BadXML(env))));
        try!(Binary::from_string(env, &prefixed)
             .and_then(|b| b.to_term(env)))
    } else {
        try!(Binary::from_string(env, &tag.name)
             .and_then(|b| b.to_term(env)))
    };
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
        .filter(|&(&(ref name, ref opt_ns), _value)| {
            if let &Some(ref ns) = opt_ns {
                if ns == "http://www.w3.org/XML/1998/namespace"
                    { true }
                else
                    { false }
            } else if name == "xmlns"
                { false }
            else
                { true }
        })
        .map(|(&(ref name, ref opt_ns), value)| {
            if let &Some (ref ns) = opt_ns {
                if ns == "http://www.w3.org/XML/1998/namespace" {
                    let prefixed = nif_try!(prefix_with(name, "xml")
                                            .or(Err (Error::BadXML(env))));
                    let attr = [nif_try!(Binary::from_string(env, &prefixed)
                                                .and_then(|b| b.to_term(env))),
                                nif_try!(Binary::from_string(env, value)
                                                .and_then(|b| b.to_term(env)))];
                    nif_try!(Tuple(&attr).to_term(env))
                } else {
                    print!("unreachable: {:?}\n\r", (name, opt_ns, value));
                    unreachable!()
                }
            } else {
                let attr = [nif_try!(Binary::from_string(env, name)
                                            .and_then(|b| b.to_term(env))),
                            nif_try!(Binary::from_string(env, value)
                                            .and_then(|b| b.to_term(env)))];
                nif_try!(Tuple(&attr).to_term(env))
            }
        })
        .collect();
    List(&l).to_term(env)
}

// There are two prefixes which are defined by XML standards:
// - xml   - http://www.w3.org/XML/1998/namespace
// - xmlns - http://www.w3.org/2000/xmlns/
// See links for definitions respectively for XML 1.0 and XML 1.1:
// - http://www.w3.org/TR/REC-xml-names/#ns-decl
// - http://www.w3.org/TR/2006/REC-xml-names11-20060816/#ns-decl
fn namespace_list(env: *mut ErlNifEnv,
                  attrs: AttrIter) -> Result<ERL_NIF_TERM, Error> {
    let l: Vec<ERL_NIF_TERM> = attrs
        .filter(|&(&(ref name, ref opt_ns), _value)| {
            if let &Some(ref ns) = opt_ns {
                if ns == "http://www.w3.org/2000/xmlns/"
                    { true }
                else
                    { false }
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
                    print!("unreachable: {:?}\n\r", (name, opt_ns, value));
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
                print!("unreachable: {:?}\n\r", (name, opt_ns, value));
                unreachable!()
            }
        })
        .collect();
    List(&l).to_term(env)
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

extern "C"
fn not_implemented(env: *mut ErlNifEnv,
                   _: c_int,
                   _: *const ERL_NIF_TERM) -> ERL_NIF_TERM
{ atom::not_implemented(env) }
