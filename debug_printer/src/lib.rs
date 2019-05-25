extern crate libc;
extern crate melib;

use melib::Envelope;
use std::ffi::CString;
use std::os::raw::c_char;

#[no_mangle]
pub extern "C" fn print_envelope(ptr: *const Envelope) -> *const c_char {
    unsafe {
        assert!(!ptr.is_null(), "Null pointer in print_envelope");
        //println!("got addr {}", p as u64);
        //unsafe { CString::new("blah".to_string()).unwrap().as_ptr() }
        let s = CString::new(format!("{:?}", *ptr)).unwrap();
        drop(ptr);
        let p = s.as_ptr();
        std::mem::forget(s);
        p
    }
}

#[no_mangle]
pub extern "C" fn get_empty_envelope() -> *mut Envelope {
    let mut ret = Envelope::default();
    let ptr = std::ptr::NonNull::new(&mut ret as *mut Envelope)
        .expect("Envelope::default() has a NULL pointer?");

    let ptr = ptr.as_ptr();
    std::mem::forget(ret);
    ptr
}

#[no_mangle]
pub extern "C" fn destroy_cstring(ptr: *mut c_char) {
    unsafe {
        let slice = CString::from_raw(ptr);
        drop(slice);
    }
}

#[no_mangle]
pub extern "C" fn envelope_size() -> libc::size_t {
    std::mem::size_of::<Envelope>()
}
