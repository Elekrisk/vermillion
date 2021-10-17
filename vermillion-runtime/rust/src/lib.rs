#![allow(clippy::missing_safety_doc)]
use std::{alloc::{self, Layout}, cell::Cell, ffi::{CStr, CString}, marker::PhantomData, mem::{self, ManuallyDrop, MaybeUninit}, ops::Deref, os::raw::c_char, ptr::{NonNull, drop_in_place}, slice, str::Utf8Error};

use num_bigint::{BigInt, Sign};
use num_traits::{FromPrimitive, ToPrimitive};

#[derive(Copy, Clone, PartialEq)]
#[repr(C)]
pub struct CValue {
    tag: Tag,
    data: u64,
}

#[repr(C)]
pub struct ManualRc<T> {
    data: NonNull<ManualRcBacking<T>>,
}

impl<T> ManualRc<T> {
    fn new(data: T) -> Self {
        Self {
            data: NonNull::new(Box::into_raw(Box::new(ManualRcBacking::new(data)))).unwrap(),
        }
    }

    fn from_raw(data: *mut ManualRcBacking<T>) -> Self {
        Self {
            data: NonNull::new(data).unwrap(),
        }
    }

    fn into_raw(s: Self) -> *mut ManualRcBacking<T> {
        s.data.as_ptr()
    }

    fn new_raw(data: T) -> *mut ManualRcBacking<T> {
        Self::into_raw(Self::new(data))
    }

    unsafe fn drop_inner(self) {
        Box::from_raw(self.data.as_ptr());
    }
}

impl<T> Deref for ManualRc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.data.as_ref().data }
    }
}

#[repr(C)]
pub struct ManualRcBacking<T> {
    counter: Cell<u64>,
    data: T
}

impl<T> ManualRcBacking<T> {
    fn new(data: T) -> Self {
        assert!(mem::align_of::<T>() <= mem::align_of::<u64>());
        
        Self {
            counter: Cell::new(1),
            data
        }
    }
}

#[repr(C)]
pub struct ManualRcSlice<T> {
    data: NonNull<ManualRcSliceBacking<T>>,
}

impl<T: Copy> ManualRcSlice<T> {
    fn copy_from_slice(slice: &[T]) -> Self {
        Self {
            data: ManualRcSliceBacking::copy_from_slice(slice),
        }
    }

    fn from_raw(data: *mut ManualRcSliceBacking<T>) -> Self {
        Self {
            data: NonNull::new(data).unwrap(),
        }
    }

    fn into_raw(s: Self) -> *mut ManualRcSliceBacking<T> {
        s.data.as_ptr()
    }

    unsafe fn drop_inner(mut self) {
        ManualRcSliceBacking::destroy(self.data)
    }
}

impl<T: Copy> Deref for ManualRcSlice<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { &self.data.as_ref().as_slice() }
    }
}

#[repr(C)]
pub struct ManualRcSliceBacking<T> {
    counter: Cell<u64>,
    length: u64,
    _marker: PhantomData<T>
}

impl<T: Copy> ManualRcSliceBacking<T> {
    fn copy_from_slice(slice: &[T]) -> NonNull<Self> {
        assert!(mem::align_of::<T>() <= mem::align_of::<u64>());
        assert!(mem::size_of::<Self>() == mem::size_of::<u64>() *  2);
        assert!(mem::align_of::<Self>() == mem::align_of::<u64>());
        
        let ptr = unsafe {
            let ptr = alloc::alloc_zeroed(Layout::from_size_align(mem::size_of::<Self>() + mem::size_of::<T>() * slice.len(), mem::align_of::<Self>()).unwrap());
            let self_ptr = ptr.cast::<Self>();
            self_ptr.write(Self {
                counter: Cell::new(1),
                length: slice.len() as _,
                _marker: PhantomData
            });

            let data_ptr = ptr.cast::<Self>().add(1).cast::<T>();

            for (i, elem) in slice.iter().enumerate() {
                data_ptr.add(i).write(*elem);
            }

            ptr.cast()
        };

        NonNull::new(ptr).unwrap()
    }

    fn as_slice(&self) -> &[T] {
        unsafe {
            let data_ptr = (self as *const Self).add(1).cast::<T>();
            slice::from_raw_parts(data_ptr, self.length as _)
        }
    }

    fn destroy(s: NonNull<Self>) {
        unsafe {
            let layout = Layout::from_size_align(mem::size_of::<Self>() + mem::size_of::<T>() * s.as_ref().length as usize, mem::align_of::<Self>()).unwrap();
            alloc::dealloc(s.as_ptr().cast(), layout);
        }
    }
}

impl ManualRcSliceBacking<u8> {
    fn as_str(&self) -> Result<&str, Utf8Error> {
        std::str::from_utf8(self.as_slice())
    }
}

impl<T: Copy + Default> ManualRcSliceBacking<T> {
    fn with_default(len: usize) -> NonNull<Self> {
        assert!(mem::align_of::<T>() <= mem::align_of::<u64>());
        assert!(mem::size_of::<Self>() == mem::size_of::<u64>() *  2);
        assert!(mem::align_of::<Self>() == mem::align_of::<u64>());
        
        let ptr = unsafe {
            let ptr = alloc::alloc_zeroed(Layout::from_size_align(mem::size_of::<Self>() + mem::size_of::<T>() * len, mem::align_of::<Self>()).unwrap());
            let self_ptr = ptr.cast::<Self>();
            self_ptr.write(Self {
                counter: Cell::new(1),
                length: len as _,
                _marker: PhantomData
            });

            let data_ptr = ptr.cast::<Self>().add(1).cast::<T>();

            for i in 0..len {
                data_ptr.add(i).write(T::default());
            }

            ptr.cast()
        };

        NonNull::new(ptr).unwrap()
    }
}

impl CValue {
    unsafe fn rc<T>(&self, tag: Tag) -> Option<ManualRc<T>> {
        if self.tag == tag {
            Some(ManualRc::from_raw(self.data as _))
        } else {
            None
        }
    }

    unsafe fn rc_slice<T: Copy>(&self, tag: Tag) -> Option<ManualRcSlice<T>> {
        if self.tag == tag {
            Some(ManualRcSlice::from_raw(self.data as _))
        } else {
            None
        }
    }

    fn rc_int(&self) -> Option<ManualRc<BigInt>> {
        unsafe { self.rc::<BigInt>(Tag::Int) }
    }

    fn rc_string(&self) -> Option<ManualRcSlice<u8>> {
        unsafe { self.rc_slice::<u8>(Tag::String) }
    }

    fn rc_tuple(&self) -> Option<ManualRcSlice<CValue>> {
        unsafe { self.rc_slice::<CValue>(Tag::Tuple) }
    }

    fn rc_function(&self) -> Option<ManualRc<FuncInner>> {
        unsafe { self.rc::<FuncInner>(Tag::Function) }
    }

    fn rc_exception(&self) -> Option<ManualRc<ExcInner>> {
        unsafe { self.rc::<ExcInner>(Tag::Exception) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
enum Tag {
    Nil = 0,
    Bool = 1,
    Char = 2,
    Float = 3,
    Int = 4,
    String = 5,
    Tuple = 6,
    Function = 7,
    Exception = 255,
}

#[repr(C)]
struct FuncInner {
    arg_count: u64,
    fn_ptr: u64,
}

#[repr(C)]
struct ExcInner {
    msg: String,
    stack_trace: Vec<FrameInfo>,
}

#[derive(Clone, PartialEq)]
struct FrameInfo {
    file_name: String,
    function_name: String,
    line: u64,
    column: u64,
}

enum Value {
    Nil,
    Bool(bool),
    Char(char),
    Float(f64),
    Int(ManualRc<BigInt>),
    String(ManualRcSlice<u8>),
    Tuple(ManualRcSlice<CValue>),
    Function(ManualRc<FuncInner>),
    Exception(ManualRc<ExcInner>),
}

impl Value {
    fn as_int(&self) -> &ManualRc<BigInt> {
        match self {
            Self::Int(v) => v,
            _ => panic!(),
        }
    }
    fn as_string(&self) -> &ManualRcSlice<u8> {
        match self {
            Self::String(v) => v,
            _ => panic!(),
        }
    }
    fn as_tuple(&self) -> &ManualRcSlice<CValue> {
        match self {
            Self::Tuple(v) => v,
            _ => panic!(),
        }
    }
    fn as_function(&self) -> &ManualRc<FuncInner> {
        match self {
            Self::Function(v) => v,
            _ => panic!(),
        }
    }
    fn as_exception(&self) -> &ManualRc<ExcInner> {
        match self {
            Self::Exception(v) => v,
            _ => panic!(),
        }
    }

    fn new_int(val: BigInt) -> Self {
        Self::Int(ManualRc::new(val))
    }

    fn new_string(val: &str) -> Self {
        Self::String(ManualRcSlice::copy_from_slice(val.as_bytes()))
    }

    fn new_tuple(val: &[CValue]) -> Self {
        Self::Tuple(ManualRcSlice::copy_from_slice(val))
    }

    fn new_function(val: FuncInner) -> Self {
        Self::Function(ManualRc::new(val))
    }

    fn new_exception(val: ExcInner) -> Self {
        Self::Exception(ManualRc::new(val))
    }

    fn name(&self) -> &'static str {
        match self {
            Value::Nil => "Nil",
            Value::Bool(_) => "Bool",
            Value::Char(_) => "Char",
            Value::Float(_) => "Float",
            Value::Int(_) => "Int",
            Value::String(_) => "String",
            Value::Tuple(_) => "Tuple",
            Value::Function(_) => "Function",
            Value::Exception(_) => "Exception",
        }
    }
}

impl From<CValue> for Value {
    fn from(cval: CValue) -> Self {
        unsafe {
            match cval.tag {
                Tag::Nil => Value::Nil,
                Tag::Bool => Value::Bool(cval.data & 1 == 1),
                Tag::Char => Value::Char(char::from_u32_unchecked(cval.data as _)),
                Tag::Float => Value::Float(f64::from_bits(cval.data)),
                Tag::Int => Value::Int(cval.rc_int().unwrap()),
                Tag::String => Value::String(cval.rc_string().unwrap()),
                Tag::Tuple => Value::Tuple(cval.rc_tuple().unwrap()),
                Tag::Function => Value::Function(cval.rc_function().unwrap()),
                Tag::Exception => Value::Exception(cval.rc_exception().unwrap()),
            }
        }
    }
}

impl From<Value> for CValue {
    fn from(val: Value) -> Self {
        match val {
            Value::Nil => Self {
                tag: Tag::Nil,
                data: 0,
            },
            Value::Bool(b) => Self {
                tag: Tag::Bool,
                data: b as _,
            },
            Value::Char(c) => Self {
                tag: Tag::Char,
                data: c as _,
            },
            Value::Float(f) => Self {
                tag: Tag::Float,
                data: f.to_bits(),
            },
            Value::Int(rc) => Self {
                tag: Tag::Int,
                data: ManualRc::into_raw(rc) as _,
            },
            Value::String(rc) => Self {
                tag: Tag::String,
                data: ManualRcSlice::into_raw(rc) as _,
            },
            Value::Tuple(rc) => Self {
                tag: Tag::Tuple,
                data: ManualRcSlice::into_raw(rc) as _,
            },
            Value::Function(rc) => Self {
                tag: Tag::Function,
                data: ManualRc::into_raw(rc) as _,
            },
            Value::Exception(rc) => Self {
                tag: Tag::Exception,
                data: ManualRc::into_raw(rc) as _,
            },
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn __print(cval: CValue) -> CValue {
    if cval.tag == Tag::Exception {
        if let Value::Exception(e) = cval.into() {
            let msg = &e.msg;
            println!("Error: {}", msg.as_str());
            println!("Stack trace:");
            for frame in e.stack_trace.as_slice() {
                println!(
                    "{}:{}:{}: in function {}",
                    frame.file_name, frame.line, frame.column, frame.function_name
                );
            }

            Value::Nil.into()
        } else {
            unreachable!()
        }
    } else {
        print!("{}", to_str(cval));
        Value::Nil.into()
    }
}

#[no_mangle]
pub unsafe extern "C" fn __println(cval: CValue) -> CValue {
    let ret = __print(cval);
    if ret.tag != Tag::Exception {
        println!();
    }
    ret
}

fn to_str(cval: CValue) -> String {
    match cval.tag {
        Tag::Nil => "nil".to_string(),
        Tag::Bool => if cval.data > 0 { "true" } else { "false" }.to_string(),
        Tag::Char => char::from_u32(cval.data as _).unwrap().to_string(),
        Tag::Float => f64::from_bits(cval.data).to_string(),
        Tag::Int => cval.rc_int().unwrap().to_string(),
        Tag::String => std::str::from_utf8(&cval.rc_string().unwrap()).unwrap().to_string(),
        Tag::Tuple => {
            let mut ret = String::new();
            let vals = cval.rc_tuple().unwrap();
            if let Some((last, rest)) = vals.split_last() {
                for val in rest {
                    ret.push_str(&to_str(*val));
                    ret.push_str(", ");
                }
                ret.push_str(&to_str(*last));
            }
            ret
        }
        Tag::Function => "Function".to_string(),
        Tag::Exception => "Exception".to_string(),
    }
}

pub mod int_funcs {
    use std::slice;

    use crate::CValue;
    use crate::ManualRcSlice;
    use crate::Value;
    use num_bigint::BigInt;
    use num_bigint::Sign;
    use num_traits::FromPrimitive;

    use crate::ManualRc;

    #[no_mangle]
    pub extern "C" fn __int_create_from_u64(val: u64) -> u64 {
        ManualRc::new_raw(BigInt::from_u64(val).unwrap()) as _
    }

    #[no_mangle]
    pub extern "C" fn __int_create_from_i64(val: i64) -> u64 {
        ManualRc::new_raw(BigInt::from_i64(val).unwrap()) as _
    }

    /// # Safety
    /// `bytes` must be at least `byte_length` bytes long.
    #[no_mangle]
    pub unsafe extern "C" fn __int_create_from_raw(
        negative: bool,
        byte_length: usize,
        bytes: *const u8,
    ) -> u64 {
        ManualRc::new_raw(BigInt::from_bytes_le(
            if negative { Sign::Minus } else { Sign::Plus },
            slice::from_raw_parts(bytes, byte_length),
        )) as _
    }

    #[no_mangle]
    pub unsafe extern "C" fn __int_destroy(ptr: u64) {
        let rc = ManualRc::<BigInt>::from_raw(ptr as _);
        debug_assert!(rc.data.as_ref().counter.get() == 0);
        rc.drop_inner();
    }

    #[no_mangle]
    pub unsafe extern "C" fn __int_to_str(cval: CValue) -> u64 {
        if let Value::Int(v) = cval.into() {
            ManualRcSlice::into_raw(ManualRcSlice::copy_from_slice(v.to_string().as_bytes())) as _
        } else {
            panic!();
        }
    }
}

mod string_funcs {
    
}

mod tuple_funcs {
    
}

mod function_funcs {
    
}

mod exception_funcs {
    use crate::{ExcInner, ManualRc};

    #[no_mangle]
    pub unsafe extern "C" fn __exception_destroy(ptr: u64) {
        let rc = ManualRc::<ExcInner>::from_raw(ptr as _);
        debug_assert!(rc.data.as_ref().counter.get() == 0);
        rc.drop_inner();
    }
}

// #[no_mangle]
// pub extern "C" fn int_to_float(cval: CValue) -> f64 {
//     if let Value::Int(val) = cval.into() {
//         let ret = val.to_f64().unwrap();
//         cval.drop();
//         ret
//     } else {
//         panic!();
//     }
// }

// #[no_mangle]
// pub extern "C" fn int_add(lhs: CValue, rhs: CValue) -> u64 {
//     if let (Value::Int(l), Value::Int(r)) = (lhs.into(), rhs.into()) {
//         let ret = Rc::into_raw(Rc::new(&**l + &**r)) as u64;
//         lhs.drop();
//         rhs.drop();
//         ret
//     } else {
//         panic!();
//     }
// }

// #[no_mangle]
// pub extern "C" fn int_sub(lhs: CValue, rhs: CValue) -> u64 {
//     if let (Value::Int(l), Value::Int(r)) = (lhs.into(), rhs.into()) {
//         let ret = Rc::into_raw(Rc::new(&**l - &**r)) as u64;
//         lhs.drop();
//         rhs.drop();
//         ret
//     } else {
//         panic!();
//     }
// }

// #[no_mangle]
// pub extern "C" fn int_mul(lhs: CValue, rhs: CValue) -> u64 {
//     if let (Value::Int(l), Value::Int(r)) = (lhs.into(), rhs.into()) {
//         let ret = Rc::into_raw(Rc::new(&**l * &**r)) as u64;
//         lhs.drop();
//         rhs.drop();
//         ret
//     } else {
//         panic!();
//     }
// }

// #[no_mangle]
// pub extern "C" fn int_idiv(lhs: CValue, rhs: CValue) -> u64 {
//     if let (Value::Int(l), Value::Int(r)) = (lhs.into(), rhs.into()) {
//         let ret = Rc::into_raw(Rc::new(&**l / &**r)) as u64;
//         lhs.drop();
//         rhs.drop();
//         ret
//     } else {
//         panic!();
//     }
// }

// #[no_mangle]
// pub extern "C" fn int_rem(lhs: CValue, rhs: CValue) -> u64 {
//     if let (Value::Int(l), Value::Int(r)) = (lhs.into(), rhs.into()) {
//         let ret = Rc::into_raw(Rc::new(&**l % &**r)) as u64;
//         lhs.drop();
//         rhs.drop();
//         ret
//     } else {
//         panic!();
//     }
// }

// #[no_mangle]
// pub extern "C" fn sub(lhs: CValue, rhs: CValue) -> CValue {
//     let left = lhs.into();
//     let right = rhs.into();
//     let ret = match (left, right) {
//         (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
//         (Value::Float(l), Value::Int(r)) => Value::Float(l - unsafe { r.to_f64().unwrap() }),
//         (Value::Int(l), Value::Float(r)) => Value::Float(unsafe { l.to_f64().unwrap() } - r),
//         (Value::Int(l), Value::Int(r)) => Value::new_int(&**l - &**r),
//         (l, r) => Value::new_exception(ExcInner {
//             msg: format!("{} - {} is invalid", l.name(), r.name()),
//             stack_trace: vec![],
//         }),
//     }
//     .into();

//     lhs.drop();
//     rhs.drop();
//     ret
// }

// #[no_mangle]
// pub extern "C" fn mul(lhs: CValue, rhs: CValue) -> CValue {
//     let left = lhs.into();
//     let right = rhs.into();
//     let ret = match (left, right) {
//         (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
//         (Value::Float(l), Value::Int(r)) => Value::Float(l * unsafe { r.to_f64().unwrap() }),
//         (Value::Int(l), Value::Float(r)) => Value::Float(unsafe { l.to_f64().unwrap() } * r),
//         (Value::Int(l), Value::Int(r)) => Value::new_int(&**l * &**r),
//         (Value::String(l), Value::Int(r)) => match r.to_usize() {
//             Some(r) => {
//                 let mut ret = String::with_capacity(l.len() * r);
//                 for _ in 0..r {
//                     ret.push_str(&l);
//                 }

//                 Value::new_string(ret)
//             }
//             None => {
//                 Value::new_exception(ExcInner {
//                     msg: format!("String cannot be multiplied {} times; max is {}", &*r, usize::MAX),
//                     stack_trace: vec![],
//                 })
//             }
//         }
//         (Value::Tuple(l), Value::Int(r)) => match r.to_usize() {
//             Some(r) => {
//                 for cval in &**l {
//                     for _ in 0..r {
//                         cval.inc_ref();
//                     }
//                 }
//                 let mut ret = Vec::with_capacity(l.len() * r);
//                 for _ in 0..r {
//                     ret.extend_from_slice(&l);
//                 }

//                 Value::new_tuple(ret)
//             }
//             None => {
//                 Value::new_exception(ExcInner {
//                     msg: format!("String cannot be multiplied {} times; max is {}", &*r, usize::MAX),
//                     stack_trace: vec![],
//                 })
//             }
//         }
//         (l, r) => Value::new_exception(ExcInner {
//             msg: format!("{} * {} is invalid", l.name(), r.name()),
//             stack_trace: vec![],
//         }),
//     }
//     .into();

//     lhs.drop();
//     rhs.drop();
//     ret
// }

// #[no_mangle]
// pub extern "C" fn div(lhs: CValue, rhs: CValue) -> CValue {
//     let left = lhs.into();
//     let right = rhs.into();
//     let ret = match (left, right) {
//         (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
//         (Value::Float(l), Value::Int(r)) => Value::Float(l / unsafe { r.to_f64().unwrap() }),
//         (Value::Int(l), Value::Float(r)) => Value::Float(unsafe { l.to_f64().unwrap() } / r),
//         (Value::Int(l), Value::Int(r)) => Value::Float(unsafe { l.to_f64().unwrap() } / unsafe { r.to_f64().unwrap() }),
//         (l, r) => Value::new_exception(ExcInner {
//             msg: format!("{} / {} is invalid", l.name(), r.name()),
//             stack_trace: vec![],
//         }),
//     }
//     .into();

//     lhs.drop();
//     rhs.drop();
//     ret
// }

// #[no_mangle]
// pub extern "C" fn idiv(lhs: CValue, rhs: CValue) -> CValue {
//     let left = lhs.into();
//     let right = rhs.into();
//     let ret = match (left, right) {
//         (Value::Int(l), Value::Int(r)) => Value::new_int(&**l / &**r),
//         (l, r) => Value::new_exception(ExcInner {
//             msg: format!("{} // {} is invalid", l.name(), r.name()),
//             stack_trace: vec![],
//         }),
//     }
//     .into();

//     lhs.drop();
//     rhs.drop();
//     ret
// }

// #[no_mangle]
// pub extern "C" fn rem(lhs: CValue, rhs: CValue) -> CValue {
//     let left = lhs.into();
//     let right = rhs.into();
//     let ret = match (left, right) {
//         (Value::Int(l), Value::Int(r)) => Value::new_int(&**l % &**r),
//         (l, r) => Value::new_exception(ExcInner {
//             msg: format!("{} % {} is invalid", l.name(), r.name()),
//             stack_trace: vec![],
//         }),
//     }
//     .into();

//     lhs.drop();
//     rhs.drop();
//     ret
// }

// #[no_mangle]
// pub extern "C" fn eq(lhs: CValue, rhs: CValue) -> CValue {
//     let l: Value = lhs.into();
//     let r: Value = rhs.into();

//     let ret = Value::Bool(l == r);

//     lhs.drop();
//     rhs.drop();

//     ret.into()
// }

// #[no_mangle]
// pub extern "C" fn neq(lhs: CValue, rhs: CValue) -> CValue {
//     let l: Value = lhs.into();
//     let r: Value = rhs.into();

//     let ret = Value::Bool(l != r);

//     lhs.drop();
//     rhs.drop();

//     ret.into()
// }

// #[no_mangle]
// pub extern "C" fn gt(lhs: CValue, rhs: CValue) -> CValue {
//     let l: Value = lhs.into();
//     let r: Value = rhs.into();

//     let ret = match (l, r) {
//         (Value::Float(l), Value::Float(r)) => Value::Bool(l > r),
//         (Value::Float(l), Value::Int(r)) => Value::Bool(l > r.to_f64().unwrap()),
//         (Value::Int(l), Value::Float(r)) => Value::Bool(l.to_f64().unwrap() > r),
//         (Value::Int(l), Value::Int(r)) => Value::Bool(l > r),
//         (l, r) => Value::new_exception(ExcInner {
//             msg: format!("{} > {} is invalid", l.name(), r.name()),
//             stack_trace: vec![]
//         })
//     };

//     lhs.drop();
//     rhs.drop();

//     ret.into()
// }

// #[no_mangle]
// pub extern "C" fn gteq(lhs: CValue, rhs: CValue) -> CValue {
//     let l: Value = lhs.into();
//     let r: Value = rhs.into();

//     let ret = match (l, r) {
//         (Value::Float(l), Value::Float(r)) => Value::Bool(l >= r),
//         (Value::Float(l), Value::Int(r)) => Value::Bool(l >= r.to_f64().unwrap()),
//         (Value::Int(l), Value::Float(r)) => Value::Bool(l.to_f64().unwrap() >= r),
//         (Value::Int(l), Value::Int(r)) => Value::Bool(l >= r),
//         (l, r) => Value::new_exception(ExcInner {
//             msg: format!("{} >= {} is invalid", l.name(), r.name()),
//             stack_trace: vec![]
//         })
//     };

//     lhs.drop();
//     rhs.drop();

//     ret.into()
// }

// #[no_mangle]
// pub extern "C" fn lt(lhs: CValue, rhs: CValue) -> CValue {
//     let l: Value = lhs.into();
//     let r: Value = rhs.into();

//     let ret = match (l, r) {
//         (Value::Float(l), Value::Float(r)) => Value::Bool(l < r),
//         (Value::Float(l), Value::Int(r)) => Value::Bool(l < r.to_f64().unwrap()),
//         (Value::Int(l), Value::Float(r)) => Value::Bool(l.to_f64().unwrap() < r),
//         (Value::Int(l), Value::Int(r)) => Value::Bool(l < r),
//         (l, r) => Value::new_exception(ExcInner {
//             msg: format!("{} < {} is invalid", l.name(), r.name()),
//             stack_trace: vec![]
//         })
//     };

//     lhs.drop();
//     rhs.drop();

//     ret.into()
// }

// #[no_mangle]
// pub extern "C" fn lteq(lhs: CValue, rhs: CValue) -> CValue {
//     let l: Value = lhs.into();
//     let r: Value = rhs.into();

//     let ret = match (l, r) {
//         (Value::Float(l), Value::Float(r)) => Value::Bool(l <= r),
//         (Value::Float(l), Value::Int(r)) => Value::Bool(l <= r.to_f64().unwrap()),
//         (Value::Int(l), Value::Float(r)) => Value::Bool(l.to_f64().unwrap() <= r),
//         (Value::Int(l), Value::Int(r)) => Value::Bool(l <= r),
//         (l, r) => Value::new_exception(ExcInner {
//             msg: format!("{} <= {} is invalid", l.name(), r.name()),
//             stack_trace: vec![]
//         })
//     };

//     lhs.drop();
//     rhs.drop();

//     ret.into()
// }

// // #[cfg(test)]
// // mod test {
// //     use std::rc::Weak;

// //     use super::*;

// //     #[test]
// //     fn create_tuple_test() {
// //         unsafe {
// //             let a = create_int_from_u64(0);
// //             let ac = a.weak();
// //             let b = create_int_from_u64(1);
// //             let bc = b.weak();
// //             let c = create_int_from_u64(2);
// //             let cc = c.weak();
// //             let d = create_int_from_u64(3);
// //             let dc = d.weak();
// //             let e = create_int_from_u64(4);
// //             let ec = e.weak();
// //             let array = [a, b, c, d, e];

// //             let counts = [ac, bc, cc, dc, ec];

// //             for count in &counts {
// //                 assert_eq!(count.count(), 1);
// //             }

// //             let tuple = create_tuple(5, array.as_ptr());

// //             for count in &counts {
// //                 assert_eq!(count.count(), 2);
// //             }

// //             a.drop();
// //             b.drop();
// //             e.drop();

// //             assert_eq!(counts[0].count(), 1);
// //             assert_eq!(counts[1].count(), 1);
// //             assert_eq!(counts[2].count(), 2);
// //             assert_eq!(counts[3].count(), 2);
// //             assert_eq!(counts[4].count(), 1);

// //             tuple.drop();

// //             assert_eq!(counts[0].count(), 0);
// //             assert_eq!(counts[1].count(), 0);
// //             assert_eq!(counts[2].count(), 1);
// //             assert_eq!(counts[3].count(), 1);
// //             assert_eq!(counts[4].count(), 0);

// //             c.drop();
// //             d.drop();
// //         }
// //     }

// //     #[test]
// //     fn str_test() {
// //         unsafe {
// //             let nil = create_nil();
// //             let bool_t = create_bool(true);
// //             let bool_f = create_bool(false);
// //             let f = create_float(32535.32534);
// //             let i = create_int_from_raw(
// //                 false,
// //                 16,
// //                 [
// //                     0x83, 0x11, 0x27, 0xf8, 0xee, 0xb0, 0x30, 0xec, 0x00, 0xb0, 0x7f, 0x11, 0x2d,
// //                     0xd0, 0x5b, 0x15,
// //                 ]
// //                 .as_ptr(),
// //             );
// //             let s = create_string_from_cstr(b"Hello, World!\0".as_ptr() as _);
// //             let t = create_tuple(4, [nil, bool_t, f, i].as_ptr());

// //             let s1 = str(nil);
// //             let s2 = str(bool_t);
// //             let s3 = str(bool_f);
// //             let s4 = str(f);
// //             let s5 = str(i);
// //             let s6 = str(s);
// //             let s7 = str(t);

// //             assert_eq!(**Value::from(s1).as_string(), "nil");
// //             assert_eq!(**Value::from(s2).as_string(), "true");
// //             assert_eq!(**Value::from(s3).as_string(), "false");
// //             assert_eq!(**Value::from(s4).as_string(), 32535.32534.to_string());
// //             assert_eq!(
// //                 **Value::from(s5).as_string(),
// //                 "28390509237490385234093802832903213443"
// //             );
// //             assert_eq!(**Value::from(s6).as_string(), "Hello, World!");
// //             assert_eq!(
// //                 **Value::from(s7).as_string(),
// //                 format!(
// //                     "nil, true, {}, 28390509237490385234093802832903213443",
// //                     &32535.32534.to_string()
// //                 )
// //             );
// //         }
// //     }

// //     #[test]
// //     fn add_test() {
// //     }

// //     trait Count {
// //         fn count(&self) -> usize;
// //     }

// //     impl<T> Count for Weak<T> {
// //         fn count(&self) -> usize {
// //             self.strong_count()
// //         }
// //     }

// //     trait ToCount {
// //         unsafe fn weak(&self) -> Box<dyn Count>;
// //     }

// //     impl ToCount for CValue {
// //         unsafe fn weak(&self) -> Box<dyn Count> {
// //             unsafe fn f<T>(v: &CValue) -> Weak<T> {
// //                 Rc::downgrade(&ManuallyDrop::new(Rc::from_raw(v.data as _)))
// //             }

// //             match self.tag {
// //                 Tag::Int => Box::new(f::<BigInt>(self)),
// //                 Tag::String => Box::new(f::<String>(self)),
// //                 Tag::Tuple => Box::new(f::<Vec<CValue>>(self)),
// //                 Tag::Function => Box::new(f::<FuncInner>(self)),
// //                 Tag::Exception => Box::new(f::<ExcInner>(self)),
// //                 _ => unimplemented!(),
// //             }
// //         }
// //     }
// // }
