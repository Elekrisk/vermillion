use std::cell::RefCell;

pub struct Arena<T> {
    /// # SAFETY:
    ///
    /// Items must never be removed before the arena is dropped.
    items: RefCell<Vec<Box<T>>>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self {
            items: RefCell::new(vec![]),
        }
    }

    pub fn add<'a>(&'a self, value: T) -> &'a T {
        let b = Box::new(value);
        let ptr = b.as_ref() as *const T;
        self.items.borrow_mut().push(b);
        unsafe { ptr.as_ref().unwrap() }
    }
}
