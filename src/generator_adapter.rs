use std::{ops::{Generator, GeneratorState}, pin::Pin};


pub struct GeneratorAdapter<T: Generator + Unpin> {
    gen: T
}

impl<T: Generator + Unpin> GeneratorAdapter<T> {
    pub fn new(gen: T) -> Self { Self { gen } }
}

impl<T: Generator + Unpin> Iterator for GeneratorAdapter<T> {
    type Item = T::Yield;

    fn next(&mut self) -> Option<Self::Item> {
        match Pin::new(&mut self.gen).resume(()) {
            GeneratorState::Yielded(v) => Some(v),
            GeneratorState::Complete(_) => None,
        }
    }
}

pub trait IntoIterator2 {
    type IntoIter;

    fn into_iter(self) -> Self::IntoIter;
}

impl<T: Generator + Unpin> IntoIterator2 for T {
    type IntoIter = GeneratorAdapter<T>;

    fn into_iter(self) -> Self::IntoIter {
        GeneratorAdapter::new(self)
    }
}
