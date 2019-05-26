use std::iter::Extend;
use std::ops::Index;

#[derive(Debug)]
pub struct StackVec<T: Default + Copy + std::fmt::Debug> {
    len: usize,
    array: [T; 32],
    heap_vec: Vec<T>,
}

impl<T: Default + Copy + std::fmt::Debug> StackVec<T> {
    pub fn new() -> Self {
        StackVec {
            len: 0,
            array: [T::default(); 32],
            heap_vec: Vec::new(),
        }
    }
    pub fn push(&mut self, ind: T) {
        if self.len == self.array.len() {
            if self.heap_vec.is_empty() {
                self.heap_vec.reserve(32);
                for _ in 0..8 {
                    self.heap_vec.push(T::default());
                }
            }
            self.heap_vec[0..8].copy_from_slice(&self.array);
            self.heap_vec.push(ind);
        } else if self.len > self.array.len() {
            self.heap_vec.push(ind);
        } else {
            self.array[self.len] = ind;
        }
        self.len += 1;
    }
    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }
        if self.len >= self.array.len() {
            self.len -= 1;
            self.heap_vec.pop()
        } else {
            let ret = self.array[self.len - 1];
            self.len = self.len - 1;
            Some(ret)
        }
    }
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
    pub fn iter(&self) -> StackVecIter<T> {
        StackVecIter {
            stack: &self,
            range: 0..self.len,
        }
    }
}

pub struct StackVecIter<'a, T: Default + Copy + std::fmt::Debug> {
    stack: &'a StackVec<T>,
    range: std::ops::Range<usize>,
}

impl<'a, T: Default + Copy + std::fmt::Debug> Iterator for StackVecIter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        if self.range.len() == 0 {
            None
        } else {
            let idx = self.range.start;
            self.range.start += 1;
            Some(&self.stack[idx])
        }
    }
}
impl<'a, T: Default + Copy + std::fmt::Debug> std::iter::DoubleEndedIterator
    for StackVecIter<'a, T>
{
    fn next_back(&mut self) -> Option<&'a T> {
        if self.range.len() == 0 {
            None
        } else {
            let idx = self.range.end - 1;
            self.range.end -= 1;
            Some(&self.stack[idx])
        }
    }
}

impl<T: Default + Copy + std::fmt::Debug> Index<usize> for StackVec<T> {
    type Output = T;

    fn index(&self, idx: usize) -> &T {
        if self.len >= self.array.len() {
            &self.heap_vec[idx]
        } else {
            &self.array[idx]
        }
    }
}

impl<T: Default + Copy + std::fmt::Debug> Extend<T> for StackVec<T> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for elem in iter {
            self.push(elem);
        }
    }
}
