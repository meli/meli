/*
 * meli - melib crate.
 *
 * Copyright 2019 Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

use std::iter::{Extend, FromIterator};
use std::ops::Index;

const STACK_VEC_CAPACITY: usize = 32;
#[derive(Debug, Default)]
pub struct StackVec<T: Default + Copy + std::fmt::Debug> {
    len: usize,
    array: [T; STACK_VEC_CAPACITY],
    heap_vec: Vec<T>,
}

impl<T: Default + Copy + std::fmt::Debug> StackVec<T> {
    pub fn new() -> Self {
        StackVec {
            len: 0,
            array: [T::default(); STACK_VEC_CAPACITY],
            heap_vec: Vec::new(),
        }
    }
    pub fn push(&mut self, ind: T) {
        if self.len == self.array.len() {
            if self.heap_vec.is_empty() {
                self.heap_vec.reserve(STACK_VEC_CAPACITY);
                for _ in 0..STACK_VEC_CAPACITY {
                    self.heap_vec.push(T::default());
                }
            }
            self.heap_vec[0..STACK_VEC_CAPACITY].copy_from_slice(&self.array);
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
        if self.len > self.array.len() {
            self.len -= 1;
            self.heap_vec.pop()
        } else {
            let ret = self.array[self.len - 1];
            self.len -= 1;
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
    pub fn remove(&mut self, i: usize) -> T {
        if self.len > self.array.len() {
            self.len -= 1;
            self.heap_vec.remove(i)
        } else {
            let ret = std::mem::replace(&mut self.array[i], T::default());
            self.len -= 1;
            for i in i..self.len {
                self.array[i] = self.array[i + 1];
            }
            ret
        }
    }

    pub fn set(&mut self, i: usize, val: T) {
        debug_assert!(i < self.len);
        if self.len > self.array.len() {
            self.heap_vec[i] = val;
            if i < self.array.len() {
                self.array[i] = val;
            }
        } else {
            self.array[i] = val;
        }
    }

    pub fn clear(&mut self) {
        self.len = 0;
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
        if self.len > self.array.len() {
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

impl<T: Default + Copy + std::fmt::Debug> FromIterator<T> for StackVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut c = StackVec::new();

        for i in iter {
            c.push(i);
        }

        c
    }
}

pub struct StackVecIterOwned<T: Default + Copy + std::fmt::Debug>(StackVec<T>);
impl<T: Default + Copy + std::fmt::Debug> IntoIterator for StackVec<T> {
    type Item = T;
    type IntoIter = StackVecIterOwned<T>;

    fn into_iter(self) -> Self::IntoIter {
        StackVecIterOwned(self)
    }
}

impl<'a, T: Default + Copy + std::fmt::Debug> IntoIterator for &'a StackVec<T> {
    type Item = &'a T;
    type IntoIter = StackVecIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T: Default + Copy + std::fmt::Debug> Iterator for StackVecIterOwned<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.remove(0))
        }
    }
}
impl<T: Default + Copy + std::fmt::Debug> std::iter::DoubleEndedIterator for StackVecIterOwned<T> {
    fn next_back(&mut self) -> Option<T> {
        if self.0.is_empty() {
            None
        } else {
            self.0.pop()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stackvec() {
        let mut stack = StackVec::from_iter(0..4 * STACK_VEC_CAPACITY);
        let mut ctr = 0;
        assert!(stack.iter().all(|&x| {
            let ret = (x == ctr);
            ctr += 1;
            ret
        }));
        for _ in 0..(3 * STACK_VEC_CAPACITY) + 1 {
            stack.pop();
        }
        ctr = 0;
        assert!(stack.iter().all(|&x| {
            let ret = (x == ctr);
            ctr += 1;
            ret
        }));
    }
}
