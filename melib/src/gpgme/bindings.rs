/*
 * melib - gpgme module
 *
 * Copyright 2020 Manos Pitsidianakis
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

#[cfg(target_pointer_width = "32")]
include!("bindings_rest_32.rs");
#[cfg(target_pointer_width = "64")]
include!("bindings_rest.rs");

macro_rules! convert_to_typedefs {
    () => {};
    (
        pub fn $func_name:ident($($arg:ident:$ty:ty),*$(,)?);
        $($tail:tt)*
    ) => {
        pub type $func_name = unsafe extern "C" fn($($arg:$ty),*);
        convert_to_typedefs! { $($tail)* }
    };
    (
        pub fn $func_name:ident($($arg:ident:$ty:ty),*$(,)?) -> *const $retval:path;
        $($tail:tt)*
    ) => {
        pub type $func_name = unsafe extern "C" fn($($arg:$ty),*) -> *const $retval;
        convert_to_typedefs! { $($tail)* }
    };
    (
        pub fn $func_name:ident($($arg:ident:$ty:ty),*$(,)?) -> *mut $retval:path;
        $($tail:tt)*
    ) => {
        pub type $func_name = unsafe extern "C" fn($($arg:$ty),*) -> *mut $retval;
        convert_to_typedefs! { $($tail)* }
    };
    (
        pub fn $func_name:ident($($arg:ident:$ty:ty),*$(,)?) -> $retval:path;
        $($tail:tt)*
    ) => {
        pub type $func_name = unsafe extern "C" fn($($arg:$ty),*) -> $retval;
        convert_to_typedefs! { $($tail)* }
    };
    (
        extern "C" {
            $($tail:tt)*
        }
    ) => {
        convert_to_typedefs! { $($tail)* }
    };
}

include!("bindings_funcs.rs");
