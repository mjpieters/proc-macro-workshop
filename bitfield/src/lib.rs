// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::{bitfield, BitfieldSpecifier};
use num_traits::{NumCast, PrimInt};
use seq::seq;

pub trait Specifier {
    const BITS: usize;
    type InterfaceType;
    type IntType: Default
        + std::fmt::Debug
        + PrimInt
        + NumCast
        + std::ops::BitOrAssign
        + From<Self::InterfaceType>;

    fn to_interface_type(v: Self::IntType) -> Self::InterfaceType;

    fn get(data: &[u8], offset: usize) -> Self::InterfaceType {
        let mut idx = offset / 8;
        let mut offset = offset % 8;
        let mut v = Self::IntType::default();
        let mut remaining = Self::BITS;
        while remaining > 0 {
            let bits = remaining.min(8 - offset);
            remaining -= bits;
            v |= (<Self::IntType as NumCast>::from(data[idx]).unwrap() >> (8 - bits - offset)
                & <Self::IntType as NumCast>::from((1 << bits) - 1).unwrap())
                << remaining;
            idx += 1;
            offset = 0;
        }
        Self::to_interface_type(v)
    }

    fn set(data: &mut [u8], offset: usize, v: Self::InterfaceType) {
        let mut idx = offset / 8;
        let mut offset = offset % 8;
        let mut remaining = Self::BITS;
        let v = <Self::IntType as From<Self::InterfaceType>>::from(v);
        while remaining > 0 {
            let bits = remaining.min(8 - offset);
            remaining -= bits;
            data[idx] |= <u8 as NumCast>::from(
                (v >> remaining & <Self::IntType as NumCast>::from((1 << bits) - 1).unwrap())
                    << (8 - bits - offset),
            )
            .unwrap();
            idx += 1;
            offset = 0;
        }
    }
}

pub trait IntTypeHelperTrait<const N: usize> {
    type Ty;
}
impl IntTypeHelperTrait<8> for () {
    type Ty = u8;
}
impl IntTypeHelperTrait<16> for () {
    type Ty = u16;
}
impl IntTypeHelperTrait<32> for () {
    type Ty = u32;
}
impl IntTypeHelperTrait<64> for () {
    type Ty = u64;
}

pub const fn next_multiple_of_8(n: usize) -> usize {
    if n < 5 {
        8
    } else {
        n.next_power_of_two()
    }
}

seq!(N in 1..=64 {
    pub enum B~N {}

    impl Specifier for B~N {
        const BITS: usize = N;
        type IntType = <() as IntTypeHelperTrait<{next_multiple_of_8(N)}>>::Ty;
        type InterfaceType = Self::IntType;

        fn to_interface_type(v: Self::IntType) -> Self::InterfaceType {
            <Self::InterfaceType as NumCast>::from(v).unwrap()
        }
    }
});

impl Specifier for bool {
    const BITS: usize = 1;
    type IntType = u8;
    type InterfaceType = bool;

    fn to_interface_type(v: Self::IntType) -> Self::InterfaceType {
        v != 0
    }
}
