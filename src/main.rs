use std::{
    fmt::Debug,
    ops::{Add, AddAssign, Mul},
};

use once_cell::sync::Lazy;

static ALPHA_TABLE: Lazy<[Alpha; 255]> = Lazy::new(Alpha::generate_table);

static ALPHA_REV_TABLE: Lazy<[u8; 256]> = Lazy::new(|| {
    let mut rev_table = [0_u8; 256];
    for (idx, Alpha(bit)) in ALPHA_TABLE.iter().enumerate() {
        rev_table[*bit as usize] = idx as u8;
    }
    rev_table
});

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Alpha(u8);

impl Debug for Alpha {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#b}", self.0)
    }
}

impl Alpha {
    const GEN: Alpha = Alpha(0b11101);
    const ZERO: Alpha = Alpha(0);

    fn pow(&self) -> Alpha {
        if self.0 & 0b1000_0000 == 0 {
            Alpha(self.0 << 1)
        } else {
            Alpha(self.0 << 1) + Alpha::GEN
        }
    }

    fn generate_table() -> [Alpha; 255] {
        let mut alpha_table = [Alpha(0); 255];
        alpha_table[0] = Alpha(1);
        for i in 1..u8::MAX as usize {
            alpha_table[i] = alpha_table[i - 1].pow();
        }
        alpha_table
    }
}

impl Add for Alpha {
    type Output = Alpha;

    fn add(self, rhs: Self) -> Self::Output {
        #[allow(clippy::suspicious_arithmetic_impl)]
        Alpha(self.0 ^ rhs.0)
    }
}
impl AddAssign for Alpha {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl Mul for Alpha {
    type Output = Alpha;
    fn mul(self, rhs: Self) -> Self::Output {
        if self == Alpha::ZERO || rhs == Alpha::ZERO {
            Alpha::ZERO
        } else {
            let lhs_idx = ALPHA_REV_TABLE[self.0 as usize];
            let rhs_idx = ALPHA_REV_TABLE[rhs.0 as usize];
            let result_idx = (lhs_idx as u16 + rhs_idx as u16) % 255;
            ALPHA_TABLE[result_idx as usize]
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Poly(Vec<Alpha>);

impl Poly {
    fn normalize(&mut self) {
        let trailing_zero = self
            .0
            .iter()
            .rev()
            .take_while(|alpha| **alpha == Alpha::ZERO)
            .count();
        self.0.truncate(self.0.len() - trailing_zero);
    }
}

impl Add for Poly {
    type Output = Poly;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        let new_len = self.0.len().max(rhs.0.len());
        self.0.resize(new_len, Alpha::ZERO);
        rhs.0.resize(new_len, Alpha::ZERO);
        for (lhs, rhs) in self.0.iter_mut().zip(rhs.0.into_iter()) {
            *lhs += rhs
        }
        self.normalize();
        self
    }
}

impl Mul for Poly {
    type Output = Poly;

    fn mul(self, rhs: Self) -> Self::Output {
        let new_len = self.0.len() + rhs.0.len() - 1;
        let mut ret = vec![Alpha::ZERO; new_len];
        for (lhs_idx, lhs_alpha) in self.0.iter().enumerate() {
            for (rhs_idx, rhs_alpha) in rhs.0.iter().enumerate() {
                ret[lhs_idx + rhs_idx] += *lhs_alpha * *rhs_alpha;
            }
        }
        let mut ret = Poly(ret);
        ret.normalize();
        ret
    }
}

fn main() {}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;

    #[test]
    fn test_alpha_table_uniqueness() {
        let table = Alpha::generate_table();
        assert_eq!(table.iter().unique().count(), 255)
    }

    #[test]
    fn test_alpha_mul() {
        assert_eq!(ALPHA_TABLE[16] * Alpha::ZERO, Alpha::ZERO);
        assert_eq!(ALPHA_TABLE[16] * ALPHA_TABLE[32], ALPHA_TABLE[48]);
        assert_eq!(ALPHA_TABLE[100] * ALPHA_TABLE[200], ALPHA_TABLE[45]);
    }

    #[test]
    fn test_poly_add() {
        assert_eq!(
            Poly(vec![ALPHA_TABLE[10], Alpha::ZERO]) + Poly(vec![ALPHA_TABLE[10], ALPHA_TABLE[20]]),
            Poly(vec![Alpha::ZERO, ALPHA_TABLE[20]])
        );
        assert_eq!(
            Poly(vec![ALPHA_TABLE[10], ALPHA_TABLE[10]])
                + Poly(vec![ALPHA_TABLE[10], ALPHA_TABLE[10]]),
            Poly(vec![])
        )
    }

    #[test]
    fn test_normalize_poly() {
        let mut lhs = Poly(vec![Alpha::ZERO, ALPHA_TABLE[10], Alpha::ZERO, Alpha::ZERO]);
        let rhs = Poly(vec![Alpha::ZERO, ALPHA_TABLE[10]]);
        lhs.normalize();
        assert_eq!(lhs, rhs)
    }

    #[test]
    fn test_poly_mul() {
        assert_eq!(
            Poly(vec![
                Alpha::ZERO,
                Alpha::ZERO,
                ALPHA_TABLE[1],
                ALPHA_TABLE[2]
            ]) * Poly(vec![ALPHA_TABLE[1], ALPHA_TABLE[1]]),
            Poly(vec![
                Alpha::ZERO,
                Alpha::ZERO,
                ALPHA_TABLE[2],
                ALPHA_TABLE[2] + ALPHA_TABLE[3],
                ALPHA_TABLE[3]
            ])
        );
        
        assert_eq!(
            Poly(vec![ALPHA_TABLE[1], ALPHA_TABLE[100]])
                * Poly(vec![ALPHA_TABLE[1], ALPHA_TABLE[100]]),
            Poly(vec![ALPHA_TABLE[2], Alpha::ZERO, ALPHA_TABLE[200]])
        )
    }
}
