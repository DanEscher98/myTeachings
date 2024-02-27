#![cfg_attr(not(feature = "std"), no_std, no_main)]

#[ink::contract]
mod flipcoin {

    #[ink(storage)]
    pub struct Flipcoin {
        value: bool,
    }

    impl Flipcoin {
        #[ink(constructor)]
        pub fn default() -> Self {
            Self::new(Default::default())
        }

        #[ink(message)]
        pub fn flip(&mut self) {
            self.value = !self.value;
        }

        #[ink(message)]
        pub fn get(&self) -> bool {
            self.value
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[ink::test]
        fn default_works() {
            let flipcoin = Flipcoin::default();
            assert_eq!(flipcoin.get(), false);
        }
    }
}
