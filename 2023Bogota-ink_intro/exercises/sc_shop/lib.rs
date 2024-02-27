#![cfg_attr(not(feature = "std"), no_std, no_main)]

#[ink::contract]
pub mod little_shop {
    use ink::{
        prelude::{format, vec::Vec},
        storage::Mapping,
    };
    use scale::{Decode, Encode};

    // This is the main contract
    #[ink(storage)]
    #[derive(Default)]
    pub struct LittleShop {
        orders: Vec<(u32, Order)>,
        orders_mapping: Mapping<u32, Order>,
    }

    /// Event emitted when a token transfer occurs
    #[ink(event)]
    pub struct Transfer {
        #[ink(topic)]
        from: Option<AccountId>,
        #[ink(topic)]
        to: Option<AccountId>,
        value: Balance,
    }

    /// Event when shop owner gets a single order
    #[ink(event)]
    pub struct GetAllOrders {
        #[ink(topic)]
        orders: Vec<(u32, Order)>,
    }

    /// Event when shop owner gets a single order
    #[ink(event)]
    pub struct GetSingleOrder {
        #[ink(topic)]
        single_order: Order,
    }

    /// Event when the shop_owner creates his shop
    #[ink(event)]
    pub struct CreatedShopAndStorage {
        #[ink(topic)]
        orders: Vec<(u32, Order)>,
    }

    #[derive(Encode, Decode, Debug, Clone)]
    #[cfg_attr(
        feature = "std",
        derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
    )]
    pub struct Order {
        list_of_items: Vec<FoodItem>,
        customer: AccountId,
        total_price: Balance,
        paid: bool,
        order_id: u32,
    }

    // For catching errors that happens during shop operations
    #[derive(Debug, PartialEq, Eq, Encode, Decode)]
    #[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
    pub enum LittleShopError {
        /// Errors types for different errors
        PaymentError,
        OrderNotCompleted,
    }

    pub type Result<T> = core::result::Result<T, LittleShopError>;

    impl LittleShop {
        #[ink(constructor)]
        pub fn new() -> Self {
            Self::default()
        }

        /// takes the order and makes the payment
        /// todo: cart feature
        #[ink(message, payable)]
        pub fn take_order_and_payment(&mut self, list_of_items: Vec<FoodItem>) -> Result<Order> {
            // Gets the caller account id
            let caller = Self::new().env().caller();

            // Avoid that owner makes an order
            assert!(
                caller != self.env().account_id(),
                "You are not the customer!"
            );

            // assert the order contains at least 1 item
            for item in &list_of_items {
                assert!(item.amount > 0, "Can't take an empty order");
            }

            // our own local id, you can change this to a hash if you want
            // but remember to make the neccessary type changes too!
            let id = self.orders.len() as u32;

            // Calculate and set order price
            let total_price = Order::total_price(&list_of_items);
            let mut order = Order::new(list_of_items, caller, id);
            order.total_price = total_price;

            assert!(!order.paid, "Can't pay for an order that is paid ");

            let multiply: Balance = 1_000_000_000_000; // thsi equals to 1 Azero
            let transfered_val = self.env().transferred_value();

            // assert the value sent == total price
            assert!(
                transfered_val == order.total_price.checked_mul(multiply).expect("Overflow!!"),
                "{}",
                format!("Please pay complete amount which is {}", order.total_price)
            );

            ink::env::debug_println!("Expected value: {}", order.total_price);
            ink::env::debug_println!(
                "Expected received payment without conversion: {}",
                transfered_val
            );

            // make payment
            match self
                .env()
                .transfer(self.env().account_id(), order.total_price)
            {
                Ok(_) => {
                    // get current length of the list orders in storage, this will act as our
                    // unique id
                    let id = self.orders.len() as u32;
                    // mark order as paid
                    order.paid = true;

                    // Emit event
                    self.env().emit_event(Transfer {
                        from: Some(order.customer),
                        to: Some(self.env().account_id()),
                        value: order.total_price,
                    });

                    // Push to storage
                    self.orders_mapping.insert(id, &order);
                    self.orders.push((id, order.clone()));
                    Ok(order)
                }
                Err(_) => Err(LittleShopError::PaymentError),
            }
        }

        #[ink(message)]
        /// gets a single order from storage
        pub fn get_single_order(&self, id: u32) -> Order {
            // get single order
            self.orders_mapping.get(id).expect("Oh no, Order not found")
        }

        #[ink(message)]
        /// gets the orders in storage
        pub fn get_orders(&self) -> Option<Vec<(u32, Order)>> {
            // get all orders
            let get_all_orders = &self.orders;

            if get_all_orders.is_empty() {
                return None;
            }
            Some(get_all_orders.to_vec())
        }
    }

    impl Order {
        fn new(list_of_items: Vec<FoodItem>, customer: AccountId, id: u32) -> Self {
            let total_price = Order::total_price(&list_of_items);
            Self {
                list_of_items,
                customer,
                total_price,
                paid: false,
                order_id: id,
            }
        }

        fn total_price(list_of_items: &Vec<FoodItem>) -> Balance {
            let mut total = 0;
            for item in list_of_items {
                total += item.price()
            }
            total
        }
    }

    #[derive(Encode, Decode, Debug, Clone)]
    #[cfg_attr(
        feature = "std",
        derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
    )]
    pub struct FoodItem {
        item: Menu,
        amount: u32,
    }

    impl FoodItem {
        fn price(&self) -> Balance {
            self.item.price() * self.amount as u128
        }
    }

    #[derive(Encode, Decode, Debug, Clone)]
    #[cfg_attr(
        feature = "std",
        derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
    )]
    pub enum Menu {
        CheeseBurger,
        Chicken,
        Veggie,
    }

    impl Menu {
        fn price(&self) -> Balance {
            match self {
                Self::CheeseBurger => 12,
                Self::Veggie => 10,
                Self::Chicken => 15,
            }
        }
    }
}
