struct Cipher {
    key: u32,
}

struct Employee {
    name: String,
    id: u32,
    age: u32,
}

impl Employee {
    fn name(&self) -> String {
        self.name.to_owned()
    }
    fn update_name(&mut self, name: String) {
        self.name = name
    }
}

impl Default for Cipher {
    fn default() -> Self {
        Self {
            key: Default::default(),
        }
    }
}

impl Cipher {
    fn new(n: u32) -> Self {
        Self { key: n }
    }

    fn encrypt(&self, msg: &str) -> String {
        msg.chars()
            .map(|c| char::from_u32(c as u32 + self.key).unwrap())
            .collect()
    }
}

fn main() {
    let cipher = Cipher::new(5);
    println!("{}", cipher.encrypt("Hola"));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic() {
        let cipher = Cipher::new(0);
        let msg: &str = "hola";
        assert_eq!(cipher.encrypt(msg), msg);
    }
}
