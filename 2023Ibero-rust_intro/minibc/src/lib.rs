use crypto_hash::{Algorithm, Hasher};
use std::time::{SystemTime, UNIX_EPOCH};
use std::io::{self, Write};
use hex;

struct Block {
    index: usize,
    timestamp: u64,
    data: String,
    prev_hash: String,
    hash: String
}

impl Block {
    fn new(index: usize, data: String, prev_hash: String) -> Block {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Failed to get system time")
            .as_secs();

        let hash = Block::calculate_hash(index, timestamp, &data, &prev_hash);

        Block {
            index,
            timestamp,
            data,
            prev_hash,
            hash,
        }
    }

    fn calculate_hash(index: usize, timestamp: u64, data: &str, prev_hash: &str) -> String {
        let mut hasher = Hasher::new(Algorithm::MD5);
        hasher.write_all(index.to_string().as_bytes()).unwrap();
        hasher.write_all(timestamp.to_string().as_bytes()).unwrap();
        hasher.write_all(data.as_bytes()).unwrap();
        hasher.write_all(prev_hash.as_bytes()).unwrap();
        let hash_bytes = hasher.finish();

        hex::encode(hash_bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn first() {
        assert_eq!(23, 45);
    } 
}
