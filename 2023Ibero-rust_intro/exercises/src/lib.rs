pub fn add(left: usize, right: usize) -> usize {
    left + right
}

fn func(mut var: &usize) {
    var = 2;
}

pub fn collatz(mut num: usize) -> usize {
    let mut steps = 0usize;
    let var: usize;
    func(&var);
    println!("{}", var);

    while num > 1 {
        if num % 2 == 0 {
            num /= 2;
        } else {
            num = 3 * num + 1;
        }
        steps += 1;
    }
    steps
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }

    #[test]
    fn collataz_basic() {
        assert_eq!(collatz(1), 0);
        assert_eq!(collatz(10), 6);
        assert_eq!(collatz(100), 25);
    }

    #[test]
    fn collataz_overflow() {
        assert_eq!(collatz(usize::MAX), 0);
    }
}

