use std::collections::VecDeque;
use std::fmt;

pub trait EventStream<K> : fmt::Debug where K : fmt::Debug{
    fn consume_next(&mut self) -> Option<K>;
    fn push_next(&mut self, val: K);
    fn push_last(&mut self, val: K);
    fn is_empty(&self) -> bool;
} 

impl <K> EventStream<K> for VecDeque<K>  where K : fmt::Debug{
    fn consume_next(&mut self) -> Option<K> {
        self.pop_front()
    }

    fn push_next(&mut self, val: K) {
        self.push_front(val);
    }

    fn push_last(&mut self, val: K) {
        self.push_back(val);
    }
    
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn push_next_inserts_on_front() {
        let mut list: VecDeque<i32> = VecDeque::new();
        list.push_next(1);
        assert_eq!(list.front(), Some(&1));
        list.push_last(100);
        list.push_next(2);
        assert_eq!(list.front(), Some(&2));
        list.push_next(3);
        assert_eq!(list.front(), Some(&3));
    }

    #[test]
    fn push_last_inserts_on_back() {
        let mut list: VecDeque<i32> = VecDeque::new();
        list.push_last(1);
        assert_eq!(list.back(), Some(&1));
        list.push_next(100);
        list.push_last(2);
        assert_eq!(list.back(), Some(&2));
        list.push_last(3);
        assert_eq!(list.back(), Some(&3));
    }

    #[test]
    fn consume_next_pops_front() {
        let mut list: VecDeque<i32> = VecDeque::new();
        list.push_next(1);
        assert_eq!(list.consume_next(), Some(1i32));
        list.push_next(1);
        list.push_last(2);
        assert_eq!(list.consume_next(), Some(1i32));
    }
}
