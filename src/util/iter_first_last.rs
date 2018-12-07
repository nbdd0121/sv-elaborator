use std::iter::Peekable;

pub trait IdentifyFirstLast: Iterator + Sized {
    fn identify_first_last(self) -> Iter<Self>;
}

impl<T: Iterator> IdentifyFirstLast for T {
    fn identify_first_last(self) -> Iter<Self> {
        Iter {
            first: true,
            iter: self.peekable()
        }
    }
}

pub struct Iter<T: Iterator> {
    first: bool,
    iter: Peekable<T>,
}

impl<T: Iterator> Iterator for Iter<T> {
    type Item = (T::Item, bool, bool);

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.iter.next().map(|e| (e, self.first, self.iter.peek().is_none()));
        self.first = false;
        ret
    }
}
