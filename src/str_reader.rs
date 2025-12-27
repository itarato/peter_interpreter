pub(crate) struct StrReader<'a> {
    stream: &'a str,
    pub(crate) pos: usize,
}

impl<'a> StrReader<'a> {
    pub(crate) fn new(stream: &'a str) -> Self {
        Self { stream, pos: 0 }
    }

    pub(crate) fn drop(&mut self, len: usize) {
        self.stream = &self.stream[len..];
        self.pos += len;
    }

    pub(crate) fn pop(&mut self, len: usize) -> &'a str {
        let out = &self.stream[..len];
        self.stream = &self.stream[len..];
        self.pos += len;
        out
    }

    pub(crate) fn pop1(&mut self) -> char {
        self.pop(1).chars().next().unwrap()
    }

    pub(crate) fn peek(&self, len: usize) -> Option<&'a str> {
        if self.stream.len() < len {
            None
        } else {
            Some(&self.stream[..len])
        }
    }

    pub(crate) fn peek1(&self) -> Option<char> {
        self.peek(1).map(|s| s.chars().next().unwrap())
    }

    pub(crate) fn pop_until<F>(&mut self, predicate: F) -> &'a str
    where
        F: Fn(char) -> bool,
    {
        self.pop(self.predicate_len(predicate, 0))
    }

    pub(crate) fn drop_until<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        let len = self.predicate_len(predicate, 0);
        self.stream = &self.stream[len..];
        self.pos += len;
    }

    pub(crate) fn len(&self) -> usize {
        self.stream.len()
    }

    fn predicate_len<F>(&self, predicate: F, skip: usize) -> usize
    where
        F: Fn(char) -> bool,
    {
        let mut len = 0;
        let mut chars = self.stream.chars().skip(skip);
        while len < self.stream.len() && chars.next().map(|c| predicate(c)).unwrap_or(false) {
            len += 1;
        }
        len
    }
}

#[cfg(test)]
mod test {
    use crate::str_reader::StrReader;

    #[test]
    fn test_pop() {
        let mut sr = StrReader::new("hello");
        assert_eq!("", sr.pop(0));
        assert_eq!("h", sr.pop(1));
        assert_eq!('e', sr.pop1());
        assert_eq!("llo", sr.pop(3));
    }

    #[test]
    fn test_peek() {
        let sr = StrReader::new("hello");
        assert_eq!(Some(""), sr.peek(0));
        assert_eq!(Some("h"), sr.peek(1));
        assert_eq!(Some('h'), sr.peek1());
        assert_eq!(Some("hello"), sr.peek(5));
        assert_eq!(None, sr.peek(6));
    }

    #[test]
    fn test_pop_and_drop_until() {
        let mut sr = StrReader::new("aaa         123            ccc");
        assert_eq!("", sr.pop_until(|c| { c.is_digit(10) }));
        assert_eq!("aaa", sr.pop_until(|c| c.is_ascii_alphabetic()));

        sr.drop_until(|c| c.is_ascii_whitespace());

        assert_eq!("123", sr.pop_until(|c| c.is_ascii_digit()));

        sr.drop_until(|c| c.is_ascii_whitespace());

        assert_eq!("ccc", sr.pop_until(|_| true));
    }

    #[test]
    fn test_len_and_drop() {
        let mut sr = StrReader::new("abc");
        assert_eq!(3, sr.len());
        sr.drop(1);
        assert_eq!(2, sr.len());
        sr.drop(2);
        assert_eq!(0, sr.len());
    }
}
