use event_stream::EventStream;
use std::collections::VecDeque;


#[derive(Debug, PartialEq)]
pub enum LineEventType {
    Line(String),
    EndOfLine,
    EndOfFile
}

#[derive(Debug, PartialEq)]
pub struct LineConsumerEvent {
    pub event_type: LineEventType,
}

#[derive(Debug)]
struct LineConsumerState {
    last_char: char
}


#[derive(Debug, PartialEq)]
pub enum AsciiSimpleType {
    Useful,
    Disposable,
    Control,
}

#[derive(Debug, PartialEq)]
pub enum AsciiType {
    Digit,
    Alphabetic,
    Delimiter,
    Special,
}

#[derive(Debug, PartialEq)]
pub struct AsciiChar {
    pub ascii_char: char,
    pub ascii_simple_type: AsciiSimpleType,
    pub ascii_type: AsciiType,
}
//impl PartialEq for AsciiChar {
//    fn eq(&self, other: &AsciiChar) -> bool {
//        return self.ascii_char == other.ascii_char &&
//            self.ascii_type == other.ascii_type &&
//            self.ascii_simple_type == other.ascii_simple_type
//    }
//
//    fn ne(&self, other: &AsciiChar) -> bool {
//        unimplemented!()
//    }
//}

#[derive(Debug)]
pub struct AsciiConsumerState {
    current: String,
    searching: Option<String>,
    context_vector: Vec<char>
}
impl AsciiConsumerState {
    fn new() -> AsciiConsumerState {
        AsciiConsumerState {
            current: String::from(""),
            searching: None,
            context_vector: vec![]
        }
    }

    fn reset_current(&mut self) {
        self.current = String::from("");
    }
}

fn get_ascii_type(c: char) -> AsciiType {
    let special_chars = vec!['!', '@', '#', '%', '"', '&', '*', '(', ')', '_', '+', '-', '=', '§', '{', '[', 'a', '}', ']', 'o', '?', '/', '°', '`', '\'', '^', '~', '<', ',', '>', '.', ':', ';', '|', '\\', '“', '”'];
    match c {
            d if d.is_digit(10) => AsciiType::Digit,
            a if a.is_alphabetic() => AsciiType::Alphabetic,
            w if w.is_whitespace() => AsciiType::Delimiter,
            s if special_chars.contains(&s) => AsciiType::Special,
            _ => panic!("Invalid character")
    }
}
trait Special {
    fn is_special(&self) -> bool;
}
impl Special for char {
    fn is_special(&self) -> bool {
        let special_chars = vec!['!', '@', '#', '%', '"', '&', '*', '(', ')', '_', '+', '-', '=', '§', '{', '[', 'a', '}', ']', 'o', '?', '/', '°', '`', '\'', '^', '~', '<', ',', '>', '.', ':', ';', '|', '\\', '“', '”'];
        return special_chars.contains(&self);
    }
}

fn get_ascii_simple_type(c: char) -> AsciiSimpleType {
    match c {
        d if d.is_digit(10) || d.is_alphabetic() || d.is_special() => AsciiSimpleType::Useful,
        w if w.is_whitespace() => AsciiSimpleType::Disposable,
        s if s.is_control() => AsciiSimpleType::Control,
        _ => panic!("Invalid character")
    }
}

pub fn consume_line(inp: LineConsumerEvent, state: &mut i32, output_stream: &mut EventStream<AsciiChar>) {
    println!("LineConsumer!");
    println!("  State: {:?}", state);
    println!("  Input: {:?}", inp);
    println!("  Output: {:?}", output_stream);
    match inp.event_type {
        LineEventType::Line(line) => {
            let mut is_last_char_whitespace = true;
            for c in line.chars() {
                if !(is_last_char_whitespace && c.is_whitespace()) {
                    output_stream.push_last(AsciiChar{
                        ascii_char: c,
                        ascii_simple_type: get_ascii_simple_type(c),
                        ascii_type: get_ascii_type(c)
                    });
                }
                is_last_char_whitespace = c.is_whitespace();
            }
        },
        LineEventType::EndOfLine => {},
        LineEventType::EndOfFile => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn must_categorize_ascii_characters() {
        let mut chars: VecDeque<AsciiChar> = VecDeque::new();
        let mut state: i32 = 0;
        let inp = LineConsumerEvent {
            event_type: LineEventType::Line(String::from("10 LET X = 30;")),
        };
        consume_line(inp, &mut state, &mut chars);

        assert_eq!(chars, vec![AsciiChar{ascii_char: '1', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Digit},
                               AsciiChar{ascii_char: '0', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Digit},
                               AsciiChar{ascii_char: ' ', ascii_simple_type: AsciiSimpleType::Disposable, ascii_type: AsciiType::Delimiter},
                               AsciiChar{ascii_char: 'L', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Alphabetic},
                               AsciiChar{ascii_char: 'E', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Alphabetic},
                               AsciiChar{ascii_char: 'T', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Alphabetic},
                               AsciiChar{ascii_char: ' ', ascii_simple_type: AsciiSimpleType::Disposable, ascii_type: AsciiType::Delimiter},
                               AsciiChar{ascii_char: 'X', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Alphabetic},
                               AsciiChar{ascii_char: ' ', ascii_simple_type: AsciiSimpleType::Disposable, ascii_type: AsciiType::Delimiter},
                               AsciiChar{ascii_char: '=', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Special},
                               AsciiChar{ascii_char: ' ', ascii_simple_type: AsciiSimpleType::Disposable, ascii_type: AsciiType::Delimiter},
                               AsciiChar{ascii_char: '3', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Digit},
                               AsciiChar{ascii_char: '0', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Digit},
                               AsciiChar{ascii_char: ';', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Special},]);
    }

    #[test]
    fn must_deduplicate_whitespaces_characters() {
        let mut chars: VecDeque<AsciiChar> = VecDeque::new();
        let mut state: i32 = 0;
        let inp = LineConsumerEvent {
            event_type: LineEventType::Line(String::from("10  LET X  = 30;")),
        };
        consume_line(inp, &mut state, &mut chars);

        assert_eq!(chars, vec![AsciiChar { ascii_char: '1', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Digit },
                               AsciiChar { ascii_char: '0', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Digit },
                               AsciiChar { ascii_char: ' ', ascii_simple_type: AsciiSimpleType::Disposable, ascii_type: AsciiType::Delimiter },
                               AsciiChar { ascii_char: 'L', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Alphabetic },
                               AsciiChar { ascii_char: 'E', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Alphabetic },
                               AsciiChar { ascii_char: 'T', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Alphabetic },
                               AsciiChar { ascii_char: ' ', ascii_simple_type: AsciiSimpleType::Disposable, ascii_type: AsciiType::Delimiter },
                               AsciiChar { ascii_char: 'X', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Alphabetic },
                               AsciiChar { ascii_char: ' ', ascii_simple_type: AsciiSimpleType::Disposable, ascii_type: AsciiType::Delimiter },
                               AsciiChar { ascii_char: '=', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Special },
                               AsciiChar { ascii_char: ' ', ascii_simple_type: AsciiSimpleType::Disposable, ascii_type: AsciiType::Delimiter },
                               AsciiChar { ascii_char: '3', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Digit },
                               AsciiChar { ascii_char: '0', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Digit },
                               AsciiChar { ascii_char: ';', ascii_simple_type: AsciiSimpleType::Useful, ascii_type: AsciiType::Special }, ]);
    }
}
