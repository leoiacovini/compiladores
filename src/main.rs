mod event_stream;
mod event_machine;

use event_stream::EventStream;
use event_machine::EventMachine;

use std::collections::LinkedList;
use std::collections::VecDeque;

use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;
use std::string::String;

fn consume_testing(inp: i32, state: &mut i32, output_stream: &mut EventStream<i32>) {
    // state += inp;
    println!("Consuming!");
    println!("  State: {:?}", state);
    println!("  Input: {:?}", inp);
    println!("  Output: {:?}", output_stream);
    
    *state += 1;
    output_stream.push_next(100);
    // println!("input: {:?}", eventmachine.input);
    // println!("output: {:?}", eventmachine.output);
    //return (state, output_stream);
}

#[derive(Debug)]
struct FileConsumerEvent<'a> {
    filename: &'a str
}

#[derive(Debug)]
struct LineConsumerEvent {
    line: String
}

#[derive(Debug)]
enum AsciiType {
    Digit,
    Alphabetic,
    Whitespace,
    Special
}

#[derive(Debug)]
struct AsciiConsumerEvent {
    ascii_char: char,
    ascii_type: AsciiType
}

fn consume_file(inp: FileConsumerEvent, state: &mut i32, output_stream: &mut EventStream<LineConsumerEvent>) {
    println!("FileConsumer!");
    println!("  State: {:?}", state);
    println!("  Input: {:?}", inp);
    println!("  Output: {:?}", output_stream);
    let f = match File::open(inp.filename) {
        Ok(file) => file,
        Err(e) => {
            panic!("File not found!");
        }
    };
    let file = BufReader::new(&f);
    for (num, file_line) in file.lines().enumerate() {
        *state = num as i32;
        match file_line {
            Ok(file_line) => output_stream.push_last(LineConsumerEvent{line: file_line}),
            Err(e) => panic!("File line not found!"),
        }
    }

}


fn consume_line(inp: LineConsumerEvent, state: &mut i32, output_stream: &mut EventStream<AsciiConsumerEvent>) {
    println!("LineConsumer!");
    println!("  State: {:?}", state);
    println!("  Input: {:?}", inp);
    println!("  Output: {:?}", output_stream);
    let special_chars = vec!['!', '@', '#', '%', '"', '&', '*', '(', ')', '_', '+', '-', '=', '§', '{', '[', 'a', '}', ']', 'o', '?', '/', '°', '`', '\'', '^', '~', '<', ',', '>', '.', ':', ';', '|', '\\', '“', '”'];
    for c in inp.line.chars() {
        match c {
            d if d.is_digit(10) => output_stream.push_last(AsciiConsumerEvent{ascii_char: d, ascii_type: AsciiType::Digit}),
            a if a.is_alphabetic() => output_stream.push_last(AsciiConsumerEvent{ascii_char: a, ascii_type: AsciiType::Alphabetic}),
            w if w.is_whitespace() => output_stream.push_last(AsciiConsumerEvent{ascii_char: w, ascii_type: AsciiType::Whitespace}),
            s if special_chars.contains(&s) => output_stream.push_last(AsciiConsumerEvent{ascii_char: s, ascii_type: AsciiType::Special}),
            _ => panic!("Invalid character")
        }
    }
}

#[derive(Debug)]
struct AsciiConsumerState {
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
fn consume_ascii(inp: AsciiConsumerEvent, state: &mut AsciiConsumerState, output_stream: &mut EventStream<String>) {
    println!("AsciiConsumer!");
    println!("  State: {:?}", state);
    println!("  Input: {:?}", inp);
    println!("  Output: {:?}", output_stream);
    match inp.ascii_type {
        AsciiType::Digit => {
            match state.current.clone().as_ref() {
                "-" | "+" | "=" => {
                    output_stream.push_last(state.current.clone());
                    state.reset_current();
                    state.current.push(inp.ascii_char);
                },
                _ => {
                    state.current.push(inp.ascii_char);
                } 
            }
        }
        AsciiType::Alphabetic => {
            match state.current.clone().as_ref() {
                "-" | "+" => {
                    output_stream.push_last(state.current.clone());
                    state.reset_current();
                    state.current.push(inp.ascii_char);
                },
                _ => {
                    state.current.push(inp.ascii_char);
                } 
            }
            
        }
        AsciiType::Special => {
            match inp.ascii_char {
                ';' => {
                    output_stream.push_last(state.current.clone());
                    state.reset_current();
                    output_stream.push_last(String::from(";"));
                },
                ',' => {
                    output_stream.push_last(state.current.clone());
                    state.reset_current();
                    output_stream.push_last(String::from(","));
                },
                '\\' => {
                    match state.context_vector.pop() {
                        Some('\\') => state.current.push(inp.ascii_char),
                        Some('"') => {
                            state.context_vector.push('"');
                            state.context_vector.push('\\');
                            state.current.push(inp.ascii_char);
                        },
                        Some(_) => {
                            println!("Not implemented item on stack!");
                        }
                        None => {
                            println!("Nothing on context stack!");
                        }
                    }
                }
                '"' => {
                    match state.context_vector.pop() {
                        Some('"') => {
                            state.current.push(inp.ascii_char);
                            output_stream.push_last(state.current.clone());
                            state.reset_current();
                        },
                        Some('\\') => {
                            state.current.push(inp.ascii_char);
                        },
                        Some(_) => {
                            println!("Not implemented");
                        },
                        None => {
                            state.context_vector.push('"');
                            state.current.push(inp.ascii_char);
                        }
                    }
                }
                s => {
                    match state.context_vector.pop() {
                        Some('"') => {
                            state.context_vector.push('"');
                            state.current.push(inp.ascii_char)
                        },
                        Some('\\') => {
                            state.current.push(inp.ascii_char)
                        },
                        Some(c) => {
                            println!("Not implemented");
                        },
                        None => {
                            if (!state.current.is_empty()) {
                                match s {
                                    '=' | '>' => {},
                                    _ => {
                                        output_stream.push_last(state.current.clone());
                                        state.reset_current();
                                    }
                                }
                                match state.current.clone().as_ref() {
                                    ">" if s == '=' => {
                                        state.current.push('=');
                                        output_stream.push_last(state.current.clone());
                                        state.reset_current();
                                    },
                                    "<" if s == '=' || s == '>' => {
                                        state.current.push(s);
                                        output_stream.push_last(state.current.clone());
                                        state.reset_current();
                                    },
                                    previous if s == '=' || s == '>' => {
                                        output_stream.push_last(state.current.clone());
                                        state.reset_current();
                                        state.current.push(s);
                                    },
                                    _ => {
                                        state.current.push(s);
                                    }
                                }
                            } else {
                                state.current.push(s);
                            }
                        }
                    }
                },
            }
        }
        AsciiType::Whitespace => {
            match state.context_vector.pop() {
                Some('"') => {
                    state.context_vector.push('"');
                    state.current.push(' ');
                },
                Some('\\') => {
                    state.current.push(' ');
                },
                Some(_) => println!("Not implemented"),
                None => {
                    if (!state.current.is_empty()) {
                        output_stream.push_last(state.current.clone());
                        state.reset_current();
                    }
                }
            }
        }
    }
}

fn main() {
    println!("Hello, compiler!");
    
    let mut i: VecDeque<i32> = VecDeque::new();
    let mut o: VecDeque<i32> = VecDeque::new();
    let mut s: i32 = 0;
    let mut s2: i32 = 0;
    let file_event = FileConsumerEvent {filename: "basic.bsc"};

    let mut stream_file_consumer: VecDeque<FileConsumerEvent> = VecDeque::new();
    let mut stream_line_consumer: VecDeque<LineConsumerEvent> = VecDeque::new();
    let mut stream_ascii_consumer: VecDeque<AsciiConsumerEvent> = VecDeque::new();
    let mut stream_string_consumer: VecDeque<String> = VecDeque::new();
    let mut ascii_state: AsciiConsumerState = AsciiConsumerState::new();
    println!("");
    {
        let mut em_file_consumer = EventMachine{
            input: &mut stream_file_consumer,
            output: &mut stream_line_consumer,
            state: &mut s,
            consume_fn: consume_file,
        };
        em_file_consumer.input.push_next(file_event);
        em_file_consumer.println();
        em_file_consumer.run_all();
        em_file_consumer.println();
    }
    println!("");
    {
        let mut em_line_consumer = EventMachine{
            input: &mut stream_line_consumer,
            output: &mut stream_ascii_consumer,
            state: &mut s,
            consume_fn: consume_line,
        };
        em_line_consumer.println();
        em_line_consumer.run_all();
        em_line_consumer.println();
    }
    println!("");
    {
        let mut em_ascii_consumer = EventMachine{
            input: &mut stream_ascii_consumer,
            output: &mut stream_string_consumer,
            state: &mut ascii_state,
            consume_fn: consume_ascii,
        };
        em_ascii_consumer.println();
        em_ascii_consumer.run_all();
        em_ascii_consumer.println();
    }
    // {
    //     let mut em = EventMachine {
    //         input: &mut i,
    //         output: &mut o,
    //         state: &mut s,
    //         consume_fn: consume_testing,
    //     };
    //     em.input.push_next(1);
    //     em.input.push_next(2);
    //     em.input.push_next(3);
    //     em.println();
    //     em.run_all();
    //     em.println();    
    // }
    // {
    //     let mut em2 = EventMachine {
    //         input: &mut o,
    //         output: &mut i,
    //         state: &mut s2,
    //         consume_fn: consume_testing,
    //     }; 
    // }
    
}
