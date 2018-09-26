mod event_machine;
use event_machine::EventMachine;

mod event_stream;
use event_stream::EventStream;

mod file_consumer;
use file_consumer::FileConsumerEvent;
use file_consumer::consume_file;

mod line_consumer;
use line_consumer::LineConsumerEvent;
use line_consumer::consume_line;
use line_consumer::AsciiChar;

use std::collections::VecDeque;

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

fn main() {
    println!("Hello, compiler!");
    
//    let mut i: VecDeque<i32> = VecDeque::new();
//    let mut o: VecDeque<i32> = VecDeque::new();
    let mut s: i32 = 0;
//    let mut s2: i32 = 0;
    let file_event = FileConsumerEvent {filename: "basic.bsc"};

    let mut stream_file_consumer: VecDeque<FileConsumerEvent> = VecDeque::new();
    let mut stream_line_consumer: VecDeque<LineConsumerEvent> = VecDeque::new();
    let mut stream_ascii_consumer: VecDeque<AsciiChar> = VecDeque::new();
//    let mut stream_string_consumer: VecDeque<String> = VecDeque::new();
//    let mut ascii_state: AsciiConsumerState = AsciiConsumerState::new();
    println!();
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
    println!();
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
    println!();
//    {
//        let mut em_ascii_consumer = EventMachine{
//            input: &mut stream_ascii_consumer,
//            output: &mut stream_string_consumer,
//            state: &mut ascii_state,
//            consume_fn: consume_ascii,
//        };
//        em_ascii_consumer.println();
//        em_ascii_consumer.run_all();
//        em_ascii_consumer.println();
//    }
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
