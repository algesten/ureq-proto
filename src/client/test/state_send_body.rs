use crate::client::SendRequestResult;

use super::scenario::Scenario;
use super::TestSliceExt;

#[test]
fn write_with_content_length() {
    let input = b"hello".as_slice();

    let scenario = Scenario::builder()
        .post("https://q.test")
        .header("content-length", input.len())
        .build();

    let mut flow = scenario.to_send_body();

    // deliberately short buffer to require multiple writes
    let mut output = vec![0; 3];

    let overhead = flow.calculate_max_input(output.len());
    assert_eq!(overhead, 3); // not chunked. entire output can be used as input.

    assert!(!flow.can_proceed());

    // 1st write
    let (input_used, output_used) = flow.write(input, &mut output).unwrap();
    assert_eq!(input_used, 3);
    assert_eq!(output_used, 3);
    assert_eq!(output[..output_used].as_str(), "hel");

    let input = &input[input_used..];
    assert!(!flow.can_proceed());

    // 2nd write
    let (input_used, output_used) = flow.write(input, &mut output).unwrap();
    assert_eq!(input_used, 2);
    assert_eq!(output_used, 2);
    assert_eq!(output[..output_used].as_str(), "lo");

    assert!(flow.can_proceed());
}

#[test]
fn write_with_content_length_empty_slices() {
    let input = b"hello".as_slice();

    let scenario = Scenario::builder()
        .post("https://q.test")
        .header("content-length", input.len())
        .build();

    let mut flow = scenario.to_send_body();
    let mut output = vec![0; 1024];

    // Useless write
    let (input_used, output_used) = flow.write(&[], &mut output).unwrap();
    assert_eq!(input_used, 0);
    assert_eq!(output_used, 0);

    assert!(!flow.can_proceed());

    // Proper write
    let (input_used, output_used) = flow.write(input, &mut output).unwrap();
    assert_eq!(input_used, 5);
    assert_eq!(output_used, 5);

    assert!(flow.can_proceed());

    // More useless writes
    let (input_used, output_used) = flow.write(&[], &mut output).unwrap();
    assert_eq!(input_used, 0);
    assert_eq!(output_used, 0);
    let (input_used, output_used) = flow.write(&[], &mut output).unwrap();
    assert_eq!(input_used, 0);
    assert_eq!(output_used, 0);
    let (input_used, output_used) = flow.write(&[], &mut output).unwrap();
    assert_eq!(input_used, 0);
    assert_eq!(output_used, 0);

    assert!(flow.can_proceed());
}

#[test]
fn write_with_chunked() {
    let input = b"hello".as_slice();

    let scenario = Scenario::builder().post("https://q.test").build();

    let mut flow = scenario.to_send_body();

    let mut output = vec![0; 21 * 1024 + 74];

    let overhead = flow.calculate_max_input(output.len());
    assert_eq!(overhead, 21554);

    assert!(!flow.can_proceed());

    // 1st write
    let (input_used, output_used) = flow.write(&input[..3], &mut output).unwrap();
    assert_eq!(input_used, 3);
    assert_eq!(output_used, 8);
    assert_eq!(output[..output_used].as_str(), "3\r\nhel\r\n");

    assert!(!flow.can_proceed());

    // 2nd write
    let (input_used, output_used) = flow.write(&input[3..], &mut output).unwrap();
    assert_eq!(input_used, 2);
    assert_eq!(output_used, 7);
    assert_eq!(output[..output_used].as_str(), "2\r\nlo\r\n");

    assert!(!flow.can_proceed());

    // write end
    let (input_used, output_used) = flow.write(&[], &mut output).unwrap();
    assert_eq!(input_used, 0);
    assert_eq!(output_used, 5);
    assert_eq!(output[..output_used].as_str(), "0\r\n\r\n");

    assert!(flow.can_proceed());
}

#[test]
fn send_body_despite_method() {
    let scenario = Scenario::builder()
        .delete("https://q.test")
        .send_body("DELETE should not have a body", true)
        .build();

    let mut flow = scenario.to_prepare();

    flow.send_body_despite_method();

    let mut flow = flow.proceed();

    // Write the prelude and discard
    flow.write(&mut vec![0; 1024]).unwrap();

    let result = flow.proceed().unwrap().unwrap();

    // We should be able to get to this state without errors.
    assert!(matches!(result, SendRequestResult::SendBody(_)));
}
