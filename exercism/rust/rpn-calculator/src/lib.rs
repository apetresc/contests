#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

fn evaluate_operation(stack: &mut Vec<i32>, op: fn(i32, i32) -> i32) -> Option<i32> {
    let y = match stack.pop() {
        Some(v) => v,
        None => return None,
    };
    let x = match stack.pop() {
        Some(v) => v,
        None => return None,
    };
    Some(op(x, y))
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    let mut stack = Vec::<i32>::new();
    for input in inputs {
        match input {
            CalculatorInput::Value(x) => stack.push(*x),
            op => {
                let op_fn = match op {
                    CalculatorInput::Add => |x: i32, y: i32| x + y,
                    CalculatorInput::Subtract => |x, y| x - y,
                    CalculatorInput::Multiply => |x, y| x * y,
                    CalculatorInput::Divide => |x, y| x / y,
                    _ => return None,
                };
                match evaluate_operation(&mut stack, op_fn) {
                    Some(v) => stack.push(v),
                    None => return None,
                }
            }
        }
    }

    match stack.len() {
        1 => Some(stack[0]),
        _ => None,
    }
}
