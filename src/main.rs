use std::env;
use logos::Logos;
use std::io::prelude::*;

use crate::ast::Token;

mod ast;


fn main() {

    ast::lex("dovid(5+8,1,d,dsdf(gh()))")


    // let args: Vec<String> = env::args().collect();

    // if args.len() < 3 {
    //     panic!("Not enough arguments");
    // }

    // let program = [Op::PUSH(5), Op::PUSH(7), Op::PLUS, Op::DUMP];

    // let filename = &args[2];

    // if args[1] == "com" {
    //     let nasm = compile_to_nasm_64(&program);

    //     // compile to binary

    //     let mut file = std::fs::File::create(format!("./bin/{}.asm", filename)).unwrap();

    //     file.write_all(&nasm.as_bytes()).unwrap();

    //     // run external assembler to compile to elf64 binary
    //     std::process::Command::new("nasm")
    //         .arg("-f")
    //         .arg("elf64")
    //         .arg("-o")
    //         .arg(format!("./bin/{}.o", filename))
    //         .arg(format!("./bin/{}.asm", filename))
    //         .output()
    //         .expect("failed to execute process");

        

    //     // compile to elf64 binary
    //     std::process::Command::new("ld")
    //         .arg("-o")
    //         .arg(format!("./bin/{}", filename))
    //         .arg(format!("./bin/{}.o", filename))
    //         .output()
    //         .expect("failed to execute process");

       

    //     // run binary
    //     let output = std::process::Command::new(format!("./bin/{}", filename))
    //         .output()
    //         .expect("failed to execute process");

    //     println!("output: {}", String::from_utf8_lossy(&output.stdout));
    // } else if args[1] == "int" {
    //     interperter(&program);
    // } 
    // else {
    //     std::process::exit(1);
    // }
}



fn compile_to_nasm_64(program: &[Op]) -> String {
    let mut nasm = String::new();
    {
        nasm.push_str("section .text\n");

        nasm.push_str("dump:\n");
        nasm.push_str("    mov     r9, -3689348814741910323\n");
        nasm.push_str("    sub     rsp, 40\n");
        nasm.push_str("    mov     BYTE [rsp+31], 10\n");
        nasm.push_str("    lea     rcx, [rsp+30]\n");
        nasm.push_str(".L2:\n");
        nasm.push_str("    mov     rax, rdi\n");
        nasm.push_str("    lea     r8, [rsp+32]\n");
        nasm.push_str("    mul     r9\n");
        nasm.push_str("    mov     rax, rdi\n");
        nasm.push_str("    sub     r8, rcx\n");
        nasm.push_str("    shr     rdx, 3\n");
        nasm.push_str("    lea     rsi, [rdx+rdx*4]\n");
        nasm.push_str("    add     rsi, rsi\n");
        nasm.push_str("    sub     rax, rsi\n");
        nasm.push_str("    add     eax, 48\n");
        nasm.push_str("    mov     BYTE [rcx], al\n");
        nasm.push_str("    mov     rax, rdi\n");
        nasm.push_str("    mov     rdi, rdx\n");
        nasm.push_str("    mov     rdx, rcx\n");
        nasm.push_str("    sub     rcx, 1\n");
        nasm.push_str("    cmp     rax, 9\n");
        nasm.push_str("    ja      .L2\n");
        nasm.push_str("    lea     rax, [rsp+32]\n");
        nasm.push_str("    mov     edi, 1\n");
        nasm.push_str("    sub     rdx, rax\n");
        nasm.push_str("    xor     eax, eax\n");
        nasm.push_str("    lea     rsi, [rsp+32+rdx]\n");
        nasm.push_str("    mov     rdx, r8\n");
        nasm.push_str("    mov     rax, 1\n");
        nasm.push_str("    syscall\n");
        nasm.push_str("    add     rsp, 40\n");
        nasm.push_str("    ret\n");

        nasm.push_str("global _start\n");
        nasm.push_str("_start:\n");
    }

    for op in program {
        match op {
            Op::PUSH(value) => {
                nasm.push_str(&format!("    push {}\n", value));
            }
            Op::MINUS => {
                nasm.push_str("    pop rax\n");
                nasm.push_str("    pop rbx\n");
                nasm.push_str("    sub rbx, rax\n");
                nasm.push_str("    push rbx\n");
            }
            Op::PLUS => {
                nasm.push_str("    pop rax\n");
                nasm.push_str("    pop rbx\n");
                nasm.push_str("    add rax, rbx\n");
                nasm.push_str("    push rax\n");
            }
            Op::DUMP => {
                nasm.push_str("    pop rdi\n");
                nasm.push_str("    call dump\n");
            }
        }
    }

    // exit with code 0
    nasm.push_str("    mov rax, 60\n");
    nasm.push_str("    mov rdi, 0\n");
    nasm.push_str("    syscall\n");

    nasm
}

fn interperter(program: &[Op]) {
    let mut stack = Vec::new();

    for op in program.iter() {
        match op {
            Op::PUSH(n) => {
                stack.push(*n);
            }
            Op::PLUS => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(a + b);
            }
            Op::MINUS => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b - a);
            }  
            Op::DUMP => {
                println!("{:#?}", stack.pop().unwrap());
            }
        }
    }
}



// Operations

enum Op {
    PUSH(i32),
    PLUS,
    MINUS,
    DUMP,
}
