# hscfp
A work in progress JVM bytecode parser that currently supports up to version 1.7

This project was made with many references to the [JVM spec](https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-2.html) 

### Goals
- Support java versions up to 14
- Comment more thoroughly
- Include tests
- Include tools for transforming bytecode
- Include tools for re-emitting transformed bytecode

### Future uses for this library
- Implement simple disassembler
- Implement simple transpiler from JVM to another target
- Implement simple decompiler after completing all previous goals
