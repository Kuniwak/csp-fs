module CSP.Core.Exe.Usage

let usage = """usage: csp run <file> <proc>
usage: csp dot <file> <proc>
usage: csp type <file> <proc>
usage: csp version
usage: csp help

COMMANDS
    run       run the specified proc on an interpreter
    dot       print a state transition diagram of the specified proc as DOT Lang
    type      infer a type of the specified proc
    version   show version
    help      show this message

OPTIONS
    <file>    the file expressed by S-expression
    <proc>    process expression to run or visualize or infer types
"""