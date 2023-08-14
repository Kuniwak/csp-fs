module CSP.Core.Exe.Usage

let usage = """usage: csp run <file> <proc>
usage: csp dot <file> <proc>
usage: csp version
usage: csp help

COMMANDS
    run       run the specified proc on an interpreter
    dot       print a state transition diagram as Dot language
    version   show version
    help      show this message

OPTIONS
    <file>    input from the file
    <proc>    process expression to run or visualize
"""