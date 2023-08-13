module CSP.Core.Exe.Usage

let usage = """
usage: csp run <file> <proc> <expr>*
usage: csp dot [<file>|--stdin] <proc> <expr>*
usage: csp version
usage: csp help

COMMANDS
    run       run the specified proc on an interpreter
    dot       print a state transition diagram as Dot language
    version   show version
    help      show this message

OPTIONS
    --stdin   Input from stdin
    <file>    Input from the file
    <proc>    Process name to run or visualize
    <expr>    Parameters for the process
"""