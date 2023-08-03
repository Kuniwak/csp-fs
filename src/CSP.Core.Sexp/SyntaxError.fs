module CSP.Core.Sexp.SyntaxError

type SyntaxError = unit

let format (err: SyntaxError): string = failwith ""