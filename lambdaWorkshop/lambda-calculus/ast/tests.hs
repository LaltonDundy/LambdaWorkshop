import Core.Internals
import Serialize

e1 = APP (ID "x") (ID "x")

e2 = LAMBDA (ID "x") e1

e3 = APP e2 e2

main = prettyPrint e3
