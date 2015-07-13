(* TITLE *)

structure P = Parse.String

datatype sexp = SList of list sexp
              | SAtom of string

fun sexp () : P.t sexp =
    P.or (P.char' #"(";
          s <- P.many (P.skipSpace; sexp ());
          P.char' #")";
          return (SList s))
         (atom <- P.takeTil' (fn c => c = #" " || c = #"(" || c = #")");
          if (String.length atom) > 0 then
              return (SAtom atom)
          else
              P.fail)

fun process s =
    P.parse (sexp ()) s

fun main () : transaction page =
    let fun f (e : sexp) =
            case e of
                SAtom s => <xml><li>{[s]}</li></xml>
              | SList l => <xml><ul>{List.mapX f l}</ul></xml>
                            val our_style = ""
    in
        input <- source "(foo (bar baz) quux)";
        return <xml>
          <head>
            <link rel="stylesheet" type="text/css" href="/sexp.css" />
          </head>
          <body>
            <label>Enter simple s-expressions here:
              <ctextbox source={input} />
            </label>
            <ul><dyn signal={v <- signal input;
                            return (case (process v) of
                                        Some sexps => f sexps
                                      | None => <xml>none</xml>)} /></ul>
          </body>
        </xml>
    end
