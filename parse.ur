(* Parser combinators in Ur
 *
 * Inspired by the discussion on the Ur/Web mailing list circa July 2014:
 *  <http://comments.gmane.org/gmane.comp.lang.ur/1712>
 * but built around more efficient string pointer passing rather than
 * lists of characters.
 *
 * Julian Squires <julian@cipht.net> / 2015
 *)

datatype result a = Success of a*string | Failure
type t a = string -> result a

fun mreturn [a] (x:a) : t a = fn s => Success (x, s)
fun mbind [a] [b] (p: t a) (f: a -> t b) : t b =
 fn s => case p s of
             Failure => Failure
           | Success (x, s') => f x s'
val monad_parse = mkMonad {Bind=@@mbind, Return=@@mreturn}

fun parse [a] p = fn s => case p s of Failure => None | Success (x,_) => Some x

fun next s = if (String.length s) > 0 then Some (String.sub s 0, String.suffix s 1) else None

fun char c : t char =
 fn s => case next s of
             Some (c', s') =>
             if c' = c then
                 Success (c, s')
             else Failure
           | None => Failure

fun char_sat t : t char =
 fn s =>
    let val c = String.sub s 0 in
        if t c then Success (c, (String.suffix s 1)) else Failure
    end

fun string t =
 fn s =>
    let val l = String.length t in
        (* XXX this conses; we need to add a basis function for this,
           or call strncmp via the FFI. *)
        if String.isPrefix {Full=s, Prefix=t} then
            Success (t, (String.suffix s l))
        else
            Failure
    end

val eof : t unit =
 fn s => if (String.length s) = 0 then Success ((), "") else Failure

val space : t unit =
    let fun f s =
            case next s of Some (c', s') => if isspace c' then f s' else Success ((), s)
                         | None => Success ((), s)
    in f end

fun many [a] p : t (list a) =
 fn s =>
    let fun f s lst =
            case p s of
                Success (a, s') => f s' (a :: lst)
              | Failure => let val l = List.rev lst in Success (l, s) end
    in f s [] end

fun string_until c : t string =
 fn s =>
    case String.index s c of
        Some i => Success (String.substring s {Start=0, Len=i}, String.suffix s i)
      | None => Failure

fun or [a] p q : t a =
 fn s => case p s of Failure => q s | success => success
