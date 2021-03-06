(* Parser combinators in Ur
 *
 * Inspired by the discussion on the Ur/Web mailing list circa July 2014:
 *  <http://comments.gmane.org/gmane.comp.lang.ur/1712>
 * but built around more efficient string pointer passing rather than
 * lists of characters.
 *
 * Julian Squires <julian@cipht.net> / 2015
 *)

functor Make(Stream : sig type t end) = struct
    datatype result a = Success of a*Stream.t | Failure
    type t a = Stream.t -> result a

    fun mreturn [a] (x:a) : t a = fn s => Success (x, s)
    fun mbind [a] [b] (p: t a) (f: a -> t b) : t b =
     fn s => case p s of
                 Failure => Failure
               | Success (x, s') => f x s'
    val monad_parse : monad t = mkMonad {Bind=@@mbind, Return=@@mreturn}

    fun parse [a] p =
     fn s => case p s of
                 Failure => None
               | Success (x,_) => Some x

    (* COMBINATORS *)
    val fail [a] : t a = fn _ => Failure

    fun or [a] p q : t a =
     fn s => case p s of Failure => q s | success => success

    fun maybe [a] p : t (option a) =
     fn s => case p s of Failure => Success (None, s)
                       | Success (a, s') => Success (Some a, s')

    fun maybe' [a] p : t unit =
     fn s => case p s of Failure => Success ((), s)
                       | Success (_, s') => Success ((), s')

    fun many [a] p =
        let fun f lst s =
                case p s of
                    Success (a, s') => f (a :: lst) s'
                  | Failure => let val l = List.rev lst in Success (l, s) end
        in f [] end

    fun skipMany [a] p =
        let fun f s =
                case p s of
                    Success (a, s') => f s'
                  | Failure => Success ((), s)
        in f end

    fun count [a] n p =
        let fun f n lst s =
                if n = 0 then Success (List.rev lst, s)
                else
                    case p s of
                        Success (a, s') => f (n-1) (a :: lst) s'
                      | Failure => Failure
        in f n [] end

    fun sepBy [a] [s] p sep =
        let fun f lst s =
                case p s of
                    Failure => Success (List.rev lst, s)
                  | Success (a, s') =>
                    case sep s' of
                        Failure => Success (List.rev (a::lst), s')
                      | Success (_, s'') => f (a::lst) s''
        in f [] end

(* orElse orFailWith <?> *)
      (* choice: not implemented because the lack of list literals makes it less convenient *)
end

structure String = struct
    structure Str = String

    structure M = Make(struct type t = string end)
    open M

    fun next s = if (Str.length s) > 0 then Some (Str.sub s 0, Str.suffix s 1) else None

    val eof : t unit =
     fn s => if (Str.length s) = 0 then Success ((), "") else Failure

    fun string t =
     fn s =>
        (* XXX this conses; we need to add a basis function for this,
         or call strncmp via the FFI. *)
        if Str.isPrefix {Full=s, Prefix=t} then
            M.Success (t, Str.suffix s (Str.length t))
        else
            M.Failure

    fun stringCI orig =
        let fun f t s =
                case (next t, next s) of
                    (Some (a,t'), Some (b,s')) =>
                    if (Char.toLower a) = (Char.toLower b) then
                        f t' s'
                    else Failure
                  (* XXX should this return the actual match?
                         returning orig allows us to avoid consing... *)
                  | (None, Some (b, s')) => Success (orig, s')
                  | (None, None) => Success (orig, "")
                  | _ => Failure
        in f orig end

    fun char c =
     fn s => case next s of
                 Some (d, s') => if c = d then Success (c, s') else Failure
               | None => Failure

    fun take n =
     fn s =>
        if Str.lengthGe s n then Success ((s,n), Str.suffix s n) else Failure

    fun drop n =
     fn s =>
        if Str.lengthGe s n then Success ((), Str.suffix s n) else Failure

    fun satisfy p =
     fn s =>
        case next s of
            None => Failure
          | Some (c,s') => if p c then Success (c, s') else Failure

    fun skip p =
     fn s =>
        case next s of
            None => Failure
          | Some (c, s') => if p c then Success ((), s') else Failure

    fun skipWhile p =
        let fun f s =
                case next s of
                    None => Success ((), "")
                  | Some (c, s') => if p c then f s' else Success ((), s)
        in f end

    fun takeWhile p =
     fn t =>
        let fun f i s =
                case next s of
                    None => Success ((t,i), "")
                  | Some (c, s') =>
                    if p c then
                        f (i+1) s'
                    else
                        Success ((t,i), s)
        in f 0 t end

    fun takeTil p = takeWhile (not <<< p)

    val takeRest = fn s => Success (s, "")

    val skipSpace = skipWhile isspace

    fun next_digit radix s =
        let val digits = "0123456789abcdefghijklmnopqrstuvwxyz" in
            (c, s) <- next s;
            i <- String.index digits (tolower c);
            if i < radix then Some (i,s) else None
        end

    (* XXX should check for overflow; also options end up consing so
       it would be nice to do this without any additional memory
       pressure. *)
    fun unsigned_int_of_radix radix =
     fn s =>
        let fun loop acc digit s_so_far =
                case digit of
                    Some (d,s) => loop (acc*radix + d) (next_digit radix s) s
                  | None => Success (acc, s_so_far)
        in case next_digit radix s of
               Some (d,s) => loop 0 (Some (d,s)) s
             | None => Failure
        end

    (*
     * val signed_int_of_radix : int -> t int
     * val double : t float
     *)

    (* fun string' s = @@bind [t] [string] [unit] monad_parse (string s) (fn _ => @@return [t] [unit] monad_parse ()) *)
end

structure String = struct
    functor Ignorance(S : sig
                  con t :: Type -> Type
                  val string : string -> t string
                  val stringCI : string -> t string
                  val char : char -> t char
                  val monad_parse : monad t
                  val takeWhile : (char -> bool) -> t (string*int)
              end) = struct
        fun string' t = _ <- S.string t; return ()
        fun stringCI' orig = _ <- S.stringCI orig; return ()
        fun char' c = _ <- S.char c; return ()

        fun takeWhile' p =
            (t,i) <- S.takeWhile p;
            return (substring t 0 i)

        fun takeTil' p = takeWhile' (not <<< p)
    end
    open String
    structure P = Ignorance(String)
    open P
    val endOfLine : t unit = (char' #"\n") `or` (string' "\r\n")
end

structure Blob = struct
    structure M = Make(struct type t = blob end)
    open M
end
