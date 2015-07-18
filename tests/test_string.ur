
open Option
open Parse.String

val end_of_line = or (char' #"\n")
                     (string' "\r\n")

val tests =
    ("eof_of_nonempty_string_fails", isNone <| parse eof "foo") ::
    ("eof_of_empty_string_succeeds", isSome <| parse eof "") ::
    ("string_succeeds", let val p = _ <- string "foo"; eof
                        in isSome <| parse p "foo" end) ::
    ("string_fails", let val p = _ <- string "foo"; _ <- string "baz"; eof
                     in isNone <| parse p "foobarbaz" end) ::
    ("stringCI_succeeds_with_more_to_come", isSome <| parse (stringCI "fOo") "FoOBAR") ::
    ("stringCI_succeeds_on_string_of_same_length", isSome <| parse (stringCI "foo") "fOo") ::
    ("stringCI_fails_on_mismatch", isNone <| parse (stringCI "foo") "fob") ::
    ("stringCI_fails_on_short_string", isNone <| parse (stringCI "foo") "fo") ::
    ("take 0 on empty string succeeds", isSome <| parse (take 0) "") ::
    ("take 1 on empty string fails", isNone <| parse (take 1) "") ::
    ("take of same length as string succeeds", isSome <| parse (take 3) "foo") ::
    ("take of less characters than string succeeds", isSome <| parse (take 3) "foobar") ::
    ("drop 0 on empty string succeeds", isSome <| parse (drop 0) "") ::
    ("drop 1 on empty string fails", isNone <| parse (drop 1) "") ::
    ("drop of same length as string succeeds", isSome <| parse (drop 3) "foo") ::
    ("drop followed by string and eof succeeds", isSome <| parse (drop 3;
                                                                  _ <- string "bar";
                                                                  eof)
                                                                 "foobar") ::
    ("satisfy isDigit 0", isSome <| parse (satisfy isdigit) "0") ::
    ("satisfy isDigit f", isNone <| parse (satisfy isdigit) "f") ::
    ("skip space succeeds", isSome <| parse (skip isspace; eof) " ") ::
    ("skip space fails", isNone <| parse (skip isspace) ".") ::
    ("skipWhile isSpace", isSome <| parse (skipWhile isspace; _ <- string "foo"; eof)
                                          "   foo") ::
    ("takeWhile isDigit", isSome <| parse (_ <- takeWhile isdigit; eof) "042") ::
    ("takeTil isSpace", case parse (a <- takeTil' isspace;
                                    skipWhile isspace;
                                    b <- takeTil' isspace;
                                    eof;
                                    return (a, b))
                                   "foo  bar" of
                            Some ("foo", "bar") => True | _ => False) ::
    ("takeRest followed by eof succeeds", isSome <| parse (_ <- takeRest; eof) "whatever") ::
    ("read unsigned decimal integer", Some 123456789 = parse (unsigned_int_of_radix 10) "0123456789") ::
    ("read partial unsigned decimal integer", Some 12345 = parse (unsigned_int_of_radix 10) "12345foo6789") ::
    ("unsuccessful unsigned decimal integer", None = parse (unsigned_int_of_radix 10) "foo12345foo6789") ::
    ("read lowercase hex integer", Some 0x0DDF00D = parse (unsigned_int_of_radix 16) "0ddf00d") ::
    ("read mixed-case hex integer", Some 0xDEADBEEF = parse (unsigned_int_of_radix 16) "DeadBeef") ::
    []
