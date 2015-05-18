(* *)

fun fst (a,b) = a

fun as_TAP results =
    let val x = String.append
        val s = "1.." `x` (show (List.length results)) `x` "\n" `x`
                        (fst <|
                         List.foldl (fn (m,v) (s,i) =>
                                        (s `x` (if v then "ok " else "not ok ") `x`
                                               (show i) `x` " - " `x` m `x` "\n",
                                         i+1))
                                    ("", 1)
                                    results)
    in returnBlob (textBlob s) (blessMime "text/plain") end

fun present (results : list (string*bool)) =
    let
        val pass = List.all (fn (_,x) => x) results
        fun result_of x = if x then <xml><span style="color:green">pass</span></xml>
                               else <xml><span style="color:red">fail</span></xml>
    in
        <xml>
          <head/>
          <body>
            <header>
              <h1>Tests {result_of pass}</h1>
            </header>
            <article>
              <p>Ran {[List.length results]} tests.</p>
              <dl>
                {List.mapX (fn (a, b) => <xml><dt>{[a]}</dt><dd>{result_of b}</dd></xml>)
                           results}
              </dl>
            </article>
          </body>
        </xml>
    end

fun main () : transaction page =
    as_TAP Test_string.tests
