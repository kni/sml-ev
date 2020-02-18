signature HASHED =
sig
  eqtype t
  val hashValue : int -> t -> int
end


functor HashArray (Hashed:HASHED) :>
sig
  type 'a hash

  val hash   : int -> 'a hash

  val update : 'a hash * Hashed.t * 'a -> unit
  val sub    : 'a hash * Hashed.t -> 'a option
  val delete : 'a hash * Hashed.t -> unit

  val fold   : (Hashed.t * 'a * 'b -> 'b) -> 'b -> 'a hash -> 'b
end
=
struct

  val hashValue = Hashed.hashValue

  datatype 'a status = Empty | Deleted | Used of Hashed.t * 'a
  datatype 'a hash   = Hash of { used: int ref, entries: 'a status array ref }

  fun hash size = Hash { used = ref 0, entries = ref (Array.array (size, Empty)) }

  fun prevId i length = if i = 0 then length - 1 else i - 1


  fun update (Hash {entries as ref arr, used}, key, value) =
    let
      fun remove arr i =
        case Array.sub (arr, i) of
            Empty       => ()
          | Deleted     => remove arr (prevId i (Array.length arr))
          | Used (k, _) => if k = key
                           then Array.update (arr, i, Deleted)
                           else remove arr (prevId i (Array.length arr))


      fun enter arr i (entry as (key, _)) =
        case Array.sub (arr, i) of
            Empty       => ( Array.update (arr, i, Used entry); true )
          | Deleted     => ( remove arr i; Array.update (arr, i, Used entry); false )
                           (* check and remove old enter from other place, then enter new in freed place *)
          | Used (k, _) => if k = key
                           then ( Array.update (arr, i, Used entry); false )
                           else enter arr (prevId i (Array.length arr)) entry


      fun rehash newLength =
        let
          val newArr            = Array.array (newLength, Empty)
          val hashFromNewLength = hashValue newLength

          fun doit ((Used (entry as (key, _))), r) = if enter newArr (hashFromNewLength key) entry then r + 1 else r
            | doit (_, r) = r
        in
          used    := Array.foldl doit 0 arr;
          entries := newArr
        end


      fun maybyRehash () =
        let
          val length = Array.length arr
        in
          if !used * 5 > length * 4 (* if 80% then rehash *)
          then rehash (length * 2)
          else ()
        end
    in
      if enter arr (hashValue (Array.length arr) key) (key, value)
      then ( used := !used + 1 ; maybyRehash () )
      else ()
    end


  fun fold f init (Hash { entries = ref e, ...}) =
    let
      fun doit (Used (k, v), r) = f (k, v, r)
        | doit (_, r)           = r
    in
      Array.foldl doit init e
    end


  fun sub (Hash {entries = ref arr, ...}, key) =
    let
      val length = Array.length arr

      fun doit i = case Array.sub (arr, i) of
            Empty       => NONE
          | Deleted     => doit (prevId i length)
          | Used (k, v) => if key = k
                           then SOME v
                           else doit (prevId i length)
    in
      doit (hashValue length key)
    end


  fun delete (Hash {entries = ref arr, ...}, key) =
    let
      val length = Array.length arr

      fun doit i = case Array.sub (arr, i) of
            Empty       => ()
          | Deleted     => doit (prevId i length)
          | Used (k, _) => if key = k
                           then Array.update (arr, i, Deleted)
                           else doit (prevId i length)
    in
      doit (hashValue length key)
    end

end



structure HashArrayInt = HashArray (
  struct
    type t = int
    fun hashValue length i = Int.mod (i, length)
  end
)


structure HashArrayLargeInt = HashArray (
  struct
    type t = LargeInt.int
    fun hashValue length i = Int.fromLarge (LargeInt.mod (i, (Int.toLarge length)))
  end
)


structure HashArrayString = HashArray (
  struct
    type t = string
    fun hashValue length str =
      Word.toInt (
        Word.mod (
          CharVector.foldr
            (fn (c, r) => Word.fromInt (Char.ord c) + 0w7 * r) 0w0 str,
            (Word.fromInt length)
        )
      )
  end
)
