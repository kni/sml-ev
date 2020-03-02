(* Copyright (C) 2016 Nick Kostyria. BSD3 license. *)

signature OS_IO_EV = sig

  type     evFd = int
  datatype evFilter = evRead | evWrite
  type     evCb = (evFd * evFilter) -> unit
  datatype evDesc = evAdd of evFd * evFilter * evCb | evDelete of evFd * evFilter
  type     ev

  exception Ev of string

  val evInit:   unit -> ev
  val evModify: ev -> evDesc list -> int
  val evWait:   ev -> Time.time option -> int
end


structure Ev :> OS_IO_EV =
struct

  structure H = HashArrayInt

  type     evFd = int
  datatype evFilter = evRead | evWrite
  type     evCb  = (evFd * evFilter) -> unit
  datatype evDesc = evAdd of evFd * evFilter * evCb | evDelete of evFd * evFilter
  type     ev = {ev: int, rH: evCb H.hash, wH: evCb H.hash}

  exception Ev of string
  val () = MLton.Exn.addExnMessager (fn Ev m => SOME ("Ev \"" ^ m ^ "\"") | _ => NONE)

  local
    open MLton.Pointer
    open MLton.Platform

    val max_events = 64
    val hash_size  = 1000


    val (kevent_size, timeout_size) =
      case Arch.host of
          Arch.X86   => (56, 8)
        | Arch.AMD64 => (64, 16)
        | Arch.ARM   => (64, 16)
        | Arch.ARM64 => raise Ev "Unsupported platform"
        |  _ => raise Ev "Unsupported platform"


    val malloc = (_import "malloc" : Word.word -> t;) o Word.fromInt
    val kevent_list_pointer = malloc (kevent_size * max_events)
    val timeout_pointer = malloc timeout_size


    val kqueue = _import "kqueue" : unit -> int;

    val kevent = _import "kevent" : int * t * int * t * int * t -> int;


    fun kevent_change kq changelist =
      let
          fun pack_kevent_struct_list p l =
            let
              fun pack_kevent_struct ((ident, filter, flags, fflags, data, udata), p) = (
                case Arch.host of
                    Arch.X86 => (
                        setWord32  (add (p, 0w0),  0, Word32.fromInt ident);
                        setInt16   (add (p, 0w4),  0, Int16.fromInt  filter);
                        setWord16  (add (p, 0w6),  0, Word16.fromInt flags);
                        setWord32  (add (p, 0w8),  0, Word32.fromInt fflags);
                        setInt64   (add (p, 0w12), 0, Int64.fromInt  data);
                        setPointer (add (p, 0w20), 0, null);
                        setInt64   (add (p, 0w24), 0, 0);
                        setInt64   (add (p, 0w32), 0, 0);
                        setInt64   (add (p, 0w40), 0, 0);
                        setInt64   (add (p, 0w48), 0, 0)
                      )
                  | Arch.AMD64 => (
                        setWord64  (add (p, 0w0),  0, Word64.fromInt ident);
                        setInt16   (add (p, 0w8),  0, Int16.fromInt  filter);
                        setWord16  (add (p, 0w10), 0, Word16.fromInt flags);
                        setWord32  (add (p, 0w12), 0, Word32.fromInt fflags);
                        setInt64   (add (p, 0w16), 0, Int64.fromInt  data);
                        setPointer (add (p, 0w24), 0, null);
                        setInt64   (add (p, 0w32), 0, 0);
                        setInt64   (add (p, 0w40), 0, 0);
                        setInt64   (add (p, 0w48), 0, 0);
                        setInt64   (add (p, 0w56), 0, 0)
                      )
                   | Arch.ARM => (
                        setWord32  (add (p, 0w0),  0, Word32.fromInt ident);
                        setInt16   (add (p, 0w4),  0, Int16.fromInt  filter);
                        setWord16  (add (p, 0w6),  0, Word16.fromInt flags);
                        setWord32  (add (p, 0w8),  0, Word32.fromInt fflags);
                        setInt64   (add (p, 0w16), 0, Int64.fromInt  data);
                        setPointer (add (p, 0w24), 0, null);
                        setInt64   (add (p, 0w32), 0, 0);
                        setInt64   (add (p, 0w40), 0, 0);
                        setInt64   (add (p, 0w48), 0, 0);
                        setInt64   (add (p, 0w56), 0, 0)
                      )
                  | _ => raise Ev "Unsupported platform"
                ;
                add (p, Word.fromInt kevent_size)
              )
            in
              Vector.foldl pack_kevent_struct p l
            end
      in
        pack_kevent_struct_list kevent_list_pointer changelist;
        kevent (kq, kevent_list_pointer, Vector.length changelist, MLton.Pointer.null, 0, MLton.Pointer.null)
      end

    fun kevent_wait kq eventlist t =
      let

        fun pack_timeout t =
          case t of
              NONE   => MLton.Pointer.null
            | SOME (s, n) => (
                 case Arch.host of
                     Arch.X86 => (
                       setInt32 (add (timeout_pointer, 0w0), 0, Int32.fromInt s);
                       setInt32 (add (timeout_pointer, 0w4), 0, Int32.fromInt n)
                     )
                   | Arch.AMD64 => (
                       setInt64 (add (timeout_pointer, 0w0), 0, Int64.fromInt s);
                       setInt64 (add (timeout_pointer, 0w8), 0, Int64.fromInt n)
                      )
                   | Arch.ARM => (
                       setInt64 (add (timeout_pointer, 0w0), 0, Int64.fromInt s);
                       setInt32 (add (timeout_pointer, 0w8), 0, Int32.fromInt n)
                      )
                   | _ => raise Ev "Unsupported platform"
                 ;
                 timeout_pointer
               )

        val cnt = kevent (kq, MLton.Pointer.null, 0, kevent_list_pointer, max_events, (pack_timeout t))

        fun unpack_kevent_struct_list p n =
          let
             fun unpack_kevent_struct p =
               let
                 val (ident, filter, flags, fflags, data) =
                   case Arch.host of
                       Arch.X86 => (
                         let
                           val ident  = Word32.toInt (getWord32 (add (p, 0w0), 0))
                           val filter = Int16.toInt  (getInt16  (add (p, 0w4), 0))
                           val flags  = Int16.toInt  (getInt16  (add (p, 0w6), 0))
                           val fflags = Word32.toInt (getWord32 (add (p, 0w8), 0))
                           val data   = Int64.toInt  (getInt64  (add (p, 0w12), 0))
                         in (ident, filter, flags, fflags, data) end
                      )
                     | Arch.AMD64 => (
                         let
                           val ident  = Word64.toInt (getWord64 (add (p, 0w0), 0))
                           val filter = Int16.toInt  (getInt16  (add (p, 0w8), 0))
                           val flags  = Int16.toInt  (getInt16  (add (p, 0w10), 0))
                           val fflags = Word32.toInt (getWord32 (add (p, 0w12), 0))
                           val data   = Int64.toInt  (getInt64  (add (p, 0w16), 0))
                         in (ident, filter, flags, fflags, data) end
                        )
                     | Arch.ARM => (
                         let
                           val ident  = Word32.toInt (getWord32 (add (p, 0w0), 0))
                           val filter = Int16.toInt  (getInt16  (add (p, 0w4), 0))
                           val flags  = Int16.toInt  (getInt16  (add (p, 0w6), 0))
                           val fflags = Word32.toInt (getWord32 (add (p, 0w8), 0))
                           val data   = Int64.toInt  (getInt64  (add (p, 0w16), 0))
                         in (ident, filter, flags, fflags, data) end
                      )
                     | _ => raise Ev "Unsupported platform"
               in
                 ((ident, filter, flags, fflags, data, {}), add (p, Word.fromInt kevent_size))
               end

            fun doit p i n =
              if i = n
              then n
              else
                let
                  val (v,p) = unpack_kevent_struct p
                in
                  Array.update (eventlist, i, v);
                  doit p (i+1) n
               end
          in
            doit p 0 n
          end

       in
         if cnt > 0
         then unpack_kevent_struct_list kevent_list_pointer cnt
         else cnt
       end


    val kevent_struct_empty = (0,0,0,0,0,{})
    val eventlist = Array.array (max_events, kevent_struct_empty)

  in
    fun evInit () =
      let
        val ev = kqueue ()
      in
        if ev = ~1 then raise Ev "evInit" else
        { ev = ev, rH = H.hash hash_size, wH = H.hash hash_size }
      end


    fun evModify (ev:ev) ev_desc_list =
      let
        fun evFilterToInt evRead  = ~1
          | evFilterToInt evWrite = ~2

        fun evFilterToH evRead  = #rH ev
          | evFilterToH evWrite = #wH ev

        fun toChange fd filter action = (fd, (evFilterToInt filter), action, 0, 0, {})

        fun evDescToChange (evAdd    (fd, filter, cb)) = toChange fd filter 1
          | evDescToChange (evDelete (fd, filter))     = toChange fd filter 2


        fun evDescFilter (evAdd (fd, filter, cb)) = (H.update ((evFilterToH filter), fd, cb); true)
          | evDescFilter (evDelete (fd, filter)) =
            let
              val H = evFilterToH filter
            in
              if isSome (H.sub (H, fd))
              then ( H.delete (H, fd) ; true )
              else false
            end


        val changelist = Vector.fromList ( map evDescToChange ( List.filter evDescFilter ev_desc_list ) )

        val _ = if Vector.length changelist > max_events then raise Ev "too big changelist" else ()

        val cnt = kevent_change (#ev ev) changelist
      in
        if cnt >= 0
        then cnt
        else raise Ev "evModify"
      end


    fun evWait (ev:ev) t =
      let
        val timeout = case t of
            SOME t =>
              let
                val s = Time.toSeconds t
                val n = Time.toNanoseconds t - s * 1000000000
              in
                SOME (Int.fromLarge s, Int.fromLarge n)
              end
          | NONE => NONE

        val cnt = kevent_wait (#ev ev) eventlist timeout

        fun intToevFilter (~1) = evRead
          | intToevFilter (~2) = evWrite
          | intToevFilter _    = raise Ev "intToevFilter"

        fun getCb fd evRead  = H.sub ((#rH ev), fd)
          | getCb fd evWrite = H.sub ((#wH ev), fd)

        fun new_loop 0 = cnt
          | new_loop i =
              let
                val (fd,f,_,_,_,_) = Array.sub (eventlist, (i-1))
                val filter = intToevFilter f
              in
                case getCb fd filter of
                    SOME cb => cb (fd, filter)
                  | NONE    => ()
                ;
                new_loop (i - 1)
              end

      in
        if cnt >= 0
        then new_loop cnt
        else cnt
      end
  end

end
