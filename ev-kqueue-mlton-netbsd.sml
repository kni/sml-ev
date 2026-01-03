(* Copyright (C) 2026 Nick Kostyria. BSD3 license. *)

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

    val max_events = 64
    val hash_size  = 1000

    val is_64bit = sizeofPointer = 0w8

    val malloc = (_import "malloc" : Word.word -> t;) o Word.fromInt

    val kevent_size = if is_64bit then 40 else 32
    val kevent_list_pointer = malloc (kevent_size * max_events)
    val timeout_pointer = malloc (if is_64bit then 16 else 16)

    val kqueue = _import "kqueue" : unit -> int;

    val kevent = _import "__kevent50" : int * t * int * t * int * t -> int;

    fun setC_Ulong(p:t, v:int):t = (
        if is_64bit
        then setWord64(p, 0, Word64.fromInt(v))
        else setWord32(p, 0, Word32.fromInt(v));
        add (p, sizeofPointer)
      )

    fun setC_Long(p:t, v:int):t = (
        if is_64bit
        then setInt64(p, 0, Int64.fromInt(v))
        else setInt32(p, 0, Int32.fromInt(v));
        add (p, sizeofPointer)
      )

    fun setC_Pointer(p, v) = ( setPointer(p, 0, v); add (p, sizeofPointer) )

    fun setC_Int16(p,  v:int) = ( setInt16(p, 0, Int16.fromInt(v));  add (p, 0wx2) )
    fun setC_Int64(p,  v:int) = ( setInt64(p, 0, Int64.fromInt(v));  add (p, 0wx8) )
    fun setC_Word16(p, v:int) = ( setWord16(p, 0, Word16.fromInt v); add (p, 0wx2) )
    fun setC_Word32(p, v:int) = ( setWord32(p, 0, Word32.fromInt v); add (p, 0wx4) )


    fun getC_Ulong(p:t):(t*int) =
      let
        val v = if is_64bit then Word64.toInt(getWord64(p, 0)) else Word32.toInt(getWord32(p, 0))
        val p = add (p, sizeofPointer)
      in (p,v) end

    fun getC_Long(p:t):(t*int) =
      let
        val v = if is_64bit then Int64.toInt(getInt64(p, 0)) else Int32.toInt(getInt32(p, 0))
        val p = add (p, sizeofPointer)
      in (p,v) end

    fun getC_Int16(p):(t*int)  = let val v = Int16.toInt(getInt16(p, 0))   val p = add (p, 0wx2) in (p,v) end
    fun getC_Int64(p):(t*int)  = let val v = Int64.toInt(getInt64(p, 0))   val p = add (p, 0wx8) in (p,v) end
    fun getC_Word16(p):(t*int) = let val v = Word16.toInt(getWord16(p, 0)) val p = add (p, 0wx2) in (p,v) end
    fun getC_Word32(p):(t*int) = let val v = Word32.toInt(getWord32(p, 0)) val p = add (p, 0wx4) in (p,v) end

    fun getC_Pointer(p) = let val v = getPointer(p, 0) val p = add (p, sizeofPointer) in (p,v) end


    fun kevent_change kq changelist =
      let
          fun pack_kevent_struct_list p l =
            let
              fun pack_kevent_struct((ident, filter, flags, fflags, data, udata), p) =
                let
                  val p = setC_Ulong(p,ident)
                  val p = setC_Word32(p, filter)
                  val p = setC_Word32(p, flags)
                  val p = setC_Word32(p, fflags)
                  val p = if is_64bit then add (p, 0wx4) else p
                  val p = setC_Int64(p, data)
                  val p = setC_Pointer(p, null)
                  val p = if is_64bit then p else add (p, 0wx4)
                in p end
            in
                Vector.foldl pack_kevent_struct p l
            end
      in
        pack_kevent_struct_list kevent_list_pointer changelist;
        kevent(kq, kevent_list_pointer, Vector.length changelist, MLton.Pointer.null, 0, MLton.Pointer.null)
      end

    fun kevent_wait kq eventlist t =
      let

        fun pack_timeout t =
          case t of
              NONE   => MLton.Pointer.null
            | SOME (s, n) =>
               let
                 val p = setC_Long (timeout_pointer, s)
                 val _ = setC_Long (p, n)
               in timeout_pointer end

        val cnt = kevent(kq, MLton.Pointer.null, 0, kevent_list_pointer, max_events, (pack_timeout t))

        fun unpack_kevent_struct_list p n =
          let
             fun unpack_kevent_struct p =
               let

                 val (p, ident)  = getC_Ulong(p)
                 val (p, filter) = getC_Word32(p)
                 val (p, flags)  = getC_Word32(p)
                 val (p, fflags) = getC_Word32(p)
                 val p = if is_64bit then add (p, 0wx4) else p
                 val (p, data)   = getC_Int64(p)
                 val (p, _)      = getC_Pointer(p)
                 val p = if is_64bit then p else add (p, 0wx4)
               in ((ident, filter, flags, fflags, data, {}), p) end

            fun doit p i n =
              if i = n
              then n
              else
                let
                  val (v,p) = unpack_kevent_struct p
                in
                  Array.update(eventlist, i, v);
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
        fun evFilterToInt evRead  = 0
          | evFilterToInt evWrite = 1

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
                val n = Time.toNanoseconds(t) - s * 1000000000
              in
                SOME (Int.fromLarge s, Int.fromLarge n)
              end
          | NONE => NONE

        val cnt = kevent_wait (#ev ev) eventlist timeout

        fun intToevFilter 0 = evRead
          | intToevFilter 1 = evWrite
          | intToevFilter _ = raise Ev "intToevFilter"

        fun getCb fd evRead  = H.sub ((#rH ev), fd)
          | getCb fd evWrite = H.sub ((#wH ev), fd)

        fun new_loop 0 = cnt
          | new_loop i =
              let
                val (fd,f,_,_,_,_) = Array.sub(eventlist, (i-1))
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
