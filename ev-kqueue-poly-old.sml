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

  local
    open Foreign

    val max_events = 8
    val hash_size  = 1000

    val libc = loadExecutable ()

    val kqueue = buildCall0 ((getSymbol libc "kqueue"), (), cInt)

    local
        val kevent_struct_conversion = cStruct6 (cUlong, cShort, cUshort, cUint, cLong, cConstStar cVoid)
        val timespec_conversion = cStruct2 (cLong, cLong)
        val kevent_ffi =
          buildCall6 ( (getSymbol libc "kevent"),
            (
              cInt,
              (cVectorPointer kevent_struct_conversion), cInt,
              (cArrayPointer kevent_struct_conversion), cInt,
              (cOptionPtr (cConstStar timespec_conversion))
            ),
            cInt
            )
    in
      fun kevent kq changelist eventlist timeout = kevent_ffi
          (kq, changelist, (Vector.length changelist), eventlist, (Array.length eventlist), timeout)
    end


    val kevent_struct_empty = (0,0,0,0,0,{})

    val changelist_zero = Vector.tabulate (0, (fn i => kevent_struct_empty ))
    val eventlist       = Array.array (max_events, kevent_struct_empty)
    val eventlist_zero  = Array.array (0, kevent_struct_empty)

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

        val cnt = kevent (#ev ev) changelist eventlist_zero NONE
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

        val cnt = kevent (#ev ev) changelist_zero eventlist timeout

        fun intToevFilter (~1) = evRead
          | intToevFilter (~2) = evWrite
          | intToevFilter _    = raise Ev "intToevFilter"

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
