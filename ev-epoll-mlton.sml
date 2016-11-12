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

    val max_events = 64
    val hash_size  = 1000

    val malloc = (_import "malloc" : Word.word -> t;) o Word.fromInt

    val epoll_event_size = 12 (* 12 for 32 and 64 bit Linux *)
    val epoll_event_pointer = malloc (epoll_event_size * max_events)

    val epoll_create   = _import "epoll_create" : int -> int;
    val epoll_ctl_ffi  = _import "epoll_ctl"    : int * int * int * t -> int;
    val epoll_wait_ffi = _import "epoll_wait"   : int * t * int * int -> int;


    fun setC_Int32(p,  v:int) = ( setInt32(p, 0, Int32.fromInt v) ;  add (p, 0wx4) )
    fun setC_Word32(p, v:int) = ( setWord32(p, 0, Word32.fromInt v); add (p, 0wx4) )

    fun getC_Int32(p):(t*int)  = let val v = Int32.toInt(getInt32(p, 0))   val p = add (p, 0wx4) in (p,v) end
    fun getC_Word32(p):(t*int) = let val v = Word32.toInt(getWord32(p, 0)) val p = add (p, 0wx4) in (p,v) end

    val EPOLL_CTL_ADD = 1 and EPOLL_CTL_DEL = 2 and EPOLL_CTL_MOD = 3

    fun epoll_ctl ev ctl fd event =
      let
        val p = epoll_event_pointer
        val p = setC_Word32(p, event)
        val p = setC_Int32(p, fd) val p = add (p, 0wx4) (* for union *)
      in
        if epoll_ctl_ffi(ev, ctl, fd, epoll_event_pointer) = 0
        then 1
        else if ctl = EPOLL_CTL_DEL then 0 else raise Ev "evModify"
      end


    val epoll_event_array = Array.array (max_events, (0,0))

    fun epoll_wait ev epoll_event_array t =
      let
        val cnt = epoll_wait_ffi(ev, epoll_event_pointer, max_events, t)

        fun doit p i n =
          if i = n
          then n
          else
            let
              val (p, events) = getC_Word32(p)
              val (p, fd)     = getC_Int32(p) val p = add (p, 0wx4) (* for union *)
            in
              Array.update(epoll_event_array, i, (events, fd));
              doit p (i+1) n
            end
      in
        if cnt >= 0
        then doit epoll_event_pointer 0 cnt
        else cnt
      end

  in

    fun evInit () =
      let
        val ev = epoll_create 1
      in
        if ev = ~1 then raise Ev "evInit" else
        { ev = ev, rH = H.hash hash_size, wH = H.hash hash_size }
      end


    fun evModify {ev=ev, rH=rH, wH=wH } ev_desc_list =
      let

        fun isNotSome v = if isSome v then false else true

        infix xorb
        fun op xorb(a:int,b:int):int = Word.toInt(Word.xorb(Word.fromInt a, Word.fromInt b))

        val EPOLLIN = 1 and EPOLLOUT = 4

        fun evModifyOne (evAdd (fd, evRead, cb))  = if isSome (H.sub (rH, fd)) then 0 else ( (
              if isSome (H.sub (wH, fd))
              then epoll_ctl ev EPOLL_CTL_MOD fd (EPOLLIN xorb EPOLLOUT)
              else epoll_ctl ev EPOLL_CTL_ADD fd EPOLLIN
              ); H.update (rH, fd, cb); 1 )

          | evModifyOne (evAdd (fd, evWrite, cb)) = if isSome (H.sub (wH, fd)) then 0 else ( (
              if isSome (H.sub (rH, fd))
              then epoll_ctl ev EPOLL_CTL_MOD fd (EPOLLIN xorb EPOLLOUT)
              else epoll_ctl ev EPOLL_CTL_ADD fd EPOLLOUT 
              ); H.update (wH, fd, cb); 1 )

          | evModifyOne (evDelete (fd, evRead))   = if isNotSome (H.sub (rH, fd)) then 0 else ( (
              if isSome (H.sub (wH, fd))
              then epoll_ctl ev EPOLL_CTL_MOD fd EPOLLOUT
              else epoll_ctl ev EPOLL_CTL_DEL fd 0
              ); H.delete (rH, fd); 1 )

          | evModifyOne (evDelete (fd, evWrite))  = if isNotSome (H.sub (wH, fd)) then 0 else ( (
              if isSome (H.sub (rH, fd))
              then epoll_ctl ev EPOLL_CTL_MOD fd EPOLLIN
              else epoll_ctl ev EPOLL_CTL_DEL fd 0
              ); H.delete (wH, fd); 1 )

      in
        foldl ( fn(ev_desc,cnt) => cnt + evModifyOne ev_desc ) 0 ev_desc_list
      end


    fun evWait (ev:ev) t =
      let

        val timeout = case t of SOME t => LargeInt.toInt(Time.toMilliseconds t) | NONE => ~1

        val cnt = epoll_wait (#ev ev) epoll_event_array timeout

        infix xorb
        infix andb

        val op xorb = Word.xorb
        val op andb = Word.andb

        val EPOLLIN = 0wx001 and EPOLLOUT = 0wx004 and EPOLLERR = 0wx008 and EPOLLHUP = 0wx010

        fun isRead  events = case (Word.fromInt events) andb (EPOLLIN  xorb EPOLLERR xorb EPOLLHUP) of 0w0 => false | _ => true
        fun isWrite events = case (Word.fromInt events) andb (EPOLLOUT xorb EPOLLERR xorb EPOLLHUP) of 0w0 => false | _ => true

        fun getCb fd evRead  = H.sub ((#rH ev), fd)
          | getCb fd evWrite = H.sub ((#wH ev), fd)

        fun doCb fd filter  =
          case getCb fd filter of
              SOME cb => cb (fd, filter)
            | NONE    => ()

        val cnt_all = ref 0
        fun cnt_all_up () = cnt_all := (!cnt_all) + 1

        fun new_loop 0 = (!cnt_all)
          | new_loop i =
              let
                val (events,fd) = Array.sub(epoll_event_array, (i-1))
              in
                if isRead  events then (doCb fd evRead ; cnt_all_up () ) else ();
                if isWrite events then (doCb fd evWrite; cnt_all_up () ) else ();
                new_loop (i - 1)
              end

      in
        if cnt >= 0
        then new_loop cnt 
        else raise Ev "evWait"
      end
  end

end
