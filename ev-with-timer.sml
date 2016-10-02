(* Copyright (C) 2016 Nick Kostyria. BSD3 license. *)

signature OS_IO_EV_TIMER = sig
  include OS_IO_EV
  val evTimerNew: ev -> int
  val evTimerAdd: ev -> int * Time.time * (unit -> unit) -> unit
  val evTimerDelete: ev -> int -> unit
end


structure EvWithTimer :> OS_IO_EV_TIMER  =
struct
  open Ev

  type ev = { ev:Ev.ev, now: Time.time ref, timers: (int * Time.time * (unit -> unit)) list ref, last_id: int ref, free_id: int list ref } 

  fun evInit () = { ev = Ev.evInit (), now = ref (Time.now ()), timers = ref [], last_id = ref 0, free_id = ref [] }


  fun evTimerNew ({ last_id=last_id, free_id=free_id, ... }:ev) =
    if List.null(!free_id)
    then
      let 
        val id = 1 + (!last_id) 
      in
        last_id := id;
        id 
      end
    else
      let
        val id = List.hd (!free_id)
      in
        free_id := List.tl (!free_id);
        id
      end


  fun evTimerAdd ({ev=ev, now=now, timers=timers, ...}:ev) (id, t, cb) =
    let
      val time = Time.+((!(now)), t)

      fun doit []      y = (id, time, cb)::y
        | doit (x::xs) y = if id = (#1 x) then doit xs y else doit xs (x::y)
    in
      timers := doit (!timers) []
    end


  fun evTimerDelete ({ timers=timers, free_id=free_id, ... }:ev) id =
    let
      fun doit []      y = y
        | doit (x::xs) y = if id = (#1 x) then doit xs y else doit xs (x::y)
    in
      timers  := doit (!timers) [];
      free_id := id::(!free_id)
    end


  fun doTimer ({ev=ev, now=now, timers=timers, free_id=free_id, ...}:ev) =
    let
      val time = !now
      fun doit [] ys = ys
        | doit ((x as (id, t, cb))::xs) ys = 
           if Time.>(t, time)
           then doit xs (x::ys) 
           else (free_id := id::(!free_id); cb (); doit xs ys)

      val curent = !timers
      val _ = timers := []
      val old = doit (curent) []
    in
      timers := (!timers) @ old
    end


  fun newTimeout ev timeout =
    let
      fun second(_,s,_) = s
      val time = !(#now ev)
      val l = !(#timers ev)
      val m = case timeout of NONE => second(List.hd(l)) | SOME timeout => Time.+(time,timeout)
      val m = List.foldl (fn((_,t,_),time) => if Time.<(t, time) then t else time) m l
      val d = Time.-(m, time)
    in
      SOME d
    end


  fun evModify (ev:ev) ev_desc_list = Ev.evModify (#ev ev) ev_desc_list


  fun evWait (ev:ev) timeout =
    let
      val _ = doTimer ev
      val timeout = if List.null(!(#timers ev)) then timeout else newTimeout ev timeout
      val cnt = Ev.evWait (#ev ev) timeout
      val _ = (#now ev) := Time.now ()
    in
      cnt
    end
end
