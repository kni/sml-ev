(* Copyright (C) 2016-2023 Nick Kostyria. BSD3 license. *)

signature OS_IO_EV = sig
  include OS_IO_EV
  val evTimerNew: ev -> int
  val evTimerAdd: ev -> int * Time.time * (unit -> unit) -> unit
  val evTimerDelete: ev -> int -> unit
  val evNowUpdate: ev -> unit
end


structure Ev :> OS_IO_EV =
struct
  open Ev

  structure H = HashArrayInt

  type ev = { ev:Ev.ev, now: Time.time ref, timers: (Time.time * (unit -> unit)) H.hash, last_id: int ref, free_id: int list ref }

  val hash_size  = 100

  fun evInit () = { ev = Ev.evInit (), now = ref (Time.now ()), timers = H.hash hash_size, last_id = ref 0, free_id = ref [] }

  fun evNowUpdate ({now=now, ...}:ev) = now := Time.now ()


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


  fun evTimerAdd ({now=now, timers=timers, ...}:ev) (id, t, cb) =
    let
      val time = Time.+((!(now)), t)
    in
      H.update (timers, id, (time, cb))
    end


  fun evTimerDelete ({ timers=timers, free_id=free_id, ... }:ev) id =
    let
    in
      H.delete (timers, id);
      free_id := id::(!free_id)
    end


  fun doTimer ({now=now, timers=timers, free_id=free_id, ...}:ev) =
    let
      val cbs = H.fold (fn (id, (time, cb), r) =>
        if Time.>(time, !now)
        then r
        else (H.delete (timers, id); cb::r)
      ) [] timers
    in
      app (fn cb => cb ()) cbs
    end


  fun newTimeout ({now=now, timers=timers, ...}:ev) timeout =
    let
      val t = case timeout of SOME t => t | NONE => Time.fromSeconds 25
      val min = H.fold (fn (id, (time, cb), min) =>
        if Time.<(time, min) then time else min
      ) (Time.+(!now, t)) timers
      val d = Time.-(min, !now)
      val d_min = Time.fromMilliseconds 1
    in
      SOME (if Time.<(d, d_min) then d_min else d)
    end


  fun evModify (ev:ev) ev_desc_list = Ev.evModify (#ev ev) ev_desc_list


  fun evWait (ev:ev) timeout =
    let
      val _ = doTimer ev
      val timeout = newTimeout ev timeout
      val cnt = Ev.evWait (#ev ev) timeout
      val _ = (#now ev) := Time.now ()
    in
      cnt
    end
end
