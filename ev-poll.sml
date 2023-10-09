signature OS_IO_EV = sig

  type     evFd = int
  datatype evFilter = evRead | evWrite
  type     evCb = (evFd * evFilter) -> unit
  datatype evDesc = evAdd of evFd * evFilter * evCb | evDelete of evFd * evFilter

  type     ev

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

  type     ev = { rH: (OS.IO.poll_desc * evCb) H.hash, wH: (OS.IO.poll_desc * evCb) H.hash }

  val hash_size  = 100

  fun evInit () = { rH = H.hash hash_size, wH = H.hash hash_size }

  val intToPollDesc = Option.valOf o OS.IO.pollDesc o Posix.FileSys.fdToIOD o Posix.FileSys.wordToFD o SysWord.fromInt
  val pollDescToInt = SysWord.toInt o Posix.FileSys.fdToWord o Option.valOf o Posix.FileSys.iodToFD o OS.IO.pollToIODesc o OS.IO.infoToPollDesc

  fun evModify (ev:ev) ev_desc_list =
    let
      fun evFilterToH evRead  = #rH ev
        | evFilterToH evWrite = #wH ev
    in
      List.app (fn d => case d of
            evAdd (fd, filter, cb) => H.update ((evFilterToH filter), fd, (intToPollDesc fd, cb))
          | evDelete (fd, filter)  => H.delete ((evFilterToH filter), fd)
        ) ev_desc_list;
      List.length ev_desc_list
    end

  fun evWait (ev:ev) t =
    let
      open OS.IO

      val ds:poll_desc list = H.fold (fn (k, (d, cb), r) => (pollIn d)::r) [] (#rH ev)
      val ds:poll_desc list = H.fold (fn (k, (d, cb), r) => (pollIn d)::r) ds (#wH ev)

      val pInfo = poll (ds, t)

      fun doit (h, fd, f) = case H.sub (h, fd) of NONE => () | SOME (d, cb) => cb (fd, f)
    in
      List.app (fn i =>
        let
          val fd = pollDescToInt i
        in
          if isIn  i then doit (#rH ev, fd, evRead) else
          if isOut i then doit (#wH ev, fd, evWrite) else
          ()
        end
      ) pInfo;
      List.length pInfo
    end
end
