fun socket_connect host port =
  let
    val h = valOf(NetHostDB.fromString host)
    val addr = INetSock.toAddr(h, port)
    val sock:(Socket.active INetSock.stream_sock) = INetSock.TCP.socket() (* active, passive - listening *)
    val _ = Socket.connect(sock, addr)
  in
    sock
  end



fun main_handle () =
let

val sock1 = socket_connect "127.0.0.1" 8081
val sock2 = socket_connect "127.0.0.1" 8082

val sockToEvFD : ('a, 'b) Socket.sock -> int = fn sock => (SysWord.toInt o Posix.FileSys.fdToWord o Option.valOf o Posix.FileSys.iodToFD o Socket.ioDesc) sock

val fd1 = sockToEvFD sock1
val fd2 = sockToEvFD sock2


local
  fun say_sock_no text sock = print (text ^ ": " ^ (Int.toString (sockToEvFD sock)) ^ "\n")
in
  val _ = (say_sock_no "sock1" sock1; say_sock_no "sock2" sock2)
end


val _ = print "-----\n"

local open Ev in
  val ev = evInit ()

  (* ... val _ = evNowUpdate ev *)
  (* This function establishes the current time by querying the kernel, it is done automatically within evInit and evWait.
   * Call this function before any evTimerAdd when evWait is not called for a very long time. *)

  fun showEvFilter evRead  = "Read"
    | showEvFilter evWrite = "Write"

  fun cb (fd, f) = print ( "cb " ^ (Int.toString fd) ^ ": " ^ (showEvFilter f) ^ "\n")

  val ev_desc_list = [evAdd (fd1, evRead, cb), evAdd (fd1, evWrite, cb), evAdd (fd2, evWrite, cb)]
  val _ = evModify ev ev_desc_list

  val t1 = evTimerNew ev
  val _ = evTimerAdd ev (t1, Time.zeroTime, (fn () => print "Timer 1\n"))

  val wait_cnt = evWait ev (SOME (Time.fromSeconds 3)) (* Time.zeroTime *)
  val _ = print ("evWait:   " ^ (Int.toString wait_cnt) ^ "\n")

  val _ = print "...\n"

  val _ = evModify ev [evDelete (fd2, evWrite)]

  val t2 = evTimerNew ev
  val _ = evTimerAdd ev (t2, Time.zeroTime, (fn () => print "Timer 2\n"))

  val wait_cnt = evWait ev (SOME (Time.fromSeconds 3)) (* Time.zeroTime *)
  val _ = print ("evWait:   " ^ (Int.toString wait_cnt) ^ "\n")

end

val _ = print "=================\n"

local open Ev in
  val ev = evInit ()

  val t1 = evTimerNew ev
  val _ = evTimerAdd ev (t1, Time.zeroTime, (fn () => print "Timer zeroTime\n"))

  val t4 = evTimerNew ev
  val _ = evTimerAdd ev (t4, Time.fromSeconds 1, (fn () => print "Timer 1 second\n"))

  val t2 = evTimerNew ev
  val _ = evTimerAdd ev (t2, Time.fromSeconds 2, (fn () => print "Timer 2 second\n"))

  val t3 = evTimerNew ev
  val _ = evTimerAdd ev (t3, Time.fromSeconds 1, (fn () => print "Timer 1 second\n"))

  val wait_cnt = evWait ev (SOME (Time.fromSeconds 3))
  val _ = print "...\n"

  val wait_cnt = evWait ev (SOME (Time.fromSeconds 3))
  val _ = print "...\n"

  val wait_cnt = evWait ev (SOME (Time.fromSeconds 3))
  val _ = print "...\n"


  val _ = evTimerAdd ev (t1, Time.zeroTime, (fn () => print "Timer zeroTime again\n"))
  val _ = evTimerAdd ev (t2, Time.fromSeconds 2, (fn () => print "Timer 2 second again\n"))

  val wait_cnt = evWait ev (SOME (Time.fromSeconds 3))
  val _ = print "...\n"

  val wait_cnt = evWait ev (SOME (Time.fromSeconds 3))
  val _ = print "...\n"

end


in () end


fun main () = main_handle () handle exc => print ("function main raised an exception: " ^ exnMessage exc ^ "\n")
