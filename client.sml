val _ = load "Socket";

local
  fun buff v = { buf=v, ofs=0, size=NONE };
  fun poll s =
      let val { rds, ... } =
              Socket.select({ rds = [Socket.sockDesc s],
                              wrs = [], exs = [],
                              timeout = SOME(Time.fromSeconds 0) });
      in case rds of
             [] => false
           | _ => true
      end;
in
  fun chatclient ipno port =
      let val sock = Socket.inetStream ();
          val addr = Socket.inetAddr ipno port;
          val _ = Socket.connect(sock,addr);
          val buffsize = 512
          fun send s =
              let val vec = Byte.stringToBytes s
              in Socket.sendVec(sock, buff vec) end;
          fun recv () =
              let
                  fun aux () =
                      if poll sock then
                          Byte.bytesToString (Socket.recvVec (sock, buffsize))
                          ^ aux ()
                      else "";
              in
                  if poll sock then
                      SOME (aux ())
                  else NONE
              end;
      in (send, recv) end
end;

