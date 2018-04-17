------------------------------------------------------------------------------
--                                                                          --
--          Copyright (C) 2017, Universidad Politécnica de Madrid           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
-------------------------------------------------------------------------------

-- Implementation of TM subsystem

with IP;            use IP;
with Measurements;  use Measurements;
with User_Interface;

with Ada.Real_Time;          use Ada.Real_Time;
with Ada.Calendar;           use Ada.Calendar;
with GNAT.Calendar.Time_IO;  use GNAT.Calendar.Time_IO;

with GNAT.Sockets;  use GNAT.Sockets;
with Ada.Streams;   use Ada.Streams;
with Ada.Unchecked_Conversion;

pragma Warnings(Off);
with System.IO;           -- for debugging puerposes
pragma Warnings(On);

package body TM_Receiver is

   ----------------------
   -- Data definitions --
   ----------------------

   Socket   : Socket_Type;
   Address  : Sock_Addr_Type;
   From     : Sock_Addr_Type;

   subtype TM_Stream is
     Stream_Element_Array (1..TM_Message'Size/8);
   function To_TM_Message is new Ada.Unchecked_Conversion
     (TM_Stream, TM_Message);

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      --  Create UDP socket
      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      -- Local address for receiving TM messages
      Address := (Family => Family_Inet,
                  Addr   => Any_Inet_Addr,
                  Port   => Port_Type(IP.TM_Port));

      Bind_Socket (Socket, Address);

      Pragma Debug
        (System.IO.Put_Line
           ("... listening for TM on port " & Address.Port'Img));
   end Init;

   -------------------
   -- Receiver task --
   -------------------

   task body TM_Receiver_Task is

      Data    : TM_Stream;
      Last    : Ada.Streams.Stream_Element_Offset;

   begin
      delay 1.0;
      pragma Debug(System.IO.Put_Line("TM receiver task started"));
      loop
         Receive_Socket (Socket, Data, Last, From);
         -- pragma Debug(System.IO.Put_Line("Received from " & Image(From)));
         declare
            Message : TM_Message :=  To_TM_Message(Data);
            SC      : Seconds_Count;
            TS      : Time_Span;
            M       : Measurement;
         begin
            case Message.Kind is
               when Basic =>
                  M := Message.Data;
                  Split(M.Timestamp, SC, TS);
--                    pragma Debug
--                      (System.IO.Put_Line("TM " & SC'Img & " " & M.Value'Img));
                  User_Interface.Put_TM(Image(Clock, "%T ")
                             & "TM " & SC'Img & " "
                             & M.Value'Img);
               when Housekeeping =>
                  Split(Message.Timestamp, SC, TS);
--                    pragma Debug
--                      (System.IO.Put_Line("TM "& SC'Img & "  HK log"));
                  User_Interface.Put_TM(Image(Clock, "%T ")
                             & "TM " & SC'Img & " "
                             & " HK log");
                  for i in 1..Message.Length loop
                     M := Message.Data_Log(i);
                     Split(M.Timestamp, SC, TS);
--                       pragma Debug
--                         (System.IO.Put_Line("            "
--                          & SC'Img & " " & M.Value'Img));
                     User_Interface.Put_TM("            "
                                & SC'Img & " " & M.Value'Img);
                  end loop;
            end case;
         end;
      end loop;

   end TM_Receiver_Task;

end TM_Receiver;
