-- $Id: gs_tm.adb 98 2017-05-25 14:12:04Z jpuente $
------------------------------------------------------------------------------
-- Project GS
-- GS_TM body
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with IP, Measurements;

with Ada.Streams;
with GNAT.Sockets;  use GNAT.Sockets;

with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with System.IO;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;

package body GS_TM is
   use Measurements;

   --------------
   -- Receiver --
   --------------

   task body TM_Receiver_Task is

      Socket   : Socket_Type;
      Address  : Sock_Addr_Type;
      From     : Sock_Addr_Type;

      subtype TM_Stream is
        Ada.Streams.Stream_Element_Array (1..TM_Message'Size);
      function To_TM_Message is new Ada.Unchecked_Conversion
        (TM_Stream, TM_Message);

      Data    : TM_Stream;
      Last    : Ada.Streams.Stream_Element_Offset;

   begin
      delay until Clock + Milliseconds(500);
      --  Create UDP socket
      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      -- Local address for receiving TM
      Address := (Family => Family_Inet,
                  Addr   => Any_Inet_Addr,
                  Port   => Port_Type(IP.TM_Port));

      Bind_Socket (Socket, Address);

      System.IO.Put_Line(" ...listening on port " & Address.Port'Img);

      loop
         begin
            Receive_Socket (Socket, Data, Last, From);
            declare
               use Ada.Calendar, GNAT.Calendar.Time_IO;

               Message : TM_Message :=  To_TM_Message(Data);
               SC      : Seconds_Count;
               TS      : Time_Span;
               M       : Measurement;
            begin
               case Message.Kind is
               when Basic =>
                  M := Message.Data;
                  Split(M.Timestamp, SC, TS);
                  System.IO.Put_Line(Image(Clock, "%T ") &
                                     "TM " & SC'Img & " " & M.Value'Img);
               when HK =>
                  Split(Message.Timestamp, SC, TS);
                  System.IO.Put_Line(Image(Clock, "%T ") &
                                     "TM "& SC'Img & "  HK log");
                  System.IO.Put_Line("         ----------------------");
                  for i in 1..Message.Length loop
                     M := Message.Data_Log(i);
                     Split(M.Timestamp, SC, TS);
                     System.IO.Put_Line("            " &
                                          SC'Img & " " & M.Value'Img);
                  end loop;
                  System.IO.Put_Line("         ----------------------");
               end case;
            end;
         exception
            when E : Socket_Error =>
               System.IO.Put_Line(Ada.Exceptions.Exception_Message(E));
         end;
      end loop;

   exception
      when E : others =>
         Close_Socket (Socket);
         System.IO.Put_Line("TM task stopped "
                            & Ada.Exceptions.Exception_Message(E));
   end TM_Receiver_Task;

end GS_TM;
