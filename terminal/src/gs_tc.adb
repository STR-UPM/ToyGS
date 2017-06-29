------------------------------------------------------------------------------
-- Project GS
-- GS_TC body
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with IP;

with GNAT.Sockets;            use GNAT.Sockets;
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Streams;

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

With System.IO;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;

package body GS_TC is
   use GNAT.Sockets;

   subtype TC_Stream is
     Ada.Streams.Stream_Element_Array (1..TC_Message'Size);
   function To_Stream is new Ada.Unchecked_Conversion
     (TC_Message, TC_Stream);

   ------------
   -- Sender --
   ------------

   task body Sender is
      Socket         : Socket_Type;
      OBSW_Address   : Sock_Addr_Type;

      T       : Time;
      SC      : Seconds_Count;
      TS      : Time_Span;

      C : Character;
   begin
      delay until Clock + Milliseconds(1000);
      Create_Socket(Socket, Family_Inet, Socket_Datagram);
      OBSW_Address := (Family_Inet, IP.OBSW_IP, IP.OBSW_Port);

      loop
         Get_Immediate(C);
         C := To_Upper(C);
         case C is
            when 'C' =>
               T := Clock;
               Split(T, SC, TS);
               declare
                  use GNAT.Calendar.Time_IO;
                  Command : TC_Message := (Kind => HK, Timestamp => T);
                  Stream  : TC_Stream  := To_Stream(Command);
                  Last    : Ada.Streams.Stream_Element_Offset;
               begin
                  Send_Socket(Socket, Stream, Last, OBSW_Address);
                  System.IO.Put_Line
                    (Image(Ada.Calendar.Clock, "%T ") &
                                     "TC " & SC'Img & "  HK request");
               end;
            when others =>
               null;
            end case;
      end loop;
   end Sender;

end GS_TC;
