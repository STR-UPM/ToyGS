------------------------------------------------------------------------------
-- $Id: gs_tm.adb 90 2017-05-04 14:54:52Z jpuente $
------------------------------------------------------------------------------
-- Project GS
-- GS_TM body
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with Parameters;
with Measurements; use Measurements;
with GUI;

with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams;
with Ada.Unchecked_Conversion;

with System.IO;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;

package body GS_TM is
   use Ada.Real_Time;

   ----------------------
   -- Data definitions --
   ----------------------

   type TM_Message (Kind : TM_Type) is
      record
         Timestamp : Time;
         case Kind is
            when Basic =>
               Data  : Measurement;
            when HK =>
               Data_Log  : HK_Data;
               Length    : Positive;
         end case;
      end record;

   Socket   : Socket_Type;
   Address  : Sock_Addr_Type;
   From     : Sock_Addr_Type;

   subtype TM_Stream is
     Ada.Streams.Stream_Element_Array (1..TM_Message'Size);
   function To_TM_Message is new Ada.Unchecked_Conversion
     (TM_Stream, TM_Message);

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      --  Create UDP socket
      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      -- Local address for receiving TM
      Address := (Family => Family_Inet,
                  Addr   => Any_Inet_Addr,
                  Port   => Port_Type(Parameters.TM_Port));

      Bind_Socket (Socket, Address);

      Pragma Debug
        (System.IO.Put_Line
           ("... listening for TM on port " & Address.Port'Img));
   end Init;

   --------------
   -- Receiver --
   --------------

   task Receiver;

   task body Receiver is
      Data    : TM_Stream;
      Last    : Ada.Streams.Stream_Element_Offset;
   begin
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
                  pragma Debug
                    (System.IO.Put_Line(Image(Clock, "%T ") &
                       "TM " & SC'Img & " " & M.Value'Img));
                  GUI.Put_TM(Image(Clock, "%T ")
                             & "TM " & SC'Img & " "
                             & M.Value'Img);
               when HK =>
                  Split(Message.Timestamp, SC, TS);
                  pragma Debug
                    (System.IO.Put_Line(Image(Clock, "%T ") &
                       "TM "& SC'Img & "  HK log"));
                  GUI.Put_TM(Image(Clock, "%T ")
                             & "TM " & SC'Img & " "
                             & "  HK log");
                  for i in 1..Message.Length loop
                     M := Message.Data_Log(i);
                     Split(M.Timestamp, SC, TS);
                     pragma Debug
                       (System.IO.Put_Line("            "
                        & SC'Img & " " & M.Value'Img));
                     GUI.Put_TM("            "
                                & SC'Img & " " & M.Value'Img);
                  end loop;
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
   end Receiver;

end GS_TM;
