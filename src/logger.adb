with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Finalization;              use Ada.Finalization;
with Ada.Text_IO;                   use Ada.Text_IO;

package body Logger is
   use Ada.Strings.Unbounded;
   ---------
   -- Log --
   ---------

   procedure Log (Message: String; Level: Log_Level) is
   begin
      Log(To_Unbounded_String(Message), Level);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Message: ASU.Unbounded_String; Level: Log_Level) is
      Log_Timestamp: constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Log_M :constant Log_Message := Log_Message(Message);
      Log : constant Log_Record_Ptr := new Log_Record'(Controlled with
                                                       Timestamp => Log_Timestamp,
                                                       Level     => Level,
                                                       Message   => Log_M);
   begin
      Log_Queue.Enqueue(Log);
   end Log;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Log: in out Log_Record) is
   begin
      null;
   end Finalize;

   procedure Init is
   begin
      Log_Writer.Start;
   end Init;

   procedure Stop is
   begin
      Log_Writer.Stop;
      Log("Logging stopped", INFO);
   end Stop;


   task body Log_Writer is
      Is_Stop_Called : Boolean := False;
      Log: Log_Record_Ptr;
   begin
      loop
         accept Start;

         begin
            loop
               Log_Queue.Dequeue(Log);
               Put_Line(To_String(Log.Message));
               exit when Is_Stop_Called;
            end loop;
         end;
         accept Setup;
         begin
            null;
         end;
         accept Stop;
         begin
            Is_Stop_Called := True;
         end;
      end loop;
   end Log_Writer;

end Logger;
