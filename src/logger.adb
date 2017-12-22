with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Finalization;              use Ada.Finalization;
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
      Log_Timestamp: Ada.Calendar.Time := Ada.Calendar.Clock;
      Log_M : aliased Log_Message := Log_Message(Message);
      Log_M_Ptr : Log_Message_Ptr := Log_M'Unchecked_Access;
      Log : Log_Record_Ptr := new Log_Record'(Controlled with
                                              Timestamp => Log_Timestamp,
                                              Level     => Level,
                                              Message   => Log_M_Ptr);
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

end Logger;
