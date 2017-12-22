with Ada.Finalization;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
package Logger is
   package ASU renames Ada.Strings.Unbounded;
   -- record for holding log elements
   type Log_Record is new Ada.Finalization.Controlled with private;
   type Log_Record_Ptr is access all Log_Record'Class;
   
   -- available log levels 
   type Log_Level is (DEBUG, INFO, WARNING, DANGER, ERROR, FATAL);
   for Log_Level use (
                      DEBUG => 1, INFO => 2, WARNING => 3
                      , DANGER => 4, ERROR => 5, FATAL => 6);
   
   procedure Log(Message: String; Level: Log_Level);
   procedure Log(Message: ASU.Unbounded_String; Level: Log_Level);   
   procedure Finalize(Log: in out Log_Record);
   
private 
   -- Message to be stored in log
   type Log_Message is new ASU.Unbounded_String;
   type Log_Message_Ptr is access all Log_Message;
   
   type Log_Record is new Ada.Finalization.Controlled with Record
      Timestamp:    Ada.Calendar.Time;
      Level:        Log_Level;
      Message:      Log_Message_Ptr;
   end record;
   -- protected log queue object
   package Log_Record_Ptr_I 
   is new Ada.Containers.Synchronized_Queue_Interfaces(Log_Record_Ptr);
   
   package Log_Record_Queue 
   is new Ada.Containers.Unbounded_Synchronized_Queues(Log_Record_Ptr_I);
   Log_Queue : Log_Record_Queue.Queue;
end Logger;
