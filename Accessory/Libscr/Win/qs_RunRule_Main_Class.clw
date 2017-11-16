         Member()
         
         MAP
         END
         
   Include('qs_RunRule_Main_Class.inc'),once
   
qs_RunRule_Main_Class.TestGender Procedure(QS:Gender)!,Long
ReturnValue Long
   Code
     ReturnValue = CHOOSE(QS:Gender = 'M' or QS:Gender = 'F' , 1, 0)
     Return ReturnValue
   
   

   