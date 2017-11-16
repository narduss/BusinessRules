#TEMPLATE(qsRules,'Qualitas Solutions - External Rules Templates'),FAMILY('ABC')
#!--------------------------------------------------------------------------
#!
#! All Rights Reserved World Wide
#! 
#!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#!                                               APPLICATION EXTENSION
#!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#!
#SYSTEM
  #EQUATE(%AppTPLFontName,'Tahoma')
#EXTENSION(qsExternalRulesGlobal,'Activate Qualitas Solutions''s External Business Rules-Global'),APPLICATION(GlobalBusinessRulesManager)
#!
#! The PREPARE section includes calls that must be performed to correctly display the prompts in your
#! template when accessed within your application
#!------------------------------------------------------------------------------------------------------------------------
#PREPARE
  #CALL(%ReadABCFiles(ABC))
  #CALL(%SetClassDefaults(ABC),'qsRulesClass','qsRulesObj:'&%ActiveTemplateInstance,'qsRulesClass')
#ENDPREPARE
#!
#!  The ATSTART section, albeit a copy of the PREPARE section, performs the same calls but, unlike the
#!  #PREPARE section that is only used while accessing the template prompts, this section is processed
#!  prior to actual source code generation
#!------------------------------------------------------------------------------------------------------------------------
#ATSTART 
  #!  Purges a bunch of template symbols which are then reloaded through a #SERVICE call into C55TPLS.DLL
  #CALL(%ReadABCFiles(ABC)) 
  #!  Verifies the instance of the class referenced by the template
  #CALL(%SetClassDefaults(ABC),'qsRulesClass','qsRulesObj:'&%ActiveTemplateInstance,'qsRulesClass')
#ENDAT
#!
#SHEET
#! Custom for this template
#!------------------------------------------------------------------------------------------------------------------------
#TAB('General')
    #INSERT  (%SETCC)
    #INSERT  (%SETAPP)
    #BOXED   ('User Override'),section,AT(,65,,40)
       #PROMPT  ('Enable User Override',Check),%qsEnableUserOverride,AT(10)
       #ENABLE(%qsEnableUserOverride=%TRUE)
          #PROMPT  ('Global Variable for User ID',@s50),%qsGlobalUser,REQ
       #ENDENABLE
    #ENDBOXED
    #BOXED   ('Debugging'),section,AT(,105,,20)
       #PROMPT  ('Disable Qualitas Solutions''s External Business Rules',Check),%qsDisableExternalRules,AT(10)
    #ENDBOXED
#ENDTAB
#TAB('Database Tables'),Where(%qsDisableExternalRules = %False)
    #DISPLAY ('')
    #BOXED('Database Tables'),Section
       #DISPLAY ('Should have TPS/SQL files as well as In-Memory')
       #DISPLAY (' ')
       #PROMPT  ('Rules-File',FILE),%Rules,REQ
       #PROMPT  ('Rule Primary Key',Key(%Rules)),%RULEKEY,REQ
       #PROMPT  ('Rule System Id',Field(%Rules)),%RSysIdRule,REQ
       #PROMPT  ('Rule Name',Field(%Rules)),%RRuleName,REQ
       #PROMPT  ('Rule Description',Field(%Rules)),%RRuleDescription,REQ
       #PROMPT  ('Rule Expression',Field(%Rules)),%RRuleExpression,REQ
       #PROMPT  ('Rule Control Name',Field(%Rules)),%RControlName,REQ
       #PROMPT  ('Rule Offset',Field(%Rules)),%ROffSet,REQ
       #PROMPT  ('Rule Active',Field(%Rules)),%RRuleActive,REQ
       #DISPLAY (' ')
       #PROMPT  ('ControlsPerRule-File',FILE),%ControlesPerRule,REQ,WHENACCEPTED(%StripExclamation(%ControlesPerRule))
       #PROMPT  ('CPR Foreign Key',KEY(%ControlesPerRule)),%ControlesPerRuleKey,REQ
       #PROMPT  ('CPR System Id',Field(%ControlesPerRule)),%CSysIqsontrolsPerRule,REQ
       #PROMPT  ('Rule System Id',Field(%ControlesPerRule)),%CSysIdRule,REQ
       #PROMPT  ('CPR Control Name',Field(%ControlesPerRule)),%CControlName,REQ
       #PROMPT  ('CPR Rule Action',Field(%ControlesPerRule)),%CRuleAction,REQ
       #DISPLAY (' ')
       #PROMPT  ('OverrideProcPerRule-File',FILE),%OverrideProcPerRule,REQ,WHENACCEPTED(%StripExclamation(%OverrideProcPerRule))
       #PROMPT  ('OPR Foreign Key',KEY(%OverrideProcPerRule)),%OverrideProcPerRuleFKey,REQ
       #PROMPT  ('OPR System Id',Field(%OverrideProcPerRule)),%OSysIDOverrideProcRule,REQ
       #PROMPT  ('Rule System Id',Field(%OverrideProcPerRule)),%OSysIdRule,REQ
       #PROMPT  ('OPR Procedure Name',Field(%OverrideProcPerRule)),%OProcedureName,REQ
       #ENABLE(%qsEnableUserOverride=%TRUE)
          #PROMPT  ('OPR Override Users',Field(%OverrideProcPerRule)),%OOverrideUsers,REQ
          #DISPLAY (' ')
          #PROMPT  ('OverrideUserPerProc-File',FILE),%OverrideUserPerProc,REQ,WHENACCEPTED(%StripExclamation(%OverrideUserPerProc))
          #PROMPT  ('OUR Foreign Key',KEY(%OverrideUserPerProc)),%OverrideUserPerProcFKey,REQ
          #PROMPT  ('OUR System Id',Field(%OverrideUserPerProc)),%OURSysIDOverrideUserProc,REQ
          #PROMPT  ('Override Proc System Id',Field(%OverrideUserPerProc)),%OURSysIdOverrideProcRule,REQ
          #PROMPT  ('OUR User Logon Id',Field(%OverrideUserPerProc)),%OURUserLogonId,REQ
       #ENDENABLE   
    #ENDBOXED           
#ENDTAB
#TAB('Queue/In-Memory'),Where(%qsDisableExternalRules = %False)
    #DISPLAY ('')
    #BOXED('In-Memory Tables'),Section
       #DISPLAY ('Use In memory driver in place of Global Queue')
       #DISPLAY ('Will fix Threading issues.')
       #DISPLAY ('Should have TPS/SQL files as well as In-Memory')
       #PROMPT  ('Use In-Memory Driver',CHECK),%UseIMD,At(10,30)
       #ENABLE(%UseIMD=%TRUE)
          #PROMPT  ('Rules - File',FILE),%IMDRules,REQ
          #PROMPT  ('Rule Primary Key',Key(%IMDRules)),%IMDRULEKEY,REQ
          #PROMPT  ('Rule System Id',Field(%IMDRules)),%IMDRSysIdRule,REQ
          #PROMPT  ('Rule Name',Field(%IMDRules)),%IMDRRuleName,REQ
          #PROMPT  ('Rule Description',Field(%IMDRules)),%IMDRRuleDescription,REQ
          #PROMPT  ('Rule Expression',Field(%IMDRules)),%IMDRRuleExpression,REQ
          #PROMPT  ('Rule Control Name',Field(%IMDRules)),%IMDRControlName,REQ
          #PROMPT  ('Rule Offset',Field(%IMDRules)),%IMDROffSet,REQ
          #DISPLAY (' ')
          #PROMPT  ('ControlsPerRule-File',FILE),%IMqsontrolesPerRule,REQ,WHENACCEPTED(%StripExclamation(%IMqsontrolesPerRule))
          #PROMPT  ('CPR Foreign Key',KEY(%IMqsontrolesPerRule)),%IMqsontrolesPerRuleKey,REQ
          #PROMPT  ('Rule System Id',Field(%IMqsontrolesPerRule)),%IMqsSysIdRule,REQ
          #PROMPT  ('CPR Control Name',Field(%IMqsontrolesPerRule)),%IMqsControlName,REQ
          #PROMPT  ('CPR Rule Action',Field(%IMqsontrolesPerRule)),%IMqsRuleAction,REQ
          #DISPLAY (' ')
          #PROMPT  ('OverrideProcPerRule-File',FILE),%IMDOverrideProcPerRule,REQ,WHENACCEPTED(%StripExclamation(%IMDOverrideProcPerRule))
          #PROMPT  ('OPR Search Key',KEY(%IMDOverrideProcPerRule)),%IMDOverrideProcPerRuleSKey,REQ
          #PROMPT  ('OPR System Id',Field(%IMDOverrideProcPerRule)),%IMDSysIdOverrideProcRule,REQ
          #PROMPT  ('Rule System Id',Field(%IMDOverrideProcPerRule)),%IMDOSysIdRule,REQ
          #PROMPT  ('OPR Procedure Name',Field(%IMDOverrideProcPerRule)),%IMDOProcedureName,REQ
          #ENABLE(%qsEnableUserOverride=%True)
             #PROMPT  ('OPR Override Users',Field(%IMDOverrideProcPerRule)),%IMDOverrideUsers,REQ
             #DISPLAY (' ')
             #PROMPT  ('OverrideUserPerRule-File',FILE),%IMDOverrideUserPerProc,REQ,WHENACCEPTED(%StripExclamation(%IMDOverrideUserPerProc))
             #PROMPT  ('OUR Search Key',KEY(%IMDOverrideUserPerProc)),%IMDOverrideUserPerProcFSKey,REQ
             #PROMPT  ('Override Proc System Id',Field(%IMDOverrideUserPerProc)),%IMDOURSysIdOverrideProcPerRule,REQ
             #PROMPT  ('OUR User Logon Id',Field(%IMDOverrideUserPerProc)),%IMDOUserLogonId,REQ
          #ENDENABLE
       #ENDENABLE
    #ENDBOXED           
#ENDTAB
#TAB('Rule Settings'),Where(%qsDisableExternalRules = %False)
    #DISPLAY ('')
    #DISPLAY ('')
    #PROMPT  ('Error-indicator Image',OPENDIALOG('Select error-indicator image file','Icons (*.ico)|*.ICO|Windows Bitmaps (*.bmp)|*.BMP|GIF files|*.gif|JPEG files (*.jpg)|*.JPG|Windows Metafiles (*.wmf)|*:WMF|PCX files|*.PCX|All files|*.*')),%GlobalErrorIndicatorImage,DEFAULT('BRuleNo.ico')
    #PROMPT  ('Default Distance from Right:',SPIN(@n3,0,999)),%GlobalRuleOffSetRight,REQ,DEFAULT(3)
    #PROMPT  ('Name:',@s255),%RuleBaseName,REQ,Default('MyGlobalRules')
    #PROMPT  ('Description:',@s255),%RuleBaseDescription,REQ,Default('GlobalRules')
    #PROMPT  ('Mode:',OPTION),%GlobalRulesTemplateMode,AT(4,,192)
    #PROMPT  ('Original',RADIO)
    #PROMPT  ('Clone',RADIO)
    #PROMPT  ('Header Icon:',OPENDIALOG('Select an Icon for a Header Icon','Icons (*.ICO)|*.ICO|Windows Bitmaps (*.bmp)|*.BMP|GIF files|*.gif|JPEG files (*.jpg)|*.JPG|Windows Metafiles (*.wmf)|*:WMF|PCX files|*.PCX|All files|*.*')),%GlobalViewRulesHeaderIcon,REQ,DEFAULT('BRules.ico')
    #PROMPT  ('Valid Rule Icon:',OPENDIALOG('Select an Icon for a Valid Rule','Icons (*.ICO)|*.ICO|Windows Bitmaps (*.bmp)|*.BMP|GIF files|*.gif|JPEG files (*.jpg)|*.JPG|Windows Metafiles (*.wmf)|*:WMF|PCX files|*.PCX|All files|*.*')),%GlobalViewRulesValidRuleIcon,REQ,DEFAULT('BRuleOk.ico')
    #PROMPT  ('Broken Rule Icon:',OPENDIALOG('Select an Icon for a Broken Rule','Icons (*.ICO)|*.ICO|Windows Bitmaps (*.bmp)|*.BMP|GIF files|*.gif|JPEG files (*.jpg)|*.JPG|Windows Metafiles (*.wmf)|*:WMF|PCX files|*.PCX|All files|*.*')),%GlobalViewRulesBrokenRuleIcon,REQ,DEFAULT('BRuleNo.ico')
#ENDTAB
#TAB('Global &Objects'),Where(%qsDisableExternalRules = %False)
    #!  This adds hidden prompts required by calls within the template
    #BOXED('Default Tag Class prompts'),AT(0,0),WHERE(%False),HIDE
       #! Define and set basic symbols to handle this ABC compliant tagging class.
       #INSERT(%OOPHiddenPrompts(ABC))
    #ENDBOXED
    #!  Visible #PROMPTs for ABC Handling
    #BUTTON('&External Rules Class'),AT(,30,170)
      #WITH(%ClassItem,'qsRulesClass' & %ActiveTemplateInstance)
        #INSERT(%GlobalClassPrompts(ABC)) 
      #ENDWITH 
    #ENDBUTTON 
#ENDTAB
#ENDSHEET

#!------------------------------------------------------------------------------------------------------------------------
#AT(%AfterGlobalIncludes)
 #IF(%qsDisableExternalRules = %False)
   !added by qsBusinessRules
   INCLUDE('ABRULE.INC'),ONCE
 #ENDIF
#ENDAT

#!------------------------------------------------------------------------------------------------------------------------
#AT(%GlobalData)
 #IF(%qsDisableExternalRules = %False)
  #CASE(%ProgramExtension)
  #OF('DLL')
    #IF(%GlobalExternal)                              #!DLL with global data external
      #IF(%ExternalSource='Dynamic Link Library (DLL)')
!added by qsBusinessRules
GlobalRule           LONG,EXTERNAL,DLL(dll_mode),THREAD

#IF(%UseIMD=%FALSE)
#INSERT(%SETRuleQExt)
#ENDIF
      #ELSE
!added by qsBusinessRules
GlobalRule           LONG,EXTERNAL,THREAD
      #ENDIF
    #ELSE                                             #!DLL with export global data
!added by qsBusinessRules
GlobalRule           LONG,THREAD

#IF(%UseIMD=%FALSE)
#INSERT(%SETRuleQ)
#ENDIF

    #ENDIF
  #OF('LIB')
    #IF(%GlobalExternal)                              #!LIB with global data external
      #IF(%ExternalSource='Dynamic Link Library (DLL)')
!added by qsBusinessRules
GlobalRule           LONG,EXTERNAL,DLL(dll_mode),THREAD

#IF(%UseIMD=%FALSE)
#INSERT(%SETRuleQExt)
#ENDIF

      #ELSE
!added by qsBusinessRules      
GlobalRule           LONG,EXTERNAL,THREAD
      #ENDIF
    #ELSE                                             #!LIB with global data local
!added by qsBusinessRules
GlobalRule           LONG,THREAD

#IF(%UseIMD=%FALSE)
#INSERT(%SETRuleQ)
#ENDIF

    #ENDIF
  #OF('EXE')
    #IF(%GlobalExternal)                              #!EXE with global data external
      #IF(%ExternalSource='Dynamic Link Library (DLL)')
!added by qsBusinessRules
GlobalRule           LONG,EXTERNAL,DLL(dll_mode),THREAD

#IF(%UseIMD=%FALSE)
#INSERT(%SETRuleQExt)
#ENDIF

      #ELSE
!added by qsBusinessRules
GlobalRule           LONG,EXTERNAL,THREAD
      #ENDIF
    #ELSE                                             #!EXE with global data local
!added by qsBusinessRules
GlobalRule           LONG,THREAD

#IF(%UseIMD=%FALSE)
#INSERT(%SETRuleQ)
#ENDIF

    #ENDIF
  #ENDCASE
 #ENDIF
#ENDAT

#!------------------------------------------------------------------------------------------------------------------------
#AT(%BeginningExports)
 #IF(%qsDisableExternalRules = %False)
  #IF(NOT %GlobalExternal)
$GLOBALRULE                                             @?
  #ENDIF
 #ENDIF
#ENDAT

#!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#!                                Procedure Extension
#!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#EXTENSION(qsExternalRulesProcedure,'Qualitas Solutions''s Check External Rules-Procedure'),REQ(qsExternalRulesGlobal)
#!
#SHEET
 #TAB('General')
    #PROMPT('Error Window Desc:',@S30),%LocalRuleWindowDesc,REQ
    #PROMPT('Check Rules After Open Window',Check),%LocalRuleCheckAfterOpenWindow,DEFAULT(1),AT(10)
    #PROMPT('Check Rules After Fields Change',Check),%LocalRuleCheckAfterFieldsChange,DEFAULT(1),AT(10)
    #PROMPT('Check Rules Before Updating Record',CHECK),%LocalRuleCheckBeforeUpdate,DEFAULT(1),AT(10)
 #ENDTAB    
#ENDSHEET

#!------------------------------------------------------------------------------------------------------------------------
#AT(%CustomGlobalDeclarations)
 #IF(%qsDisableExternalRules = %False)
  #IF(%GlobalErrorIndicatorImage)
      #PROJECT(%GlobalErrorIndicatorImage)
  #ENDIF
  #IF(%GlobalViewRulesHeaderIcon)
      #PROJECT(%GlobalViewRulesHeaderIcon)
  #ENDIF
  #IF(%GlobalViewRulesValidRuleIcon)
      #PROJECT(%GlobalViewRulesValidRuleIcon)
  #ENDIF
  #IF(%GlobalViewRulesBrokenRuleIcon)
      #PROJECT(%GlobalViewRulesBrokenRuleIcon)
  #ENDIF
 #ENDIF
#ENDAT

#!------------------------------------------------------------------------------------------------------------------------
#AT(%LocalDataClassData)
 #IF(%qsDisableExternalRules = %False)
!added by qsBusinessRules
BusinessRulesManager  RulesManager
%RuleBaseName         RulesCollection   !Business Rule Manager for %RuleBaseDescription

LocalWindowQueue         QUEUE,PRE(LWQ)
ControlName                CSTRING(256)
ControlEquate              UNSIGNED
                         END
 #ENDIF
#ENDAT

#!------------------------------------------------------------------------------------------------------------------------
#AT(%WindowManagerMethodcodeSection,'Init','(),BYTE'),PRIORITY(6500)
 #IF(%qsDisableExternalRules = %False)
#Embed(%StartApplyingRules,'Start applying the Rules')
 !added by qsBusinessRules
 #!#IF(Records(RulesQueue) > 1)
 BIND('Action',ThisWindow.Request)
 %RuleBaseName.SetErrorImage('~%GlobalErrorIndicatorImage')
 %RuleBaseName.SetDescription('%LocalRuleWindowDesc')
 #!#EndIF
 #!Use ControlName because that will leave of List boxes ect
 Free(LocalWindowQueue)
 #FOR(%Control)
    #IF(%ControlUse = '')
   LWQ:ControlName = Upper(Sub('%control',2,Len('%control')))
    #Else
   LWQ:ControlName = Upper('%ControlUse')
    #ENDIF
   LWQ:ControlEquate = %Control
   Add(LocalWindowQueue,LWQ:ControlName)
 #ENDFOR

#Embed(%BeforeApplyingRules,'Before applying the Rules')
  !added by qsBusinessRules
  !Run through rules
  #IF(%UseIMD=%FALSE)
  #!NON-IMD
  Loop I# = 1 to Records(RulesQueue)
     Get(RulesQueue,I#)
     If not error()
        !Check if rule's control is on screen
        LWQ:ControlName = Upper(RQ:ControlName)
        Get(LocalWindowQueue,LWQ:ControlName)
        If not error()
#Embed(%CheckProcBeforeApplyingRules,'Check Procedure Before applying the Rules')
           !If rule's control is on screen - add rule
           !Check if rule is not overridden on this procedure
           RPQ:SysIdRule     = RQ:SysIdRule
           RPQ:ProcedureName = Upper('%Procedure')
           Get(RulesProcQueue,RPQ:SysIdRule,RPQ:ProcedureName)
           If error()
              !Procedure is not overridden - add Rule
#Embed(%CheckProcNotOverriddenApplyRules,'Check Procedure Not Overridden Apply the Rules')
              %RuleBaseName.AddRule(Clip(RQ:RuleName),Clip(RQ:RuleDescription),Clip(RQ:RuleExpression),LWQ:ControlEquate,RQ:Offset)
              !Check rule's rule-action-control
              RCQ:SysIdRule = RQ:SysIdRule
              Get(RulesControlQueue,RCQ:SysIdRule)
              If not error()
                 Position# = Pointer(RulesControlQueue) + 1
                 Loop
                    !If rule's rule-action-control is on window - add control to rule
                    LWQ:ControlName = RCQ:ControlName
                    Get(LocalWindowQueue,LWQ:ControlName)
                    If not error()
#Embed(%CheckProcNotOverriddenAddControlToRuleC,'Check Procedure Not Overridden Add Control to Rule - Begin')
                       %RuleBaseName.AddControlToRule(Clip(RQ:RuleName),LWQ:ControlEquate,RCQ:RuleAction)
#Embed(%CheckProcNotOverriddenAddControlToRuleD,'Check Procedure Not Overridden Add Control to Rule - End')                                
                    End
                    Get(RulesControlQueue,Position#)
                    If error() or RCQ:SysIdRule <> RQ:SysIdRule then break.
                    Position# += 1
                 End
              End
           End
        End
     End
  End
  #ELSE
  #!IMD
#Embed(%CheckIfRulesBeforeApplyingRulesB,'Check if Rules Before applying the Rules - Begin')
  !added by qsBusinessRules
  Access:%IMDRules.Open()
  Access:%IMDRules.UseFile()
  %IMDRSysIdRule = 0
  Set(%IMDRULEKEY,%IMDRULEKEY)
  Loop 
     If Access:%IMDRules.Next() <> Level:Benign THEN BREAK.
#Embed(%CheckIfRulesBeforeApplyingRulesC,'Check if Rules Before applying the Rules - Begin')
     !Check if rule's control is on screen
     LWQ:ControlName = Upper(%IMDRControlName)
     Get(LocalWindowQueue,LWQ:ControlName)
     If not error()
#Embed(%CheckProcBeforeApplyingRules,'Check Procedure Before applying the Rules')
        !Found rule control on screen - add rule
        !First Check if rule is not overridden on this procedure
        Access:%IMDOverrideProcPerRule.Open()
        Access:%IMDOverrideProcPerRule.UseFile()
        %IMDOSysIdRule     = %IMDRSysIdRule
        %IMDOProcedureName = Upper('%Procedure')
        If Access:%IMDOverrideProcPerRule.TryFetch(%IMDOverrideProcPerRuleSKey) = Level:Benign
#Embed(%CheckProcControlApplyRulesA,'Check Procedure Control Apply the Rules - Begin')                         
           #IF(%qsEnableUserOverride=%TRUE)
           !Procedure found - check users assigned to Procedure       
           If %IMDOverrideUsers = True 
              Access:%IMDOverrideUserPerProc.Open()
              Access:%IMDOverrideUserPerProc.UseFile()
              %IMDOURSysIdOverrideProcPerRule = %IMDSysIdOverrideProcRule
              %IMDOUserLogonId             = Upper(%qsGlobalUser)
              If Access:%IMDOverrideUserPerProc.TryFetch(%IMDOverrideUserPerProcFSKey) = Level:Benign 
                 !User found - so override rule - don't add the rule
#Embed(%CheckProcControlApplyRulesB,'Check Procedure Control Apply the Rules - Begin')                 
              Else   
                 !User is not overridden - add Rule
#Embed(%CheckProcControlApplyRulesC,'Check Procedure Control Apply the Rules - Begin')                 
                 %RuleBaseName.AddRule(Clip(%IMDRRuleName),Clip(%IMDRRuleDescription),Clip(%IMDRRuleExpression)|
                                       ,LWQ:ControlEquate,%IMDROffSet)
                 !Check rule's rule-action-control
                 Access:%IMqsontrolesPerRule.Open()
                 Access:%IMqsontrolesPerRule.UseFile()
                 %IMqsSysIdRule = %IMDRSysIdRule
                 Set(%IMqsontrolesPerRuleKey,%IMqsontrolesPerRuleKey)
                 Loop
                    If Access:%IMqsontrolesPerRule.Next() <> Level:Benign THEN BREAK.
                    If %IMqsSysIdRule <> %IMDRSysIdRule then break.
                    !If rule's rule-action-control is on window - add control to rule
                    LWQ:ControlName = %IMqsControlName
                    Get(LocalWindowQueue,LWQ:ControlName)
                    If not error()
#Embed(%CheckProcNotOverriddenAdsControlToRuleBI,'Check Procedure Not Overridden Add Control to Rule - Begin')                    
                       %RuleBaseName.AddControlToRule(Clip(%IMDRRuleName),LWQ:ControlEquate,%IMqsRuleAction)
#Embed(%CheckProcNotOverriddenAddControlToRuleEI,'Check Procedure Not Overridden Add Control to Rule - End')                       
                    End
                 End !Loop
#Embed(%CheckProcControlApplyRulesE,'Check Procedure Control Apply the Rules - End')                 
              End
              Access:%IMDOverrideUserPerProc.Close()
           Else
              !Found Procedure and no users assigned - DON'T add a rule
           End
           #ENDIF
        Else
           !Procedure is not overridden - add Rule
#Embed(%CheckProcControlApplyRulesB1,'Check Procedure Control Apply the Rules - Begin')
           %RuleBaseName.AddRule(Clip(%IMDRRuleName),Clip(%IMDRRuleDescription),Clip(%IMDRRuleExpression)|
                                 ,LWQ:ControlEquate,%IMDROffSet)
           !Check rule's rule-action-control
           Access:%IMqsontrolesPerRule.Open()
           Access:%IMqsontrolesPerRule.UseFile()
           %IMqsSysIdRule = %IMDRSysIdRule
           Set(%IMqsontrolesPerRuleKey,%IMqsontrolesPerRuleKey)
           Loop
              If Access:%IMqsontrolesPerRule.Next() <> Level:Benign THEN BREAK.
              If %IMqsSysIdRule <> %IMDRSysIdRule then break.

              !If rule's rule-action-control is on window - add control to rule
              LWQ:ControlName = %IMqsControlName
              Get(LocalWindowQueue,LWQ:ControlName)
              If not error()
#Embed(%CheckProcNotOverriddenAddControlToRuleBI,'Check Procedure Not Overridden Add Control to Rule - Begin')
                 %RuleBaseName.AddControlToRule(Clip(%IMDRRuleName),LWQ:ControlEquate,%IMqsRuleAction)
#Embed(%CheckProcNotOverriddenAddControlToRuleEI,'Check Procedure Not Overridden Add Control to Rule - End')                 
              End
           End !Loop
#Embed(%CheckProcControlApplyRulesE1,'Check Procedure Control Apply the Rules - End')           
        End
     End
  End
  Access:%IMqsontrolesPerRule.Close()
  Access:%IMDOverrideProcPerRule.Close()
  Access:%IMDRules.Close()
#Embed(%CheckIfRulesBeforeApplyingRulesE,'Check if Rules Before applying the Rules - End')
  #END
#Embed(%AfterApplyingRules,'After applying the Rules')
%RuleBaseName.SetEnumerateIcons('~%GlobalViewRulesHeaderIcon','~%GlobalViewRulesValidRuleIcon','~%GlobalViewRulesBrokenRuleIcon')
BusinessRulesManager.AddRulesCollection(%RuleBaseName)
BusinessRulesManager.SetEnumerateIcons('~%GlobalViewRulesHeaderIcon','~%GlobalViewRulesValidRuleIcon','~%GlobalViewRulesBrokenRuleIcon')
BusinessRulesManager.SetGlobalRuleReferences(GlobalRule)
#Embed(%FinishedApplyingRules,'Finished with applying the Rules')
 #ENDIF
#ENDAT

#!------------------------------------------------------------------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(9500)
 #IF(%qsDisableExternalRules = %False)
#IF(%LocalRuleCheckAfterOpenWindow)
  #!#IF(ITEMS(%AUXLRules))
!added by qsBusinessRules
BusinessRulesManager.CheckAllRules(1)                    #<!Checking all Rules in RulesManager
  #!#ENDIF
#ENDIF
 #ENDIF
#ENDAT

#!------------------------------------------------------------------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'TakeNewSelection','(),BYTE'),PRIORITY(6900)
 #IF(%qsDisableExternalRules = %False)
#IF(%LocalRuleCheckAfterFieldsChange)
  #!#IF(ITEMS(%AUXLRules))
  !added by qsBusinessRules
  IF FIELD()
     UPDATE
     BusinessRulesManager.CheckAllRules(True)                 #<!Checking all Rules in RulesManager
     DISPLAY
     !ThisMakeOver.Refresh(MO:WinType)
  END
  #!#ENDIF
#ENDIF
 #ENDIF
#ENDAT

#!------------------------------------------------------------------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'TakeAccepted','(),BYTE'),PRIORITY(6400)
 #IF(%qsDisableExternalRules = %False)
#IF(%LocalRuleCheckAfterFieldsChange)
  #!#IF(ITEMS(%AUXLRules))
  !added by qsBusinessRules
  IF ACCEPTED()
     UPDATE
     BusinessRulesManager.CheckAllRules(True)                 #<!Checking all Rules in RulesManager
     DISPLAY
     !ThisMakeOver.Refresh(MO:WinType)
  END
  #!#ENDIF
#ENDIF
 #ENDIF
#ENDAT

#!------------------------------------------------------------------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'TakeAccepted','(),BYTE'),PRIORITY(2250)
 #IF(%qsDisableExternalRules = %False)
#!#IF(ITEMS(%AUXLRules))
!added by qsBusinessRules
BusinessRulesManager.TakeAccepted(Accepted())                                                      #<!RulesManager traps to determine if error-indicator was clicked
#!#ENDIF
 #ENDIF
#ENDAT

#!------------------------------------------------------------------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'TakeCompleted'),PRIORITY(2800)
 #IF(%qsDisableExternalRules = %False)
  #IF(%LocalRuleCheckBeforeUpdate=%True)
!added by qsBusinessRules
IF BusinessRulesManager.CheckAllRules(0)
  RETURN Level:Notify
END
  #ENDIF
 #ENDIF
#ENDAT

#!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#!                                Code Extension
#!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Code(qsExternalRulesCodeReadRules,'Qualitas Solutions''s Read Rules into Global Queues/IMDD-Code'),REQ(qsExternalRulesGlobal)
#!
#SHEET
 #TAB('General')
    #PROMPT('Open and Close Rule Files',Check),%LocalOpenCloseRuleFiles,AT(10),DEFAULT(1)
 #ENDTAB   
#ENDSHEET

 #IF(%qsDisableExternalRules = %False)
   #IF(%LocalOpenCloseRuleFiles = %True)
 !added by qsBusinessRules
 Access:%Rules.Open()
 Access:%Rules.UseFile()
   #ENDIF
   #IF(%UseIMD=%FALSE)
   !#NON-IMD
 #! Rules Queue
 #!-----------------------------------------------------------------------------
 Free(RulesQueue)
 %RSysIdRule = 0
 Set(%RULEKEY,%RULEKEY)
 Loop
   If Access:%Rules.Next() <> Level:Benign THEN BREAK.
   If RQ:RuleActive = False then Cycle.
   
   RQ:SysIdRule       = %RSysIdRule
   RQ:RuleName        = %RRuleName
   RQ:RuleDescription = Clip(%RRuleDescription)
   RQ:RuleExpression  = Clip(%RRuleExpression)
   RQ:ControlName     = Clip(%RControlName)
   If Sub(RQ:ControlName,1,1) = '?'
      RQ:ControlName = Sub(RQ:ControlName,2,Len(Clip(RQ:ControlName)))
   End
   RQ:Offset = Clip(%ROffSet)
   Add(RulesQueue,RQ:ControlName)
   If error() then stop(error()).
 End
   #ELSE
   !#IMD
 #!IMD - Rules File
 #!-----------------------------------------------------------------------------
 Access:%IMDRules.Open()
 Access:%IMDRules.UseFile()
#Embed(%LoadRulesInMemoryTableA,'Load Rules In Memory Table - Begin')
 %RSysIdRule = 0
 Set(%RULEKEY,%RULEKEY)
 Loop
   If Access:%Rules.Next() <> Level:Benign THEN BREAK.
#Embed(%LoadRulesInMemoryTableB,'Load Rules In Memory Table')
   If %RRuleActive = False then Cycle.
   
   %IMDRSysIdRule       = %RSysIdRule
   %IMDRRuleName        = %RRuleName
   %IMDRRuleDescription = Clip(%RRuleDescription)
   %IMDRRuleExpression  = Clip(%RRuleExpression)
   %IMDRControlName     = Clip(%RControlName)
   If Sub(%IMDRControlName,1,1) = '?'
      %IMDRControlName = Sub(%IMDRControlName,2,Len(Clip(%IMDRControlName)))
   End
   %IMDROffSet = Clip(%ROffSet)
#Embed(%LoadRulesInMemoryTableBeforeAdd,'Load Rules In Memory Table - BeforeAdd')
   If Access:%IMDRules.Insert() <> Level:Benign then stop(error()).
 End
#Embed(%LoadRulesinMemoryTableC,'Load Rules In Memory Table - End')
 Access:%IMDRules.Close()
   #ENDIF
   #IF(%LocalOpenCloseRuleFiles = %True)
 Access:%Rules.Close()
 
 Access:%ControlesPerRule.Open()
 Access:%ControlesPerRule.UseFile()
   #ENDIF
#!-----------------------------------------------------------------------------
#!Rules Controls Queue
   #IF(%UseIMD=%FALSE)
   !#NON-IMD
 Free(RulesControlQueue)
 %CSysIdRule            = 0
 %CSysIqsontrolsPerRule = 0
 Set(%ControlesPerRuleKey,%ControlesPerRuleKey)
 Loop
   If Access:%ControlesPerRule.Next() <> Level:Benign THEN BREAK.
   RCQ:SysIdRule   = %CSysIdRule
   RCQ:ControlName = Clip(%CControlName)
   If Sub(RCQ:ControlName,1,1) = '?'
      RCQ:ControlName = Sub(RCQ:ControlName,2,Len(Clip(RCQ:ControlName)))
   End
   RCQ:RuleAction = Clip(%CRuleAction)
   Add(RulesControlQueue,RCQ:SysIdRule)
   If error() then stop(error()).
 End
   #ELSE
   !#IMD
 #!IMD Rules Controls
 #!-----------------------------------------------------------------------------
 Access:%IMqsontrolesPerRule.Open()
 Access:%IMqsontrolesPerRule.UseFile()
#Embed(%LoaqsontrolsPerRuleInMemoryTableB,'Load Controls Per Rule In Memory Table - Begin')
 %CSysIdRule            = 0
 %CSysIqsontrolsPerRule = 0
 Set(%ControlesPerRuleKey,%ControlesPerRuleKey)
 Loop
   If Access:%ControlesPerRule.Next() <> Level:Benign THEN BREAK.
#Embed(%LoadRulesInMemoryTableD,'Load Rules In Memory Table')
   %IMqsSysIdRule   = %CSysIdRule
   %IMqsControlName = Clip(%CControlName)
   If Sub(%IMqsControlName,1,1) = '?'
      %IMqsControlName = Sub(%IMqsControlName,2,Len(Clip(%IMqsControlName)))
   End
   %IMqsRuleAction = Clip(%CRuleAction)
   If Access:%IMqsontrolesPerRule.Insert() <> Level:Benign then stop(error()).
 End
#Embed(%LoadControlsPerRuleInMemoryTableE,'Load Controls Per Rule In Memory Table - End')
 Access:%IMqsontrolesPerRule.Close()
   #ENDIF
   #IF(%LocalOpenCloseRuleFiles = %True)
 Access:%ControlesPerRule.Close()

 Access:%OverrideProcPerRule.Open()
 Access:%OverrideProcPerRule.UseFile()
   #ENDIF
#!-----------------------------------------------------------------------------
#! Rules - Procedure Queue
   #IF(%UseIMD=%FALSE)
   !#NON-IMD
 Free(RulesProcQueue)
 %OSysIdRule             = 0
 %OSysIDOverrideProcRule = 0
 Set(%OverrideProcPerRuleFKey,%OverrideProcPerRuleFKey)
 Loop
   If Access:%OverrideProcPerRule.Next() <> Level:Benign THEN BREAK.
   RPQ:SysIdRule     = %OSysIdRule
   RPQ:ProcedureName = Clip(%OProcedureName)
   Add(RulesProcQueue,RPQ:SysIdRule,RPQ:ProcedureName)
   If error() then stop(error()).
 End
   #ELSE
   !#IMD
 #!IMD Rules Procedures
 #!-----------------------------------------------------------------------------
 Access:%IMDOverrideProcPerRule.Open()
 Access:%IMDOverrideProcPerRule.UseFile()
#Embed(%LoadOverrideProcPerRuleInMemoryTableB,'Load Override Procedure Per Rule In Memory Table - Begin')
 %OSysIdRule             = 0
 %OSysIDOverrideProcRule = 0
 Set(%OverrideProcPerRuleFKey,%OverrideProcPerRuleFKey)
 Loop
   If Access:%OverrideProcPerRule.Next() <> Level:Benign THEN BREAK.
#Embed(%LoadRulesInMemoryTableE,'Load Rules In Memory Table')
   %IMDOSysIdRule     = %OSysIdRule
   %IMDOProcedureName = Clip(%OProcedureName)
   %IMDOverrideUsers  = %OOverrideUsers
   %IMDSysIdOverrideProcRule = %OSysIDOverrideProcRule
   If Access:%IMDOverrideProcPerRule.Insert() <> Level:Benign then stop(error()).
 End
#Embed(%LoadOverrideProcPerRuleInMemoryTableE,'Load Override Procedure Per Rule In Memory Table - End')
 Access:%IMDOverrideProcPerRule.Close()
   #ENDIF
   #IF(%LocalOpenCloseRuleFiles = %True)
 Access:%OverrideProcPerRule.Close()
   #ENDIF
   #IF(%UseIMD=%TRUE and %qsEnableUserOverride=%True)
   !#IMD
 Access:%OverrideUserPerProc.Open()
 Access:%OverrideUserPerProc.UseFile()
 #!-----------------------------------------------------------------------------
 #!IMD Rules Override Users per Procedures   
 Access:%IMDOverrideUserPerProc.Open()
 Access:%IMDOverrideUserPerProc.UseFile()
#Embed(%LoadOverrideUserPerProcInMemoryTableB,'Load Override user Per Procedure In Memory Table - Begin')
 %OURSysIdOverrideProcRule = 0
 %OSysIDOverrideUserProc = 0
 Set(%OverrideUserPerProcFKey,%OverrideUserPerProcFKey)
 Loop
   If Access:%OverrideUserPerProc.Next() <> Level:Benign THEN BREAK.
 #Embed(%LoadRulesInMemoryTableF,'Load Rules In Memory Table')
   %IMDOURSysIdOverrideProcPerRule = %OURSysIdOverrideProcRule
   %IMDOUserLogonID             = %OUserLogonID
   If Access:%IMDOverrideUserPerProc.Insert() <> Level:benign then stop(error()).
 End
#Embed(%LoadOverrideUserPerProcInMemoryTableE,'Load Override Procedure Per Rule In Memory Table - End')
 Access:%IMDOverrideUserPerProc.Close()
     #IF(%LocalOpenCloseRuleFiles = %True)
 Access:%OverrideUserPerproc.Close()
     #ENDIF
   #ENDIF
 #ENDIF

#!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#!                                Code Extension
#!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Code(qsExternalRulesCodeResetRules,'Qualitas Solutions''s Reset Rules into Global Queues/IMDD-Code')
#!
 #IF(%qsDisableExternalRules = %False)
   #IF(%UseIMD=%FALSE)
   !#NON-IMD
 !added by qsBusinessRules
 Free(RulesQueue)
 %RSysIdRule = 0
 Set(%RULEKEY,%RULEKEY)
 Loop
   If Access:%Rules.Next() <> Level:Benign THEN BREAK.
   If RQ.RuleActive = False then Cycle.
   RQ:SysIdRule       = %RSysIdRule
   RQ:RuleName        = %RRuleName
   RQ:RuleDescription = Clip(%RRuleDescription)
   RQ:RuleExpression  = Clip(%RRuleExpression)
   RQ:ControlName     = Clip(%RControlName)
   If Sub(RQ:ControlName,1,1) = '?'
      RQ:ControlName = Sub(RQ:ControlName,2,Len(Clip(RQ:ControlName)))
   End
   RQ:Offset = Clip(%ROffSet)
   Add(RulesQueue,RQ:ControlName)
   If error() then stop(error()).
 End
   #ELSE
   !#IMD
 !added by qsBusinessRules
 Access:%IMDRules.Open()
 Access:%IMDRules.UseFile()
 %IMDRSysIdRule = 0
 Set(%IMDRULEKEY,%IMDRULEKEY)
 Loop Until Access:%IMDRules.Next()
      If Not(Error())
         Delete(%IMDRules)
      End
 End
   #ENDIF
 #ENDIF
#!------------------------------------------------------------------------------------------------------------------------
#GROUP(%SETCC)
  #BOXED (''),Section,AT(80,5,110,80)
     #DISPLAY ('Qualitas Solutions''s External'),prop(PROP:FontStyle,700),prop(PROP:FontName,%AppTPLFontName)
     #DISPLAY ('Business Rules'),prop(PROP:FontStyle,700),prop(PROP:FontName,%AppTPLFontName)
     #DISPLAY ('Version 1.03'),prop(PROP:FontStyle,700),prop(PROP:FontName,%AppTPLFontName)
     #DISPLAY ('Copyright © 2011 by'),prop(PROP:FontName,%AppTPLFontName)
     #DISPLAY ('Qualitas Solution Pty Ltd'),prop(PROP:FontName,%AppTPLFontName)
     #DISPLAY ('www.qualitas-solutions.co.za'),prop(PROP:FontName,%AppTPLFontName)
  #ENDBOXED

#!------------------------------------------------------------------------------------------------------------------------
#GROUP(%SETAPP)
#BOXED('Qualitas Solutions External Rules Templates'),SECTION,AT(,20,,35)
  #DISPLAY ('Define Business Rules in an external file')
  #DISPLAY ('Rules are applied at runtime')
#ENDBOXED

#!------------------------------------------------------------------------------------------------------------------------
#GROUP(%SETRuleQ)
!Added by qsExternalRules
RulesQueue           QUEUE,PRE(RQ)
SysIdRule              LONG
RuleName               STRING(255)
RuleDescription        STRING(255)
RuleExpression         STRING(255)
ControlName            STRING(255)
Offset                 LONG
                     END
RulesControlQueue    QUEUE,PRE(RCQ)
SysIdRule              LONG
ControlName            STRING(255)
RuleAction             BYTE
                     END
RulesProcQueue       QUEUE,PRE(RPQ)
SysIdRule              LONG
ProcedureName          STRING(255)
                     END

#!------------------------------------------------------------------------------------------------------------------------
#GROUP(%SETRuleQExt)
!Added by qsExternalRules
RulesQueue           QUEUE,PRE(RQ),EXTERNAL,DLL(dll_mode)
SysIdRule              LONG
RuleDescription        STRING(255)
RuleExpression         STRING(255)
ControlName            STRING(255)
Offset                 LONG
                     END
RulesControlQueue    QUEUE,PRE(RCQ),EXTERNAL,DLL(dll_mode)
SysIdRule              LONG
ControlName            STRING(255)
RuleAction             BYTE
                     END
RulesProcQueue       QUEUE,PRE(RPQ),EXTERNAL,DLL(dll_mode)
SysIdRule              LONG
ProcedureName          STRING(255)
                     END

!-----------------------------------------------------------------------------------------------------
#GROUP (%StripExclamation,*%zField)
#!-----------------------------------------------------------------------------------------------------
  #IF(SUB(%zField,1,1)='!')
    #SET(%zField,SUB(%zField,2,LEN(%zField)-1))
  #ENDIF
