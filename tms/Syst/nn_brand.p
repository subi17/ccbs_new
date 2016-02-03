/* ---------------------------------------------------------------------
  MODULE .......: NN_brand
  FUNCTION .....: MAIN MODULE
  SOVELLUTUS ...: NN Tele1
  AUTHOR .......: PT / KL
  CREATED ......: 19.05.98
  MODIFIED .....: 11.08.98 pt  INFO mode
                  19.08.98 pt  run nninfo when in info mode
                  04.02.99 kl  common version
                  28.04.99 pt  user rights are checked
                  16.06.99 pt  TmsUser.StartMenu -> default start menu
                  03.09.99 kl  default userid from unix
                  20.11.01 pt  common var ergo-kbd
                  13.05.02 aam called from brand.p
                  01.09.03 jp   TMS+
                  03.03.05 kl return correctly
                  03.08.06 tk set qupd = true when running from find module

  VERSION ......: M15
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/tmsparam2.i}
{Mc/lib/tokenlib.i}

def new shared var siirto as char.
def new shared var order as int.

def var f_level like MenuTree.Level               no-undo.
def var f_xlevel like f_level                     no-undo.
def var s_level  like f_level                     no-undo  init "1".
def var item_type   as int  format "ZZ9"   extent 8  no-undo.
def var f_name   as char                extent 8  no-undo.
def var f_title  as char                extent 8  no-undo.
def var f_id     as char                extent 8  no-undo.
def var vartype  as i                   extent 8  no-undo.
def var first_time as log init true               no-undo.
def var firstc   as log.
def var ftitle   as char format "x(75)" extent 23 no-undo.
def var title_width     as int                    no-undo.
def var mlevel   as int                           no-undo.
def var i        as int                           no-undo.
def var x        as int                           no-undo.
def var empty    as char                          no-undo.

def var info     as lo                            no-undo.
def var OK       as log format "Yes/No".
def var ekaker2  as log init true no-undo.
def var f_code as char format "x(12)" no-undo.
def var pcolor   as char no-undo.
def var pclear   as char no-undo.

form
   skip(17)
   with width 80 row 1 col 1 color value(pclear)
   frame menu_frame.

form  /* menu title stack */
   ftitle[1]  skip ftitle[2]  skip ftitle[3]  skip ftitle[4]  skip ftitle[ 5] skip
   ftitle[6]  skip ftitle[7]  skip ftitle[8]  skip ftitle[9]  skip ftitle[10] skip
   ftitle[11] skip ftitle[12] skip ftitle[13] skip ftitle[14]
with                
  no-label no-box overlay row 1 centered color value(pclear) frame stack.

form /* empty row prior to function keys */
   empty format "x(80)"
   with no-label no-box row 19 overlay frame empty.

form /* function codes under function key labels */
   f_id[1] format "x(8)"
   f_id[2] format "x(8)"
   f_id[3] format "x(8)"
   f_id[4] format "x(8)"
   space(9)
   f_id[5] format "x(8)"
   f_id[6] format "x(8)"
   f_id[7] format "x(8)"
   f_id[8] format "x(8)"

with no-label no-box overlay row 22 frame f_code.

form /* function codes under function key labels ERGO*/
   f_id[1] format "x(8)"
   f_id[2] format "x(8)"
   f_id[3] format "x(8)"
   space(4)
   f_id[4] format "x(8)"
   f_id[5] format "x(8)"
   SPACE(4)
   f_id[6] format "x(8)"
   f_id[7] format "x(8)"
   SPACE(4)
   f_id[8] format "x(8)"

with no-label no-box overlay row 22 frame f_code-ergo.



form
  f_code label "Function (code or name)" format "x(30)"
  help "Give the name or code of the function. Press Enter/F1/F2"
  with overlay centered row 9 frame f_code_search.


find first Company no-lock WHERE
           Company.Brand = gcBrand NO-ERROR.
ASSIGN
   empty = fill(" ",78)
   pvm   = TODAY
   ynimi = IF AVAILABLE Company THEN Company.CompName ELSE "Unknown"
   yvari = FALSE.


/* start from main level */
if not ymoni then multiuser = true.
else multiuser = false.
f_level = s_level.


pause 0.
cfc = "sel". run ufcolor. pcolor = ctc. pclear = cfc.
view frame menu_frame.

mlevel = 1.

MAIN_LOOP:
repeat with frame menu_frame:

   /* set main title bar */
   find first MenuTree where  Level = "0" and Position = 1 no-lock no-error.

   if available MenuTree then do:
      assign
      ftitle[1] = " " + ynimi + " (" + gcBrand + 
                       ") " + MenuTitle + " " + string(pvm,"99-99-99")
      substring(ftitle[1],59) = string("(" + MenuId + ")","x(10)")
      title_width = length(ftitle[1]).

      do with frame stack:
         pause 0.
         display ftitle[1] .
         color display value(pcolor) ftitle[1].
      end.   
   end.

   else do:
      bell.
      message "Menu tree is not defined !".
      pause 2 no-message.
      return.
   end.

   pause 0 no-message.
   view.
   assign x = 40 - (title_width / 2).
   /* menun otsikot */
   do with frame stack:
      pause 0.
      display ftitle[mlevel].
      color display value(pcolor) ftitle[mlevel].
   end.

   if first_time then do with frame login:

     assign first_time = false si-pvm = pvm.

     find TmsUser where TmsUser.UserCode = katun no-lock.

     /* set user's default menu level */
     if TmsUser.StartMenu ne "" then do:
            FIND MenuTree WHERE 
                 MenuTree.MenuId  = TmsUser.StartMenu AND
                 MenuTree.MenuType = 2 /* menu, not  some module */
            NO-LOCK NO-ERROR.
            IF AVAIL MenuTree THEN 
               ASSIGN s_level = MenuTree.Level + string(MenuTree.Position).
            ELSE DO:
               message 
               "User record refers to unknown or invalid" skip
               "Function Code '" + TmsUser.StartMenu + "' !"           skip(1)
               "Default menu  settings are used"
                VIEW-AS ALERT-BOX error 
                TITLE " Users Start Menu is Invalid ".
                s_level = "1".
            END.   
     END.
     Else s_level = "1".

     f_level =  s_level.

   end.


   /* search all menu items on this level */

   assign item_type = 0 f_name = "" ufk = 0 ehto = 0 f_id = "" vartype = 0.
   for each  MenuTree no-lock where
             MenuTree.Level = f_level and
             MenuTree.State[1] = false  /* NOT denied */:

            IF  MenuType NE 3 AND
               (Menutree.tokencode = '' or
                getTMSRight(MenuTree.tokencode) = '')
            then next.


      ASSIGN
      ufk    [MenuTree.Position]  = MenuNum       /* NO. of ufkey menu text */
      f_name [MenuTree.Position]  = Module      /* name of module        */
      item_type [MenuTree.Position]  = MenuType  /* Type of Item  
                                                     module/menu/return */
      f_title[MenuTree.Position]  = MenuTitle     /* Title for a submenu */
      f_id   [MenuTree.Position]  = MenuId        /* Function code  */.
   end.

f_code:
   repeat with frame menu_frame:
      /* wait for the user's choice */
      assign ufk[9] = 1.


      /* show function codes */
      pause 0.
      IF ergo-kbd THEN display f_id with frame f_code-ERGO.
                  ELSE display f_id with frame f_code.
      ehto = 3. run ufkey.
      info = false. 
ACTION: repeat:
         readkey. nap = keylabel(lastkey).

         if keylabel(lastkey) = "?" then do:
            info = true.
            disp "INFO MODE: PRESS F1 ... F7 FOR DESCRIPTION" 
            with overlay frame imode centered row 10 
            color messages.
            next.
         end.   

         /* brand selection */
         if nap = "f11" then leave main_loop. 

         toimi = minimum(lookup(nap,"1,2,3,4,5,6,7,8,return"),9).
         if toimi = 0 then
         toimi = lookup(nap,"f1,f2,f3,f4,f5,f6,f7,f8,enter").
         pause 0 no-message.

         /* hide function codes from screen */
         hide frame f_code      no-pause.
         hide frame f_code-ERGO no-pause.


         if toimi > 0 or length(nap) = 1 then leave.
      end.

      if info then do trans:
         if toimi < 8 then do:

            hide message no-pause.
            run nninfo(f_id[toimi]).
            hide frame info  no-pause.
         end.
         info = false.
         next.
      end.

      /* in case no function key was hit .. */
      if toimi = 0 then do on endkey undo, next f_code:
         if length(nap) > 1 then undo, retry.
         assign f_code = nap firstc = true.
         pause 0 no-message.
         assign ufk = 0  ufk[1] = 35 ufk[2] = 717 ufk[8] = 8
         ehto = 3. run ufkey.
         update f_code with frame f_code_search editing:

            if firstc then do:
               apply keycode("home").
               apply keycode("cursor-right").
               firstc = false.
            end.

            readkey. nap=keylabel(lastkey).

            if nap = "f8" then do:
               hide frame f_code_search no-pause.
               undo f_code, next MAIN_LOOP.
            end.

            order = lookup(nap,"f1,f2").
            if order > 0 then do:
               assign siirto = input f_code.
               run utose.p.
               if siirto = "" then next.

               disp siirto @ f_code with frame f_code_search.
               leave.
            end.
            apply lastkey.
         end.

         hide frame f_code_search no-pause.
         if f_code = "" then next MAIN_LOOP.

         find first MenuTree where 
                    MenuTree.MenuId = f_code 
         no-lock no-error.

         if not avail MenuTree then do:
            bell.
            message "Function '" + f_code + "' is not available !".
            pause 2 no-message.
            next MAIN_LOOP.
         end.

         IF getTMSRight(MenuTree.tokencode) = '' THEN DO:
            BELL.
            message "No permission to run function '" + f_code + "' !".
            pause 2 no-message.
            NEXT MAIN_LOOP.
         END.

         /* shall a module be run ? */
         if MenuType = 1 then do:
            if search(MenuTree.Module + ".r") = ? then do:
               bell.
               message "Function is not available !".
               pause 2 no-message.
            end.
            else do:

               assign
               qcode  = MenuTree.MenuId
               qtitle = MenuTree.MenuTitle
               qupd   = TRUE.

               run value(MenuTree.Module).

               /* clear screen */
               hide frame menu_frame no-pause.
               hide message no-pause.
               next MAIN_LOOP.
            end.
         end.
         else if MenuType = 2 then do:
            /* Open a submenu */
            assign
            mlevel = mlevel + 1
            i = (title_width / 2) - (length(MenuTitle) / 2)
            substring(ftitle[mlevel],i) = MenuTitle
            substring(ftitle[mlevel],59) = string("(" + MenuId + ")","x(10)")
            title_width = length(ftitle[mlevel])
            f_xlevel = f_level /* later we return here */
            f_level = Level + string(Position).
            if f_code = "-" then f_level = "1".

            next MAIN_LOOP.
         end.

         next f_code.
      end.


      if toimi = 9 then do: /* Quick return to main level */
         clear frame stack no-pause.
         assign mlevel = 1 f_level = s_level.
         next MAIN_LOOP.
      end.

      if item_type[toimi] = 3 then do: /* one level backwards */

         if mlevel = 1 then do:
            OK = false.
            message " USER <" + katun + 
                    ">: Are You sure you want to log out?" 
            VIEW-AS ALERT-BOX
            BUTTONS YES-NO 
            TITLE " CONFIRM LOGOUT "
            update OK.

            if OK then do:
               RETURN "LEAVE". /* ohjelman lopetus */
            end.
         end.
         else do:

            if f_xlevel = "" then 
            f_level = substring(f_level,1,length(f_level) - 1).
            else assign f_level = f_xlevel f_xlevel = "".

            if f_level = "" then f_level = s_level.
            /* remove downmost title row from stack */
            color display value (pclear) ftitle[mlevel] with frame stack.
            display "" @ ftitle[mlevel] with frame stack.
            assign mlevel = maximum(1,mlevel - 1).
            next MAIN_LOOP.
         end.
      end.
      else if item_type[toimi] = 2 then do: /* move into submenu */
         /* save title of next menu level */
         assign
         mlevel = mlevel + 1
         ftitle[mlevel] = f_title[toimi]
         title_width = maximum(title_width,length(ftitle[mlevel])).

         /* level key dor db search */
         f_level = f_level + string(toimi).

         /* align title width according to the longest title */
         do i = 1 to mlevel:
            x = title_width - length(ftitle[i]).
            if x > 0 then assign
            x = x / 2
            ftitle[i] = substring(substring(empty,1,x) +
            ftitle[i] + substring(empty,1,x),1,title_width).
         end.
         substring(ftitle[mlevel],59) = string("(" + f_id[toimi] + ")","x(10)").
         next MAIN_LOOP.
      end.

      else do: /* run a program module */
         if search(f_name[toimi] + ".r") = ? then do:
            bell.
            message "Function is not available !".
            pause 2 no-message.
         end.
         else do:
            assign
               qcode  =  f_id   [toimi]
               qtitle =  f_title[toimi].
               qupd   =  TRUE . 

            run value(f_name[toimi]).
            /* clear screen */
            hide frame menu_frame no-pause.

            pause 0. display empty with frame empty. pause 0.
         end.
         next MAIN_LOOP.
      end.

   end. /* f_code */

end. /* MAIN_LOOP */

/* brand selection */
IF nap = "f11" THEN DO:
   /* choose a new brand */
   RETURN. 
END.

quit.
