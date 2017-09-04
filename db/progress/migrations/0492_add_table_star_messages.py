from gearbox.migrations import Migration

class AddTablemessages(Migration):

    database = "star"

    def up(self):
        t = self.table('messages', area="Sta_Data_256", label="Messages", dump_name="messages")
        t.column('messageID', 'integer', format=">>>>>9", initial="0", max_width=4, label="MessageID", position=2, order=30, help="Message number.")
        t.column('filename', 'character', format="X(50)", initial="", max_width=100, label="Filename", position=3, order=20, help="Filename with directoryname.Blank.. Universal numbers")
        t.column('messageText', 'character', format="X(50)", initial="", max_width=100, label="Text", position=4, order=40, help="Message text. Show's on beginin os message and logs")
        t.column('messageDescription', 'character', format="X(256)", initial="", max_width=512, label="Description", position=5, order=50, help="Message description. Shows only on end on message")
        t.column('viewAsType', 'character', format="X(15)", initial="MESSAGE", max_width=30, label="VIEW-AS", position=6, order=60, help="View-as type")
        t.column('buttonsType', 'character', format="X(15)", initial="OK", max_width=30, label="Buttons", position=7, order=70, help="Buttons type")
        t.column('logFile', 'logical', format="yes/no", initial="no", max_width=1, label="LogFile", view_as="VIEW-AS TOGGLE-BOX", position=8, order=80, help="Does this message goes to logfile")
        t.column('messageType', 'character', format="X(10)", initial="Star", max_width=20, label="Type", position=9, order=10, help="Message Type")
        t.index('messageId', [['messageType'], ['filename'], ['messageID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('filename', [['filename'], ['messageType'], ['messageID']], area="Sta_Index_2", unique=True)
        t.index('MessageText', [['messageText']], area="Sta_Index_2")

    def down(self):
        self.drop_table('messages')
