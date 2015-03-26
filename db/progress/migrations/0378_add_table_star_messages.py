from gearbox.migrations import Migration

class Addmessages(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('messages', area='Sta_Data_256',
                       label='Messages')
        t.column('messageType', 'character', format='X(10)', initial='Star',
                 label='Type',
                 help='Message Type')
        t.column('filename', 'character', format='X(50)', initial='',
                 label='Filename',
                 help='Filename with directoryname.Blank.. Universal numbers')
        t.column('messageID', 'integer', format='>>>>>9', initial='0',
                 label='MessageID',
                 help='Message number.')
        t.column('messageText', 'character', format='X(50)', initial='',
                 label='Text',
                 help='Message text. Show\'s on beginin os message and logs')
        t.column('messageDescription', 'character', format='X(256)', initial='',
                 label='Description',
                 help='Message description. Shows only on end on message')
        t.column('viewAsType', 'character', format='X(15)', initial='MESSAGE',
                 label='VIEW-AS',
                 help='View-as type')
        t.column('buttonsType', 'character', format='X(15)', initial='OK',
                 label='Buttons',
                 help='Buttons type')
        t.column('logFile', 'logical', initial='no',
                 label='LogFile',
                 help='Does this message goes to logfile', view_as='VIEW-AS TOGGLE-BOX')
        t.index('filename', ['filename', 'messageType', 'messageID'], area='Sta_Index_2',
                unique=True)
        t.index('messageId', ['messageType', 'filename', 'messageID'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('MessageText', ['messageText'], area='Sta_Index_2')

    def down(self):
        self.drop_table('messages')

