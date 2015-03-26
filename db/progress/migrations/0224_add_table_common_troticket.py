from gearbox.migrations import Migration

class AddTroTicket(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TroTicket', area='Sta_Data_256',
                       label='Trouble Tickets',
                       dump_name='troticke',
                       desc='Trouble Tickets')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('CreStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Date and time when trouble ticket was created',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('CreUser', 'character', initial='',
                 label='C-user',
                 column_label='C-user',
                 help='User who created this trouble ticket')
        t.column('TTType', 'character', initial='',
                 label='Type',
                 column_label='Type',
                 help='Code of trouble type')
        t.column('Handler', 'character', initial='',
                 label='Responsible Person',
                 column_label='Responsible Person',
                 help='Code of the responsible person',
                 description='Responsible Person')
        t.column('TTStatus', 'character', mandatory=True, initial='',
                 label='Status',
                 column_label='Status',
                 help='Code of trouble status')
        t.column('CLI', 'character', format='X(16)', initial='',
                 label='A-Sub No./MSISDN No',
                 column_label='A-Sub No./MSISDN No',
                 help='Customer\'s A-sub Number with Area Code/MSISDN Subscriber No',
                 description='A-Sub No./MSISDN No')
        t.column('Contact', 'character', format='x(30)', initial='',
                 label='Contact Person',
                 column_label='Contact Person',
                 help='Customer\'s contact person name')
        t.column('SolvedStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Solved',
                 column_label='Solved',
                 help='Date and time when trouble  was solved',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('Solver', 'character', initial='',
                 label='Solved By',
                 column_label='Solved By',
                 help='Person who rectified the problem.',
                 description='Person who rectified the problem.')
        t.column('TTNum', 'integer', format='>,>>>,>>9', initial='0',
                 label='Reference',
                 column_label='Reference',
                 help='Reference no. for Customer',
                 description='Auto-generated sequence number for Trouble Ticket')
        t.column('CustType', 'logical', format='Mobile/Fixed', initial='Mobile',
                 label='Customer Type',
                 help='Mobile or Fixed Telephone problem?')
        t.column('TargetStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Target',
                 column_label='Created',
                 help='Date and time of target time for Trouble ticket to be solved',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('EMail', 'character', format='X(30)', initial='',
                 label='Email',
                 column_label='Email',
                 help='Email id comtact of person in case of any trouble.')
        t.column('MobileNbr', 'character', format='X(20)', initial='',
                 label='GSM No.',
                 column_label='GSM No.',
                 help='Mobile No, of person to deliver SMS messages')
        t.column('Memo', 'character', format='x(60)', initial='',
                 column_label='Memo',
                 help='Explanation field for trouble ticket')
        t.index('CustNum', ['CustNum'], area='Sta_Index_2')
        t.index('tr-nro', ['CLI'], area='Sta_Index_2')
        t.index('TTNum', ['TTNum'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TroTicket')

