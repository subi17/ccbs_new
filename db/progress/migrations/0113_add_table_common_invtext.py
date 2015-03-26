from gearbox.migrations import Migration

class AddInvText(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('InvText', area='Sta_Data_32',
                       label='Invoice Texts',
                       dump_name='invtext',
                       desc='Texts to be printed to invoices')
        t.column('ITNum', 'integer', format='>>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='Internal sequenco no. of an Invoice Text element')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From Date',
                 column_label='From',
                 help='First effective date')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To Date',
                 column_label='To',
                 help='Last effective date')
        t.column('Target', 'character', format='x(16)', initial='',
                 column_label='Target',
                 help='Target into which this text element belongs to',
                 description='One of pre-defined targets (hard-coded by programmer)')
        t.column('KeyValue', 'character', mandatory=True, format='x(16)', initial='',
                 column_label='KeyValue',
                 help='Unique value of hosting record',
                 description='Key value of hosting rec converted into formatted string expression')
        t.column('Position', 'integer', format='9', initial='1',
                 label='Pos',
                 column_label='Pos',
                 help='Position on invoice 1:InvStart 2:InvEnd 3:SectStart 4:SectEnd')
        t.column('TxtTitle', 'character', format='x(40)', initial='',
                 label='Title',
                 column_label='Title',
                 help='Short header/description of message (not to be printed)')
        t.column('InvText', 'character', format='x(255)', initial='',
                 label='Text',
                 column_label='Text',
                 help='Information text', view_as='VIEW-AS EDITOR SIZE 60 BY 7 scrollbar-vertical')
        t.column('Language', 'integer', format='>>9', initial='0',
                 column_label='Language',
                 help='Text language code')
        t.column('Report', 'integer', format='>>9', initial='0',
                 label='Printed To',
                 column_label='Printed',
                 help='Where to print; 0=invoice, 1=reminder, 2=eMail')
        t.column('EPLForm', 'character', initial='',
                 label='EPL Form',
                 column_label='EPLForm',
                 help='EPL form number.')
        t.column('Attachment', 'character', format='x(30)', initial='',
                 label='Attachments',
                 column_label='Attachm.',
                 help='Attachments to eMail')
        t.column('InfoType', 'character', initial='',
                 label='Information Type',
                 column_label='Type',
                 help='Type of information text')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('RemLevel', 'integer', format='>9', initial='0',
                 label='Reminder Level',
                 column_label='RemLevel',
                 help='Reminder level, i.e. how many times invoice has been reminded')
        t.column('LetterClass', 'integer', format='9', initial='0',
                 label='Letter Class',
                 column_label='LCLass',
                 help='Letter class for EPL')
        t.column('AddrTarget', 'integer', format='9', initial='0',
                 label='Address Target',
                 column_label='AddrTarg',
                 help='Address where letter is sent to')
        t.column('MainTitle', 'character', format='x(50)', initial='',
                 label='Main Title',
                 help='Main title for letter')
        t.column('PrintOnce', 'logical', initial='no',
                 label='Print Only Once',
                 column_label='Once',
                 help='Use this text only once per customer')
        t.column('FilePrefix', 'character', initial='',
                 label='File Prefix',
                 column_label='Prefix',
                 help='Prefix for printing file name')
        t.column('CStateList', 'character', format='x(12)', initial='',
                 label='Claim States',
                 column_label='Cl.State',
                 help='List of claiming states for which this text is used')
        t.index('FromDate', ['Brand', ('FromDate', 'DESCENDING'), 'Target', 'KeyValue'], area='Sta_Index_2')
        t.index('ITNum', ['ITNum'], area='Sta_Index_2',
                unique=True)
        t.index('target', ['Brand', 'Target', 'KeyValue', 'FromDate'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('InvText')

