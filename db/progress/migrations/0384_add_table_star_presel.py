from gearbox.migrations import Migration

class AddPresel(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('Presel', area='Schema Area',
                       label='Preselection transactions',
                       dump_name='presel',
                       desc='Preselction transactions')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('CLI', 'character', format='x(20)', initial='',
                 label='Subscriber No',
                 column_label='Subscriber No',
                 help='Subscriber (Telephone) number with area code')
        t.column('PsType', 'integer', format='9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='Type of presel. 0):None 1):Nat 2):Intn\'l 3):Both',
                 valexp='INPUT pstype < 4',
                 valmsg='Type of preselction MUST be 0 ... 3 !')
        t.column('PsSeq', 'integer', format='zzzzzz9', initial='0',
                 label='Sequence',
                 column_label='Seq',
                 help='Internal sequence no for a transaction')
        t.column('Orderer', 'character', format='x(60)', initial='',
                 label='Name of Orderer',
                 column_label='Orderer',
                 help='Name of person who made this order at customer')
        t.column('ReturnCode', 'integer', format='>9', initial='0',
                 label='Rc',
                 column_label='Rc',
                 help='Return Code')
        t.column('ErrText', 'character', format='x(20)', initial='',
                 label='Error expl',
                 column_label='Expl',
                 help='Additional explanation of error')
        t.column('AuthNo', 'character', format='x(10)', initial='',
                 column_label='AuthNo',
                 help='Number or code of authorization')
        t.column('AuthDate', 'date', format='99-99-99',
                 column_label='AuthDate',
                 help='Date of Authorisation')
        t.column('CrStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='TimeStamp: created')
        t.column('ChStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Changed',
                 column_label='Changed',
                 help='Date and Time When Preselect record was last changed',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('ConfDate', 'date', format='99-99-99',
                 label='Date of confirm',
                 column_label='ConfDate',
                 help='Date of confirmation')
        t.column('FileSeq1', 'integer', format='>>>>>9', initial='0',
                 label='OutFileSeq',
                 column_label='OutFileSeq',
                 help='Sequence of OutputFile')
        t.column('SentDate', 'date', format='99-99-99',
                 column_label='SentDate',
                 help='Date when preselection has been sent')
        t.column('FileSeq2', 'integer', format='>>>>>9', initial='0',
                 label='InFileSeq',
                 column_label='InFileSeq',
                 help='Sequence of InputFile')
        t.column('AuthNo2', 'character', format='x(10)', initial='',
                 label='AuthNo',
                 column_label='AuthNo',
                 help='Second Authorisation code')
        t.column('CustName', 'character', format='x(30)', initial='',
                 label='Customer\'s name',
                 column_label='Customer\'s name')
        t.column('OrgCode', 'character', format='x(11)', initial='',
                 label='Org/pers. nr',
                 column_label='Org/pers. nr',
                 help='Customer\'s organisation number or personal number')
        t.index('CLI', ['CLI'], area='Schema Area',
                primary=True, unique=True)
        t.index('CustNum', ['CustNum', 'CLI'], area='Schema Area')
        t.index('fileseq', ['FileSeq1'], area='Schema Area')

    def down(self):
        self.drop_table('Presel')

