from gearbox.migrations import Migration

class AddCLI(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CLI', area='Sta_Data_256',
                       label='CLI',
                       dump_name='cli',
                       desc='CLI numbers')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer number')
        t.column('CLI', 'character', format='x(16)', initial='',
                 label='A-Sub No.',
                 column_label='A-Sub.No',
                 help='Customer\'s A-subscriber Number with Area Code')
        t.column('Date', 'date', format='99-99-99', initial='today',
                 label='Created',
                 column_label='Created',
                 help='Date when a-number was created / updated')
        t.column('OwnerName', 'character', format='x(30)', initial='',
                 label='Owner',
                 column_label='Owner\'s name',
                 help='Name of A-subscriber number\'s owner')
        t.column('MthLimit', 'integer', format='>,>>>,>>9', initial='0',
                 label='ASub. mth limit',
                 column_label='ASub. mth limit',
                 help='A-Subscribers monthly limit')
        t.column('ValueLimit', 'integer', format='>,>>>,>>9', initial='0',
                 label='ASub. limit',
                 column_label='ASub. limit',
                 help='A-Subscribers monthly limit')
        t.column('Ref', 'character', format='x(12)', initial='',
                 label='RefNum',
                 column_label='RefNum',
                 help='Reference number for additional usage')
        t.column('Active', 'logical', format='Y/N', initial='no',
                 label='Open',
                 column_label='Open',
                 help='Is this a-number open (ie. can this number call) ?')
        t.column('Pwd', 'integer', format='9999', initial='0',
                 label='Password',
                 column_label='Password',
                 help='ASUB password')
        t.column('CrStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Time stamp of connection')
        t.column('ClStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Closed',
                 column_label='Closed',
                 help='Time stamp of disconnection')
        t.column('CallLimit', 'integer', format='>,>>>,>>9', initial='0',
                 label='ASub. call limit',
                 column_label='ASub. call limit',
                 help='A-Subscribers call limit')
        t.column('BillTarget', 'integer', format='>9', initial='0',
                 label='Billing Target',
                 column_label='Bill.Targ',
                 help='Customer\'s billing target')
        t.column('SerNum', 'integer', mandatory=True, format='zzzzzzz9', initial='0',
                 column_label='SerNum',
                 help='Consecutive number (sequence) of series')
        t.column('Contract', 'character', initial='',
                 label='Contract ID',
                 column_label='ContrID')
        t.index('CLI', ['CLI', 'CustNum'], area='Sta_Index_2',
                primary=True)
        t.index('Contract', ['Contract'], area='Sta_Index_2')
        t.index('CustNum', ['CustNum', 'BillTarget', 'CLI'], area='Sta_Index_2')
        t.index('SerNum', ['SerNum', 'CLI'], area='Sta_Index_2')
        t.index('Valid', ['CLI', ('CrStamp', 'DESCENDING'), ('ClStamp', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('CLI')

